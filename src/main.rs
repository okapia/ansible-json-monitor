#![allow(clippy::upper_case_acronyms)]

extern crate clap;
extern crate chrono;
extern crate libflate;
extern crate rayon;
extern crate serde_json;
extern crate toml;
extern crate unicode_width;

use chrono::prelude::*;
use itertools::{Either, Itertools};
use libflate::gzip::Decoder;
use rayon::prelude::*;
use regex::Regex;
use std::convert::TryFrom;
use std::error::Error;
use std::ffi::OsString;
use std::fmt;
use std::fs;
use std::fs::File;
use std::io::{self, Seek, SeekFrom, Write};
use std::iter;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use clap::{Parser, ColorChoice};
use toml::Value;
use unicode_width::UnicodeWidthStr;

mod tabulate;

#[derive(Debug)]
struct Summary {
    changed: i64,
    failures: i64,
    ignored: i64,
    rescued: i64,
    ok: i64,
    skipped: i64,
    end: DateTime<FixedOffset>,
    host: String,
}

#[derive(Default, Debug, PartialOrd, Ord, PartialEq, Eq)]
struct Bytes(i64);
impl std::fmt::Display for Bytes {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match ['K', 'M', 'G', 'T', 'P']
            .iter()
            .fold((self.0, 0, None), |acc, x| {
                if acc.0 > 1000 {
                    (acc.0 / 1024, acc.0 % 1024 / 100 % 10, Some(x))
                } else {
                    acc
                }
            }) {
            (bare, _, None) => write!(f, "{}", bare),
            (units, 0, Some(u)) => f.pad(format!("{}{}", units, u).as_str()),
            (units, dec, Some(u)) => f.pad(format!("{}.{}{}", units, dec, u).as_str()),
        }
    }
}

impl std::str::FromStr for Bytes {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut l = s.len() - 1;
        // Support suffixes for Kb, Mb, Gigs etc.
        let mult = match s.as_bytes()[l] {
            b'K' => 1 << 10,
            b'M' => 1 << 20,
            b'G' => 1 << 30,
            b'T' => 1 << 40,
            _ => {
                l += 1;
                1
            }
        };
        let b = mult * s[..l].parse::<i64>()?;

        Ok(Bytes(b))
    }
}

#[derive(Default, Debug, PartialOrd, Ord, PartialEq, Eq)]
struct Percentage(i64);
impl std::fmt::Display for Percentage {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.pad(format!("{}%", self.0).as_str())
    }
}

impl std::str::FromStr for Percentage {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // ignore trailing %
        let mut l = s.len();
        if s.as_bytes()[l - 1] == b'%' {
            l -= 1
        }
        let p = s[..l].parse::<i64>()?;

        Ok(Percentage(p))
    }
}

/// representation of fields in the main summary view
#[derive(Debug)]
enum Field {
    Changed,
    Date,
    Failures,
    Hostname,
    Ok,
    Skipped,
}

impl std::str::FromStr for Field {
    type Err = Fail;
    fn from_str(arg: &str) -> std::result::Result<Self, Fail> {
        let arg = &arg.to_ascii_lowercase()[..];
        if "date".starts_with(arg) {
            Ok(Field::Date)
        } else if "changed".starts_with(arg) {
            Ok(Field::Changed)
        } else if "failures".starts_with(arg) {
            Ok(Field::Failures)
        } else if "hostname".starts_with(arg) {
            Ok(Field::Hostname)
        } else if "ok".starts_with(arg) {
            Ok(Field::Ok)
        } else if "skipped".starts_with(arg) {
            Ok(Field::Skipped)
        } else {
            Err(Fail::Msg(format!("unrecognised sort field: {}", arg)))
        }
    }
}

#[derive(Debug, Clone)]
struct Facts {
    host: String,
    age: DateTime<FixedOffset>,
    value: serde_json::Value,
}

/// representation of first command-line argument
#[derive(Copy, Clone, Debug, PartialEq)]
enum Table {
    Cpu,
    Df,
    Docker,
    Net,
    OS,
    Packages,
    Python,
    Services,
    Swap,
    Summary,
    Tasks,
    Virt,
    Zfs,
    Zpool,
}

impl TryFrom<&OsString> for Table {
    type Error = Fail;
    fn try_from(value: &OsString) -> Result<Self, Self::Error> {
        let a = match value.to_str() {
            Some(val) => val.to_ascii_lowercase(),
            None => return Err(Fail::Msg("unrecognised fact type".to_string())),
        };
        let arg = &a[..];
        if "df".starts_with(arg) || "fs".starts_with(arg) {
            Ok(Table::Df)
        } else if "net".starts_with(arg) {
            Ok(Table::Net)
        } else if "os".starts_with(arg) {
            Ok(Table::OS)
        } else if "cpu".starts_with(arg) {
            Ok(Table::Cpu)
        } else if "packages".starts_with(arg) {
            Ok(Table::Packages)
        } else if "summary".starts_with(arg) {
            Ok(Table::Summary)
        } else if "services".starts_with(arg) {
            Ok(Table::Services)
        } else if "swap".starts_with(arg) {
            Ok(Table::Swap)
        } else if "python".starts_with(arg) {
            Ok(Table::Python)
        } else if "tasks".starts_with(arg) {
            Ok(Table::Tasks)
        } else if "virt".starts_with(arg) {
            Ok(Table::Virt)
        } else if "docker".starts_with(arg) {
            Ok(Table::Docker)
        } else if "zpool".starts_with(arg) {
            Ok(Table::Zpool)
        } else if "zfs".starts_with(arg) {
            Ok(Table::Zfs)
        } else {
            Err(Fail::Msg(format!("unrecognised fact type: {}", arg)))
        }
    }
}

#[derive(Parser)]
#[command(
    name = "ajmon",
    about = "Simple ansible monitoring based in JSON files.",
    color = ColorChoice::Never,
)]
struct Opt {
    /// specify an input JSON file
    #[arg(short = 'f', num_args = 1)]
    infile: Vec<PathBuf>,
    /// specify output fields
    #[arg(short = 'o')]
    fields: Option<OsString>,
    /// specify field to order summary by
    #[arg(short = 's')]
    order: Option<OsString>,
    /// suppress header line when printing tables
    #[arg(short = 'H')]
    no_header: bool,
    /// produce JSON output for tasks and facts
    #[arg(short = 'J')]
    json: bool,
    /// specify filters to apply to tables
    #[arg()]
    filters: Vec<OsString>,
}

fn getcfg() -> Result<String, Fail> {
    match fs::read_to_string("/etc/ansible-json-monitor.conf") {
        Ok(s) => Ok(s.parse::<Value>()?["path"].as_str().unwrap().to_string()),
        Err(_) => Ok("/var/log/ansible".to_string()),
    }
}

/// representation of the outcome of an ansible task
#[derive(Clone, Copy, Debug, Default, PartialOrd, Ord, PartialEq, Eq)]
enum Status {
    Ok,
    Changed,
    #[default]
    Failed,
}

impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (self, f.alternate()) {
            (Status::Ok, false) => f.pad(" "),
            (Status::Failed, false) => f.pad("F"),
            (Status::Changed, false) => f.pad("C"),
            (Status::Ok, true) => f.pad("ok"),
            (Status::Failed, true) => f.pad("failed"),
            (Status::Changed, true) => f.pad("changed"),
        }
    }
}

impl std::str::FromStr for Status {
    type Err = Fail;
    fn from_str(arg: &str) -> std::result::Result<Self, Self::Err> {
        let arg = &arg.to_ascii_lowercase()[..];
        if "failed".starts_with(arg) {
            Ok(Status::Failed)
        } else if "changed".starts_with(arg) {
            Ok(Status::Changed)
        } else if "ok".starts_with(arg) {
            Ok(Status::Ok)
        } else {
            Err(Fail::Msg(format!("unrecognised task status: {}", arg)))
        }
    }
}

#[derive(Debug)]
enum Fail {
    Msg(String),
    OsMsg(OsString),
    Missing(&'static str),
    Type(&'static str, &'static str),
}

impl fmt::Display for Fail {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Msg(m) => write!(fmt, "error: {}", m),
            Self::OsMsg(m) => write!(fmt, "error: {:?}", m),
            Self::Missing(element) => write!(
                fmt,
                "error: json data didn't match assumptions: element {} missing",
                element
            ),
            Self::Type(expected, element) => write!(
                fmt,
                "error: json data didn't match assumptions: expected \"{}\" to be of type {}",
                element, expected
            ),
        }
    }
}
impl Error for Fail {}

/// Error types for which messages can be passed on unchanged.
trait DirectErrMsg: ToString {}
impl DirectErrMsg for std::io::Error {}
impl DirectErrMsg for toml::de::Error {}
impl DirectErrMsg for regex::Error {}

impl<T: DirectErrMsg> From<T> for Fail {
    fn from(e: T) -> Self {
        Self::Msg(e.to_string())
    }
}

impl From<std::ffi::OsString> for Fail {
    fn from(s: std::ffi::OsString) -> Self {
        let mut x = OsString::from("could not interpret string: ");
        x.push(s);
        Self::OsMsg(x)
    }
}

impl From<chrono::ParseError> for Fail {
    fn from(_: chrono::ParseError) -> Self {
        Self::Msg("invalid date string in json file".to_string())
    }
}

impl From<serde_json::Error> for Fail {
    fn from(_: serde_json::Error) -> Self {
        Self::Msg("json data not well-formed".to_string())
    }
}

fn dir_contents(dir: &Path) -> Result<Vec<PathBuf>, Fail> {
    Ok(std::fs::read_dir(dir)?
        .filter_map(|x| x.ok())
        .map(|x| x.path())
        .collect())
}

fn open_json_file<P: AsRef<Path>>(filename: P) -> Result<serde_json::Value, Fail> {
    let mut file_reader = File::open(&filename)?;
    match Decoder::new(&file_reader) {
        Err(_) => {
            file_reader.seek(SeekFrom::Start(0))?;
            Ok(serde_json::from_reader(file_reader)?)
        }
        Ok(r) => Ok(serde_json::from_reader(r)?),
    }
}

fn gather_facts_errors(
    filename: &Path,
    fact_class: &str,
) -> Result<Vec<Result<Facts, Fail>>, Fail> {
    let json = match open_json_file(filename) {
        Ok(j) => j,
        Err(e) => {
            return Ok(vec![Err(Fail::Msg(format!(
                "{}: {}",
                filename.display(),
                e
            )))])
        }
    };
    let (fact_alias, info) = match fact_class {
        // alternate name for module
        "setup" => (Some("gather_facts"), Some("ansible_facts")),
        "docker_image_info" => (Some("podman_image_info"), None),
        _ => (None, Some("ansible_facts")),
    };
    let mut facts = Vec::new();
    let (successes, failures): (Vec<_>, Vec<_>) = json
        .array("plays")?
        .iter()
        .flat_map(|x| match x.array("tasks") {
            Ok(v) => v.iter().map(Ok).collect(),
            Err(e) => vec![Err(e)],
        })
        .partition_map(|r| match r {
            Ok(v) => Either::Left(v),
            Err(v) => Either::Right(v),
        });
    for fail in &failures {
        eprintln!("{}", fail);
    }
    for task in successes {
        let start = DateTime::parse_from_rfc3339(
            task.object("task")?.object("duration")?.string("start")?,
        )?;
        for (host, value) in task
            .object("hosts")?
            .as_object()
            .ok_or(Fail::Type("hosts", "object"))?
            .iter()
            .filter(|(_host, value)| {
                value.string("action").ok().is_some_and(|x| x == fact_class || Some(x) == fact_alias)
                    && !value.bool("failed").unwrap_or(false) // not failed
                    && !value.bool("skipped").unwrap_or(false) // not skipped
                    && value // not filtered
                        .object("invocation")
                        .and_then(|x| x.object("module_args"))
                        .and_then(|x| x.get("filter").ok_or(Fail::Missing("filter")))
                        .ok()
                        .is_none_or(|x| match x {
                            serde_json::Value::Array(a) => a.is_empty(),
                            serde_json::Value::String(s) => s == "*",
                            _ => true,
                        })
            })
        {
            match (value.get("results"), info) {
                (Some(results), Some(fact_key)) => {
                    for factoid in results.as_array().ok_or(Fail::Type("results", "array"))? {
                        facts.push(Ok(Facts {
                            host: host.to_string(),
                            age: start,
                            value: factoid.object(fact_key)?.clone(),
                        }));
                    }
                }
                (None, Some(fact_key)) => facts.push(Ok(Facts {
                    host: host.to_string(),
                    age: start,
                    value: value.object(fact_key)?.clone(),
                })),
                (None, None) => facts.push(Ok(Facts {
                    host: host.to_string(),
                    age: start,
                    value: value.clone(),
                })),
                _ => unreachable!(),
            }
        }
    }
    Ok(facts)
}

fn gather_facts(dir: &Path, fact_class: &str) -> Vec<Result<Facts, Fail>> {
    match gather_facts_errors(dir, fact_class) {
        Ok(v) => v,
        Err(e) => vec![Err(e)],
    }
}

fn distill_fs_facts(
    facts: Vec<Facts>,
    fields: Option<OsString>,
    sort_by: Option<OsString>,
    filters: Vec<String>,
    header: bool,
) -> Result<(), Fail> {
    tabulate!(fields, sort_by = (HOST), filters, header, facts.iter()
        .map(|fact| -> Result<_,Fail> {
            Ok(fact.value.array("ansible_mounts")?.iter().zip(iter::repeat(&fact.host[..])))
        }).filter_map(Result::ok).flatten() // throwing away errors here
        .filter(|(f,_)| f.number("block_total").unwrap_or(1) != 0 &&
                !f.string("fstype").unwrap_or("").starts_with("nfs")), {
        + < HOST: String => { &|(_,host): (_,&str)| Ok(host.to_string()) },
        + < DEVICE: String => { &|(mount,_): (&serde_json::Value,_)| Ok(mount.string("device")?.to_string()) },
        + < FSTYPE: String => { &|(mount,_): (&serde_json::Value,_)| Ok(mount.string("fstype")?.to_string()) },
        + > TOTAL: Bytes => { &|(mount,_): (&serde_json::Value,_)| Ok(Bytes(mount.number("block_total")?)) },
        - > SIZE: Bytes => { &|(mount,_): (&serde_json::Value,_)| Ok(Bytes(mount.number("block_size")?)) },
        + > USED: Bytes => { &|(mount,_): (&serde_json::Value,_)| Ok(Bytes(mount.number("block_used")?)) },
        + > AVAIL: Bytes => { &|(mount,_): (&serde_json::Value,_)| Ok(Bytes(mount.number("block_available")?)) },
        + > CAPACITY: Percentage => { &|(mount,_): (&serde_json::Value,_)|
              Ok(Percentage((100 * mount.number("block_used")?) / mount.number("block_total")?)) },
        + < MOUNTPOINT: String => { &|(mount,_): (&serde_json::Value,_)| Ok(mount.string("mount")?.to_string()) }
    });
    Ok(())
}

fn distill_net_facts(
    facts: Vec<Facts>,
    fields: Option<OsString>,
    sort_by: Option<OsString>,
    filters: Vec<String>,
    header: bool,
) -> Result<(), Fail> {
    tabulate!(fields, sort_by = (HOST), filters, header, facts.iter()
        .filter_map(|fact| fact.value.as_object().map(|s| s.iter().zip(iter::repeat(&fact.host[..]))))
        .flatten()
        .filter(|((key,device),_)| *key != "ansible_default_ipv4" && device.string("macaddress").is_ok()), {
        + < HOST: String => { &|(_,host): (_,&str)| Ok(host.to_string()) },
        + < INTERFACE: String => { &|((_,d),_): ((_,&serde_json::Value),_)| Ok(d.string("device")?.to_string()) },
        + < IP: String => { &|((_,d),_): ((_,&serde_json::Value),_)| d.object("ipv4").and_then(|x| x.string("address")).or(Ok("")).map(|x| x.to_string()) },
        - < NETMASK: String => { &|((_,d),_): ((_,&serde_json::Value),_)| d.object("ipv4").and_then(|x| x.string("netmask")).or(Ok("")).map(|x| x.to_string()) },
        - < IPV6: String => { &|((_,d),_): ((_,&serde_json::Value),_)| Ok(d.array("ipv6").ok().and_then(|w| w.first().and_then(|x| x.string("address").ok())).unwrap_or("").to_string()) },
        + < TYPE: String => { &|((_,d),_): ((_,&serde_json::Value),_)| Ok(d.string("module").unwrap_or_else(|_| d.string("type").unwrap_or("")).to_string()) },
        + > SPEED: i64 => { &|((_,d),_): ((_,&serde_json::Value),_)| Ok(d.number("speed").unwrap_or(0)) },
        - > MTU: i64 => { &|((_,d),_): ((_,&serde_json::Value),_)| Ok(d.number_coerce("mtu").unwrap_or(0)) },
        + < MAC: String => { &|((_,d),_): ((_,&serde_json::Value),_)| Ok(d.string("macaddress")?.to_string()) },
    });
    Ok(())
}

fn distill_os_facts(
    facts: Vec<Facts>,
    fields: Option<OsString>,
    sort_by: Option<OsString>,
    filters: Vec<String>,
    header: bool,
) -> Result<(), Fail> {
    tabulate!(fields, sort_by = (HOST), filters, header, facts.iter(), {
        + < HOST: String => { &|fact: &Facts| Ok(fact.host.clone()) },
        + < SYSTEM: String => { &|fact: &Facts| Ok(fact.value.string("ansible_system")?.to_string()) },
        + < DISTRIBUTION: String => { &|fact: &Facts| Ok(fact.value.string("ansible_distribution")?.to_string()) },
        + > VERSION: String => { &|fact: &Facts| Ok(fact.value.string("ansible_distribution_version")?.to_string()) },
        - > MAJOR: String => { &|fact: &Facts| Ok(fact.value.string("ansible_distribution_major_version").unwrap_or("unknown").to_string()) },
        - < FAMILY: String => { &|fact: &Facts| Ok(fact.value.string("ansible_os_family")?.to_string()) },
        + < KERNEL: String => { &|fact: &Facts| Ok(fact.value.string("ansible_kernel")?.to_string()) },
        - < PACKAGE: String => { &|fact: &Facts| Ok(fact.value.string("ansible_pkg_mgr")?.to_string()) },
        - < SERVICE: String => { &|fact: &Facts| Ok(fact.value.string("ansible_service_mgr")?.to_string()) },
        - > UPTIME: i64 => { &|fact: &Facts| fact.value.number("ansible_uptime_seconds") },
    });
    Ok(())
}

fn distill_python_facts(
    facts: Vec<Facts>,
    fields: Option<OsString>,
    sort_by: Option<OsString>,
    filters: Vec<String>,
    header: bool,
) -> Result<(), Fail> {
    tabulate!(fields, sort_by = (HOST), filters, header, facts.iter()
        .filter_map(|fact| fact.value.object("ansible_python").map(|o| (fact,o)).ok()), {
        + < HOST: String => { &|(fact,_): (&Facts,_)| Ok(fact.host.clone()) },
        + < IMPLEMENTATION: String => { &|(_,py): (_, &serde_json::Value)| Ok(py.string("type")?.to_string()) },
        + > VERSION: String => { &|(fact,_): (&Facts,_)| Ok(fact.value.string("ansible_python_version")?.to_string()) },
        + < BINARY: String => { &|(_,py): (_, &serde_json::Value)| Ok(py.string("executable")?.to_string()) },
        - < MAJOR: i64 => { &|(_,py): (_, &serde_json::Value)| py.object("version")?.number("major") },
        - < MINOR: i64 => { &|(_,py): (_, &serde_json::Value)| py.object("version")?.number("minor") },
        - < MICRO: i64 => { &|(_,py): (_, &serde_json::Value)| py.object("version")?.number("micro") },
        - < LEVEL: String => { &|(_,py): (_, &serde_json::Value)| Ok(py.object("version")?.string("releaselevel")?.to_string()) },
    });
    Ok(())
}

fn distill_cpu_facts(
    facts: Vec<Facts>,
    fields: Option<OsString>,
    sort_by: Option<OsString>,
    filters: Vec<String>,
    header: bool,
) -> Result<(), Fail> {
    tabulate!(fields, sort_by = (HOST), filters, header, facts.iter(), {
        + < HOST: String => { &|fact: &Facts| Ok(fact.host.clone()) },
        + < ARCH: String => { &|fact: &Facts| Ok(fact.value.string("ansible_architecture")?.to_string()) },
        - < MACHINE: String => { &|fact: &Facts| Ok(fact.value.string("ansible_machine")?.to_string()) },
        + > MEMORY: Bytes => { &|fact: &Facts| Ok(Bytes(fact.value.number("ansible_memtotal_mb")? << 20)) },
        + > CORES: i64 => { &|fact: &Facts| Ok(fact.value.number("ansible_processor_cores").unwrap_or(1)) },
        + > COUNT: i64 => { &|fact: &Facts| fact.value.number_coerce("ansible_processor_count") },
        + < PRODUCT: String => { &|fact: &Facts| Ok(fact.value.string("ansible_product_name")?.to_string()) },
        - < FORM: String => { &|fact: &Facts| Ok(fact.value.string("ansible_form_factor")?.to_string()) },
    });
    Ok(())
}

fn distill_swap_facts(
    facts: Vec<Facts>,
    fields: Option<OsString>,
    sort_by: Option<OsString>,
    filters: Vec<String>,
    header: bool,
) -> Result<(), Fail> {
    tabulate!(fields, sort_by = (HOST), filters, header, facts.iter(), {
        + < HOST: String => { &|fact: &Facts| Ok(fact.host.clone()) },
        + > MEMORY: Bytes => { &|fact: &Facts| Ok(Bytes(fact.value.number("ansible_memtotal_mb")? << 20)) },
        - > ALLOC: Bytes => { &|fact: &Facts| Ok(Bytes(fact.value.number("ansible_swap_allocated_mb").unwrap_or(0) << 20)) },
        - > RESERVED: Bytes => { &|fact: &Facts| Ok(Bytes(fact.value.number("ansible_swap_reserved_mb").unwrap_or(0) << 20)) },
        + > FREE: Bytes => { &|fact: &Facts| Ok(Bytes(fact.value.number("ansible_swapfree_mb")? << 20)) },
        + > TOTAL: Bytes => { &|fact: &Facts| Ok(Bytes(fact.value.number("ansible_swaptotal_mb")? << 20)) },
    });
    Ok(())
}

fn distill_virt_facts(
    facts: Vec<Facts>,
    fields: Option<OsString>,
    sort_by: Option<OsString>,
    filters: Vec<String>,
    header: bool,
) -> Result<(), Fail> {
    tabulate!(fields, sort_by = (HOST), filters, header, facts.iter(), {
        + < HOST: String => { &|fact: &Facts| Ok(fact.host.clone()) },
        + < ROLE: String => { &|fact: &Facts| Ok(fact.value.string("ansible_virtualization_role")?.to_string()) },
        + > TYPE: String => { &|fact: &Facts| Ok(fact.value.string("ansible_virtualization_type")?.to_string()) },
    });
    Ok(())
}

fn distill_package_facts(
    facts: Vec<Facts>,
    fields: Option<OsString>,
    sort_by: Option<OsString>,
    filters: Vec<String>,
    header: bool,
) -> Result<(), Fail> {
    tabulate!(fields, sort_by = (HOST), filters, header, facts.iter()
        .map(|fact| Some(fact.value
            .as_object()?
            .get("packages")?
            .as_object()?
            .iter()
            .map(|(_,pkg)| pkg.as_array())
            .filter_map(|n| n)
            .flatten()
            .zip(iter::repeat(&fact.host[..]))))
        .filter_map(|n| n)
        .flatten(), {
        + < HOST: String => { &|(_,host): (_,&str)| Ok(host.to_string()) },
        + < NAME: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("name")?.to_string()) },
        + > VERSION: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("version")?.to_string()) },
        + > RELEASE: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("release")?.to_string()) },
        - > EPOCH: i64 => { &|(val,_): (&serde_json::Value,_)| Ok(val.number("epoch").unwrap_or(0i64)) },
        - < ARCH: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("arch").unwrap_or("(none)").to_string()) },
        - < SOURCE: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("source")?.to_string()) },
    });
    Ok(())
}

fn distill_service_facts(
    facts: Vec<Facts>,
    fields: Option<OsString>,
    sort_by: Option<OsString>,
    filters: Vec<String>,
    header: bool,
) -> Result<(), Fail> {
    tabulate!(fields, sort_by = (HOST,NAME), filters, header, facts.iter()
        .map(|fact| Some(fact.value
            .as_object()?
            .get("services")?
            .as_object()?
            .iter()
            .zip(iter::repeat(&fact.host[..]))))
        .filter_map(|n| n)
        .flatten(), {
        + < HOST: String => { &|(_,host): (_,&str)| Ok(host.to_string()) },
        + < NAME: String => { &|((key,_),_): ((&String,_),_)| Ok(key.clone()) },
        + < STATE: String => { &|((_,val),_): ((_,&serde_json::Value),_)| Ok(val.string("state")?.to_string()) },
        - < STATUS: String => { &|((_,val),_): ((_,&serde_json::Value),_)| Ok(val.string("status")?.to_string()) },
        - < SOURCE: String => { &|((_,val),_): ((_,&serde_json::Value),_)| Ok(val.string("source")?.to_string()) },
    });
    Ok(())
}

fn distill_docker_info(
    facts: Vec<Facts>,
    fields: Option<OsString>,
    sort_by: Option<OsString>,
    filters: Vec<String>,
    header: bool,
) -> Result<(), Fail> {
    tabulate!(fields, sort_by = (HOST,CREATED), filters, header, facts.iter()
        .filter_map(|fact| fact.value.array("images")
        .map(|s| s.iter().zip(iter::repeat(&fact.host[..]))).ok()).flatten(), {
        + < HOST: String => { &|(_,host): (_,&str)| Ok(host.to_string()) },
        + < REPOSITORY: String => { &|(val,_): (&serde_json::Value,_)| {
            if let Ok(repos) = val.array("RepoTags")
                    .and_then(|x| if x.is_empty() { Err(Fail::Msg("empty".to_string())) } else { Ok(x) }) {
                let repo = repos[0].as_str().ok_or(Fail::Type("string", "what"))?;
                if let Some(begin) = repo.rfind(':') {
                    Ok(repo[..begin].to_string())
                } else {
                    Ok(repo.to_string())
                }
            } else {
                Ok("none".to_string())
            }
        } },
        + < TAG: String => { &|(val,_): (&serde_json::Value,_)| {
            if let Ok(repos) = val.array("RepoTags")
                    .and_then(|x| if x.is_empty() { Err(Fail::Msg("empty".to_string())) } else { Ok(x) }) {
                let repo = repos[0].as_str().ok_or(Fail::Type("string", "what"))?;
                if let Some(begin) = repo.rfind(':') {
                    Ok(repo[begin + 1..].to_string())
                } else {
                    Err(Fail::Msg("tag not found in docker repository".to_string()))
                }
            } else {
                Ok("none".to_string())
            }
        } },
        + < ID: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("Id")?[..12].to_string()) },
        + < CREATED: String => { &|(val,_): (&serde_json::Value,_)| Ok(DateTime::parse_from_rfc3339(val.string("Created")?)?.format("%F %T").to_string()) },
    });
    Ok(())
}

fn distill_zpool_facts(
    facts: Vec<Facts>,
    fields: Option<OsString>,
    sort_by: Option<OsString>,
    filters: Vec<String>,
    header: bool,
) -> Result<(), Fail> {
    tabulate!(fields, sort_by = (HOST,NAME), filters, header, facts.iter()
        .filter_map(|fact| fact.value.array("ansible_zfs_pools")
        .map(|s| s.iter().zip(iter::repeat(&fact.host[..]))).ok()).flatten(), {
        + < HOST: String => { &|(_,host): (_,&str)| Ok(host.to_string()) },
        + < NAME: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("name")?.to_string()) },
        + > SIZE: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("size")?.to_string()) },
        + > ALLOC: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("allocated")?.to_string()) },
        + > FREE: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("free")?.to_string()) },
        + > FRAG: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("fragmentation")?.to_string()) },
        + > CAP: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("capacity")?.to_string()) },
        + > DEDUP: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("dedupratio")?.to_string()) },
        + < HEALTH: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("health")?.to_string()) },
    });
    Ok(())
}

fn distill_zfs_facts(
    facts: Vec<Facts>,
    fields: Option<OsString>,
    sort_by: Option<OsString>,
    filters: Vec<String>,
    header: bool,
) -> Result<(), Fail> {
    tabulate!(fields, sort_by = (HOST,NAME), filters, header, facts.iter()
        .filter_map(|fact| fact.value.array("ansible_zfs_datasets")
        .map(|s| s.iter().zip(iter::repeat(&fact.host[..]))).ok()).flatten(), {
        + < HOST: String => { &|(_,host): (_,&str)| Ok(host.to_string()) },
        + < NAME: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("name")?.to_string()) },
        + > USED: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("used")?.to_string()) },
        + > AVAIL: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("available").unwrap_or("-").to_string()) },
        + > REFER: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("referenced")?.to_string()) },
        - > ATIME: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("atime").unwrap_or("-").to_string()) },
        - > COMPRESSION: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("compression").unwrap_or("-").to_string()) },
        - > COMPRESSRATIO: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("compressratio")?.to_string()) },
        - < DEDUP: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("dedup").unwrap_or("-").to_string()) },
        - < QUOTA: String => { &|(val,_): (&serde_json::Value,_)| Ok(val.string("quota").unwrap_or("-").to_string()) },
        + < MOUNTPOINT: String => { &|(mount,_): (&serde_json::Value,_)| Ok(mount.string("mountpoint").unwrap_or("-").to_string()) }
    });
    Ok(())
}

fn play_end(json: &serde_json::Value) -> Result<chrono::DateTime<chrono::FixedOffset>, Fail> {
    let duration = &json
        .array("plays")?
        .last()
        .ok_or_else(|| Fail::Msg("no plays found in json file".to_string()))?
        .object("play")?
        .object("duration")?;
    Ok(DateTime::parse_from_rfc3339(
        duration
            .string("end")
            .or_else(|_| duration.string("start"))?,
    )?)
}

fn get_summary(dir: &PathBuf) -> Vec<Result<Summary, Fail>> {
    let json = match open_json_file(dir) {
        Ok(j) => j,
        Err(e) => return vec![Err(Fail::Msg(format!("{}: {}", dir.display(), e)))],
    };
    let end = match play_end(&json) {
        Ok(d) => d,
        Err(e) => return vec![Err(Fail::Msg(format!("{}: {}", dir.display(), e)))],
    };
    match json.object("stats").and_then(|o| {
        o.as_object().ok_or_else(|| {
            Fail::Msg(format!(
                "{}: JSON not well-formed - wrong type for element stats",
                dir.display()
            ))
        })
    }) {
        Ok(stats) => stats
            .iter()
            .map(|(host, stats)| -> Result<Summary, Fail> {
                Ok(Summary {
                    changed: stats.number("changed")?,
                    failures: stats.number("failures")?,
                    ignored: stats.number("ignored").unwrap_or(0),
                    rescued: stats.number("rescued").unwrap_or(0),
                    ok: stats.number("ok")?,
                    skipped: stats.number("skipped")?,
                    end,
                    host: host.to_string(),
                })
            })
            .collect(),
        Err(e) => vec![Err(e)],
    }
}

// helpers for getting data from the JSON tree while handling errors
trait ChildGrab {
    fn object(&self, s: &'static str) -> Result<&serde_json::Value, Fail>;
    fn array(&self, s: &'static str) -> Result<&std::vec::Vec<serde_json::Value>, Fail>;
    fn bool(&self, s: &'static str) -> Result<bool, Fail>;
    fn number(&self, s: &'static str) -> Result<i64, Fail>;
    fn number_coerce(&self, s: &'static str) -> Result<i64, Fail>;
    fn string(&self, s: &'static str) -> Result<&str, Fail>;
    fn string_maybe(&self, s: &str) -> Option<&str>;
    fn stringarray(&self, s: &'static str, join: &str) -> Result<Option<String>, Fail>;
}
impl ChildGrab for serde_json::Value {
    fn object(&self, s: &'static str) -> Result<&serde_json::Value, Fail> {
        self.get(s).ok_or(Fail::Missing(s))
    }
    fn array(&self, s: &'static str) -> Result<&std::vec::Vec<serde_json::Value>, Fail> {
        self.get(s)
            .ok_or(Fail::Missing(s))?
            .as_array()
            .ok_or(Fail::Type("array", s))
    }
    fn bool(&self, s: &'static str) -> Result<bool, Fail> {
        self.get(s)
            .ok_or(Fail::Missing(s))?
            .as_bool()
            .ok_or(Fail::Type("boolean", s))
    }
    fn number(&self, s: &'static str) -> Result<i64, Fail> {
        self.get(s)
            .ok_or(Fail::Missing(s))?
            .as_i64()
            .ok_or(Fail::Type("number", s))
    }
    fn number_coerce(&self, s: &'static str) -> Result<i64, Fail> {
        match self.get(s).ok_or(Fail::Missing(s))? {
            serde_json::Value::Number(x) => x.as_i64().ok_or(Fail::Type("number", s)),
            serde_json::Value::String(y) => y
                .as_str()
                .parse::<i64>()
                .map_err(|_| Fail::Type("number", s)),
            _ => Err(Fail::Type("number", s)),
        }
    }
    fn string(&self, s: &'static str) -> Result<&str, Fail> {
        self.get(s)
            .ok_or(Fail::Missing(s))?
            .as_str()
            .ok_or(Fail::Type("string", s))
    }
    fn string_maybe(&self, s: &str) -> Option<&str> {
        // could raise an error if it is present but not a string
        self.get(s).and_then(|x| x.as_str())
    }
    fn stringarray(&self, s: &'static str, join: &str) -> Result<Option<String>, Fail> {
        let x = self
            .get(s)
            .ok_or(Fail::Missing(s))?
            .as_array()
            .ok_or(Fail::Type("array", s))?
            .iter()
            .map(|x| {
                x.as_str()
                    .ok_or_else(|| {
                        Fail::Msg(format!(
                            "JSON not well-formed - found non-string elements in {} array",
                            s
                        ))
                    })
                    .map(|x| x.to_string())
            })
            .collect::<Result<Vec<_>, _>>()?
            .join(join);
        Ok(if x.is_empty() { None } else { Some(x) })
    }
}

fn tasks(
    json: &serde_json::Value,
    hostname: Option<String>,
) -> Result<impl Iterator<Item = (&serde_json::Value, &serde_json::Value)>, Fail> {
    Ok(json
        .array("plays")?
        .iter()
        .map(|play| play.array("tasks"))
        .filter_map(Result::ok) // passing on errors would be better, this filters
        .flatten()
        .filter_map(move |task| {
            if let Ok(hosts) = task.object("hosts") {
                let tasks = match &hostname {
                    Some(h) => hosts.get(h),
                    None => hosts.as_object().unwrap().values().next(),
                }?;
                return if let Some(serde_json::Value::Bool(true)) = tasks.get("skipped") {
                    None
                } else if let Ok(t) = task.object("task") {
                    // passing on the Result would be better
                    Some((tasks, t))
                } else {
                    None
                };
            }
            None
        }))
}

fn task_list(
    entry: &Path,
    hostname: Option<String>,
    fields: Option<OsString>,
    sort_by: Option<OsString>,
    filters: Vec<String>,
) -> Result<(), Fail> {
    let json = open_json_file(entry)?;

    tabulate!(fields, sort_by = (NUMBER), filters, false, tasks(&json, hostname)?.enumerate(), {
        + > NUMBER: usize => { &|(num,_)| Ok(num) },
        + < STATUS: Status => { &|(_,(thishost,_)): (_,(&serde_json::Value,_))|
            Ok(if thishost
                .get("failed")
                .map_or(Ok(false), |x| x.as_bool().ok_or(Fail::Type("bool", "failed")))?
            {
                Status::Failed
            } else if thishost
                .get("changed")
                .ok_or(Fail::Missing("changed"))?
                .as_bool()
                .ok_or(Fail::Type("bool", "changed"))?
            {
                Status::Changed
            } else {
                Status::Ok
            })
        },
        - < ACTION: String => { &|(_,(thishost,_)): (_,(&serde_json::Value,_))| Ok(thishost.string("action")?.to_string()) },
        + < DESCRIPTION: String => { &|(_,(_,task)): (_,(_,&serde_json::Value))| Ok(task.string("name")?.to_string()) }
    });
    Ok(())
}

fn results_command(result: &serde_json::Value) -> Result<(), Fail> {
    if let Some(line) = result
        .string_maybe("cmd")
        .map(|x| x.to_string())
        .or_else(|| result.stringarray("cmd", " ").unwrap_or(None))
    {
        println!("Command: {}\nStatus: {}", line, result.number("rc")?);
    }
    if let Some(stderr) = result.stringarray("stderr_lines", "\n")? {
        println!("stderr:\n{}", stderr);
    }
    if let Some(stdout) = result.stringarray("stdout_lines", "\n")? {
        println!("stdout:\n{}", stdout);
    }
    Ok(())
}

fn results_dest(result: &serde_json::Value) -> Result<(), Fail> {
    println!("File: {}", result.string("dest")?);
    Ok(())
}

fn results_find(result: &serde_json::Value) -> Result<(), Fail> {
    println!("Matched: {}", result.number("matched")?);
    for f in result.array("files")? {
        println!("Path: {}", f.string("path")?);
    }
    Ok(())
}

fn results_get_url(result: &serde_json::Value) -> Result<(), Fail> {
    println!("URL: {}", result.string("url")?);
    println!("File: {}", result.string("dest")?);
    if let Ok(status) = result.number("status_code") {
        println!("Status: {}", status);
    }
    if let Ok(size) = result.number("size") {
        println!("Size: {}", size);
    }
    if let Ok(response) = result.string("response") {
        println!("Response: {}", response);
    }
    Ok(())
}

fn results_include_role(result: &serde_json::Value) -> Result<(), Fail> {
    println!("Role: {}", result.object("include_args")?.string("name")?);
    Ok(())
}

fn results_include_tasks(result: &serde_json::Value) -> Result<(), Fail> {
    // this is missing where items are skipped
    if let Ok(file) = result.string("include") {
        println!("File: {}", file);
    }
    Ok(())
}

fn results_invpath(result: &serde_json::Value) -> Result<(), Fail> {
    let args = result.object("invocation")?.object("module_args")?;
    println!("File: {}", args.string("path")?);
    Ok(())
}

fn results_lineinfile(result: &serde_json::Value) -> Result<(), Fail> {
    let args = result.object("invocation")?.object("module_args")?;
    println!("File: {}", args.string("path")?);
    println!("Line: {:.66}", args.string("line")?);
    Ok(())
}

fn results_name(result: &serde_json::Value) -> Result<(), Fail> {
    println!("Name: {}", result.string("name")?);
    Ok(())
}

fn results_package(result: &serde_json::Value) -> Result<(), Fail> {
    let args = result.object("invocation")?.object("module_args")?;
    if let Some(names) = args.stringarray("name", ", ")? {
        println!("Name{}: {}", if names.len() > 1 { "s" } else { "" }, names);
    }
    if let Ok(msg) = result.string("msg") {
        if !msg.is_empty() {
            println!("Message: {}", msg);
        }
    }
    Ok(())
}

fn results_path(result: &serde_json::Value) -> Result<(), Fail> {
    println!("File: {}", result.string("path")?);
    Ok(())
}

fn results_setup(result: &serde_json::Value) -> Result<(), Fail> {
    let facts = result.object("ansible_facts")?;
    println!("Product: {}", facts.string("ansible_product_name")?);
    println!(
        "Distribution: {} {}",
        facts.string("ansible_distribution")?,
        facts.string("ansible_distribution_version")?
    );
    Ok(())
}

fn results_slurp(result: &serde_json::Value) -> Result<(), Fail> {
    println!("Source: {}", result.string("source")?);
    Ok(())
}

fn results_stat(result: &serde_json::Value) -> Result<(), Fail> {
    let args = result.object("invocation")?.object("module_args")?;
    let stat = result.object("stat")?;
    let exists = stat.bool("exists").unwrap_or(false);
    println!(
        "{} file: {}",
        if exists { "Existing" } else { "Nonexistent" },
        args.string("path")?
    );
    if exists {
        for (x, desc) in [
            ("isblk", "block device"),
            ("ischr", "character device"),
            ("isdir", "directory"),
            ("isfifo", "fifo"),
            ("islnk", "symbolic link"),
            ("isreg", "normal file"),
            ("issock", "socket"),
        ] {
            if stat.bool(x).unwrap_or(false) {
                println!("Type: {}", desc);
                break;
            }
        }
        if let Ok(owner) = stat.string("pw_name") {
            println!("Owner: {}", owner);
        }
        if let Ok(group) = stat.string("gr_name") {
            println!("Group: {}", group);
        }
        print!("Permissions: ");
        for perm in [
            "rusr", "wusr", "xusr", "rgrp", "wgrp", "xgrp", "roth", "woth", "xoth",
        ] {
            print!(
                "{}",
                if stat.bool(perm).unwrap_or(false) {
                    perm.chars().next().unwrap()
                } else {
                    '-'
                }
            );
        }
        println!(" ({})", stat.string("mode")?);
    }
    Ok(())
}

fn results_nop(_result: &serde_json::Value) -> Result<(), Fail> {
    Ok(())
}

fn task<P: AsRef<Path>>(
    entry: &P,
    hostname: Option<String>,
    item: u32,
    opt_json: bool,
) -> Result<(), Fail> {
    let json = open_json_file(entry)?;
    let (thishost, task) = tasks(&json, hostname)?
        .nth(item as usize)
        .ok_or_else(|| Fail::Msg(format!("{}: task not found", item)))?;
    if opt_json {
        println!("{:#}", thishost);
        return Ok(());
    }

    let status = if thishost
        .get("failed")
        .is_some_and(|x| x.as_bool().unwrap())
    {
        Status::Failed
    } else if thishost.bool("changed")? {
        Status::Changed
    } else {
        Status::Ok
    };

    let act = thishost.string("action")?;
    let name = task.string("name")?;
    let duration = task.object("duration")?;
    let end = DateTime::parse_from_rfc3339(duration.string("end")?)?;
    let start = DateTime::parse_from_rfc3339(duration.string("start")?)?;
    let duration = end - start;
    println!("{}", name);
    println!(
        "\nStart:  {}        Elapsed: {}ms",
        start.format("%Y-%m-%d %H:%M:%S"),
        duration.num_milliseconds()
    );
    println!("Module: {:19}        Status:  {:#}", act, status);
    println!("{}", "=".repeat(72));

    // handle case where no_log: true was set for the task
    if let Some(s) = thishost.string_maybe("censored") {
        println!("{}", s);
        return Ok(());
    }

    let action = match act {
        "alternatives" => results_invpath,
        "assemble" => results_dest,
        "blockinfile" => results_invpath,
        "copy" => results_dest,
        "command" => results_command,
        "file" => results_path,
        "find" => results_find,
        "gather_facts" => results_setup,
        "get_url" => results_get_url,
        "include_role" => results_include_role,
        "include_tasks" => results_include_tasks,
        "ini_file" => results_path,
        "lineinfile" => results_lineinfile,
        "package" => results_package,
        "replace" => results_path,
        "setup" => results_setup,
        "service" => results_name,
        "slurp" => results_slurp,
        "stat" => results_stat,
        "systemd" => results_name,
        "template" => results_dest,
        "user" => results_name,
        "yum" => results_package,
        &_ => results_nop,
    };

    if let Some(serde_json::Value::Array(r)) = thishost.get("results") {
        // check for _ansible_item_label: true to confirm we have an item loop
        let mut found = false;
        for res in r {
            if found {
                println!("{}", "-".repeat(72));
            }
            // TODO: label might not be a string
            if let Ok(lbl) = res.string("_ansible_item_label") {
                let result = match (
                    res.bool("_ansible_item_result"),
                    res.bool("failed"),
                    res.bool("changed"),
                    res.bool("skipped"),
                ) {
                    (_, _, _, Ok(true)) => "skipped",
                    (_, Ok(true), _, _) => "failed",
                    (_, _, Ok(true), _) => "changed",
                    (Ok(true), _, _, _) => "ok",
                    (Ok(false), _, _, _) => "fail",
                    (_, Ok(false), _, _) => "ok",
                    (_, _, Ok(false), _) => "ok",
                    _ => "unknown",
                };

                println!("{:43} {}", lbl, result);
            }

            found = true;
            if let Some(msg) = res.string_maybe("msg") {
                if !msg.is_empty() && msg != "OK" {
                    println!("{}", msg);
                }
            }
            action(res)?;
        }
        if found {
            return Ok(());
        }
    }

    if let Some(msg) = thishost.string_maybe("msg") {
        if !msg.is_empty() && msg != "OK" {
            println!("{}", msg);
        }
    }
    action(thishost)
}

fn summary(
    hosts: Vec<Summary>,
    fields: Option<OsString>,
    sort_by: Option<OsString>,
    filters: Vec<String>,
    header: bool,
) -> Result<(), Fail> {
    tabulate!(fields, sort_by = (DATE, HOST), filters, header, hosts.iter(), {
        + > CHANGED: i64 => { &|x: &Summary| Ok(x.changed) },
        + > FAILURES: i64 => { &|x: &Summary| Ok(x.failures + x.ignored + x.rescued) },
        - > IGNORED: i64 => { &|x: &Summary| Ok(x.ignored) },
        - > RESCUED: i64 => { &|x: &Summary| Ok(x.rescued) },
        - > FATAL: i64 => { &|x: &Summary| Ok(x.failures) },
        + > OK: i64 => { &|x: &Summary| Ok(x.ok) },
        + > SKIPPED: i64 => { &|x: &Summary| Ok(x.skipped) },
        + < DATE: String => { &|x: &Summary| Ok(x.end.format("%F").to_string()) },
        - < TIME: String => { &|x: &Summary| Ok(x.end.format("%T").to_string()) },
        + < HOST: String => { &|x: &Summary| Ok(x.host.clone()) },
    });
    Ok(())
}

fn main() {
    let opt = Opt::parse();

    let mut table: Option<Table> = None;
    let mut task_num: Option<u32> = None;
    let mut host_filter = Vec::new();
    let mut field_filter = Vec::new();
    let mut first: bool = true;

    for arg in &opt.filters {
        // Remaining options
        // Named table (in first position only)
        if first {
            if let Ok(h) = Table::try_from(arg) {
                table = Some(h);
                continue;
            }
            first = false;
        }
        let argstr = match arg.to_str() {
            Some(a) => a,
            None => {
                eprintln!("Invalid hostname or filter: {:?}", arg);
                ::std::process::exit(1);
            }
        };

        // This excludes ! because as an operator, = follows it
        let operators = [b'=', b'~', b'>', b'<'];
        if let Ok(i) = argstr.parse::<u32>() {
            // Task number
            if task_num.is_some() {
                eprintln!("Only one task number can be specified");
                ::std::process::exit(1);
            }
            task_num = Some(i);
        } else if argstr
            .as_bytes()
            .iter()
            .any(|x| operators.iter().any(|o| o == x))
        {
            field_filter.push(argstr.to_string());
        } else {
            // Host name
            host_filter.push(argstr.to_string());
        }
    }

    // Get JSON files on which we'll operate
    let mut in_files = if !opt.infile.is_empty() {
        opt.infile
    } else {
        let path = match getcfg() {
            Err(e) => {
                eprintln!("{}", e);
                ::std::process::exit(1);
            }
            Ok(s) => PathBuf::from(s),
        };

        if !host_filter.is_empty() {
            host_filter
                .iter()
                .map(|host| {
                    let mut fullname = path.clone();
                    let mut file = host.clone();
                    file.push_str(".json.gz");
                    fullname.push(file);
                    fullname
                })
                .collect()
        } else {
            let dir = Path::new(&path);
            match dir_contents(dir) {
                Ok(x) => x,
                Err(e) => {
                    eprintln!("{}", e);
                    ::std::process::exit(1);
                }
            }
        }
    };
    if in_files.is_empty() {
        eprintln!("At least one input JSON file is required");
        ::std::process::exit(1);
    }

    let table = table.unwrap_or_else(|| {
        if host_filter.len() == 1 || task_num.is_some() {
            Table::Tasks
        } else {
            Table::Summary
        }
    });

    if task_num.is_some() && table != Table::Tasks {
        eprintln!("A task number is not applicable to the '{:?}' table", table);
        ::std::process::exit(1);
    }

    if table == Table::Summary {
        let (successes, failures): (Vec<_>, Vec<_>) = in_files
            .par_iter()
            .flat_map(get_summary)
            .partition_map(|r| match r {
                Ok(v) => Either::Left(v),
                Err(v) => Either::Right(v),
            });
        for fail in &failures {
            eprintln!("{}", fail);
        }
        match summary(
            successes,
            opt.fields,
            opt.order,
            field_filter,
            !opt.no_header,
        ) {
            Ok(x) => x,
            Err(e) => {
                eprintln!("{}", e);
                ::std::process::exit(1);
            }
        };
        if !failures.is_empty() {
            ::std::process::exit(1);
        }
    } else if table == Table::Tasks {
        if in_files.len() > 1 {
            eprintln!("Tasks can only be listed for a single host");
            ::std::process::exit(1);
        }
        let filename = in_files.pop().unwrap();
        let hostname = host_filter.pop();
        if let Err(e) = if let Some(tnum) = task_num {
            task(&filename, hostname, tnum, opt.json)
        } else {
            task_list(&filename, hostname, opt.fields, opt.order, field_filter)
        } {
            eprintln!("{:?}: {}", filename, e);
            ::std::process::exit(1);
        }
    } else {
        let distill_facts = match table {
            Table::Cpu => distill_cpu_facts,
            Table::Df => distill_fs_facts,
            Table::Docker => distill_docker_info,
            Table::Net => distill_net_facts,
            Table::OS => distill_os_facts,
            Table::Packages => distill_package_facts,
            Table::Python => distill_python_facts,
            Table::Swap => distill_swap_facts,
            Table::Services => distill_service_facts,
            Table::Virt => distill_virt_facts,
            Table::Zpool => distill_zpool_facts,
            Table::Zfs => distill_zfs_facts,
            _ => unreachable!(),
        };
        let module = match table {
            Table::Docker => "docker_image_info",
            Table::Packages => "package_facts",
            Table::Services => "service_facts",
            Table::Zpool => "zpool_facts",
            Table::Zfs => "zfs_facts",
            _ => "setup",
        };
        let (mut all, failures): (Vec<Facts>, Vec<_>) = in_files
            .par_iter()
            .flat_map(|x| gather_facts(x, module))
            .partition_map(|r| match r {
                Ok(v) => Either::Left(v),
                Err(v) => Either::Right(v),
            });
        for fail in &failures {
            eprintln!("{}", fail);
        }
        // cut the list down to just the most recent for each host
        all.sort_by(|a, b| a.host.cmp(&b.host).then_with(|| b.age.cmp(&a.age)));
        all.dedup_by_key(|x| x.host.to_string());
        if opt.json {
            if let Some(i) = all.first() {
                println!("{:#}", i.value);
                ::std::process::exit(1);
            }
        }
        match distill_facts(all, opt.fields, opt.order, field_filter, !opt.no_header) {
            Ok(x) => x,
            Err(e) => {
                eprintln!("{}", e);
                ::std::process::exit(1);
            }
        };
        if !failures.is_empty() {
            ::std::process::exit(1);
        }
    }
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Opt::command().debug_assert()
}
