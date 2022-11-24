# ansible-json-monitor

A simple tool for tracking the output of automated ansible job runs using JSON
files.

With ansible running periodically and in automated runs some form of tracking
is needed to identify when playbooks fail and a particular system needs
attention. One nice thing about ansible is that it scales down well for simple
cases where you just want to manage a few machines. However, ansible AWX can be
somewhat overkill for simple cases and the focus seems to be on virtual server
deployments rather than, for example, physical desktops.

ansible-json-monitor involves just a simple command-line tool - ajmon - which
takes JSON output files from ansible runs and produces a condensed summary.
There is no always-on service or REST APIs requiring a master node. Just a
directory full of files.

## Getting Started and Basic Usage

To get JSON output from ansible you need to set `ANSIBLE_STDOUT_CALLBACK=json`
in the environment or `ansible.cfg` file to select the callback plugin for JSON
output. With recent releases of ansible this plugin is distributed in the
`ansible.posix` collection. As this goes to standard output, you need shell
redirection to save it to a file which should then be compressed. To try this
out we'll need something along these lines:

    ANSIBLE_STDOUT_CALLBACK=json ansible-playbook plays.yml > output.json
    gzip output.json

We now have a file named output.json.gz. We can get a basic overview of the results:

    ajmon -f output.json.gz
    CHANGED  FAILURES    OK  SKIPPED        DATE  HOST
          0         1   163       37  2019-06-07  localhost

By passing a hostname as the first normal argument to ajmon, we get a numbered
list of all the tasks along with an indication of any that failed or which
involved a change. Furthermore, the task number can be specified to get more
details on the run of a specific task. The output in that case varies depending
on the action.

There's also options to filter and sort tables by particular fields so for
example, it is simple to limit the summary to those involving failures.

Ansible typically starts by running the setup action to gather a whole range of
"facts" about the target system. ajmon can pick out these facts and present a
number of different tables using based on the extracted data. So, for example, you
can see the operating system versions as follows:

    ajmon -f output.json.gz os
    HOST    SYSTEM  DISTRIBUTION VERSION KERNEL
    example FreeBSD FreeBSD         12.0 12.0-RELEASE-p8

For full details of the available options, there is a man page included.

## How to set it up

To be useful, the tool needs a directory full of compressed json files
containing the output of ansible runs.

At the moment, this primarily assumes one JSON file per-host which maps to a
typical ansible-pull style setup. Given machines all sharing a file server, a
shared checkout of the playbooks in a particular directory also works well. The
following is an example shell script for running ansible, though it may require
tweaks such as to use ansible-pull.

    #!/bin/sh
    set -e
    exec >/dev/null 2>&1
    log="/path/to/logs/staging/$HOST.json.gz"
    cd /path/to/playbooks
    ANSIBLE_STDOUT_CALLBACK=json ANSIBLE_RETRY_FILES_ENABLED=0 ansible-playbook -l $HOSTS -c local site.yml | su user -c "gzip -c > $log && mv $log /path/to/logs/ansible/."

This might typically be installed as /etc/cron.daily/ansible. If you're managing
desktops, it can also be useful to arrange for ansible to run on boot.

Using su may not be necessary depending on how you have permissions configured.
Sending the output to a staging directory and moving it over at the end avoids
issues with potentially incomplete files. The ANSIBLE_RETRY_FILES_ENABLED
option is required because ansible notifies about retry files to stdout
resulting in an invalid JSON file.

This only keeps one output file per-host. It would certainly be possible to
keep old ones but the single file avoids the need to cleanup old files and old
files aren't especially useful anyway.
