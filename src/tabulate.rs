#![macro_use]

// This macro takes an iterator and prints a table row for each iteration.
// Sorting, filtering and customising which columns are printed is supported.
// Parameters are:
//   fields: Option<OsString>  - fields to output  (-o option)
//   sort_by: Option<OsString> - fields to sort on (-s option)
//     followed by = ( field, ...) to indicate defaults
//  filters: Vec<String>       - list of filters (normal arguments)
//  header: bool               - whether to print a header (-H option)'
//  iterator                   - iterator over table rows
//  { field specification, ... } where each specifcation follows:
//    +/-                      - include field by default (+) or not (-)
//    </>                      - left (<) or right (>) align value
//    field: type              - name of field (uppercase, ASCII only) and type
//    => { lambda to get from iterator },

macro_rules! tabulate (
    // internal rule to extract just those fields that are output by default
    (@default () -> ($($body:tt)*)) => {[$($body)*]};
    (@default (+ $e:ident $($tail:tt)*) -> ($($body:tt)*)) => { tabulate!(@default ($($tail)*) -> ($($body)* TblField::$e,))};
    (@default (- $e:ident $($tail:tt)*) -> ($($body:tt)*)) => { tabulate!(@default ($($tail)*) -> ($($body)*))};
    (@default $($tail:tt)*) => { tabulate!(@default ($($tail)*) -> ())};
    (@sortlist () -> ($($body:tt)*)) => {[$($body)*]};
    (@sortlist ($e:ident $($tail:tt)*) -> ($($body:tt)*)) => { tabulate!(@sortlist ($($tail)*) -> ($($body)* TblField::$e,))};
    (@sortlist $($tail:ident),*) => { tabulate!(@sortlist ($($tail)*) -> ())};
    (@alignment <) => { true };
    (@alignment >) => { false };

    // normal invocation
    ( $fields:ident, $sort_field:ident = ($($sort_default:ident),*), $filters:expr, $print_header:expr, $iter:expr, { $( $def:tt $align:tt $field:ident : $type:ty => $block:expr ),+ $(,)* } ) => {

    #[allow(non_snake_case)]
    #[derive(Debug,Default)]
    struct TblData {
        $($field : $type),+
    }
    #[allow(dead_code, non_camel_case_types)]
    #[derive(Copy,Clone,Debug,Eq,PartialEq)]
    enum TblField {
        $( $field, )+
    }
    let alignment = [ $( tabulate!(@alignment $align) ),* ];

    #[allow(non_snake_case)]
    #[derive(Debug)]
    enum TblFilter { // reference value and field for filtering
        $( $field($type), )+
    }

    #[derive(Debug)]
    enum TblOperator { Eq, Ne, Gt, Lt, Match(regex::Regex), NMatch(regex::Regex), }

    // A list of columns that we sort or filter by so need.
    let mut extract = Vec::new(); // Maybe use BitSet once stable.

    // Parse the filter arguments into our internal structure.
    let mut filters = Vec::new();
    for filt in $filters {
        if let Some(op) = filt.find(|c| "=~><!".find(c).is_some()) {
            let (first, end) = filt.split_at(op);
            let mut opiter = end.bytes();
            let mut idx = 1;
            let mut regmatch = false;
            let operator = match opiter.next() {
                Some(b'=') => TblOperator::Eq,
                Some(b'>') => TblOperator::Gt,
                Some(b'<') => TblOperator::Lt,
                Some(b'~') => { regmatch = true; TblOperator::Match(Regex::new(&end[1..])?) }
                Some(b'!') =>
                    match opiter.next() {
                        Some(b'=') => {
                            idx +=1;
                            TblOperator::Ne
                        }
                        Some(b'~') => {
                            regmatch = true;
                            TblOperator::NMatch(Regex::new(&end[2..])?)
                        }
                        _ => return Err(Fail::Msg("invalid filter operator".to_string())),
                    },
                _ => unreachable!(),
            };
            let first = &first.to_ascii_uppercase()[..];
            let end = &end[idx..];
            let (filter, col) = $(
                if stringify!($field).starts_with(first) {
                    if regmatch {
                       (TblFilter::$field(Default::default()), TblField::$field)
                    } else {
                        match <$type>::from_str(end) {
                            Ok(x) => (TblFilter::$field(x), TblField::$field),
                            Err(e) => return Err(Fail::Msg(format!("{}: {}", e.to_string(), end))),
                        }
                    }
                } else )+
                {
                    return Err(Fail::Msg(format!("Unrecognised table field: {}", first)));
                };
            extract.push(col);
            filters.push((operator, filter));
        }
    }

    // Construct a list of the fields to be output.
    let mut render = Vec::new();
    let render = match $fields {
        None => &tabulate!(@default $( $def $field )+), // use defaults
        Some(l) => {
            let specified = l.into_string()?;
            for f in specified.split(',') {
                let f = &f.to_ascii_uppercase()[..];
                render.push($(
                    if stringify!($field).starts_with(f) {
                        TblField::$field
                    } else )+ {
                        return Err(Fail::Msg(format!("Unrecognised table field: {}", f)));
                    });
            }

           render.as_slice()
        }
    };

    // Construct a list of the fields to sort by.
    let mut sort_by = Vec::new();
    let sort_by = match $sort_field {
        None => &tabulate!(@sortlist $($sort_default),* ),
        Some(l) => {
            let specified = l.into_string()?;
            for f in specified.split(',') {
                let f = &f.to_ascii_uppercase()[..];
                sort_by.push($(
                    if stringify!($field).starts_with(f) {
                        TblField::$field
                    } else )+ {
                        return Err(Fail::Msg(format!("Unrecognised table field: {}", f)));
                    });
            }

           sort_by.as_slice()
        }
    };

    // Need all columns we have an interest in - printing, sorting or filtering
    for column in render.iter().chain(sort_by.iter()) {
        if ! extract.iter().find(|c| *c == column).is_some() {
            extract.push(*column);
        }
    }

    // Create table of closures for extracting relevant columns along
    // with the table headings.
    let mut columns = Vec::<&dyn Fn(&mut _,_) -> Result<(),Fail>>::new();
    for column in extract {
        columns.push(match column {
            $( TblField::$field => { &|item: &mut TblData, line| -> Result<(),Fail> { let call: &dyn Fn(_) -> Result<_,Fail> = $block; item.$field = call(line)?; Ok(()) }}),+
        });
    }

    // Start the table with the headers (along with widths).
    let mut headings = Vec::with_capacity(render.len());
    let mut min_widths = Vec::with_capacity(render.len());
    for column in render {
        if $print_header {
            // match might be cleaner than :? but this works fine enough.
            let hdr = format!("{:?}", column);
            min_widths.push(hdr.len()); // These are always only ever ASCII.
            headings.push(hdr);
        } else {
            min_widths.push(0);
        }
    }

    // Populate table in data form - text can be lossy, affecting
    // sorting, e.g. for dates where we have the time.
    let mut data_rows = Vec::new();
    for line in $iter {
        let mut item: TblData = TblData { ..Default::default() };
        for column in &columns {
            column(&mut item, line)?;
        }
        if filters.iter().all(|(op, filter)|
           match filter {
                $( TblFilter::$field(value) => {
                    match op {
                        TblOperator::Eq => item.$field == *value,
                        TblOperator::Ne => item.$field != *value,
                        TblOperator::Gt => item.$field > *value,
                        TblOperator::Lt => item.$field < *value,
                        TblOperator::Match(re) => re.is_match(&format!("{}", item.$field)[..]),
                        TblOperator::NMatch(re) => !re.is_match(&format!("{}", item.$field)[..]),
                    }
                })+
            }) {
            data_rows.push(item);
        }
    }

    // Sort the rows while we have them in binary data form.
    data_rows.sort_by(|a, b| sort_by.iter().find_map(|x| match match x {
        $( TblField::$field => a.$field.cmp(&b.$field) ),+
    }{
        std::cmp::Ordering::Equal => None,
        l => Some(l),
    }).unwrap_or(std::cmp::Ordering::Equal));

    // Create a new table with the data instead in test form.
    let mut text_rows = Vec::with_capacity(data_rows.len() + 1);
    if $print_header && data_rows.len() > 0 {
        text_rows.push(headings);
    }
    for line in data_rows {
        let mut text_cols = Vec::with_capacity(render.len());
        for (column, width) in render.iter().zip(&mut min_widths) {
            let cell = match column {
                $( TblField::$field => format!("{}", line.$field) ),+
            };
            let cellw = UnicodeWidthStr::width(&cell[..]);
            if cellw > *width {
                *width = cellw;
            }
            text_cols.push(cell);
        }
        text_rows.push(text_cols);
    }

    // Finally write out the table all on one go
    let stdout = io::stdout(); // get the global stdout entity
    let mut handle = stdout.lock(); // acquire a lock on it
    for row in text_rows {
        match writeln!(handle, "{}", row.iter()
            .zip(&min_widths)
            .zip(render)
            .map( |((col, width), column)| {
                // field widths with formatting don't allow for double-width chars
                let adjustment = UnicodeWidthStr::width(&col[..]) - col.chars().count();
                if alignment[*column as usize] {
                    format!("{:1$}", col, width - adjustment)
                } else {
                    format!("{:>1$}", col, width - adjustment)
                }
            })
            .join(" ").trim_end()) {
            Err(e) => {
                if e.kind() == io::ErrorKind::BrokenPipe {
                    ::std::process::exit(0); // graceful exit for broken pipe
                }
            }
            Ok(()) => ()
        }
    }
  };
);
