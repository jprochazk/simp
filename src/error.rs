use crate::span::Span;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone, Debug)]
pub struct Error {
    span: Span,
    message: String,
}

pub fn error(message: impl Into<String>, span: impl Into<Span>) -> Error {
    Error {
        span: span.into(),
        message: message.into(),
    }
}

impl Error {
    pub fn render<'error, 'src>(&'error self, src: &'src str) -> ErrorDisplay<'error, 'src> {
        ErrorDisplay { error: self, src }
    }
}

pub struct ErrorDisplay<'error, 'src> {
    error: &'error Error,
    src: &'src str,
}

impl std::fmt::Display for ErrorDisplay<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let span = self.error.span;

        let loc = Location::from_source_span(self.src, span);
        let ln = loc.line_num;
        let lw = num_digits(loc.line_num);
        let pos = span.start() - loc.line_start;
        let len = if span.end() > loc.line_end {
            loc.line_end - span.start()
        } else {
            span.end() - span.start()
        };
        let line = &self.src[loc.line_start..loc.line_end];

        writeln!(f, "{}", self.error.message)?;
        writeln!(f, "{ln} |  {line}")?;
        writeln!(f, "{:lw$} |  {:pos$}{:^<len$}", "", "", "^")
    }
}

impl std::error::Error for Error {}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error ({}): {}", self.span, self.message)
    }
}

impl<T> From<Error> for Result<T, Error> {
    fn from(value: Error) -> Self {
        Err(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    line_num: usize,
    column: usize,
    line_start: usize,
    line_end: usize,
}

impl Location {
    pub(crate) fn from_source_span(source: &str, span: Span) -> Self {
        let line_start = source[..span.start()]
            .rfind('\n')
            .map(|v| v + 1)
            .unwrap_or(0);
        let line_num = 1 + source[..line_start].lines().count();
        let column = span.start() - line_start;
        let line_end = source[span.start()..]
            .find('\n')
            .map(|v| v + span.start())
            .unwrap_or(source.len());

        Self {
            line_num,
            column,
            line_start,
            line_end,
        }
    }

    #[inline]
    pub fn line(&self) -> usize {
        self.line_num
    }

    #[inline]
    pub fn column(&self) -> usize {
        self.column
    }

    #[inline]
    pub fn line_span(&self) -> Span {
        Span {
            start: self.line_start as u32,
            end: self.line_end as u32,
        }
    }
}

pub(crate) fn num_digits(n: usize) -> usize {
    if n == 0 {
        1
    } else {
        (n as f64).log10().floor() as usize + 1
    }
}
