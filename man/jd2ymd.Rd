\name{jd2ymd}
\Rdversion{1.1}
\alias{jd2ymd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
JD to year, month, date
}
\description{
Convert Julian date to UT1 year, month, and date.
}
\value{
 Date for 0h, UT1, with class POSIXct
}
\references{
  Fliegel & Van Flandern, Comm. ACM 10, 657 (1968), whose algorithm
  uses FORTRAN integer mathematics
}
\author{
Andrew Harris
}
\seealso{
\code{\link{weekdays}}, \code{\link{dmjd2ut}}
}
\examples{
jd2ymd(2456092.5) # returns 0h date, 2012-06-14 UT
jd2ymd(2456092.6) # returns 0h date, 2012-06-14 UT
jd2ymd(2456092.4) # returns 0h date, 2012-06-13 UT
}
\keyword{ chron }

