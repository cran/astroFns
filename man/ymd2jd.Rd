\name{ymd2jd}
\Rdversion{1.1}
\alias{ymd2jd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Year, month, day to 0h on Julian day
}
\value{
  Julian date
}
\description{
Convert year, month, day to 0h on Julian day.
}
\usage{
ymd2jd(yr = 2012, mo = 1, dy = 1)
}
\arguments{
  \item{yr}{UT1 Year}
  \item{mo}{UT1 Month number}
  \item{dy}{UT1 Day number}
}
\details{
  Returns Julian date of 0 hours on the specified day.  To get to noon
  on day, the time origin of Julian days, add 0.5.
}
\references{
  Fliegel & Van Flandern, Comm. ACM 10, 657 (1968), whose algorithm
  uses FORTRAN integer mathematics.  See also the Explanatory Supplement
  to the Astronomical Almanac, ed. P.K. Seidelmann, c. 1992.
}
\author{
Andrew Harris
}
\seealso{
\code{\link{weekdays}}, \code{\link{ut2dmjd}}
}
\examples{
# Ensure enough digits to see result, then return to previous value
dig <- getOption('digits')
options(digits=16)
ymd2jd(yr=2000, mo=1, dy=1)
ymd2jd(yr=2000, mo=1, dy=1.3)  # rounds to nearest day
options(digits=dig)
jd2ymd(ymd2jd(yr=2000, mo=1, dy=1))
}
\keyword{ chron }

