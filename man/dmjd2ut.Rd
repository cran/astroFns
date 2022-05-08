\name{dmjd2ut}
\Rdversion{1.1}
\alias{dmjd2ut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DMJD to UT
}
\description{
Decimal modified Julian date to Universal time.
}
\usage{
dmjd2ut(dmjd, tz='UTC')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dmjd}{Time in decimal Modified Julian Date}
  \item{tz}{Time zone string}
}
\details{
  Calculation is always from UTC, but it is possible to correct to local time
  zone with \code{tz} (see \code{\link{Sys.timezone}}).  For instance, \code{tz = 'EST5EDT'}
  converts to U.S. Eastern time, with EST or EDT based on the system's knowledge
  of the date for switching between the two.  Set the number of digits
  after the decimal place for seconds, \code{n}, with
  \code{options('digits.secs'=n)}.
}
\value{
  Time string with class POSIXct
}
\author{
Andrew Harris
}

\seealso{
  \code{\link{ut2dmjd}}, \code{\link{ymd2jd}},
  \code{\link{strptime}}, \code{\link{ISOdatetime}},
\code{\link{axis.POSIXct}} for time in plot axes;
\code{\link{as.POSIXct}} to recover time in plot from \code{locator()}
}
\examples{
dmjd2ut(56951.54183613)

sd <- getOption('digits.secs')
dmjd2ut(ut2dmjd(2010, 1, 5, 2, 34, 17.8115))
options('digits.secs' = 3)
dmjd2ut(ut2dmjd(2015, 1, 5, 2, 34, 17.8115))
options('digits.secs' = sd)

dmjd2ut(ut2dmjd(2015, 1, 5, 2, 34, 17.8115), tz='CET')
dmjd2ut(ut2dmjd(2015, 8, 5, 2, 34, 17.8115), tz='CET')
dmjd2ut(ut2dmjd(2015, 1, 5, 2, 34, 17.8115), tz='EST5EDT')
dmjd2ut(ut2dmjd(2015, 8, 5, 2, 34, 17.8115), tz='EST5EDT')

dmjd2ut(ymd2jd(2001, 1, 1) - 2400000.5)
}
\keyword{ chron }

