\name{hms2rad}
\Rdversion{1.1}
\alias{hms2rad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Hours, minutes, and seconds to radians
}
\description{
  Angular conversion from hours, minutes, and seconds to radians.

}
\usage{
hms2rad(h = '12h 3m 45.6s')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{String hours, minutes, and seconds}
}

\details{Function reads a string (the input is a string to allow
  conversion of angles between -1 and zero hours) with hours, minutes,
  and seconds separated by any of characters d, m, s, a colon, or a
  comma.  Spaces are not valid separators, as they are removed as part
  of input parsing.  Zeros are the default if values for minutes or
  seconds are missing from the string.  A minus sign before the hours
  indicates negative hours.  Decimal values are allowed in any
  position. }

\value{
  Angle in radians.
}
\author{
Andrew Harris
}
\seealso{
\code{\link{dms2rad}}, \code{\link{rad2hms}}, \code{\link{rad2dms}}
}
\examples{
hms2rad('10, 22, 14')
hms2rad('-0:30')
hms2rad('0h30')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }

