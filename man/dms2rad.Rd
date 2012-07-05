\name{dms2rad}
\Rdversion{1.1}
\alias{dms2rad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Degrees, minutes, and seconds to radians
  }
\description{
  Angular conversion from degrees, minutes, and seconds to radians
  }
\usage{
dms2rad(d = '33d 09m 35.0s')
}
\arguments{
  \item{d}{String containing degrees, minutes, and seconds}
}

\details{Function reads a string (the input is a string to allow
  conversion of angles between -1 and zero degrees) with degrees,
  minutes, and seconds separated by any of characters d, m, s, a colon,
  or a comma.  Spaces are not valid separators, as they are removed as
  part of input parsing.  Decimal values are allowed in any position.
  Zeros are the default if values for minutes or seconds are missing
  from the string. A minus sign, W, or w before the degrees indicates
  negative degrees.  Positive degrees are denoted by no character, +, E,
  or e before the degrees values.}

\value{ Angle in radians }

\author{ Andrew Harris }

\seealso{ \code{\link{hms2rad}}, \code{\link{rad2dms}},
  \code{\link{rad2hms}} }

\examples{
dms2rad('10, 22, 14')
dms2rad('10:22:14')
dms2rad('10d22m14s')
dms2rad('-0, 30')
dms2rad('-77d30.5m')
dms2rad('W 77d30.5m')
dms2rad(-77.5083333)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
