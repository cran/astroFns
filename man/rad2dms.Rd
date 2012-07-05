\name{rad2dms}
\Rdversion{1.1}
\alias{rad2dms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Convert radians to degrees, minutes, and seconds
  }
\description{
  Angular conversion from radians to degrees, minutes, and seconds
}
\usage{
rad2dms(rad = 1, places = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rad}{Decimal radians}
  \item{places}{Number of decimal places in seconds term (0:6)}
}
\details{
Convert radians to degrees, minutes, and seconds.
}
\value{
Fixed-format string with sign, then degrees, minutes, and seconds separated by colons.
}
\author{
Andrew Harris
}
\seealso{
\code{\link{rad2hms}}, \code{\link{dms2rad}}, \code{\link{hms2rad}}
}
\examples{
rad2dms(2.44)
rad2dms(dms2rad(c('-1,4,5.12', '10:04: 5.3')), places=3)
rad2dms(-66.5 * pi/180) # from degrees to dms
}
\keyword{misc }

