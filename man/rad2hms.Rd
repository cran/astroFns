\name{rad2hms}
\Rdversion{1.1}
\alias{rad2hms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Convert radians to hours, minutes, and seconds
}
\description{
  Angular conversion from radians to hours, minutes, and seconds
}
\usage{
rad2hms(rad = 1, places = 1)
}
\arguments{
  \item{rad}{Decimal radians}
  \item{places}{Number of decimal places in seconds term (0:6)}
}
\value{
Fixed-format string with hours, minutes, and seconds separated by colons.
}
\author{
Andrew Harris
}
\seealso{
\code{\link{rad2dms}}, \code{\link{dms2rad}}, \code{\link{hms2rad}}
}
\examples{
rad2hms(2.44)
rad2hms(hms2rad(c('10:04:5.12', '27,04,5.3', '-3:0:0')), places=3)
rad2hms(266.5 * pi/180) # from degrees to hms
}
\keyword{misc }
