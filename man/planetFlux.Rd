\name{planetFlux}
\Rdversion{1.1}
\alias{planetFlux}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Flux density from a thermal disk
}
\description{
  The flux density from a disk-shaped blackbody with uniform temperature
  observed in a Gaussian beam.  
}
\usage{
planetFlux(T = 195, dp = 14.8, thetab = 19.4, f = 32)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T}{Disk's physical temperature}
  \item{dp}{Planet diameter, arcsec}
  \item{thetab}{Beam FWHM, arcsec}
  \item{f}{Observing frequency, GHz}
}
\details{
Geometry is for a uniform-temperature disk, a planet to some
approximation, in a Gaussian beam.
}
\value{
Flux density in janskys
}
\author{
Andrew Harris
}
\note{For a physical Mars model, see <http://www.aoc.nrao.edu/~bbutler/work/mars/model/>}
\examples{
planetFlux()
}
\keyword{ misc }
