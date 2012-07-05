\name{beamDiskOverlap}
\Rdversion{1.1}
\alias{beamDiskOverlap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Gaussian beam and disk overlap with shift
}
\description{
Calculate the overlap integral of a 2-D Gaussian beam and a uniform
disk, including a shift between the centers of the beam and disk.
}
\usage{
beamDiskOverlap(s = 0, r = 1, theta.fwhm = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{Shift between centers}
  \item{r}{Disk radius}
  \item{theta.fwhm}{Gaussian beam FWHM}
}
\details{
Converts the 2-D integral to 1-D for speed.  Use consistent units.
}
\value{
Value of the overlap integral, normalized to unity for a beam much
smaller than the disk.
}
\references{
"Telescope illumination and beam measurements for submillimeter
astrononomy," A.I. Harris, Internat. J. IR and mm Waves, 9, 231 (1988)
}
\author{
Andrew Harris
}
\examples{
s <- seq(0, 10, 0.1)
plot(s, beamDiskOverlap(s, 4, 1), t='l', col=4)
}
\keyword{ misc }

