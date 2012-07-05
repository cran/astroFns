\name{angSep}
\Rdversion{1.1}
\alias{angSep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Angular separation of two sky positions}
\description{
  \code{angSep} calculates the angular separation of two sky positions
  using spherical trigonometry.
}
\usage{
angSep(ra1, dec1, ra2, dec2)
}
\arguments{
  \item{ra1}{Right ascention (string) of the first position.}
  \item{dec1}{Declination of (string) the first position.}
  \item{ra2}{Right ascention (string) the second position.}
  \item{dec2}{Declination of (string) the second position.}
}
\details{
Enter positions as text strings with fields separated by characters d,
h, m, s, a colon, or a comma, e.g. '17, 42, 28', '-28h43m03s', or
'- 28 :43 : 3'.
Spaces are removed in input conversion.
This is a
spherical trigonometry calculation, valid for small and large distances.
}
\value{
  Returns angluar separation in decimal degrees.
}
\author{Andrew Harris}

\seealso{
  See \code{\link{dms2rad}}, \code{\link{hms2rad}} for input conversions.
}

\examples{
angSep('1, 59, 03', '-3, 40, 44', '2, 30', '5, 40, 03')
angSep('1h59m03s', '-3d40m44s', '2h30', '5h40m03')
angSep('1', '0', '2', '0')
angSep('   1,  40,  4', '  -  5, 6', '3', '1')
}
\keyword{misc}

