\name{b2j}
\Rdversion{1.1}
\alias{b2j}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  B1950 to J2000 coordinate conversion
}
\description{
Precession from B1950 to J2000
}
\usage{
b2j(ra = "17h42m29.3076s", dec = "-28d59m18.484s")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ra}{B1950 Right ascention (string)}
  \item{dec}{B1950 Declination (string)}
}
\details{
Enter positions as text strings with fields separated by characters d,
h, m, s, a colon, or a comma, e.g. '17, 42, 28', '-28h43m03s', or '- 28
:43 : 3'.
Spaces are removed in input conversion.
Trailing missing values are taken as zero.  The code uses an
approximate formula for precession; spot checks give results accurate
within a few tenths of an arcsecond.
}
\value{
  List with strings in:
    \item{ra2000}{J2000 Right ascension}
    \item{dec2000}{J2000 Declination}
}
\note{
  Calculation based on power-law expansion of exact function.
}
\references{
Explanatory supplement to the Astronomical
Almanac, Seidelmann (ed.), c.~1992, chapter 3.213
}
\author{
Andrew Harris
}
\seealso{
  \code{\link{j2b}}.  See \code{\link{dms2rad}}, \code{\link{hms2rad}}
  for input conversions.
}
\examples{
b2j()
b2j(ra='17, 43', dec='-28, 47, 30')
b2j(ra='17, 43', dec=' -  28,  47, 30')
b2j(ra='17h43m', dec='-28d47m30s')
tmp <- b2j(ra='17, 43', dec=' -  28,  47, 30')
str(tmp)
tmp
}
\keyword{ misc }
