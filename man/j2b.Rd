\name{j2b}
\Rdversion{1.1}
\alias{j2b}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  J2000 to B1950 coordinate converstion
}
\description{
Precession from J1950 to B2000
}
\usage{
j2b(ra = "17:30:30", dec = "-28:47")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ra}{J2000 Right ascention (string)}
  \item{dec}{J2000 Declination (string)}
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
    \item{ra1950}{B1950 Right ascension}
    \item{dec1950}{B1950 Declination}
}
  
\note{
  Values based on power-law expansion of more exact calculation.
}

\references{
Explanatory supplement to the Astronomical
Almanac, Seidelmann (ed.), c.~1992, chapter 3.213
}

\author{
Andrew Harris
}

\seealso{
  \code{\link{b2j}}.  See \code{\link{dms2rad}}, \code{\link{hms2rad}} for input conversions.
}
\examples{
j2b()
j2b(ra='17h43m', dec='-28d47m30s')
tmp <- j2b(ra='17, 43', dec=' -  28,  47, 30')
str(tmp)
tmp
}
\keyword{ misc }
