\name{gmst1}
\alias{gmst1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ GMST1 (Greenwich Mean Siderial Time at 0h, UT1) from UT1 date}
\description{
  Calculate Greenwich Mean Siderial Time at 0h, UT1 (GMST1) from UT1
  year, month, and day.
}
\usage{
gmst1(yr = 2012, mo = 1, dy = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{yr}{UT1 year (integer)}
  \item{mo}{UT1 month (integer)}
  \item{dy}{UT1 day (integer) }
}
\details{
  Function calculates Greenwich Mean Siderial Time at 0h, UT1 (GMST1) given UT1
  year, month, and day.
}
\value{
  Returns fractional hours of GMST1 with class fracHrs.  The
  corresponding print method gives hh:mm:ss format rounded to \code{n} decimal
  places in seconds by setting \code{options('digits.secs'=n)}.
}

\references{Explanatory Supplement to the Astronomical Almanac
Seidelmann (ed), c. 1992}
\author{Andrew Harris}

\note{Multiply UT1 fractional day by
1.002737909350795 to get fractional sidereal day.
  }

\seealso{ \code{\link{ymd2jd}} }

\examples{
  out <- gmst1(yr=2012, mo=7, dy=8)
  str(out)
  out
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chron }

