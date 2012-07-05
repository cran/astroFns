\name{ut2lst}
\alias{ut2lst}
\alias{ut2ha}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Universal time to local sidereal time or hour angle
}
\description{
Functions to calculate local sidereal time (LST) or hour angle (HA) from
Universal time (strictly, UTC1).
}
\usage{
ut2lst(yr = 2012, mo = 1, dy = 1, hr = 0, mi = 0, se = 0,
lon.obs = "W 79d 50.5m")

ut2ha(yr = 2012, mo = 1, dy = 1, hr = 0, mi = 0, se = 0,
ra.sou = "13h 31m 08.3s", lon.obs = "W 79d 50m 23.4s")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{yr}{UT1 Year}
  \item{mo}{UT1 Month number}
  \item{dy}{UT1 Day number}
  \item{hr}{UT1 Hour}
  \item{mi}{UT1 Minute}
  \item{se}{UT1 Seconds}
  \item{ra.sou}{String with source Right Ascension}
  \item{lon.obs}{String with observatory longitude}
}

\details{ If this input is \code{hr = Sys.time()} the function uses system time,
  including conversion to UT.  UT is within a few seconds of UT1.  }

\value{ Returns decimal local sidereal time in range 0 to 24 hours and
  hour angle from -1 to 12 hours, with class \code{fracHrs} (prints as
  h:m:s).  For elapsed siderial time difference over multiple sidereal
  days, difference UT days (from e.g. \code{\link{ut2dmjd}}) and
  multiply by 1.002737909350795.  }

\references{
Greenwich mean sidereal time (GMST) at 0h UT1 from the "Explanatory
Supplement to the Astronomical Almanac, " Seidelmann (ed), c. 1992.
Approximate equation of the equinoxes from
http://aa.usno.navy.mil/faq/docs/GAST.php.
}
\author{
Andrew Harris
}
\note{
Spot checks show values match tabulated values in The Astronomical Almanac
within ~0.01 seconds.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ymd2jd}}, \code{\link{gmst1}}, \code{\link{dms2rad}} and
\code{\link{hms2rad}} for input formats, \code{\link{Sys.time}},
\code{\link{Sys.timezone}} and time zone examples in \code{\link{as.POSIXlt}}.
}
\examples{
# LST at UT1 midnight on the first of every month for Green Bank, WV, USA
midLST <- ut2lst(yr = 2012, mo = 1:12, dy = 1, hr = 0, mi = 0, se = 0,
                 lon.obs="W 79d 50.5m")
str(midLST)
midLST

# LST at EST midnight on the first of every month for Green Bank, WV, USA
# (EST = UT1-5 hours)
midLST <- ut2lst(yr = 2012, mo = 1:12, dy = 1, hr = -5, mi = 0, se = 0,
                 lon.obs="W 79d 50.5m")
str(midLST)
midLST

# LST in Green Bank, WV, USA, now, and 12 hours from now.
ut2lst(Sys.time())
ut2lst(Sys.time() + 12*3600)

# Hour angle of 3C286 in Green Bank now (using function defaults)
ut2ha(Sys.time())

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chron }
