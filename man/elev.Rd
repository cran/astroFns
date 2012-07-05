\name{elev}
\alias{elev}
\alias{azimuth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Source elevation }
\description{
  Calculates source elevation and azimuth in degrees given declination, hour angle, and
  observatory latitude.
}
\usage{
elev(dec.sou = "33d 09m 35.0s", ha = 0, lat.obs = "38d 25m 59.2s")

azimuth(dec.sou = "33d 09m 35.0s", ha = 0, lat.obs = "38d 25m 59.2s")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dec.sou}{Source declination (string)}
  \item{ha}{Hour angle (decimal hours)}
  \item{lat.obs}{Observatory latitude (string)}
}
\details{
Enter latitude as s text string with fields separated by characters d,
h, m, s, a colon, or a comma, e.g. '38d25m59.2s' or '38, 25, 59.2' or
'38:25:59.2' or '38:25.987' for
the Green Bank Telescope.
Spaces are removed in input conversion.
Decimal values for degrees or minutes are allowed.
Trailing missing values are taken as zero.
}
\value{
  Source elevation or azimuth (E from N) in degrees.
}
\references{"Astrophysical Formulae," K.R. Lang, Springer c. 1986, 5-45}
\author{Andrew Harris}
\note{Geometrical calculation only, no corrections for refraction,
  aberration, precession, etc.}
\seealso{\code{\link{dms2rad}}, \code{\link{hms2rad}} for input formats,
  \code{\link{ut2ha}} to convert UT to hour angle.}
\examples{
# Maximum elevation at Green Bank
elev(dms2rad('-28, 20'))

# Maximum elevation at Mauna Kea
elev(dms2rad('-28, 20'), 0, '19:49')

# Plot elevation and azimugh vs. hour angle
ha <- seq(0, 24, 0.25)
el <- elev('30d 33m 22s', ha)
plot(ha, el, t='l', col=4)
az <- azimuth('30d 33m 22s', ha)
plot(ha, az, t='l', col=4)

# Plot elevation and azimuth vs. UT (using many defaults)
h.ut <-  seq(0, 24, 0.25)
el <- elev(dec.sou='30d 33m 22s', ha=ut2ha(hr=h.ut))
plot(h.ut, el, t='l', col=4)
az <- azimuth(dec.sou='30d 33m 22s', ha=ut2ha(hr=h.ut))
plot(h.ut, az, t='l', col=4)


}
\keyword{ misc }

