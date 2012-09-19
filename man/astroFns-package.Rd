\name{astroFns-package}
\Rdversion{1.1}
\alias{astroFns-package}
\alias{astroFns}
\docType{package}
\title{
  Astronomy: time and position functions, misc. utilities
    }
\description{
  Collection of time, position, and utility functions for astronomy:
  Julian and Modified Julian Day transformations, J2000/B1950 coordinate
  transformations, GMST at 0h from UT, UT to LST and hour angle, angular
  unit transformations and distance, etc.  Precision generally at the
  level of a millisecond in time for JD, 0.1 s for LST, and a few tenths
  of an arcsecond in position for B2000-J1950 and vice versa.
  Additional functions include flux from
  thermal disk-shaped source, and others.  Functions were originally assembled
  to support single-dish radio astronomy planning and observations.
  Cosmology-related functions are in the \code{cosmoFns} package.
}
\details{
\tabular{ll}{
Package: \tab astroFns\cr
Type: \tab Package\cr
Version: \tab 4.1-0\cr
Date: \tab 2012-09-19\cr
License: \tab GPL (>=2) \cr
LazyLoad: \tab yes\cr
}
}

\author{
Andrew Harris
Maintainer: Andrew Harris <harris@astro.umd.edu>
}

\keyword{ package }

