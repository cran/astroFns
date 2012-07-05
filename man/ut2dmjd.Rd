\name{ut2dmjd}
\Rdversion{1.1}
\alias{ut2dmjd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
UT to DMJD
}
\description{
  Universal time to decimal modified Julian date.
  }
\usage{
ut2dmjd(yr = 2012, mo = 1, dy = 1, hr = 0, mi = 0, se = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{yr}{UT year}
  \item{mo}{UT month}
  \item{dy}{UT day}
  \item{hr}{UT hour}
  \item{mi}{UT minute}
  \item{se}{UT second}
}
\value{
Decimal modified Julian date.
}
\author{
Andrew Harris
}

\note{Uses \code{\link{ymd2jd}} to calculate Julian date}

\seealso{
\code{\link{dmjd2ut}}
}
\examples{
ut2dmjd(yr=2000, mo=1, dy=1, hr=0, mi=0, se=0)
format(ut2dmjd(yr=2012, mo=5, dy=20, hr=7, mi=8, se=39), digits=10)
}
\keyword{ chron }
