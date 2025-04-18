% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imageDimnames.R
\name{imageDimnames}
\alias{imageDimnames}
\title{Create an image of matrix or 2-D array with labels}
\usage{
imageDimnames(
  mat,
  xlab = NULL,
  ylab = NULL,
  xaxisSide = 1,
  yaxisSide = 2,
  axisLas = 1,
  drawBorders = TRUE,
  borderCol = 1,
  borderLty = 1,
  borderLwd = 1,
  addLabels = TRUE,
  labels = sinkr::ac(c(mat)),
  labelCol = "black",
  labelFont = 1,
  ...
)
}
\arguments{
\item{mat}{matrix or 2-D array}

\item{xlab}{label for x-axis}

\item{ylab}{label for y-axis}

\item{xaxisSide}{side for x-axis}

\item{yaxisSide}{side for y-axis}

\item{axisLas}{text direction for axes}

\item{drawBorders}{logical. Include borders between grids.}

\item{borderCol}{if drawborders is true, color for borders}

\item{borderLty}{if drawborders is true, line type for borders}

\item{borderLwd}{if drawborders is true, line width for borders}

\item{addLabels}{add labels for matrix values}

\item{labels}{string of labels (default is taken from matrix)}

\item{labelCol}{color for grid labels}

\item{labelFont}{font for grid labels}

\item{...}{other arguments to pass to image}
}
\value{
image plot
}
\description{
Create an image of matrix or 2-D array with labels
}
\examples{

mat <- matrix(-8:7, 4, 4)
imageDimnames(mat)
imageDimnames(mat, labelCol = c(1,2)[(c(mat) < 0)+1])
imageDimnames(mat, labelCol = c(1,2)[(c(mat) < 0)+1], borderLwd = 2)

imageDimnames(volcano, drawBorders = FALSE, addLabels = FALSE, axisLas = 2)

imageDimnames(provideDimnames(volcano), addLabels = FALSE)

tmp <- tapply(warpbreaks$breaks, warpbreaks[,-1], sum)
imageDimnames(tmp, labelCol = 4)

}
