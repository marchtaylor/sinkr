% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addImg.R
\name{addImg}
\alias{addImg}
\title{Add image to plot region}
\usage{
addImg(obj, x = NULL, y = NULL, width = NULL, interpolate = TRUE)
}
\arguments{
\item{obj}{an image file imported as an array (e.g. png::readPNG, jpeg::readJPEG)}

\item{x}{mid x coordinate for image}

\item{y}{mid y coordinate for image}

\item{width}{width of image (in x coordinate units)}

\item{interpolate}{(passed to graphics::rasterImage) A 
logical vector (or scalar) indicating whether to apply 
linear interpolation to the image when drawing.}
}
\value{
adds graphics to plot
}
\description{
Add image to plot region
}
\examples{

\donttest{
library(png)
myurl <- paste0("https://upload.wikimedia.org/wikipedia/commons/thumb/", 
  "e/e1/Jupiter_\%28transparent\%29.png/242px-Jupiter_\%28transparent\%29.png")
myurl <- paste0("https://upload.wikimedia.org/wikipedia/commons/4/47/",
  "PNG_transparency_demonstration_1.png")
z <- tempfile()
download.file(myurl,z,mode="wb")
pic <- readPNG(z)
file.remove(z) # cleanup
dim(pic)

image(volcano)
addImg(pic, x = 0.3, y = 0.5, width = 0.5)
}

}
