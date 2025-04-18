% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strPart.R
\name{strPart}
\alias{strPart}
\title{Extract a defined part of a text string with separated elements}
\usage{
strPart(x, part = 1, split = NULL, fixed = TRUE, returnVector = TRUE, ...)
}
\arguments{
\item{x}{character vector, each element of which is to be split. Other inputs, 
including a factor, will give an error.}

\item{part}{numeric. Part of string (between splits) to return.}

\item{split}{character. The splitting character. See \code{\link[base]{strsplit}}}

\item{fixed}{logical. Default TRUE. See \code{\link[base]{strsplit}}}

\item{returnVector}{logical. Function relies on \code{\link[base]{lapply}}, 
and thus returns a list. Set to 'TRUE' (Default) to return a vector.}

\item{...}{Further arguments passed to \code{\link[base]{strsplit}}.}
}
\value{
vector or list of character strings
}
\description{
Wrapper for \code{\link[base]{strsplit}}. One defines the element of the string
  to return after splitting, with option to convert back to a character vector 
  or leave as list. See \code{\link[base]{strsplit}} for further description of
  most arguments.
}
\examples{

# make example text string
txtStr <- paste(LETTERS[1:10], 13:22, letters[11:20], sep = "_")
txtStr

strPart(x = txtStr, part = 1, split = "_")
strPart(x = txtStr, part = 2, split = "_")
strPart(x = txtStr, part = 3, split = "_")

strPart(x = txtStr, part = 3, split = "_", returnVector = FALSE)

}
