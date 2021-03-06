% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patviewer.r
\name{patview.download.file}
\alias{patview.download.file}
\title{Downloads PatentsView.org files}
\usage{
patview.download.file(url, dest.file = character(0),
  dest.dir = character(0))
}
\arguments{
\item{url}{URL of the file}

\item{dest.file}{Safe as name. Default is the same as it is in the url.}

\item{dest.dir}{Where to save. Default to the working directory patview-data-XXXXXXXX-zip, where XXXXXXXX is the verstion (i.e., date) of files automatically detected from the url (assumes certain form of url)}
}
\value{
Saved file path.
}
\description{
Downloads PatentsView.org files
}
