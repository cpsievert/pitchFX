\name{docsToDataFrame}
\alias{docsToDataFrame}
\title{Turn XML documents into a Data Frames}
\usage{
  docsToDataFrame(docs, node, fields, urls)
}
\arguments{
  \item{docs}{XML documents}

  \item{node}{XML node of interest}

  \item{fields}{"Comlpete" set of field names for the data
  frame}

  \item{urls}{character vector of URLs used to build data
  frame}
}
\value{
  Returns a data frame.
}
\description{
  This function adds NAs to missing attributes.
}

