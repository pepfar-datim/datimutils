% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMetadata.R
\name{metadataFilter}
\alias{metadataFilter}
\alias{\%.in\%}
\alias{\%.~in\%}
\alias{\%.token\%}
\alias{\%.~token\%}
\alias{\%.le\%}
\alias{\%.lt\%}
\alias{\%.ge\%}
\alias{\%.gt\%}
\alias{\%.Like\%}
\alias{\%.~Like\%}
\alias{\%.^Like\%}
\alias{\%.~^Like\%}
\alias{\%.Like$\%}
\alias{\%.~Like$\%}
\alias{\%.like\%}
\alias{\%.^like\%}
\alias{\%.~^like\%}
\alias{\%.~like\%}
\alias{\%.like$\%}
\alias{\%.~like$\%}
\alias{\%.eq\%}
\alias{\%.~eq\%}
\title{metadataFilter(values, property, operator)}
\usage{
metadataFilter(values, property, operator)

property %deq% value

property %d!eq% value

property %dlike% value

property %d!like% value

property %din% values

property %d!in% values

property \%.in\% values

property \%.~in\% values

property \%.token\% values

property \%.~token\% values

property \%.le\% values

property \%.lt\% values

property \%.ge\% values

property \%.gt\% values

property \%.Like\% values

property \%.~Like\% values

property \%.^Like\% values

property \%.~^Like\% values

property \%.Like$\% values

property \%.~Like$\% values

property \%.like\% values

property \%.^like\% values

property \%.~^like\% values

property \%.~like\% values

property \%.like$\% values

property \%.~like$\% values

property \%.eq\% values

property \%.~eq\% values
}
\arguments{
\item{values}{the values in property:operator:value}

\item{property}{the property in property:operator:value}

\item{operator}{the operator in property:operator:value}
}
\value{
property:operator:value
}
\description{
used to format filter strings for metadata calls
}
