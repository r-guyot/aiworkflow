% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflows.R
\name{split_text_as_paragraphs}
\alias{split_text_as_paragraphs}
\title{Split text into paragraphs}
\usage{
split_text_as_paragraphs(text)
}
\arguments{
\item{text}{a single piece of text to break down into paragraphs.}
}
\description{
\code{split_text_as_paragraphs} Splits text into sentences-based chunks.
}
\details{
Splits text into paragraphs-based chunks, leveraging the tokenizers library.
}
\examples{
# c("Hi! How are you?\n\n Do you want to go for a walk?") |> split_text_as_paragraphs()

}
