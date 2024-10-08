% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qdrant.R
\name{qdrant_get_collection_details}
\alias{qdrant_get_collection_details}
\title{Qdrant: Get collection details}
\usage{
qdrant_get_collection_details(conn, collection_name = NA_character_)
}
\arguments{
\item{conn}{a connection object created by get_qdrant_connection()}

\item{collection_name}{the name of a collection for which you want to check the details. Defaults to NA.}
}
\description{
\code{qdrant_get_collection_details} establishes a connection with a qdrant instance and returns the details of a specific collection.
'
}
\details{
Returns the details of a specific qdrant collection.
}
\examples{
conn <- get_qdrant_connection()
qdrant_create_new_collection(conn, collection_name="story_of_my_life")
qdrant_get_collection_details(conn, collection_name="story_of_my_life")
qdrant_delete_collection(conn, collection_name="story_of_my_life")

}
