% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflows.R
\name{set_overall_background}
\alias{set_overall_background}
\title{Set overall background info for your model before an answer is formulated}
\usage{
set_overall_background(workflow_obj, overall_background)
}
\arguments{
\item{workflow_obj}{A workflow object containing all parameters describing the workflow required}

\item{overall_background}{a single-element text vector that contains the background information you want the system prompt to have}
}
\value{
a workflow object with the new added overall_background parameter
}
\description{
\code{set_overall_background} lets you give some additional background info that is supposed to be used by your model for every answer.
}
\details{
Setting background info can help with general knowledge or reference that you expect the LLM to have.
Say, you have specific personal information that you want to enter that is relevant for answering several questions, you want to put it in here.
This is different from RAG, where a RAG system will basically pull relevant information for every specific question.
Here, this is more akin to letting the LLM have fundamental, general knowledge.
}
\examples{
my_workflow <- ai_workflow() |> 
set_system_prompt(system_prompt="You are a helpful AI assistant. 
Answer to the best of your knowledge") |>
set_audience("Marketing Professionals") |>
set_overall_background("Our company, YOMAN & Co, has been struggling with our
 recent products because of lack of market understanding.") 

}
