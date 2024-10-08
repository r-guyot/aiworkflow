% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflows.R
\name{set_style_of_voice}
\alias{set_style_of_voice}
\title{Define a specific style of voice that you want the LLM to use when answering}
\usage{
set_style_of_voice(workflow_obj, style_of_voice)
}
\arguments{
\item{workflow_obj}{an ai_workflow object created by ai_workflow() in the first place.}

\item{style_of_voice}{a text description of the person of the style of person you want the LLM to imitate.}
}
\value{
a workflow object with the new added style_of_voice parameter
}
\description{
\code{set_style_of_voice} lets you define a specific style of voice that you want the LLM to use when answering
}
\details{
This lets you define a specific style of voice that you want the LLM to use when answering
}
\examples{
my_workflow <- ai_workflow() |> 
set_system_prompt(system_prompt="You are a helpful AI assistant. 
Answer to the best of your knowledge") |>
set_audience("Marketing Professionals") |>
set_style_of_voice("Snoop Dog") 

}
