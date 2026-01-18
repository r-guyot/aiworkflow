#' Confirm connection to a Llama.cpp server is working
#'
#' @description
#' `test_llamacpp_connection` tests a connection to a llama.cpp server.
#'
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom glue glue
#'
#' @details
#' A simple function to test the connection to a llama.cpp server.
#'
#' @param ip_ad the IP address of the server running Llama.cpp. Default is localhost 127.0.0.1.
#' @param port the port used to run the Llama.cpp service. Default is 8080.
#' @export
test_llamacpp_connection <- function(ip_ad="127.0.0.1", port="8080") {
  
  url <- glue::glue("{ip_ad}:{port}")
  
  tryCatch({
    req <- request(url)
    test_result <- req |> req_perform()
    if (test_result$status_code=="200") { return(TRUE)}
  },
  error=function(error) {print(error)})
  
}



get_llamacpp_connection <- function(ip_ad="127.0.0.1", port="8080") {
  
    return(list(llamacpp_server_ip=ip_ad, llamacpp_server_port=port))

}




get_llamacpp_model_info <- function(llamacpp_connection, model) {
  
  url <- glue::glue("{llamacpp_connection$llamacpp_server_ip}:{llamacpp_connection$llamacpp_server_port}/api/show")
  tryCatch({
    req <- httr2::request(url) 
    result <- req |> 
      httr2::req_body_json(data = list(
        name = model)) |> 
      httr2::req_perform() 
    if (result$status_code=="200") { 
      return(httr2::resp_body_json(result) |> convert_llamacpp_model_info_response_to_tibble())
    }
  },
  error=function(error) {print(error)})
  
}

get_list_llamacpp_models <- function(ollama_connection) {
  
  url <- glue::glue("{llamacpp_connection$llamacpp_server_ip}:{llamacpp_connection$llamacpp_server_port}/api/tags")
  tryCatch({
    req <- httr2::request(url) 
    result <- req |>
      httr2::req_perform() 
    if (result$status_code=="200") { 
      return(httr2::resp_body_json(result) |> convert_ollama_tags_response_to_tibble())
    }
  },
  error=function(error) {print(error)})
}


convert_llamacpp_tags_response_to_tibble <- function(llamacpp_response) {
  
  # to implement
  
}


convert_llamacpp_model_info_response_to_tibble <- function(llamacpp_response) {
  
  temp <- llamacpp_response
  
  context_length <- temp$model_info$llama.context_length
  template <- temp$template |> unique()
  parent_model <- temp$details$parent_model
  format <- temp$details$format
  parameter_size <- temp$details$parameter_size
  quantization_level <- temp$details$quantization_level
  
  res <- tibble::tibble(context_length, template, parent_model, format, parameter_size, quantization_level)
  
  return(res)
}