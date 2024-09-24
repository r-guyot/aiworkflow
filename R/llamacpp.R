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
