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
  
    return(list(ip_ad=ip_ad, port=port))

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


get_llamacpp_chat_completion <- function(llamacpp_connection,
                                                    model,
                                                    embedding_model,
                                                    prompts_vector,
                                                    available_tools = NULL,
                                                    tool_functions = NULL,
                                                    max_iterations = 5,
                                                    num_predict = 200,
                                                    temperature = 0.8,
                                                    role = "user",
                                                    repeat_penalty = 1.2,
                                                    seed = sample(1:10000000, 1),
                                                    system_prompt = NA,
                                                    context_info = NA,
                                                    context_usage_mandatory = FALSE,
                                                    num_ctx = NA,
                                                    output_text_only = FALSE,
                                                    verbose = FALSE) {
  
  require(httr2)
  require(jsonlite)
  
  # Construct base URL
  base_url <- paste0("http://", llamacpp_connection$ip_ad, ":", 
                     llamacpp_connection$port, "/v1/chat/completions")
  
  # Function to process a single prompt with tools
  process_single_prompt_with_tools <- function(prompt_text, current_seed = NULL) {
    
    # Initialize conversation history
    messages <- list()
    
    # Add system prompt if provided
    if (!is.na(system_prompt) && !is.null(system_prompt) && nchar(system_prompt) > 0) {
      messages <- append(messages, list(list(
        role = "system",
        content = system_prompt
      )))
    }
    
    # Add context info if provided
    if (!is.na(context_info) && !is.null(context_info) && nchar(context_info) > 0) {
      context_role <- if (context_usage_mandatory) "system" else "user"
      messages <- append(messages, list(list(
        role = context_role,
        content = paste0("Context: ", context_info)
      )))
    }
    
    # Add initial user message
    messages <- append(messages, list(list(
      role = role,
      content = prompt_text
    )))
    
    iteration <- 0
    
    while (iteration < max_iterations) {
      iteration <- iteration + 1
      
      if (verbose) {
        cat(paste0("\n--- Iteration ", iteration, " for prompt: '", 
                   substr(prompt_text, 1, 50), "...' ---\n"))
      }
      
      # Build request body
      body <- list(
        model = model,
        messages = messages,
        temperature = temperature,
        max_tokens = num_predict,
        seed = if (!is.null(current_seed)) current_seed else seed,
        frequency_penalty = repeat_penalty - 1
      )
      
      # Add tools only if provided
      if (!is.null(available_tools) && length(available_tools) > 0) {
        body$tools <- available_tools
      }
      
      if (!is.na(num_ctx) && !is.null(num_ctx)) {
        body$n_ctx <- num_ctx
      }
      
      # Make the request
      response <- request(base_url) |>
        req_headers("Content-Type" = "application/json") |>
        req_body_json(body) |>
        req_timeout(300) |>
        req_retry(max_tries = 3) |>
        req_error(is_error = function(resp) FALSE) |>
        req_perform()
      
      if (resp_status(response) != 200) {
        warning(paste0("Request failed with status ", resp_status(response), 
                       ": ", resp_body_string(response)))
        return(NULL)
      }
      
      result <- resp_body_json(response)
      assistant_message <- result$choices[[1]]$message
      
      # Add assistant message to conversation
      messages <- append(messages, list(assistant_message))
      
      # Check if assistant wants to call tools
      if (!is.null(assistant_message$tool_calls) && 
          length(assistant_message$tool_calls) > 0) {
        
        if (verbose) {
          cat(paste0("Tool calls requested: ", length(assistant_message$tool_calls), "\n"))
        }
        
        # Execute each tool call
        for (tool_call in assistant_message$tool_calls) {
          tool_name <- tool_call[["function"]][["name"]]
          tool_args <- fromJSON(tool_call[["function"]][["arguments"]])
          tool_id <- tool_call$id
          
          if (verbose) {
            cat(paste0("Calling tool: ", tool_name, "\n"))
            cat(paste0("Arguments: ", toJSON(tool_args, auto_unbox = TRUE), "\n"))
          }
          
          # Execute the tool function if tool_functions is available
          if (!is.null(tool_functions) && tool_name %in% names(tool_functions)) {
            tryCatch({
              # Call the actual R function with the arguments
              tool_result <- do.call(tool_functions[[tool_name]], as.list(tool_args))
              tool_output <- toJSON(tool_result, auto_unbox = TRUE)
            }, error = function(e) {
              tool_output <- toJSON(list(error = paste0("Tool execution failed: ", e$message)), 
                                    auto_unbox = TRUE)
            })
          } else {
            tool_output <- toJSON(list(error = paste0("Tool '", tool_name, "' not found or no tool functions provided")), 
                                  auto_unbox = TRUE)
          }
          
          if (verbose) {
            cat(paste0("Tool result: ", tool_output, "\n"))
          }
          
          # Add tool result to messages
          messages <- append(messages, list(list(
            role = "tool",
            tool_call_id = tool_id,
            name = tool_name,
            content = tool_output
          )))
        }
        
        # Continue loop to let assistant process tool results
        next
        
      } else {
        # No tool calls - we're done
        if (verbose) {
          cat("No tool calls. Conversation complete.\n")
        }
        
        if (output_text_only) {
          return(assistant_message$content)
        } else {
          return(list(
            content = assistant_message$content,
            messages = messages,
            iterations = iteration,
            full_response = result
          ))
        }
      }
    }
    
    # Max iterations reached
    warning(paste0("Maximum iterations (", max_iterations, ") reached for prompt: '", 
                   substr(prompt_text, 1, 50), "...'"))
    
    if (output_text_only) {
      return(assistant_message$content)
    } else {
      return(list(
        content = assistant_message$content,
        messages = messages,
        iterations = iteration,
        full_response = result,
        max_iterations_reached = TRUE
      ))
    }
  }
  
  # Process prompts vector
  if (length(prompts_vector) == 1) {
    # Single prompt
    return(process_single_prompt_with_tools(prompts_vector[1]))
    
  } else {
    # Multiple prompts - vectorized operation
    results <- list()
    
    for (i in seq_along(prompts_vector)) {
      # Generate a different seed for each prompt
      current_seed <- seed + i - 1
      
      results[[i]] <- process_single_prompt_with_tools(prompts_vector[i], current_seed)
    }
    
    # If output_text_only, return as character vector
    if (output_text_only) {
      return(sapply(results, function(x) if (is.null(x)) NA_character_ else x))
    } else {
      return(results)
    }
  }
}




get_llamacpp_completion <- function(llamacpp_connection, 
                                    model,
                                    prompts_vector,
                                    output_text_only = FALSE, 
                                    num_predict = 200,
                                    temperature = 0.8,
                                    repeat_penalty = 1.2,
                                    top_k = 40,
                                    top_p = 0.95,
                                    min_p = 0.05,
                                    seed = sample(1:10000000, 1),
                                    stop = NULL,
                                    num_ctx = NA,
                                    suffix = NULL,
                                    echo = FALSE,
                                    n = 1) {
  
  require(httr2)
  require(jsonlite)
  
  # Construct base URL for completions endpoint
  base_url <- paste0("http://", llamacpp_connection$ip_ad, ":", 
                     llamacpp_connection$port, "/v1/completions")
  
  # Function to process a single prompt
  process_single_completion <- function(prompt_text, current_seed = NULL) {
    
    # Build request body
    body <- list(
      model = model,
      prompt = prompt_text,
      max_tokens = num_predict,
      temperature = temperature,
      top_k = top_k,
      top_p = top_p,
      min_p = min_p,
      seed = if (!is.null(current_seed)) current_seed else seed,
      frequency_penalty = repeat_penalty - 1,
      echo = echo,
      n = n
    )
    
    # Add optional parameters
    if (!is.na(num_ctx) && !is.null(num_ctx)) {
      body$n_ctx <- num_ctx
    }
    
    if (!is.null(stop) && length(stop) > 0) {
      body$stop <- stop
    }
    
    if (!is.null(suffix)) {
      body$suffix <- suffix
    }
    
    # Make the request
    response <- request(base_url) |>
      req_headers(
        "Content-Type" = "application/json"
      ) |>
      req_body_json(body) |>
      req_timeout(300) |>
      req_retry(max_tries = 3) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    
    # Check for errors
    if (resp_status(response) != 200) {
      warning(paste0("Request failed with status ", resp_status(response), 
                     ": ", resp_body_string(response)))
      return(NULL)
    }
    
    # Parse response
    result <- resp_body_json(response)
    
    # Return based on output_text_only flag
    if (output_text_only) {
      return(result$choices[[1]]$text)
    } else {
      return(result)
    }
  }
  
  # Process prompts vector
  if (length(prompts_vector) == 1) {
    # Single prompt
    return(process_single_completion(prompts_vector[1]))
    
  } else {
    # Multiple prompts - vectorized operation
    results <- list()
    
    for (i in seq_along(prompts_vector)) {
      # Generate a different seed for each prompt
      current_seed <- seed + i - 1
      
      results[[i]] <- process_single_completion(prompts_vector[i], current_seed)
    }
    
    # If output_text_only, return as character vector
    if (output_text_only) {
      return(sapply(results, function(x) if (is.null(x)) NA_character_ else x))
    } else {
      return(results)
    }
  }
}


load_llamacpp_model <- function(llamacpp_connection, model_name) {
  
  require(httr2)
  require(jsonlite)
  
  # Construct URL for model unloading
  load_url <- paste0("http://", llamacpp_connection$ip_ad, ":", 
                       llamacpp_connection$port, "/models/load")
  
  # Build request body
  body <- list(model = model_name)
  
  # Make POST request
  response <- request(load_url) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(body) |>
    req_method("POST") |>
    req_timeout(60) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  
  # Check response
  if (resp_status(response) == 200) {
    cat(paste0("Model '", model_name, "' loaded successfully.\n"))
    return(TRUE)
  } else {
    warning(paste0("Failed to load model. Status: ", resp_status(response), 
                   "\nResponse: ", resp_body_string(response)))
    return(FALSE)
  }
}


unload_llamacpp_model <- function(llamacpp_connection, model_name) {
  
  require(httr2)
  require(jsonlite)
  
  # Construct URL for model unloading
  unload_url <- paste0("http://", llamacpp_connection$ip_ad, ":", 
                       llamacpp_connection$port, "/models/unload")
  
  # Build request body
  body <- list(model = model_name)
  
  # Make POST request
  response <- request(unload_url) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(body) |>
    req_method("POST") |>
    req_timeout(60) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  
  # Check response
  if (resp_status(response) == 200) {
    cat(paste0("Model '", model_name, "' unloaded successfully.\n"))
    return(TRUE)
  } else {
    warning(paste0("Failed to unload model. Status: ", resp_status(response), 
                   "\nResponse: ", resp_body_string(response)))
    return(FALSE)
  }
}