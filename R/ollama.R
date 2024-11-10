#' Confirm connection to ollama is working
#'
#' @description
#' `test_ollama_connection` says hello and uses the name of the person(s) as an argument. y
#'
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom glue glue
#'
#' @details
#' A simple function to test the connect to ollama
#'
#' @returns Returns TRUE if the connection works, or an error object it does not.
#' @examples
#' #test_ollama_connection(ip_ad="127.0.0.1", port="11434")
#' 
#' @param ip_ad the IP address of the server running ollama. Default is localhost 127.0.0.1.
#' @param port the port used to run the ollama service. Default is 11374.
#' @export
test_ollama_connection <- function(ip_ad="127.0.0.1", port="11434") {
  
  url <- glue::glue("{ip_ad}:{port}")
  
  tryCatch({
    req <- httr2::request(url)
    test_result <- req |> httr2::req_perform()
    if (test_result$status_code=="200") { return(TRUE)}
  },
  error=function(error) {print(error)})
}

#' Define a connection to a local ollama server
#'
#' @description
#' `get_ollama_connection` sets variables that are used to define the connection to an ollama server.
#'
#' @details
#' A simple function to set the connection details to an ollama server.
#'
#' @param ip_ad the IP address of the server running ollama. Default is localhost 127.0.0.1.
#' @param port the port used to run the ollama service. Default is 11374.
#' @returns A list that contains information about the ollama connection.
#' @examples
#' #get_ollama_connection(ip_ad="127.0.0.1", port="11434")
#' 
#' @export
get_ollama_connection <- function(ip_ad="127.0.0.1", port="11434") {
  
  if (test_ollama_connection()==T) {
    return(list(ollama_server_ip=ip_ad, ollama_server_port=port))
  }
  
}


#' Get a list of models available from the ollama server
#'
#' @description
#' `get_list_ollama_models` gets a list of models available from the ollama server
#'
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom glue glue
#'
#' @details
#' A simple function to get the models available from the ollama server.
#'
#' @param ollama_connection an ollama connection objection object that was created by get_ollama_connection().
#' @returns A dataframe that contains information about the LLM available from the running ollama server
#' @examples
#' ollama_conn <- get_ollama_connection()
#' list_of_models <- get_list_ollama_models(ollama_conn)
#' 
#' @export
get_list_ollama_models <- function(ollama_connection) {
  
  url <- glue::glue("{ollama_connection$ollama_server_ip}:{ollama_connection$ollama_server_port}/api/tags")
  tryCatch({
    req <- httr2::request(url) 
    result <- req |>
      httr2::req_perform() 
    if (result$status_code=="200") { 
        return(httr2::resp_body_json(result) |> convert_ollama_tags_response_to_tibble())
      }
    },
  error=function(error) {print(error)})
  8
}


#' Applies a processing skill to the current workflow
#'
#' @description
#' `apply_processing_skill` applies a processing skill (i.e. a pre-engineered prompt)
#'
#' @importFrom glue glue
#' @importFrom cli cli_alert
#'
#' @details
#' A simple function to apply a pre-defined prompt format (i.e. a processing skill) to the current workflow.
#' You can find out about the existing processing skills by using the list_processing_skills() function.
#' This function is not usually made to be used on its own, but through the set_processing_skill() function
#'
#' @param prompts_vector a vector containing existing prompts or text on which the skill will be applied.
#' @param processing_skill character vector containing the filepath to the processing skill text template of the skill to be applied.
#' @param processing_skill_args a list of additional parameters to feed to a processing skill, if it accepts them. 
#' @returns A new vector containing the modified prompts after applying the processing skill template.
apply_processing_skill <- function(prompts_vector, processing_skill=NA, processing_skill_args=list()) {
  
  if (!is.na(processing_skill)) {
  
    if (!identical(processing_skill_args,list())) {
      list2env(processing_skill_args,envir = .GlobalEnv)
      cli::cli_alert("Applying additional variables to working env")
    }  
  
  text_to_replace <- prompts_vector
  new_vector <- glue::glue(processing_skill)
  return(new_vector) } else {
    return(prompts_vector)
  } 
}


#' Get a completion from ollama server
#'
#' @description
#' `get_ollama_completion` get a completion from the ollama server API
#'
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom httr2 resp_body_json
#' @importFrom httr2 req_body_json
#' @importFrom glue glue
#' 
#' @details
#' A simple function to test the connect to ollama
#'
#' @param ollama_connection a connection object that has the information on how to connect to the ollama server
#' @param model Name of the model to use on the ollama server. Note that you can only use the models that are available.
#' @param prompts_vector A vector containing one or more messages acting as prompts for the completion.
#' @param output_text_only  A boolean value (default False) indicating if you just want the text message as output (TRUE) or the whole response coming from the server.
#' @param num_predict The number of tokens to generate in the response (maximum amount)
#' @param system_prompt The system prompt used for your LLM.
#' @param temperature The temperature value for the answer of the model. A temperature of 0 gives always the same answer. A temperature of 1 has a lot more variation. Default is 0.8.
#' @export
get_ollama_completion <- function(ollama_connection, 
                                  model, 
                                  prompts_vector, 
                                  output_text_only=F, 
                                  num_predict=200, 
                                  temperature=0.8, 
                                  system_prompt=NA) {
  
  url <- glue::glue("{ollama_connection$ollama_server_ip}:{ollama_connection$ollama_server_port}/api/generate")
  # streaming if off by default right now
  stream=F
  
  tryCatch({
    result_list <- list()
    for (one_prompt in prompts_vector) {
    req <- httr2::request(url)
    
    data_prep = list(
      model=model, 
      prompt=one_prompt,
      stream=F,
      options=list(
        num_predict=num_predict, 
        temperature=temperature)
    )
    
    if (!is.na(system_prompt)) {
      
      data_prep[["system"]] <- system_prompt
    }
    
    result <- req |> 
      httr2::req_body_json(data = data_prep 
        ) |>
      httr2::req_perform() 
    if (result$status_code=="200") { 
      if (output_text_only==F) {
      result_list[[one_prompt]] <- httr2::resp_body_json(result)
      } else {
      result_list[[one_prompt]] <- httr2::resp_body_json(result)$response
      }
      
      }
    }
    # after loop is done
    if (output_text_only==F) {
      result_flat <- data.table::rbindlist(result_list)
    } else {
      result_flat <- unname(unlist(result_list))
    }
    return(result_flat)
    },
  error=function(error) {print(error)})
  
}

#' Convert an ollama server completion response to a tibble
#'
#' @description
#' `get_ollama_completion` get a completion from the ollama server API
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#'
#' @details
#' A function to convert the full output of ollama to a tibble format
#'
#' @param ollama_response This should contain the full response of a ollama API completion call.  
convert_ollama_completion_response_to_tibble <- function(ollama_response) {
  
  result_tibble <- ollama_response |> tibble::as_tibble() |>  dplyr::select(-context) |> dplyr::distinct() 
  return(result_tibble)
}


#' Convert an ollama server tags response to a tibble
#'
#' @description
#' `convert_ollama_tags_response_to_tibble` get a completion from the ollama server API
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr bind_cols
#' @importFrom tibble add_column
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#' @importFrom data.table rbindlist
#'
#' @details
#' A function to convert the full output of ollama to a tibble format
#'
#' @param ollama_response This should contain the full response of the ollama API call for tags. 
convert_ollama_tags_response_to_tibble <- function(ollama_response) {
  
  result_list <- list()
  i <- 1
  for (one_model in ollama_response$models) {
    #one_model <- ollama_response$models[[1]]
    temp <- one_model |> tibble::as_tibble()
    flattened_list <- purrr::map(one_model, ~ .x |> unlist(recursive = FALSE))
    details_unpacked <- flattened_list$details |> tibble::as_tibble()
    flattened_list[["details"]] <- NULL
    result <- flattened_list |> as_tibble()
    result <- result |> dplyr::bind_cols(details_unpacked)
    result_list[[i]] <- result
    i <- i + 1
  }
  result <- data.table::rbindlist(result_list,fill = T)
  return(result)
}


#' Get chat completion from ollama server
#'
#' @description
#' `get_ollama_chat_completion` get a chat completion from the ollama server API
#'
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom httr2 resp_body_json
#' @importFrom httr2 req_body_json
#' @importFrom glue glue
#' @importFrom cli cli_alert
#' @importFrom cli cli_abort
#' 
#' @details
#' A function to get a chat completion from an ollama server.
#'
#' @param ollama_connection a connection object that has the information on how to connect to the ollama server
#' @param model Name of the model to use on the ollama server. Note that you can only use the models that are available.
#' @param embedding_model Name of the embedding model to use on the ollama server to create embeddings on the fly for prompts.
#' @param prompts_vector a vector containing one or more Messages acting as prompt for the completion.
#' @param images_vector an optional vector (defaults to NA) containing images to be send to the model for completion.
#' @param output_text_only  A Boolean value (default False) indicating if you just want the text message as output (TRUE) or the whole response coming from the server.
#' @param num_predict The number of tokens to generate in the response (maximum amount). Defaults to 200.
#' @param temperature The temperature value for the answer of the model. A temperature of 0 gives always the same answer. A temperature of 1 has a lot more variation. Default is 0.8.
#' @param role the role that is taken during the chat prompt. Typically you should be the "user", but it can be also "assistant" or "system". Defaults to "user".
#' @param repeat_penalty The penalty you give to the model so that you avoid it repeats the same tokens. Default is 1.2.
#' @param seed The seed used to generate the answer. Note that the seed has no effect if the temperature is zero. By default it is a random number between 1 and 10000000
#' @param system_prompt The system prompt used for your LLM.
#' @param context_info This is used for RAG, when you want to provide context to your LLM to ground its answers. Currently incompatible with template_prompt
#' @param context_usage_mandatory a boolean value (defaults to FALSE) to let the LLM know if it should ONLY use the context to answer, or if it can use the context optionally to answer a question.
#' @param tools a R list of tools (i.e. functions) that can be passed to the LLM. Note that this is only supported by specific LLMs like Llama3.1 - this may not work at all on LLMs that were not trained on tools calling.
#' @examples
#' conn <- get_ollama_connection()
#' get_ollama_chat_completion(ollama_connection=conn, 
#' model+"llama3:8b-instruct-q4_K_S", prompts_vector="is the sky blue at night?")
#' @export
get_ollama_chat_completion <- function(ollama_connection, 
                                       model,
                                       embedding_model,
                                       prompts_vector,
                                       images_vector=NA,
                                       output_text_only=F, 
                                       num_predict=200,
                                       temperature=0.8,
                                       role="user",
                                       repeat_penalty=1.2,
                                       seed=sample(1:10000000,1),
                                       system_prompt=NA,
                                       context_info=NA,
                                       context_usage_mandatory=FALSE,
                                       num_ctx=NA,
                                       tools=NA,
                                       vision=FALSE
                                       ) {
  
  url <- glue::glue("{ollama_connection$ollama_server_ip}:{ollama_connection$ollama_server_port}/api/chat")
  
  # streaming if off by default right now
  stream=F

  if (vision==TRUE) {
    if (length(prompts_vector)!=length(images_vector)) {
      cli::cli_abort("The images_vector needs to have the same length as the prompts vector")
    }
  } else { 
    # if vision is turned off, ignore all images passed to the prompt
    images_vector <- NA 
    }
  
  if (is.na(seed)) {
    seed <- sample(1:10000000,1)
  }
  
  if (is.na(num_ctx)) {
    num_ctx <- 2048
  }
  
  tryCatch({
    
    #print(prompts_vector)
    
    options_combined <- list(
      num_predict=num_predict, 
      temperature=temperature,
      repeat_penalty=repeat_penalty,
      num_ctx=num_ctx,
      seed=seed)
    
    #print(options_combined)
    
    req <- httr2::request(url) 
    
    result_list <- list()
    #print(length(prompts_vector))
    
    prompt_index <- 0
    
    for (one_prompt in prompts_vector) {
      
      prompt_index <- prompt_index + 1
      
      # in case there is context info to pass
      if (any(!is.na(context_info))) {
        
        question_vec <- get_ollama_embeddings(ollama_connection = ollama_connection,input = one_prompt, model=embedding_model)
        results_similarity <- retrieve_similar_vectors(context_df = context_info, prompt_vector = question_vec, max_results = 10, similarity_threshold = 0.6)
        context_elements <- results_similarity |> paste0(collapse="\n")

        if (context_usage_mandatory==FALSE) {
        
        one_prompt <- glue::glue("Here is some additional context that you can use to formulate your answer, if relevant: 
    Context ---
    {context_elements}
    End of Context ---
    If the context is not helpful to answer the question, you can ignore the context.
    {one_prompt}")
        
        }
        
        if (context_usage_mandatory==TRUE) {
                
        one_prompt <- glue::glue("Here is some factual context that needs to be used to formulate answer:
    Context ---
    {context_elements}
    End of Context ---
    Only answer the following question using the context only and NO prior information! If the context is not helpful to answer the question, ONLY reply 'There is no such information in the context.' and nothing else.
    Keep your answer short, precise, to the point, without making useless comments about unrelated facts.
    {one_prompt}")
        
        }
          
      }
    
    #using a case_when results in something really weird, a duplication of the message list. ifelse fixes that. Not sure why?
    if (is.na(system_prompt)) {
    messages_to_send <-   list(
      list(
        role=role,
        content=one_prompt
      ))
    if (vision==T) {
      messages_to_send[[1]][["images"]] <- list(images_vector[prompt_index])
    }
    } else {
      messages_to_send <- list(
        list(
          role="system",
          content=system_prompt
        ),
        list(
          role=role,
          content=one_prompt
        ))
      if (vision==T) {
        messages_to_send[[2]][["images"]] <- list(images_vector[prompt_index])
      }
    }
    
    #print(messages_to_send)
      
      data_to_send <- list(
        model=model,
        messages=messages_to_send,
        stream=F,
        options=options_combined
      )
      
      if (any(!is.na(tools))) {
        cli::cli_alert("Adding tools")
        data_to_send$tools <- tools
      }
      
    result <- req |> 
      httr2::req_body_json(data = data_to_send
      ) |>
      httr2::req_perform()
    
    if (result$status_code=="200") { 
      # it tools are used we force output_text_only=F for now to analyze the answer
      if (output_text_only==F | any(!is.na(tools))) {
        #print("going through tools path")
        result_list[[one_prompt]] <- httr2::resp_body_json(result)
        
        # process tool calls if they exist
        if("tool_calls" %in% names(result_list[[one_prompt]]$message)) { 
          messages_to_send[[1]] <- append(messages_to_send[[1]],result_list[[one_prompt]]$message)
          arguments <- list()
          tool_result <- list()
          i <- 0
          for (one_tool in result_list[[one_prompt]]$message$tool_calls) {
            i <- i + 1
            function_name_found <- one_tool$`function`$name
            arguments_to_get <- (names(one_tool$`function`$arguments))
            for (one_arg in arguments_to_get) {
              arguments[[one_arg]] <- one_tool$`function`$arguments[[one_arg]]
            }
            #calling the function here
            #implement a check if the function does not exist
            if (function_name_found %in% list_global_functions()) {
              tool_result[[i]] <- do.call(get(function_name_found), arguments)
            } else {
              cli_alert("The tool called requested the function {function_name_found} but it does not seem to exist in the global environment.")
              stop()
            }
          }

        answer_is_complete <- FALSE
        while(!answer_is_complete) {
          if (is.na(system_prompt)) {
            messages_to_send <-   list(
              list(
                role=role,
                content=one_prompt
              ))
          } else {
            messages_to_send <- list(
              list(
                role="system",
                content=system_prompt
              ),
              list(
                role=role,
                content=one_prompt
              ))
          }
          
          current_length <- length(messages_to_send)
          # add second item that comes from the tool request
          messages_to_send[[current_length+1]] <- result_list[[one_prompt]]$message
          
          # iterate on tool calls in case several tools are called
          for (one_tool_result in tool_result) {
            
            # check current length of messages list
            current_length <- length(messages_to_send)
            
            # add the answer from the tool that comes from the function call. Note that llama3.1 at least excepts some kind of JSON output.
            messages_to_send[[current_length+1]] <- list(role="tool",content=one_tool_result)
          }
          
          data_to_send <- list(
            model=model,
            messages=messages_to_send,
            stream=F,
            options=options_combined
          )
          
          result <- req |> 
            httr2::req_body_json(data = data_to_send
            ) |> 
            #httr2::req_verbose(body_req=T) |>
            httr2::req_perform()
          
          # next line makes the result available for text extraction
          if (output_text_only==F) {
            result_list[[one_prompt]] <- httr2::resp_body_json(result)  
          }
          if (output_text_only==T) {
          result_list[[one_prompt]] <- httr2::resp_body_json(result)$message$content
          }
          
          answer_is_complete <- TRUE
        }
        
        
      } 
      
      } else {
        # the result list is a list that contains fields like model, created_at, 
        # message (message contains sub fields such as role and content)
        # done_reason, done, total_duration, load_duration, 
        # prompt_eval_count, prompt_eval_duration, eval_count, eval_duration
        # message -> content contains the text answer from the LLM
        result_list[[one_prompt]] <- httr2::resp_body_json(result)$message$content
      }
    }
    
    }
    
    # final formatting
    if (output_text_only==F) {
      result_flat <- data.table::rbindlist(result_list)
    } else {
      result_flat <- unname(unlist(result_list))
    }

    return(result_flat)
  },
  error=function(error) {print(error)})
  
}



#' Get information about one ollama model
#'
#' @description
#' `get_ollama_model_info` Gets information about one model available through ollama.
#'
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom glue glue
#'
#' @details
#' Gets information about one model available through ollama.
#'
#' @param ollama_connection a connection object that has the information on how to connect to the Oollama server
#' @param model the name of the model that you want to get info from.
#'
#' @examples
#' #ollama_conn <- get_ollama_connection()
#' #get_ollama_model_info(ollama_connection= ollama_conn, model="llama3:8b-instruct-q5_0")
#' 
#' @export
get_ollama_model_info <- function(ollama_connection, model) {
  
  url <- glue::glue("{ollama_connection$ollama_server_ip}:{ollama_connection$ollama_server_port}/api/show")
  tryCatch({
    req <- httr2::request(url) 
    result <- req |> 
      httr2::req_body_json(data = list(
        name = model)) |> 
      httr2::req_perform() 
    if (result$status_code=="200") { 
      return(httr2::resp_body_json(result) |> convert_ollama_model_info_response_to_tibble())
    }
  },
  error=function(error) {print(error)})
  
}

#' Convert ollama response for model info to a tibble
#'
#' @description
#' `convert_ollama_model_info_response_to_tibble` Converts the ollama response for model info to a tibble
#'
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom glue glue
#'
#' @details
#' Converts the ollama response for model info to a tibble
#'
#' @param ollama_response a response object coming from a payload delivered by the ollama server instance.
convert_ollama_model_info_response_to_tibble <- function(ollama_response) {

    temp <- ollama_response |> tibble::as_tibble()
    details <- temp$details
    details <- details |> tibble::as_tibble() |> dplyr::select(-families)
    result <- temp |> dplyr::select(-details) |> tibble::add_column(details) |> dplyr::distinct()
    return(result)

}


#' Get embeddings for a piece of context through an ollama server instance
#'
#' @description
#' `get_ollama_embeddings` Retrieves the embeddings for a string (can be a series of words, sentence, paragraph, or whole document) through ollama
#'
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom glue glue
#'
#' @details
#' Converts the ollama response for model info to a tibble
#'
#' @param ollama_connection a connection object that has the information on how to connect to the ollama server
#' @param model The model used to generates the embeddings. Defaults to "all-minilm". You can select any other model available for generating embeddings.
#' @param input A single character vector that contains the text to convert to embeddings
#' 
#' @examples
#' conn <-get_ollama_connection()
#' embeddings <- get_ollama_embeddings(conn, input="Hi there, how are you doing?")
#' @export
get_ollama_embeddings <- function(ollama_connection, model="bge-large", input) {
  
  url <- glue::glue("{ollama_connection$ollama_server_ip}:{ollama_connection$ollama_server_port}/api/embed")
  
  tryCatch({
    req <- httr2::request(url) 
    result <- req |> 
      httr2::req_body_json(data = list(
        model = model,
        input = input)) |> 
      httr2::req_perform() 
    if (result$status_code=="200") {
      return(httr2::resp_body_json(result)$embeddings |> unlist() |> unname())
    }
  },
  error=function(error) {print(error)})
  
}

# we can add more metadata here as well
make_embedding_list <- function(ollama_connection,input, model="bge-large") {
  
  return(list(embeddings=get_ollama_embeddings(
    ollama_connection = ollama_connection, 
    input=input,
    model = model),
    text=input))
  
}




# wip, not done
semantic_splitter <- function(n_sentences_to_group, embed_model, similarity_breakpoint_threshold=0.75) {
  
  # get several sentences at once on two chunks
  # calculate similarity between both chunks
  # if similar enough, group them together
  # if different enough, put them in different block
  # continue from next block and loop
  
}

#' Get embeddings for a piece of context through an ollama server instance020
#'
#' @description
#' `generate_document_embeddings` Generates vector embeddings for a document through ollama
#'
#' @details
#' Converts the ollama response for model info to a tibble
#'
#' @param ollama_connection a connection object that has the information on how to connect to the ollama server
#' @param document_path the document path to create embeddings from
#' @param splitter the type of splitter that is going to be used. Defaults to "sentence". "paragraph" can also be used.
#' @param model The model used to generates the embeddings. Defaults to "bge-large". You can select any other model available for generating embeddings.
#' 
#' @examples
#' #conn<-get_ollama_connection()
#' #embeddings<- get_ollama_embeddings(ollama_connection=conn, prompt="Roma is a beautiful city.")
#' @export
generate_document_embeddings <- function(ollama_connection, document_path, splitter="sentence", model="bge-large") {
  
  document_text <- readLines(document_path,warn = F) |> paste(collapse = "\n")
  
  if (splitter=="sentence") {
    chunks <- document_text |> split_text_as_sentences()
  }
  
  if (splitter=="paragraph") {
    chunks <- document_text |> split_text_as_paragraphs()
  }
  
  list_embeddings <- list()
  i <- 1
  for (one_chunk in chunks) {
    list_embeddings[[i]] <- make_embedding_list(ollama_connection = ollama_connection,
                                              input = one_chunk, model=model)
    i <- i + 1
  }
  
  return(list_embeddings)
}


make_matrix_of_embeddings <- function(ollama_connection,text_vectors) {
  
  result_list <- lapply(text_vectors,  make_embedding_list, ollama_connection=ollama_connection)
  mat <- Reduce(rbind, lapply(result_list, function(x) x$embeddings))
  return(mat)

}



unload_model <- function(workflow_obj) {
  
  #workflow_obj <- caca
  
  if ("connector" %in% names(workflow_obj)) {
  if (workflow_obj[["connector"]]!="ollama") {
    cli::cli_abort("Error: this function to unload a model only works with Ollama.")
  }
    
    url <- glue::glue("{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/api/generate")
    data <- list("model"=workflow_obj[["model"]], "keep_alive"=0)
  }
  if ("connector" %in% names(workflow_obj[["workflow"]])) {
    if (workflow_obj[["workflow"]][["connector"]]!="ollama") {
      cli::cli_abort("Error: this function to unload a model only works with Ollama.")
    }
    
    url <- glue::glue("{workflow_obj[['workflow']][['ip_addr']]}:{workflow_obj[['workflow']][['port']]}/api/generate")
    data <- list("model"=workflow_obj[['workflow']][["model"]], "keep_alive"=0)
  }
  
  if ("workflows" %in% names(workflow_obj)) {
    length <- length(workflow_obj[["workflows"]])
    url <- glue::glue("{workflow_obj[['workflows']][[length]][['ip_addr']]}:{workflow_obj[['workflows']][[length]][['port']]}/api/generate")
    data <- list("model"=workflow_obj[['workflows']][[length]][["model"]], "keep_alive"=0)
  }
  
  req <- httr2::request(url) 
  result <- req |> 
    httr2::req_body_json(data = data
    ) |>
    httr2::req_perform()
  
  return(workflow_obj)
}