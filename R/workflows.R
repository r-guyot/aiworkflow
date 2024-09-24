#' Execute an AI workflow
#'
#' @description
#' `execute_workflow` executes an AI workflow by combining prompt vectors and a workflow object.
#'
#' @importFrom  cli cli_alert
#' @details
#' This function executes an AI workflow by combining prompt vectors and a workflow object.
#'
#' @param prompts_vector A vector containing the prompts to be executed by the AI workflow 
#' @param workflow_obj A workflow object containing all parameters describing the flow required
#' @export
execute_workflow <- function(prompts_vector, workflow_obj) {
  
  workflow_obj <- workflow_obj |> set_default_missing_parameters_in_workflow()
  
  if ("current_timedate" %in% names(workflow_obj)) {
    workflow_obj[["system_prompt"]] <- paste0(workflow_obj[["system_prompt"]],"\n","The current time and date is: ",
                                              workflow_obj[["current_timedate"]],". Take this in account in case your answers are time related.")
  }
  
  if ("overall_background" %in% names(workflow_obj)) {
    workflow_obj[["system_prompt"]] <- paste0(workflow_obj[["system_prompt"]],"\n","Here is some additional background that is very important for you to take in account:\n",
                                              workflow_obj[["overall_background"]],"\n\nAdapt your answers accordingly to ensure you take this context in account.")
  }
  
  if ("audience" %in% names(workflow_obj)) {
    workflow_obj[["system_prompt"]] <- paste0(workflow_obj[["system_prompt"]],"\n","You are writing for the following audience: ",
                                              workflow_obj[["audience"]],", so adapt your answers accordingly to match their expectations.")
  }
  
  if ("style_of_voice" %in% names(workflow_obj)) {
    workflow_obj[["system_prompt"]] <- paste0(workflow_obj[["system_prompt"]],"\n","Answer in the voice and speaking style of the following character: ",
                                              workflow_obj[["style_of_voice"]],", and adapt your answers accordingly to be as similar as possible to this persona.")
  }
  
  if ("format_request" %in% names(workflow_obj)) {
    workflow_obj[["system_prompt"]] <- paste0(workflow_obj[["system_prompt"]],"\n",workflow_obj[["format_request"]])
  }
  
  if ("processing_skill" %in% names(workflow_obj)) {
    processing_skill <- workflow_obj[["processing_skill"]]
    
    processing_skill_args <- workflow_obj[["processing_skill_args"]]
    
    lines <- unlist(strsplit(processing_skill, "\n"))
    system_pos <- as.numeric(which(grepl("@SYSTEM", lines))) + 1
    chat_pos <- as.numeric(which(grepl("@CHAT", lines))) -1
    part_for_system_prompt <- lines[system_pos:chat_pos]
    part_for_system_prompt <- paste0(part_for_system_prompt, collapse = "\n")
    
    workflow_obj[["system_prompt"]] <- paste0(part_for_system_prompt,"\n\n",workflow_obj[["system_prompt"]])
    
    chat_pos <- as.numeric(which(grepl("@CHAT", lines))) +1
    total_length <- length(lines)
    part_for_single_prompt <- lines[chat_pos:total_length]
    part_for_single_prompt <- paste0(part_for_single_prompt, collapse = "\n")
    
    processing_skill <- part_for_single_prompt
    
  } else {
    processing_skill <- NA
  }
  
  if ("tools" %in% names(workflow_obj)) {
    tools_to_pass <- workflow_obj[["tools"]]
  } else {
    tools_to_pass <- NA
  }
  
  if ("context" %in% names(workflow_obj)) {
    context_to_pass <- workflow_obj[["context"]]
  } else {
    context_to_pass <- NA
  }
  
  if (workflow_obj[["connector"]] == "ollama") {
    
    ollama_conn <- get_ollama_connection(ip_ad = workflow_obj[["ip_addr"]], port = workflow_obj[["port"]])
    
    if (workflow_obj[["mode"]] == "completion") {
      cli::cli_alert("Completion mode")
      result <- get_ollama_completion(ollama_connection = ollama_conn, 
                                      model = workflow_obj[["model"]],
                                      prompts_vector = apply_processing_skill(prompts_vector, processing_skill = processing_skill, processing_skill_args = processing_skill_args),
                                      output_text_only = T,
                                      num_predict = workflow_obj[["n_predict"]],
                                      temperature = workflow_obj[["temperature"]],
                                      system_prompt = workflow_obj[["system_prompt"]] 
      )
    }
    
    if (workflow_obj[["mode"]] == "chat") {
      cli::cli_alert("Chat mode")
      result <- get_ollama_chat_completion(ollama_connection = ollama_conn, 
                                           model = workflow_obj[["model"]],
                                           prompts_vector = apply_processing_skill(prompts_vector, processing_skill = processing_skill, processing_skill_args = processing_skill_args),
                                           output_text_only = T,
                                           num_predict = workflow_obj[["n_predict"]],
                                           temperature = workflow_obj[["temperature"]],
                                           system_prompt = workflow_obj[["system_prompt"]],
                                           context_info = context_to_pass,
                                           tools = tools_to_pass
      )
    }
    
    return(result)
  }
  
}

#' Process Prompts starting from a workflow
#'
#' @description
#' `process_prompts` is a way to process a vector of prompts by starting from a workflow.
#'
#' @details
#' This function provides a way to process a vector of prompts by starting from a workflow.
#'
#' @param prompts_vector A vector containing the prompts to be executed by the AI workflow 
#' @param workflow_obj A workflow object containing all parameters describing the flow required
#' @export
process_prompts <- function(workflow_obj, prompts_vector) {
  
  # if the workflow is not atomic but a chain, follow this path
  if ("workflow_type" %in% names(workflow_obj)) {
  if (workflow_obj[["workflow_type"]]=="chain") {
    workflow_memory <- list()
    workflow_memory[["workflow"]] <- list()
    workflow_memory[["prompts_vector"]] <- list()
    workflow_memory[["res"]] <- list()
    for (i in 1:length(workflow_obj[["workflow_element"]])) {
      workflow_memory[["workflow"]][[i]] <- workflow_obj[["workflow_element"]][[i]]
      if (i==1) {
      workflow_memory[["prompts_vector"]][[i]] <- list(prompts_vector)
      }
      workflow_memory[["res"]][[i]] <- list(execute_workflow(prompts_vector = unlist(workflow_memory[["prompts_vector"]][[i]]), workflow_obj = workflow_memory[["workflow"]][[i]]))
      if (i < length(workflow_obj[["workflow_element"]])) {
        workflow_memory[["prompts_vector"]][[i+1]] <- workflow_memory[["res"]][[i]]
      }
    }
    return(workflow_memory)
  }
  } else {
  
  # create memory of workflow objects used
  workflow_memory <- list(workflow=workflow_obj, prompts_vector=list(prompts_vector))
  workflow_memory[["res"]] <- list(execute_workflow(prompts_vector = prompts_vector, workflow_obj = workflow_obj))
  return(workflow_memory)
  }
  
}

#' Switch to workflow
#'
#' @description
#' `switch_to_workflow` is a function that makes it possible to chain several workflows once after the other using pipes
#'
#' @details
#' This function will send the output from the previous workflow to the next one.
#'
#' @param new_workflow A workflow object to execute after the last one 
#' @param workflow A workflow object containing all parameters describing the flow required
#' @export
switch_to_workflow <- function(workflow, new_workflow) {
  
  current_length_wflow <- length(workflow[["workflow"]])
  workflow[["workflow"]][[current_length_wflow+1]] <- new_workflow
  current_length <- length(workflow[["res"]])
  workflow[["res"]][[current_length+1]] <- execute_workflow(prompts_vector = workflow[["res"]][[current_length]], workflow_obj = workflow[["workflow"]][[current_length_wflow+1]])
  return(workflow)
  
}

#' Display Intermediate Answer
#'
#' @description
#' `display_intermediate_answer` is a function that displays the results of the last executed workflow
#'
#' @details
#' This function will print the output of the last workflow executed. Especially helpful when chaining several workflows.
#'
#' @param workflow A workflow object containing all parameters describing the flow required
#' @export
display_intermediate_answer <- function(workflow) {
  
  if ("res" %in% names(workflow)) {
    current_length <- length(workflow[["res"]])
    cli::cli_alert("Intermediate answer from last step:")
    cli::cli_inform(workflow[["res"]][[current_length]])
  }
  
  return(workflow)
}

#' Pull Final Answer
#'
#' @description
#' `pull_final_answer` is a function to extract the final answer from a series of workflows
#'
#' @details
#' This function will extract the final text result that's usually coming from a series of workflows.
#'
#' @param workflow A workflow object containing all parameters describing the flow required
#' @export
pull_final_answer <- function(workflow) {
  
  if ("res" %in% names(workflow)) {
    current_length <- length(workflow[["res"]])
    return(workflow[["res"]][[current_length]])
  } else {
    cli::cli_abort("No result found through the workflow.")
  }
  
}


#' Save workflow
#'
#' @importFrom jsonlite toJSON
#' @importFrom cli cli_alert
#'
#' @description
#' `save_workflow` makes it possible to save your workflow settings for later re-use.
#' @details
#' A function to save your workflow settings to a specific JSON file. 
#' To reload the settings use the load_workflow() function.
#' 
#' @param workflow_obj A workflow object containing all parameters describing the workflow required
#' @param filepath filepath to a JSON file that will capture your workflow configuration.
#' @returns the workflow itself after saving it, so that it can be included inside a series of pipes.
#' @examples
#' my_workflow <- ai_workflow() |> set_connector("ollama")  |>
#'  set_model(model_name= "llama3:8b-instruct-q5_K_S") |>
#'  set_n_predict(500) |>
#'  set_temperature(0.8) |> 
#'  set_default_missing_parameters_in_workflow() |> 
#'  set_system_prompt("You are a computer hardware and software specialist.") |>
#'  save_workflow(filepath="myworkflow.json")
#' @export
save_workflow <- function(workflow_obj, filepath) {
  
  json_object <- workflow_obj |> jsonlite::toJSON(pretty = T) 
  writeLines(json_object, con = filepath)
  cli::cli_alert("The workflow has been saved to {filepath}.")
  return(workflow_obj)
}

#' Load workflow
#'
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_alert
#' 
#' @description
#' `load_workflow` makes it possible to reload your workflow from a saved configuration file in JSON.
#' @details
#' A function to reload your workflow from a specific JSON file that you have created with save_workflow_settings().
#' 
#' @param filepath filepath to the JSON file that contains the parameters of the workflow, that you have created with save_workflow().
#' 
#' @returns the workflow after loading all of its settings and parameters.
#' @examples
#' my_workflow <- load_workflow(filepath="myworkflow.json")
#' @export
load_workflow <- function(filepath) {
  
  tryCatch({
  parameters <- readLines(filepath) |> jsonlite::fromJSON()
  cli::cli_alert("The workflow has been successfully reloaded from {filepath}.")
  return(parameters)
  }, error= function(e) {e} )
  
}


#' Set Defaults for missing workflow parameters
#'
#' @description
#' `set_default_missing_parameters_in_workflow` ensure all parameters are present by filling the gaps
#' 
#' @details
#' A function to add default values for missing parameters in the user-defined workflow.
#'
#' @param workflow_obj A workflow object containing all parameters describing the flow required
#' @param silent ensure that the action happens without any warning sent to the standard output
#' @returns the workflow object along with the added parameters that were missing.
#' @examples
#' # my_workflow <- ai_workflow() |> set_default_missing_parameters_in_workflow()
#' @export
set_default_missing_parameters_in_workflow <- function(workflow_obj, silent=F) {
  
  frequency_penalty_dflt <- 1.0
  presence_penalty_dflt <- 1.5
  repeat_penalty_dflt <- 1.2
  temperature_dflt <- 0.8
  n_predict_dflt <- 200
  mode_dflt <- "chat"
  system_prompt_dflt <- "You are a helpful AI assistant."
  
  if (is.null(workflow_obj[["frequency_penalty"]])) {
    workflow_obj <- workflow_obj |> set_frequency_penalty(frequency_penalty = frequency_penalty_dflt)
    if (silent==F) { cli::cli_alert("Frequency Penalty was not specified and given a default value of {frequency_penalty_dflt}.") }
  }
  if (is.null(workflow_obj[["presence_penalty"]])) {
    workflow_obj <- workflow_obj |> set_presence_penalty(presence_penalty = presence_penalty_dflt)
    if (silent==F) { cli::cli_alert("Presence Penalty was not specified and given a default value of {presence_penalty_dflt}.") }
  }
  if (is.null(workflow_obj[["repeat_penalty"]])) {
    workflow_obj <- workflow_obj |> set_repeat_penalty(repeat_penalty = repeat_penalty_dflt)
    if (silent==F) { cli::cli_alert("Repeat Penalty was not specified and given a default value of {repeat_penalty_dflt}.") }
  }
  if (is.null(workflow_obj[["temperature"]])) {
    workflow_obj <- workflow_obj |> set_temperature(temperature = temperature_dflt)
    if (silent==F) { cli::cli_alert("Temperature was not specified and given a default value of {temperature_dflt}.") }
  }
  if (is.null(workflow_obj[["n_predict"]])) {
      workflow_obj <- workflow_obj |> set_n_predict(n_predict = n_predict_dflt)
      if (silent==F) { cli::cli_alert("N_predict was not specified and given a default value of {n_predict_dflt}.") }
  }
  if (is.null(workflow_obj[["mode"]])) {
    workflow_obj <- workflow_obj |> set_mode(mode = mode_dflt)
    if (silent==F) { cli::cli_alert("Mode was not specified and '{mode_dflt}' was selected by default.") }
  }
  if (is.null(workflow_obj[["system_prompt"]])) {
    workflow_obj <- workflow_obj |> set_system_prompt(system_prompt = system_prompt_dflt)
    if (silent==F) { cli::cli_alert("System Prompt was not specified and given a default value of '{system_prompt_dflt}'.") }
  }
  return(workflow_obj)
}

#' Define AI workflow
#'
#' @description
#' `ai_workflow` creates a list that encapsulates parameters related to the AI workflow you want to design.
#'
#' @details
#' The starting point of defining an AI workflow in this package. A workflow object contains a connector and a model as placeholders.
#' @examples
#' my_workflow <- ai_workflow()
#' @returns an empty list in the ai_workflow() format that will be used to capture all settings and parameters of the workflow.
#' @export
ai_workflow <- function() {
  return(list(connector=NA,model=NA))
}

#' Set the model to be used by the workflow
#'
#' @description
#' `set_model` sets the model to be used by a workflow object.
#'
#' @details
#' A simple function to set the model to be used. Note that this model needs to be available at the instance you connect to.
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param model_name the name of the model to use in the workflow
#' @examples
#' my_workflow <- ai_workflow() |> 
#' set_model(model_name="llama3:8b-instruct-q5_0")
#' 
#' @export
set_model <- function(workflow_obj, model_name) {
  workflow_obj[["model"]] <- model_name
  return(workflow_obj)
}

#' Set the connector required to operate the workflow. 
#'
#' @description
#' `set_connector` sets the connector type expected for the server calls. This can for example be an Ollama server or a Llama.cpp server.
#'
#' @details
#' Set the connector required to operate the workflow. A connector is a server that will be reached by an API. It can either be a local or a remote server.
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param connector the name of the connector type to use. It can be either ollama or llamacpp (to be taken care of in later versions). 
#' @examples
#' my_workflow <- ai_workflow() |> 
#' set_connector(connector="ollama")
#' @export
set_connector <- function(workflow_obj, connector) {
  workflow_obj[["connector"]] <- connector
  if (connector=="ollama") {
    workflow_obj[["ip_addr"]] <- "127.0.0.1"
    cli::cli_alert("Default IP address has been set to 127.0.0.1.")
    workflow_obj[["port"]] <- "11434"
    cli::cli_alert("Default port has been set to 11434.")
  } else {
    stop("Connectors others than Ollama are not currently supported.")
  }
  return(workflow_obj)
}

#' Set the IP Address required to connect to an API server. 
#'
#' @description
#' `set_ip_addr` sets the IP Address related to the API server.
#'
#' @details
#' Set the IP Address related to the API server.
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param ip_addr IP address of the server.
#' @examples 
#' wflow <- ai_workflow() |> set_connector("ollama") |> 
#' set_ip_addr(ip_addr="127.0.0.1")
#' wflow <- ai_workflow() |> set_connector("ollama") |> 
#' set_ip_addr(ip_addr="192.168.1.56")
#' 
#' @export
set_ip_addr <- function(workflow_obj, ip_addr) {
  workflow_obj[["ip_addr"]] <- ip_addr
  cli::cli_alert("IP address has been changed to {ip_addr}.")
  return(workflow_obj)
}

#' Set the port required to connect to the API server. 
#'
#' @description
#' `set_port` sets the port related to the API server.
#'
#' @details
#' Set the IP Address related to the API server.
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param port the port used to access the API server.
#' @examples
#' wflow <- ai_workflow() |> set_connector("ollama") |> 
#' set_ip_addr(ip_addr="127.0.0.1") |> set_port(port="11434")
#' @export
set_port <- function(workflow_obj, port) {
  workflow_obj[["port"]] <- port
  cli::cli_alert("Port has been changed to {port}.")
  return(workflow_obj)
}

#' Set the number of tokens to be predicted (maximum) by the flow.
#'
#' @description
#' `set_n_predict` sets the maximum number of tokens to be predicted by the flow.
#'
#' @details
#' This sets the maximum number of tokens to be predicted by the flow. Note that this does not mean that the LLM will constrain its answer to that number, 
#' If the planned answer exceeds the n_predict value, the answer will simply stop at that point.
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param n_predict the number of tokens to be predicted (maximum) by the flow.
#' @examples 
#' wflow <- ai_workflow() |> set_connector("ollama") |> 
#' set_n_predict(n_predict=500)
#' @export
set_n_predict <- function(workflow_obj, n_predict) {
  workflow_obj[["n_predict"]] <- n_predict
  return(workflow_obj)
}

#' Set the temperature of the model used by the workflow.
#'
#' @description
#' `set_temperature` sets the temperature for the model used in the workflow.
#'
#' @details
#' Set the temperature to be used by the model in the workflow. A temperature of zero will always give the same answer. 
#' A temperature of 1 will give very random answers that may not make full sense. 
#' Ideally if you want some level of randomness while still remaining sensible answers, you should target a value around 0.6 or 0.7.
#' If you are trying to validate an AI workflow for specific answers to specific prompts, it is highly recommended to work with a temperature of zero.
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param temperature the temperature value used by the model. Should be between 0 and 1 (boundaries included).
#' @examples
#' my_workflow <- ai_workflow() |> 
#' set_model(model_name="llama3:8b-instruct-q5_0") |> 
#' set_temperature(0.8)
#' @export
set_temperature <- function(workflow_obj, temperature) {
  if (is.numeric(temperature) & temperature>=0 & temperature <=1) {
    workflow_obj[["temperature"]] <- temperature
    return(workflow_obj)
  } else {
    stop("Temperature should be numerical and in between 0 and 1.")
  }
}

#' Set the repeat penalty of the model used by the flow.
#'
#' @description
#' `set_repeat_penalty` sets the repeat penalty related to the model in the workflow.
#'
#' @details
#' Set the repeat_penalty to be used by the model in the workflow.
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param repeat_penalty the repeat penalty value used by the model. A value of 1 means there is no penalty. A value higher than 1 means there is increased penalty on repetition of tokens.
#' @examples
#' my_workflow <- ai_workflow() |> 
#' set_model(model_name="llama3:8b-instruct-q5_0") |> 
#' set_repeat_penalty(1.3)
#' 
#' @export
set_repeat_penalty <- function(workflow_obj, repeat_penalty) {
  workflow_obj[["repeat_penalty"]] <- repeat_penalty
  return(workflow_obj)
}

#' Set the presence penalty of the model used by the flow.
#'
#' @description
#' `set_presence_penalty` sets the presence penalty related to the model in the workflow.
#'
#' @details
#' Set the presence_penalty to be used by the model in the workflow.
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param presence_penalty the presence penalty value used by the model. A value closer to 1 encourages the model to generate more novel and diverse text. A lower value, closer to 0, encourages cliche phrases.
#' @examples
#' my_workflow <- ai_workflow() |> 
#' set_model(model_name="llama3:8b-instruct-q5_0") |> 
#' set_presence_penalty(0.9)
#' 
#' @export
set_presence_penalty <- function(workflow_obj, presence_penalty) {
  workflow_obj[["presence_penalty"]] <- presence_penalty
  return(workflow_obj)
}

#' Set the frequency penalty of the model used by the flow.
#'
#' @description
#' `set_frequency_penalty` sets the frequency penalty related to the model in the workflow.
#'
#' @details
#' Set the frequency_penalty to be used by the model in the workflow.
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param frequency_penalty the frequency penalty value used by the model. A higher value (closer to 1) makes the AI avoid repeating words or phrases, while a lower value (closer to 0) allows for more repetitions.
#' @returns the workflow object with the frequency penalty specified as new parameter to be applied.
#' @examples
#' my_workflow <- ai_workflow() |> 
#' set_model(model_name="llama3:8b-instruct-q5_0") |> 
#' set_frequency_penalty(0.6)
#' @export
set_frequency_penalty <- function(workflow_obj, frequency_penalty) {
  workflow_obj[["frequency_penalty"]] <- frequency_penalty
  return(workflow_obj)
}

#' Set the seed of the model used by the workflow.
#'
#' @description
#' `set_seed` sets the seed for the model.
#'
#' @details
#' Set the seed to be used by the model in the workflow.
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param seed the seed to be used by the model in the workflow, if you intend to fix it. The same seed will give the same answer. It is interesting to fix it if you want to compare the effect of other parameters.
#' @returns the workflow object with the seed to be applied.
#' @examples
#' my_workflow <- ai_workflow() |> 
#' set_model(model_name="llama3:8b-instruct-q5_0") |> 
#' set_seed(12312312312)
#' @export
set_seed <- function(workflow_obj, seed) {
  if (is.numeric(seed)) {
    workflow_obj[["seed"]] <- seed
    return(workflow_obj)
  } else {
    stop("Seed needs to be numeric.")
  }
}

#' Set the mode of the model used by the workflow.
#'
#' @description
#' `set_mode` sets the mode of the model in the workflow. This usually means either 'chat', 'completion' or 'embeddings'.
#'
#' @details
#' This sets the mode of the model in the workflow. This usually means either 'chat','completion' or 'embeddings'.
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param mode the mode to be used by the model in the workflow - it needs to be either 'chat', 'completion' or 'embeddings'.
#' @returns the workflow object with the mode to be used applied as a new parameter.
#' @examples
#' my_workflow <- ai_workflow() |> 
#' set_model(model_name="llama3:8b-instruct-q5_0") |> 
#' set_mode("chat")
#' @export
set_mode <- function(workflow_obj, mode) {
  if (mode %in% c("completion","chat","embeddings")) {
    workflow_obj[["mode"]] <- mode
    return(workflow_obj)
  } else {
    cli::cli_alert_warning("Mode can only take three values, 'completion', 'chat' and 'embeddings'.")
    stop()
  }
}

#' Set the system prompt to be used by the model.
#'
#' @description
#' `set_system_prompt` sets the system prompt to be used by the model.
#'
#' @details
#' This sets the system prompt of the model in the workflow. This is where you can give additional guidance or a personality to change the way your model will answer prompts.
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param system_prompt the mode to be used by the model in the workflow - it needs to be either 'chat' or 'completion'.
#' @returns the workflow object with the system prompt specified as new parameter.
#' @examples
#' my_workflow <- ai_workflow() |> 
#' set_system_prompt(system_prompt="You are a helpful AI assistant. 
#' Answer to the best of your knowledge")
#' my_workflow <- ai_workflow() |> 
#' set_system_prompt(system_prompt="You are a helpful AI assistant
#'  and an expert in financial analysis.")
#' @export
set_system_prompt <- function(workflow_obj, system_prompt) {
    workflow_obj[["system_prompt"]] <- system_prompt
    return(workflow_obj)
}

#' Set the processing skill that you want to give the workflow.
#'
#' @description
#' `set_processing_skill` sets the processing skill to give to the workflow.
#'
#' @details
#' This sets the processing skill that you want to give the workflow. You can list up the processing skills available by default with list_processing_skills().
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param processing_skill string - the processing skill you want to give to the workflow.
#' @param ... additional parameters that will be treated if they are supported by the processing skill
#' @returns the workflow object with the appropriate processing skill applied on the prompt vector(s)
#' @examples
#' my_workflow <- ai_workflow() |> set_model(model_name="llama3:8b-instruct-q5_0") |> 
#' set_processing_skill("fix_copy")
#' @export
set_processing_skill <- function(workflow_obj, processing_skill, ...) {
  
  # Capture additional arguments
  additional_args <- list(...)
  
  processing_skill_source <- glue::glue("{processing_skill}.txt")
  inst_dir <- here::here("inst")
  skills <- list_processing_skills()
  if (processing_skill %in% skills) {
    processing_skill_prompt <- readLines(glue::glue("{inst_dir}/skills/{processing_skill}.txt"),warn = F) |> paste(collapse = "\n")
    
    # find all arguments required for replacement in the prompts
    pattern <- "\\{([^}]*)\\}"
    
    # Use gregexpr to find all matches
    matches <- gregexpr(pattern, processing_skill_prompt, perl = TRUE)
    
    # Extract the matches
    matched_strings <- regmatches(processing_skill_prompt, matches) |> unlist() |> unique()
    matched_variables <- gsub(matched_strings, pattern="\\{|\\}",replacement="")
    matched_variables <- matched_variables[matched_variables!="text_to_replace"]
    unmatched_variables <- matched_variables[!(matched_variables %in% names(additional_args))]
    
    if (length(unmatched_variables)>0) {
      stop(glue::glue("This processing skill needs that you pass the extra arguments as well: {paste(unmatched_variables,collapse=', ')}"))
    }
    
    workflow_obj[["processing_skill"]] <- processing_skill_prompt
    
    if (length(additional_args)>0) {
    workflow_obj[["processing_skill_args"]] <- additional_args
    } else {
      workflow_obj[["processing_skill_args"]] <- list()
    }
    
    return(workflow_obj)
  }
}

#' list the processing skills 
#'
#' @description
#' `list_processing_skills` lists the processing skills (i.e. prompts already defined) that you can use out of the box with this package.
#'
#' @details
#' This gives you a vector of all the skills available to use by default. This can then be used along with the set_processing_skill() function.
#' @returns a list of vectors describing the skills available currently in the package.
#'
#' @examples
#' list_processing_skills()
#'
#' @export
list_processing_skills <- function() {
  
  inst_dir <- here::here("inst")
  files_in_inst <- list.files(glue::glue("{inst_dir}/skills"), recursive = TRUE, full.names = FALSE)
  skills <- gsub(files_in_inst, pattern="\\.txt", replacement = "")
  return(skills)
  
}

#' Inspect a specific processing skill 
#'
#' @description
#' `inspect_processing_skill` lets you inspect how a specific processing skill is written.
#'
#' @details
#' This outputs the text contained in the processing skill. If the processing skill name you are looking for is not found, it will throw an error.
#' @returns a text vector containing the exact prompt used by that processing skill.
#'
#' @param processing_skill the name of the processing skill you want the model to use for a specific task
#' @examples
#' inspect_processing_skill("break_down_task")
#'
#' @export
inspect_processing_skill <- function(processing_skill) {
  
  if (processing_skill %in% list_processing_skills()) {
    inst_dir <- here::here("inst")
    cat(paste0(readLines(con = glue::glue("{inst_dir}/skills/{processing_skill}.txt")),collapse = "\n"))
  } else {
    cli::cli_alert("Could not find the processing skill {processing_skill} you asked for.")
  }
  
}

#' Define a specific audience you want the model to prepare an answer for
#'
#' @description
#' `set_audience` lets you define for which audience the model should answer. This will tweak its answers accordingly to match their expectations.
#'
#' @details
#' This lets you modify a typical workflow by adding information about the expected audience.
#' @returns a workflow object with the new added audience parameter
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param audience a text description of the audience you want the LLM to write for.
#' @examples
#' my_workflow <- ai_workflow() |> 
#' set_system_prompt(system_prompt="You are a helpful AI assistant. 
#' Answer to the best of your knowledge") |>
#' set_audience(audience="Marketing Professionals")
#' 
#' my_workflow <- ai_workflow() |> 
#' set_system_prompt(system_prompt="You are a helpful AI assistant. 
#' Answer to the best of your knowledge") |>
#' set_audience(audience="5 years old kids")
#' 
#' @export
set_audience <- function(workflow_obj,audience) {
  
  workflow_obj[["audience"]] <- audience
  return(workflow_obj)

}

#' Define a specific style of voice that you want the LLM to use when answering
#'
#' @description
#' `set_style_of_voice` lets you define a specific style of voice that you want the LLM to use when answering
#'
#' @details
#' This lets you define a specific style of voice that you want the LLM to use when answering
#' @returns a workflow object with the new added style_of_voice parameter
#'
#' @param workflow_obj an ai_workflow object created by ai_workflow() in the first place.
#' @param style_of_voice a text description of the person of the style of person you want the LLM to imitate.
#' @examples
#' my_workflow <- ai_workflow() |> 
#' set_system_prompt(system_prompt="You are a helpful AI assistant. 
#' Answer to the best of your knowledge") |>
#' set_audience("Marketing Professionals") |>
#' set_style_of_voice("Snoop Dog") 
#'
#' @export
set_style_of_voice <- function(workflow_obj, style_of_voice) {
  
  workflow_obj[["style_of_voice"]] <- style_of_voice
  return(workflow_obj)
  
}


#' Set overall background info for your model before an answer is formulated
#'
#' @description
#' `set_overall_background` lets you give some additional background info that is supposed to be used by your model for every answer.
#'
#' @details
#' Setting background info can help with general knowledge or reference that you expect the LLM to have. 
#' Say, you have specific personal information that you want to enter that is relevant for answering several questions, you want to put it in here.
#' This is different from RAG, where a RAG system will basically pull relevant information for every specific question. 
#' Here, this is more akin to letting the LLM have fundamental, general knowledge.
#' 
#' @returns a workflow object with the new added overall_background parameter
#'
#' @param workflow_obj A workflow object containing all parameters describing the workflow required
#' @param overall_background a single-element text vector that contains the background information you want the system prompt to have
#'
#' @examples
#' my_workflow <- ai_workflow() |> 
#' set_system_prompt(system_prompt="You are a helpful AI assistant. 
#' Answer to the best of your knowledge") |>
#' set_audience("Marketing Professionals") |>
#' set_overall_background("Our company, YOMAN & Co, has been struggling with our
#'  recent products because of lack of market understanding.") 
#'
#' @export
set_overall_background <- function(workflow_obj, overall_background) {
  
  workflow_obj[["overall_background"]] <- overall_background
  return(workflow_obj)
  
}

#' Set the current time and date as addition reference
#'
#' @description
#' `set_current_time_and_date_reference` lets you set the current time and date as addition reference as part of the background for the LLM's answer.
#'
#' @details
#' Setting some additional time and date reference in the background info to be used by the LLM. 
#' This can be very useful if you expect your LLM to answer about questions related to the future, present or past.
#'
#' @importFrom lubridate wday
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @importFrom lubridate day
#' @importFrom glue glue
#'
#' @param workflow_obj A workflow object containing all parameters describing the workflow required
#' 
#' @returns a workflow object with the new added current_timedate parameter
#'
#' @examples
#' my_workflow <- ai_workflow() |> 
#' set_system_prompt(system_prompt="You are a helpful AI assistant. 
#' Answer to the best of your knowledge") |>
#' set_current_time_and_date_reference()
#'
#' @export
set_current_time_and_date_reference <- function(workflow_obj) {
  
  current_date <- Sys.time()
  day_of_week <- lubridate::wday(current_date,label = T) |> as.character()
  month <- lubridate::month(current_date,label = T) |> as.character()
  year <- lubridate::year(current_date)
  day <- lubridate::day(current_date)
  time <- format(Sys.time(), "%Hh:%Mm:%Ss")
  workflow_obj[["current_timedate"]] <- glue::glue("{time} on {day_of_week} {month} {day}, {year}")
  return(workflow_obj)
  
}

#' Adds context to be used by the model when answering
#'
#' @description
#' `add_context` lets you add context info (in the shape of a context dataframe containing embeddings and text info) to supplement or narrow down the answers from the LLM.
#'
#' @details
#' For this to work you need to add a context_df that was build using the generate_document_embeddings function.
#' 
#' @param workflow_obj A workflow object containing all parameters describing the workflow required
#' @param context_df a dataframe containing the text embeddings and text information that can be used to retrieve relevant context
#' @returns a workflow object with the new added context parameter.
#'
#' @examples
#' 
#' conn <- get_ollama_connection()
#' 
#' document <- "Standing proudly on the Île de la Cité in the heart of Paris, 
#' France's capital city, lies one of the world's most beloved and historic 
#' landmarks: the magnificent Notre Dame Cathedral. This Gothic masterpiece 
#' has been welcoming pilgrims and tourists alike for over 850 years, since its 
#' construction began in 1163 under King Louis VII. With its towering spires, 
#' stunning stained glass windows, and intricate stone carvings, this beautiful 
#' church is a testament to medieval architecture and engineering skill. 
#' Unfortunately, a devastating fire ravaged the cathedral on April 15, 2019, 
#' but thanks to swift action from firefighters and restoration efforts 
#' underway, Notre Dame continues to inspire awe in those who visit her."
#' 
#' writeLines(document, con = "doc1.txt")
#' 
#' write_vectors_to_feather_file(file_name = "doc1.feather",
#' vector_data = generate_document_embeddings(conn, 
#' document_path = "doc1.txt", splitter = "paragraph"))
#' 
#' notre_dame_embeddings <- load_context_embeddings_from_feather_files(filenames = "doc1.feather")
#' 
#' my_workflow <- ai_workflow() |> 
#' set_system_prompt(system_prompt="You are a helpful AI assistant. 
#' Answer to the best of your knowledge.") |>
#' add_context(context_df = notre_dame_embeddings)
#'
#' @export
add_context <- function(workflow_obj, context_df) {
  
  workflow_obj[["context"]] <- context_df
  return(workflow_obj)
  
}



#' Make Cosine Similarity Matrix
#'
#' @description
#' `make_cosine_similarity_matrix` lets you calculate the cosine similarity of a given matrix.
#'
#' @details
#' Calculate the cosine similarity matrix.
#'
#' @param input_matrix  the input matrix that is used to derive a cosine similarity matrix in the end
#' 
#' @examples
#' conn <- get_ollama_connection()
#' 
#' document <- "Standing proudly on the Île de la Cité in the heart of Paris, 
#' France's capital city, lies one of the world's most beloved and historic 
#' landmarks: the magnificent Notre Dame Cathedral. This Gothic masterpiece 
#' has been welcoming pilgrims and tourists alike for over 850 years, since its 
#' construction began in 1163 under King Louis VII. With its towering spires, 
#' stunning stained glass windows, and intricate stone carvings, this beautiful 
#' church is a testament to medieval architecture and engineering skill. 
#' Unfortunately, a devastating fire ravaged the cathedral on April 15, 2019, 
#' but thanks to swift action from firefighters and restoration efforts 
#' underway, Notre Dame continues to inspire awe in those who visit her."
#' 
#' writeLines(document, con = "doc1.txt")
#' 
#' context_df <- convert_batch_documents_to_embeddings(ollama_connection = conn, 
#'                                       document_path_list = list("doc1.txt"))
#' 
#' prompt <- "When was Notre Dame in Paris built?"
#' prompt_vector <- get_ollama_embeddings(ollama_connection = conn, input =  prompt)
#' 
#' whole_list <- c(list(prompt_vector), context_df$embeddings)
#' mat <- do.call(rbind,whole_list)
#' cos_sim_mat <- make_cosine_similarity_matrix(mat)
#' 
#' @returns a cosine similarity matrix
#' @export
make_cosine_similarity_matrix <- function(input_matrix) {
  
  mat <- input_matrix
  dot_products <- mat %*% t(mat)
  magnitudes <- sqrt(rowSums(mat^2))
  cosine_similarity_matrix <- dot_products / (outer(magnitudes, magnitudes, "*"))
  return(cosine_similarity_matrix)
  
}


#' Retrieve Similar Vectors
#'
#' @description
#' `retrieve_similar_vectors` lets you retrieve the most similar content, based on cosine similarity, from a given context.
#'
#' @details
#' This function provides a way to make a simple RAG process that will retrieve content based on a context dataframe and a prompt vector.
#' 
#' @param context_df a context dataframe that contains text embeddings and text information for retrieval
#' @param prompt_vector a prompt transformed into a vector of embeddings in order to kick off the search
#' @param max_results the maximum number of results to be retrieved at once
#' @param similarity_threshold the threshold between 0 and 1 (defaults to 0.5) to remove the least relevant results. 1 means perfect similarity, 0 no similarity at all.
#' 
#' @returns the text vectors that are similar enough to a given prompt.
#'
#' @examples
#' conn <- get_ollama_connection()
#' 
#' document <- "Standing proudly on the Île de la Cité in the heart of Paris, 
#' France's capital city, lies one of the world's most beloved and historic 
#' landmarks: the magnificent Notre Dame Cathedral. This Gothic masterpiece 
#' has been welcoming pilgrims and tourists alike for over 850 years, since its 
#' construction began in 1163 under King Louis VII. With its towering spires, 
#' stunning stained glass windows, and intricate stone carvings, this beautiful 
#' church is a testament to medieval architecture and engineering skill. 
#' Unfortunately, a devastating fire ravaged the cathedral on April 15, 2019, 
#' but thanks to swift action from firefighters and restoration efforts 
#' underway, Notre Dame continues to inspire awe in those who visit her."
#' 
#' writeLines(document, con = "doc1.txt")
#' 
#' context_df <- convert_batch_documents_to_embeddings(ollama_connection = conn, 
#'                                       document_path_list = list("doc1.txt"))
#' 
#' prompt <- "When was Notre Dame in Paris built?"
#' prompt_vector <- get_ollama_embeddings(ollama_connection = conn, input =  prompt)
#' similar_text <- retrieve_similar_vectors(context_df, prompt_vector)
#'
#' @export
retrieve_similar_vectors <- function(context_df, prompt_vector, max_results=10, similarity_threshold=0.5) {
  
  max_results <- 10
  similarity_threshold <- 0.5
  whole_list <- c(list(prompt_vector), context_df$embeddings)
  mat <- do.call(rbind,whole_list)
  cos_sim_mat <- make_cosine_similarity_matrix(mat)
  similarities_to_prompt_vector <- cos_sim_mat[1, ]
  
  tib <- similarities_to_prompt_vector |> tibble::as_tibble()
  tib$names <-  c("prompt",context_df$text)
  vectors_to_get <- tib[2:nrow(tib),] |> dplyr::arrange(dplyr::desc(value)) |> dplyr::filter(value>=similarity_threshold) |> dplyr::pull(names)
  vectors_to_get <- vectors_to_get |> utils::head(max_results)
  
  if (!identical(vectors_to_get,character(0))) {
    final_context = vectors_to_get
  } else {
    final_context <- "No relevant information available on this topic."
  }
  
  return(final_context)
  
}

#' Write Vectors to Feather File
#'
#' @description
#' `write_vectors_to_feather_file` lets you write vector data to a feather file
#'
#' @importFrom arrow write_feather
#' @details
#' This function provides a simple way to save vector data (embeddings) to a binary feather file, instead of using regular databases.
#' 
#' @returns nothing.
#'
#' @param vector_data vector data (embeddings) already generated by another function.
#' @param file_name a file name to save the embeddings into a feather file
#'
#' @examples
#' conn <- get_ollama_connection()
#' 
#' document <- "Standing proudly on the Île de la Cité in the heart of Paris, 
#' France's capital city, lies one of the world's most beloved and historic 
#' landmarks: the magnificent Notre Dame Cathedral. This Gothic masterpiece 
#' has been welcoming pilgrims and tourists alike for over 850 years, since its 
#' construction began in 1163 under King Louis VII. With its towering spires, 
#' stunning stained glass windows, and intricate stone carvings, this beautiful 
#' church is a testament to medieval architecture and engineering skill. 
#' Unfortunately, a devastating fire ravaged the cathedral on April 15, 2019, 
#' but thanks to swift action from firefighters and restoration efforts 
#' underway, Notre Dame continues to inspire awe in those who visit her."
#' 
#' writeLines(document, con = "doc1.txt")
#' 
#' write_vectors_to_feather_file(file_name = "doc1.feather",
#' vector_data = generate_document_embeddings(conn, 
#' document_path = "doc1.txt",
#' splitter = "paragraph"))
#'
#' @export
write_vectors_to_feather_file <- function(vector_data, file_name) {
  
  embeddings_list <- lapply(vector_data, function(x) x$embeddings)
  text_list <- lapply(vector_data, function(x) x$text)
  df <- data.frame(embeddings=I(embeddings_list),text=unlist(I(text_list)))
  arrow::write_feather(x = df,sink = file_name)
}


#' Load Context Embeddings From Feather Files
#'
#' @description
#' `load_context_embeddings_from_feather_files` lets you load vector data to a feather file
#'
#' @details
#' This function provides a simple way to load vector data (embeddings) from a binary feather file, instead of using regular databases.
#' 
#' @returns the vector data contained in the feather file.
#'
#' @param filenames a list of feather filenames that contain vector embeddings and text information
#' 
#' @examples
#' 
#' conn <- get_ollama_connection()
#' 
#' document <- "Standing proudly on the Île de la Cité in the heart of Paris, 
#' France's capital city, lies one of the world's most beloved and historic 
#' landmarks: the magnificent Notre Dame Cathedral. This Gothic masterpiece 
#' has been welcoming pilgrims and tourists alike for over 850 years, since its 
#' construction began in 1163 under King Louis VII. With its towering spires, 
#' stunning stained glass windows, and intricate stone carvings, this beautiful 
#' church is a testament to medieval architecture and engineering skill. 
#' Unfortunately, a devastating fire ravaged the cathedral on April 15, 2019, 
#' but thanks to swift action from firefighters and restoration efforts 
#' underway, Notre Dame continues to inspire awe in those who visit her."
#' 
#' writeLines(document, con = "doc1.txt")
#' 
#' write_vectors_to_feather_file(file_name = "doc1.feather",
#' vector_data = generate_document_embeddings(conn, 
#' document_path = "doc1.txt",
#' splitter = "paragraph"))
#' 
#' load_context_embeddings_from_feather_files(filenames = "doc1.feather")
#'
#' @export
load_context_embeddings_from_feather_files <- function(filenames) {
  
  temp <- list()
  for (i in filenames) {
    temp[[i]] <- arrow::read_feather(i)
  }
  temp <- rbindlist(temp)
  return(temp)
  
}


#' Split text into paragraphs
#'
#' @description
#' `split_text_as_paragraphs` Splits text into sentences-based chunks.
#'
#' @importFrom tokenizers tokenize_paragraphs
#' @details
#' Splits text into paragraphs-based chunks, leveraging the tokenizers library.
#'
#' @param text a single piece of text to break down into paragraphs.
#' @examples
#' # c("Hi! How are you?\n\n Do you want to go for a walk?") |> split_text_as_paragraphs()
#' 
#' @export
split_text_as_paragraphs <- function(text) {
  
  paragraphs <- tokenizers::tokenize_paragraphs(text)
  return(unlist(paragraphs))
  
}

#' Split text into sentences
#'
#' @description
#' `split_text_as_sentences` Splits text into sentences-based chunks.
#'
#' @importFrom tokenizers tokenize_sentences
#' @details
#' Splits text into sentences-based chunks, leveraging the tokenizers library.
#'
#' @param text a single piece of text to break down.
#' @examples
#' # c("Hi! How are you? Do you want to go for a walk?") |> split_text_as_sentences()
#' 
#' @export
split_text_as_sentences <- function(text) {
  
  sentences <- tokenizers::tokenize_sentences(text)
  return(unlist(sentences))
  
} 

#' Add tools declaration for the LLM to use
#'
#' @description
#' `split_text_as_sentences` lets you add some tools (functions) that can be used if needed by the LLM to answer questions
#'
#' @details
#' Lets you add some tools (functions) that can be used by the LLM. 
#' This only works for models where the tool calling function is supported, which is the case for Llama3.1 for example. 
#' The structure expected is a R list.
#' @param workflow_obj A workflow object containing all parameters describing the workflow required
#' @param tools a list of tools declared a R list, see examples.
#' @examples
#' tool_list <- list(
#'  list(type="function",
#'       "function"=list(
#'         name="get_flight_times",
#'         description="get the flight times between two cities",
#'         parameters= list(
#'           type="object",
#'           properties = list(
#'             departure=list(
#'               type="string",
#'               description="the departure city (airport code)"
#'             ),
#'             arrival=list(
#'               type="string",
#'               description="the arrival city (airport code)"
#'             )
#'           ),
#'           required=c("departure", "arrival")
#'         )
#'       )))
#' 
#' myflow_test <- ai_workflow() |>
#'    set_connector("ollama")  |> 
#'    set_model(model_name= "llama3.1:8b-instruct-q5_K_M") |>
#'    set_n_predict(1000) |>
#'    set_temperature(0.8) |> 
#'    set_default_missing_parameters_in_workflow() |> 
#'    add_tools_declaration(tool_list)
#'    
#' @export
add_tools_declaration <- function(workflow_obj, tools) {
  
  if (is.list(tools)) {
  workflow_obj[["tools"]] <- tools
  } else {
    cli::cli_alert("The tools object you provided is not a R list.")
    stop("Improper format for tools")
  }
  
  return(workflow_obj)
}

#' Convert Batch documents to Embeddings
#'
#' @description
#' `convert_batch_documents_to_embeddings` converts whole batch of documents in vector embeddings at once
#'
#' @details
#' Lets you convert as many documents as necessary by targeting a path list of documents, which can be easily generated by functions or manually.
#' It returns a dataframe in the end that can be used to provide context to a LLM workflow.
#'
#' @param document_path_list a list containing full paths to files that need to be converted to embeddings.
#' @param splitter the splitter type to use when preparing embeddings. Defaults to "paragraph". "sentence" can also be used.
#' @param ollama_connection a connector object to an ollama instance to provide the embeddings.
#' @param model the model to use for creating the embeddings. Defaults to "bge-large"
#' @examples
#' 
#' conn <- get_ollama_connection()
#' 
#' document <- "Standing proudly on the Île de la Cité in the heart of Paris, 
#' France's capital city, lies one of the world's most beloved and historic 
#' landmarks: the magnificent Notre Dame Cathedral. This Gothic masterpiece 
#' has been welcoming pilgrims and tourists alike for over 850 years, since its 
#' construction began in 1163 under King Louis VII. With its towering spires, 
#' stunning stained glass windows, and intricate stone carvings, this beautiful 
#' church is a testament to medieval architecture and engineering skill. 
#' Unfortunately, a devastating fire ravaged the cathedral on April 15, 2019, 
#' but thanks to swift action from firefighters and restoration efforts 
#' underway, Notre Dame continues to inspire awe in those who visit her."
#' 
#' writeLines(document, con = "doc1.txt")
#' 
#' convert_batch_documents_to_embeddings(ollama_connection = conn, 
#' document_path_list = list("doc1.txt"))
#' 
#' @export
convert_batch_documents_to_embeddings <- function(ollama_connection, document_path_list, splitter="paragraph", model="bge-large") {

  #get_embedding_size with a short example
  text <- "hi there!"
  test_doc_path <- tempfile()
  writeLines(text, test_doc_path)
  
  test_vector_data <- generate_document_embeddings(ollama_connection = ollama_connection, 
                                              document_path = test_doc_path, splitter = splitter,
                                              model = model)
  test_embeddings_list <- lapply(test_vector_data, function(x) x$embeddings)
  test_text_list <- lapply(test_vector_data, function(x) x$text)
  ncols_expected <- ncol(data.frame(embeddings=I(test_embeddings_list), text=unlist(I(test_text_list)))) 
  
  # real loop here
  embeds <- list()
  for (one_document in document_path_list) {
    vector_data <- generate_document_embeddings(ollama_connection = ollama_connection, 
                                                document_path = one_document, splitter = splitter,
                                                model = model)
    embeddings_list <- lapply(vector_data, function(x) x$embeddings)
    text_list <- lapply(vector_data, function(x) x$text)
    if (ncol(data.frame(embeddings=I(embeddings_list),text=unlist(I(text_list))))==ncols_expected) {
    embeds[[basename(one_document)]] <- data.frame(embeddings=I(embeddings_list),text=unlist(I(text_list)))
    } else {
      cli_alert("Skipped document {one_document} as there was a mismatch in resulting columns")
    }
  }
  
  embeds_final <- rbindlist(embeds)
  return(embeds_final)
}

#' List global functions
#'
#' @description
#' `list_global_functions` lists global functions available in the environment.
#'
#' @details
#' lists global functions available. This is useful to check if the functions used for tools calling are present or not.
#'
#' @examples
#' list_global_functions()
#' 
#' @export
list_global_functions <- function() {
  # Get all objects in the global environment
  all_objects <- ls(envir = .GlobalEnv, all.names = TRUE)
  
  # Identify which objects are functions
  function_names <- sapply(all_objects, function(x) is.function(get(x, envir = .GlobalEnv)))
  
  # Filter and return function names
  functions_only <- all_objects[function_names]
  return(functions_only)
}


#' Extract Snippets
#'
#' @description
#' `extract_snippets` makes it easy to extract snippets in generated answers from the LLM
#'
#' @details
#' This function makes it easy to extract snippets in generated answers from the LLM
#'
#' @examples
#' llm_answer <- "This is a simple function in r:
#' ```
#' yo <- function(message) {
#' print(message)
#' }
#' ```
#' Enjoy this function!"
#' 
#' extracted_function <- llm_answer |> extract_snippets()
#' @param text the text to extract snippets from.
#' @export
extract_snippets <- function(text) {
  # Regular expression to match snippets (starts with ``, ends with ```)
  snippet_regex <- "(?s)```[a-zA-Z0-9_]*\\n(.*?)```"
  
  # Find all matches in the input text
  matches <- regmatches(text, gregexpr(snippet_regex, text, perl = TRUE))
  
  # Extract the content between the backticks
  snippets <- unlist(lapply(matches, function(x) {
    gsub("```[a-zA-Z0-9_]*\\n|```", "", x)
  }))
  
  # Return the extracted snippets
  return(snippets)
}

#' Add a step (i.e. another workflow) to an existing workflow
#'
#' @description
#' `add_workflow_step` adds another workflow to an existing one. By default it chains the new workflow to the previous one(s).
#'
#' @details
#' This function will add a new workflow to an existing one. By default the way the new workflow is added is by chaining it to the previous one.
#' The way this works is that it will use the previous output of the last workflow element as input for the next one. 
#'
#' @examples
#' myflow_template <- ai_workflow() |> 
#' set_connector("ollama") |>
#'   set_model(model_name= "llama3.1:8b-instruct-q5_K_M") |> 
#'   set_n_predict(1000) |> 
#'   set_temperature(0.8) 
#' 
#' 
#' @param workflow_obj the previous workflow object that you want to build on
#' @param workflow_obj_to_add the workflow object you want to add on top of the existing one
#' @param type the type of step you want to add to the existing workflow. Defaults to "chain". 
#' @export
add_workflow_step <- function(workflow_obj, workflow_obj_to_add, type="chain") {
  
  #check current type of workflow: if it's just a single one we call it atomic.
  if ("workflow_type" %in% names(workflow_obj)) {
    atomic_workflow <- F
  } else {
    atomic_workflow <- T
  }
  
  # create a chain if it's not the current se
  if (atomic_workflow == T) {
  new_workflow_obj <- list(workflow_type="chain", workflow_element=list(workflow_obj,workflow_obj_to_add))
  return(new_workflow_obj)
  }

  # if this is not an atomic workflow we add a new workflow element  
  if (atomic_workflow ==F) {
    current_length_workflow <- length(workflow_obj[["workflow_element"]])
    workflow_obj[["workflow_element"]][[current_length_workflow+1]] <- workflow_obj_to_add 
    return(workflow_obj)
  }
  
}

#' Request JSON answer from the LLM
#'
#' @description
#' `request_json_answer` request the current workflow to answer using a JSON format.
#'
#' @details
#' This function will request the LLM to answer using a JSON format. By default it will simply focus on a simple JSON format with just answer as a single object.
#' You can add a different JSON format as an argument.
#'
#' @examples
#' myflow_template <- ai_workflow() |> 
#' set_connector("ollama")  |> 
#'   set_model(model_name= "llama3.1:8b-instruct-q5_K_M") |> 
#'   set_n_predict(1000) |> 
#'   set_temperature(0.8) |> 
#'   request_json_answer()
#' 
#' @param workflow_obj the current workflow object that you want to build on
#' @param json_object_format the format required for the answer. 
#' @export
request_json_answer <- function(workflow_obj, json_object_format=list()) {
  
  # default
  if (identical(json_object_format,list())) {
    
    workflow_obj[["format_request"]] <- "You HAVE to answer using a correct JSON syntax.
This is the syntax we want:

\\{\"answer\": \"(put your answer here)\"
\\}

"
    return(workflow_obj)
  } else {
    
    # not working currently
    specific_json_format <- jsonlite::toJSON(json_object_format,auto_unbox = T)
        json_str <- specific_json_format
    json_str <- gsub(pattern = '\\\\', replacement='\\\\\\\\',x= json_str)  # Escape backslashes
    json_str <- gsub(pattern = '"', replacement = '\\"', x = json_str)  # Escape double quotes
    specific_json_format <- json_str
    
    workflow_obj[["format_request"]] <- glue::glue("You HAVE to answer using a correct JSON syntax.
This is the syntax we want:

{specific_json_format}

")
    
    return(workflow_obj)
    
  }
    
}

#' Parse JSON answer from the LLM
#'
#' @description
#' `parse_json_result` attempts to parse the JSON result from the LLM
#'
#' @details
#' This function will assume that the result from the LLM is provided in a JSON format
#' If the format is correct, it will parse the result as a R object.
#' You would typically expect such a format if you used the request_json_answer() function.
#' This is typically used in a pipe, after pull_final_answer(). 
#'
#' @examples
#' myflow_template <- ai_workflow() |> 
#' set_connector("ollama")  |> 
#'   set_model(model_name= "llama3.1:8b-instruct-q5_K_M") |> 
#'   set_n_predict(1000) |> 
#'   set_temperature(0.8) |> 
#'   request_json_answer()
#'   
#' myflow_template |> 
#' process_prompts("what is the usual color of the sky on Earth?") |>
#' pull_final_answer() |>
#' parse_json_result()
#' 
#' @param json_string the JSON string you want to parse
#' @export
parse_json_result <- function(json_string) {
  
  # remove typical escape characters that are not needed anymore
  cleaned_up_string <- gsub(json_string,pattern="\\\\",replacement="") 
  
  if (jsonlite::validate(cleaned_up_string)==TRUE) {
  parsed_result <- jsonlite::parse_json(cleaned_up_string)
  } else {
    cli::cli_alert("JSON object could not be parsed")
  }
  return(parsed_result)
  
}

