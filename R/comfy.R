
find_pos_neg_prompt_node <- function(workflow_obj,polarity) {
  
  if (!polarity %in% c("positive","negative")) {
    cli::cli_abort("Error: polarity needs to be either 'positive' or 'negative'")
  }
  
  for (i in seq_along(workflow_obj[["comfyui_workflow"]])) {
    name <- names(workflow_obj[["comfyui_workflow"]][i])
    
    if ("inputs" %in% names(workflow_obj[["comfyui_workflow"]][[i]])) {
      if (polarity %in% names(workflow_obj[["comfyui_workflow"]][[i]][["inputs"]]) ) {
        return(workflow_obj[["comfyui_workflow"]][[name]][["inputs"]][[polarity]][[1]])
      }
    }
  }
  
}

set_comfyui_positive_prompt <- function(workflow_obj, positive_prompt) {
  
  node_id <- find_pos_neg_prompt_node(workflow_obj,polarity = "positive")
  print(node_id)
  if (workflow_obj[["comfyui_workflow"]][[node_id]][["class_type"]]=="CLIPTextEncode") {
    workflow_obj[["comfyui_workflow"]][[node_id]][["inputs"]][["text"]] <- positive_prompt
  }
  return(workflow_obj)
  
}

set_comfyui_negative_prompt <- function(workflow_obj, negative_prompt) {
  
  node_id <- find_pos_neg_prompt_node(workflow_obj,polarity = "negative")
  print(node_id)
  if (workflow_obj[["comfyui_workflow"]][[node_id]][["class_type"]]=="CLIPTextEncode") {
    workflow_obj[["comfyui_workflow"]][[node_id]][["inputs"]][["text"]] <- negative_prompt
  }
  return(workflow_obj)
  
}

set_comfyui_image_size <- function(workflow_obj, resolution) {
  
  resolution_accepted <- c("1024x1024",
                           "1152x896","896x1152",
                           "1216x832","832x1216",
                           "1344x768","768x1344",
                           "1536x640","640x1536")
    
  if (!resolution %in% resolution_accepted) {
    cli::cli_abort("Error: resolution needs to meet one of these values: '{paste(resolution_accepted,collapse=', ')}'.")
  }
  
  width_set <- strsplit(resolution,split = "x")[[1]][1]
  height_set <- strsplit(resolution,split = "x")[[1]][2]
  
    for (i in seq_along(workflow_obj[["comfyui_workflow"]])) {
      name <- names(workflow_obj[["comfyui_workflow"]][i])
      
      if ("inputs" %in% names(workflow_obj[["comfyui_workflow"]][[i]])) {
        if ("width" %in% names(workflow_obj[["comfyui_workflow"]][[i]][["inputs"]]) ) {
          workflow_obj[["comfyui_workflow"]][[name]][["inputs"]][["width"]] <- width_set
          workflow_obj[["comfyui_workflow"]][[name]][["inputs"]][["height"]] <- height_set
        }
      }
    } 
  return(workflow_obj)
}


set_comfyui_sampler <- function(workflow_obj, sampler) {
  
  sampler_accepted <- c("euler",
                           "euler_ancestral",
                           "dpmpp_sde","dpmpp_sde_gpu",
                           "dpmpp_2m_sde","dpmpp_2m_sde_gpu",
                           "dpmpp_3m_sde","dpmpp_3m_sde_gpu")
  
  if (!sampler %in% sampler_accepted) {
    cli::cli_abort("Error: sampler needs to meet one of these values: '{paste(sampler_accepted,collapse=', ')}'.")
  }
  
  for (i in seq_along(workflow_obj[["comfyui_workflow"]])) {
    name <- names(workflow_obj[["comfyui_workflow"]][i])
    
    if ("inputs" %in% names(workflow_obj[["comfyui_workflow"]][[i]])) {
      
      if ("sampler_name" %in% names(workflow_obj[["comfyui_workflow"]][[i]][["inputs"]]) ) {
        workflow_obj[["comfyui_workflow"]][[name]][["inputs"]][["sampler_name"]] <- sampler
      }
    }
  } 
  return(workflow_obj)
}


set_comfyui_scheduler <- function(workflow_obj, scheduler) {
  
  scheduler_accepted <- c("normal",
                        "exponential",
                        "karras","simple")
  
  if (!scheduler %in% scheduler_accepted) {
    cli::cli_abort("Error: scheduler needs to meet one of these values: '{paste(scheduler_accepted,collapse=', ')}'.")
  }
  
  for (i in seq_along(workflow_obj[["comfyui_workflow"]])) {
    name <- names(workflow_obj[["comfyui_workflow"]][i])
    
    if ("inputs" %in% names(workflow_obj[["comfyui_workflow"]][[i]])) {
      
      if ("scheduler" %in% names(workflow_obj[["comfyui_workflow"]][[i]][["inputs"]]) ) {
        workflow_obj[["comfyui_workflow"]][[name]][["inputs"]][["scheduler"]] <- scheduler
      }
    }
  } 
  return(workflow_obj)
}


set_seed_comfyui <- function(workflow_obj,seed=NA) {
  
  if (is.na(seed)) {
    seed <- sample(1:10e12,1)
    cli::cli_alert("Seed set randomly at {seed}")
  } else {
    seed <- seed
  }
  
  for (i in seq_along(workflow_obj[["comfyui_workflow"]])) {
    name <- names(workflow_obj[["comfyui_workflow"]][i])
    
    if ("inputs" %in% names(workflow_obj[["comfyui_workflow"]][[i]])) {
      if ("seed" %in% names(workflow_obj[["comfyui_workflow"]][[i]][["inputs"]]) ) {
        workflow_obj[["comfyui_workflow"]][[name]][["inputs"]][["seed"]] <- seed
      }
    }
  }
　return(workflow_obj) 
}


set_checkpoint_comfyui <- function(workflow_obj, checkpoint) {

  for (i in seq_along(workflow_obj[["comfyui_workflow"]])) {
    name <- names(workflow_obj[["comfyui_workflow"]][i])
    
    if ("inputs" %in% names(workflow_obj[["comfyui_workflow"]][[i]])) {
      if ("ckpt_name" %in% names(workflow_obj[["comfyui_workflow"]][[i]][["inputs"]]) ) {
        workflow_obj[["comfyui_workflow"]][[name]][["inputs"]][["ckpt_name"]] <- checkpoint
      }
    }
  }
  return(workflow_obj) 
}

set_cfg_comfyui <- function(workflow_obj, cfg) {
  
  if (is.character(cfg)) { cfg <- as.integer(cfg) }
  if (is.na(cfg)) { cli::cli_abort("cfg needs to be defined as an integer") }
    print(cfg)
  for (i in seq_along(workflow_obj[["comfyui_workflow"]])) {
    name <- names(workflow_obj[["comfyui_workflow"]][i])
    
    if ("inputs" %in% names(workflow_obj[["comfyui_workflow"]][[i]])) {
      if ("cfg" %in% names(workflow_obj[["comfyui_workflow"]][[i]][["inputs"]]) ) {
        workflow_obj[["comfyui_workflow"]][[name]][["inputs"]][["cfg"]] <- cfg
      }
    }
  }
  return(workflow_obj) 
}

set_steps_comfyui <- function(workflow_obj, steps=20) {
  
  for (i in seq_along(workflow_obj[["comfyui_workflow"]])) {
    name <- names(workflow_obj[["comfyui_workflow"]][i])
    
    if ("inputs" %in% names(workflow_obj[["comfyui_workflow"]][[i]])) {
      if ("steps" %in% names(workflow_obj[["comfyui_workflow"]][[i]][["inputs"]]) ) {
        workflow_obj[["comfyui_workflow"]][[name]][["inputs"]][["steps"]] <- steps
      }
    }
  }
  return(workflow_obj) 
}

set_custom_comfyui_workflow <- function(workflow_obj, comfyui_workflow_json_filepath) {
  
  if (file.exists(comfyui_workflow_json_filepath)) {
    json_object <- readLines(comfyui_workflow_json_filepath,warn = F)
    json_object <- paste0(json_object, collapse = "\n")
    workflow_obj[["comfyui_workflow_type"]] <- "custom"
    # checking if the proper prompts markers are available
    if(!grepl(pattern = "<<<POSITIVE_PROMPT>>>",x = json_object)) { cli::cli_alert("Warning: <<<POSITIVE_PROMPT>>> string not found in your workflow.") }
    if(!grepl(pattern = "<<<NEGATIVE_PROMPT>>>",x = json_object)) { cli::cli_alert("Warning: <<<NEGATIVE_PROMPT>>> string not found in your workflow.") }
    
    payload_in_list <- fromJSON(txt=json_object, simplifyVector = F)
    workflow_obj[["comfyui_workflow"]] <- payload_in_list
    return(workflow_obj)
  }
  
}

set_simple_comfyui_workflow_negative_prompt <- function(workflow_obj, negative_prompt) {
  
  if (!"comfyui_workflow" %in% names(workflow_obj)) {
    cli::cli_abort("Error: there is no current comfyui workflow, cannot assign negative prompt")
  }
  
  if (workflow_obj[["comfyui_workflow_type"]]!="simple") {
    cli::cli_abort("Error: this function only applies to simple comfyui workflows.")
  }

  json_object <- gsub(x = workflow_obj[["comfyui_workflow"]], pattern="<<<NEGATIVE_PROMPT>>>",replacement = negative_prompt)
  return(workflow_obj)
}

set_simple_comfyui_worfklow <- function(workflow_obj, 
                                        checkpoint=NA,
                                        steps=20,
                                        seed=sample(1:10000000000, 1),
                                        scheduler="normal", 
                                        sampler="euler") {
  
  available_models <- get_comfyui_model_checkpoints(workflow_obj)
  
  json_object = '
{
    "3": {
        "class_type": "KSampler",
        "inputs": {
            "cfg": 8,
            "denoise": 1,
            "latent_image": [
                "5",
                0
            ],
            "model": [
                "4",
                0
            ],
            "negative": [
                "7",
                0
            ],
            "positive": [
                "6",
                0
            ],
            "sampler_name": "<<<SAMPLER>>>",
            "scheduler": "<<<SCHEDULER>>>",
            "seed": <<<SEED>>>,
            "steps": <<<STEPS>>>
        }
    },
    "4": {
        "class_type": "CheckpointLoaderSimple",
        "inputs": {
            "ckpt_name": "<<<CHECKPOINT>>>"
        }
    },
    "5": {
        "class_type": "EmptyLatentImage",
        "inputs": {
            "batch_size": 1,
            "height": 1024,
            "width": 1024
        }
    },
    "6": {
        "class_type": "CLIPTextEncode",
        "inputs": {
            "clip": [
                "4",
                1
            ],
            "text": "<<<POSITIVE_PROMPT>>>"
        }
    },
    "7": {
        "class_type": "CLIPTextEncode",
        "inputs": {
            "clip": [
                "4",
                1
            ],
            "text": "<<<NEGATIVE_PROMPT>>>"
        }
    },
    "8": {
        "class_type": "VAEDecode",
        "inputs": {
            "samples": [
                "3",
                0
            ],
            "vae": [
                "4",
                2
            ]
        }
    },
    "9": {
        "class_type": "SaveImage",
        "inputs": {
            "filename_prefix": "ComfyUI",
            "images": [
                "8",
                0
            ]
        }
    }
}
'
  if (is.na(checkpoint)) {
    default_checkpoint <- available_models[1]
    cli::cli_alert("Default checkpoint for comfyui set to {default_checkpoint}.")
    json_object <- gsub(x = json_object, pattern="<<<CHECKPOINT>>>",replacement = default_checkpoint)
  } else {
    if (checkpoint %in% available_models) {
      json_object <- gsub(x = json_object, pattern="<<<CHECKPOINT>>>",replacement = checkpoint)
    } else {
      cli::cli_abort("Error: Checkpoint not found in available models.")
    }
    
  }
  
  json_object <- gsub(x = json_object, pattern="<<<STEPS>>>",replacement = steps)
  json_object <- gsub(x = json_object, pattern="<<<SEED>>>",replacement = seed)
  json_object <- gsub(x = json_object, pattern="<<<SCHEDULER>>>",replacement = scheduler)
  json_object <- gsub(x = json_object, pattern="<<<SAMPLER>>>",replacement = sampler)
  
  workflow_obj[["comfyui_workflow"]] <- json_object
  workflow_obj[["comfyui_workflow_type"]] <- "simple"
  
  return(workflow_obj)
  
}






queue_prompt <- function(workflow_obj, prompt_json) {
  
  #comfy_workflow <- list()
  #comfy_workflow[["client_id"]] <- client_id
  #prompt_json <- fromJSON(txt=prompt_text, simplifyVector = F)
  
  data_prep <- list("prompt"=prompt_json, "client_id"=workflow_obj[["client_id"]])
  
  #comfy_workflow[["ip"]] <- "127.0.0.1"
  #comfy_workflow[["port"]] <- "8188"
  
  req <- httr2::request(glue::glue("http://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/prompt"))
  
  result <- req |> 
    httr2::req_body_json(data = data_prep 
    ) |> httr2::req_perform() 
  
  if (result$status_code==200) {
    
    prompt_id_returned <- result |> httr2::resp_body_json()
    return(prompt_id_returned)
  }

}

get_history <- function(workflow_obj, prompt_id) {
  
  req <- httr2::request(glue::glue("http://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/history/{prompt_id}"))
  result <- req |> httr2::req_perform() 
  if (result$status_code==200) {
    history_nodes <- result |> httr2::resp_body_json()
    return(history_nodes)
  }
  
}


process_prompts_comfyui <- function(workflow_obj, prompt) {

  #comfy_workflow <- img_gen
  #prompt <- "a beautiful Japanese woman with sunglasses with pink frames and blue tint"
  #prompt_text <- gsub(x = workflow_obj[["comfyui_workflow"]], pattern="<<<POSITIVE_PROMPT>>>",replacement = prompt)
  
  workflow_obj <- workflow_obj |> set_comfyui_positive_prompt(positive_prompt = prompt)
  
  #print(prompt_text)
  client_id <- workflow_obj[["client_id"]]
  
  ws <- websocket::WebSocket$new(glue::glue("ws://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/ws?clientID={workflow_obj[['client_id']]}"))

  prompt_id <- workflow_obj |> queue_prompt(workflow_obj[["comfyui_workflow"]])
  prompt_id <- prompt_id$prompt_id
  
  while(check_comfyui_queue_prompt_status(workflow_obj, prompt_id)=="ongoing") {
    Sys.sleep(2)
    print("ongoing...")
  }
  
  pics_list <- get_comfyui_pictures_list(workflow_obj, prompt_id)
  
  resulting_images <- list()
  for (one_image in pics_list) {
  resulting_images <- append(resulting_images,
                             get_image_from_comfyui(workflow_obj, one_image))
  }
  
  ws$close()
  return(resulting_images)
  
}


get_comfyui_model_checkpoints <- function(workflow_obj) {
  
  ws <- websocket::WebSocket$new(glue::glue("ws://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/ws?clientID={workflow_obj[['client_id']]}"))
  
  req <- httr2::request(glue::glue("http://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/models/checkpoints"))
  result <- req |> httr2::req_perform() 
  ws$close
  if (result$status_code==200) {
    checkpoints <- result |> httr2::resp_body_json()
    return(unlist(checkpoints))
  }
}



get_image_from_comfyui <- function(workflow_obj, image_filename) {
  print(image_filename)
  req <- httr2::request(glue::glue("http://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/view?filename={image_filename}"))
  result <- req |> httr2::req_perform() 
  if (result$status_code==200) {
  result_img <- result |> httr2::resp_body_raw()
  tempfile_for_img <- tempfile()
  result_img_raw <- magick::image_read(path = result_img)
  return(result_img_raw)
  }
}



check_comfyui_queue_prompt_status <- function(workflow_obj, prompt_id) {
  
  req <- httr2::request(glue::glue("http://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/queue"))
  result <- req |> httr2::req_perform() 
  if (result$status_code==200) {
    
    result_list <- result |> httr2::resp_body_json()

    if (identical(result_list[["queue_running"]],list()) & identical(result_list[["queue_pending"]],list())) {
      return("finished")
    } else {
      return("ongoing")
    }
    
  }
}
  

get_comfyui_pictures_list <- function(workflow_obj, prompt_id) {  
  history <- workflow_obj |> get_history(prompt_id = prompt_id)
  history <- history[[1]]
  image_list <- list()
  for (one_node in history[["outputs"]]) {
    for (one_image in one_node[["images"]]) {
      filename <- (one_image$filename)
      subfolder <- (one_image$subfolder)
      if (subfolder!="") {
      image_list <- append(image_list, glue("{subfolder}/{filename}"))
      } else {  
        image_list <- append(image_list, glue("{filename}"))
        }
    }
  }
  return(image_list)
  
}
