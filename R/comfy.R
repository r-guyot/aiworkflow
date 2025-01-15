


cfy_make_bw_image <- function(prompt_text, accent=1.6) {
  
  modified_prompt <- glue::glue("{prompt_text} (black and white image:{accent})")
  return(modified_prompt)
  
}


cfy_randomize_camera_angle <- function(prompt_text, accent=1.5) {
  
  camera_angles <- list(
    "front_view",
    "straight on shot",
    "dutch angle shot",
    "selfie",
    "side view",
    "profile",
    "lateral view",
    "flank view",
    "back view",
    "from behind",
    "turn around",
    "back",
    "bird eye view",
    "top down view",
    "overhead shot",
    "from above",
    "high angle",
    "slightly above",
    "from below",
    "worm eye view",
    "low view",
    "low angle",
    "from bottom",
    "fish eye shot",
    "wide angle view",
    "upside down view")
  
  addition <- sample(camera_angles,size = 1) |> unlist()
  addition_text <- glue::glue("({addition}:{accent})")
  
  modified_prompt <- glue::glue("{prompt_text} {addition_text}")
  return(modified_prompt)
  
}

#' ComfyUI: Find the Node of the Negative Prompt
#'
#' @description
#' `cfy_find_pos_neg_prompt_node` lets you find the node reference of the negative prompt from ComfyUI.
#' @export
cfy_find_pos_neg_prompt_node <- function(workflow_obj,polarity) {
  
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

#' ComfyUI: Set Positive Prompt for Image Generation
#'
#' @description
#' `cfy_set_positive_prompt` lets you set the positive prompt to use to generate images in ComfyUI.
#' @export
cfy_set_positive_prompt <- function(workflow_obj, positive_prompt) {
  
  node_id <- cfy_find_pos_neg_prompt_node(workflow_obj, polarity = "positive")
  #print(node_id)
  if (workflow_obj[["comfyui_workflow"]][[node_id]][["class_type"]]=="CLIPTextEncode") {
    workflow_obj[["comfyui_workflow"]][[node_id]][["inputs"]][["text"]] <- positive_prompt
  }
  return(workflow_obj)
  
}

#' ComfyUI: Set Negative Prompt for Image Generation
#'
#' @description
#' `cfy_set_negative_prompt` lets you set the negative prompt to use to generate images in ComfyUI.
#' @export
cfy_set_negative_prompt <- function(workflow_obj, negative_prompt) {
  
  node_id <- cfy_find_pos_neg_prompt_node(workflow_obj, polarity = "negative")
  #print(node_id)
  if (workflow_obj[["comfyui_workflow"]][[node_id]][["class_type"]]=="CLIPTextEncode") {
    workflow_obj[["comfyui_workflow"]][[node_id]][["inputs"]][["text"]] <- negative_prompt
  }
  return(workflow_obj)
  
}

#' ComfyUI: set image size
#'
#' @description
#' `cfy_set_image_size` lets you set the image size for the present comfyUI workflow
#' @export
cfy_set_image_size <- function(workflow_obj, resolution) {
  
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



#' ComfyUI: set sampler
#'
#' @description
#' `cfy_set_sampler` lets you set the sampler for the present comfyUI workflow
#' @export
cfy_set_sampler <- function(workflow_obj, sampler) {
  
  sampler_accepted <- c("euler",
                           "euler_ancestral",
                           "dpmpp_sde","dpmpp_sde_gpu",
                           "dpmpp_2m_sde","dpmpp_2m_sde_gpu",
                           "dpmpp_3m_sde","dpmpp_3m_sde_gpu")
  
  if (!sampler %in% sampler_accepted) {
    cli::cli_abort("Error: sampler needs to meet one of these values: '{paste(sampler_accepted,collapse=', ')}'.")
  }
  
  # go across all nodes of the comfyui workflow
  for (i in seq_along(workflow_obj[["comfyui_workflow"]])) {
    name <- names(workflow_obj[["comfyui_workflow"]][i])
    
    # find something called inputs
    if ("inputs" %in% names(workflow_obj[["comfyui_workflow"]][[i]])) {
      
      # find the sampler_name parameter
      if ("sampler_name" %in% names(workflow_obj[["comfyui_workflow"]][[i]][["inputs"]]) ) {
        workflow_obj[["comfyui_workflow"]][[name]][["inputs"]][["sampler_name"]] <- sampler
      }
    }
  } 
  return(workflow_obj)
}

#' ComfyUI: set scheduler 
#'
#' @description
#' `cfy_set_scheduler` lets you set the scheduler for the present comfyUI workflow
#' @export
cfy_set_scheduler <- function(workflow_obj, scheduler) {
  
  scheduler_accepted <- c("normal",
                        "exponential",
                        "karras",
                        "simple")
  
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



#' ComfyUI: set Seed Change Parameter 
#'
#' @description
#' `cfy_set_seed_change` lets you set the seed change parameter to use for the present comfyUI workflow
#' @export
cfy_set_seed_change <- function(workflow_obj, seed_change) {
  
  accepted_values <- c("fixed","increment","decrement","randomize")
  
  if (seed_change %in% accepted_values) {
    workflow_obj[["seed_change"]] <- seed_change
    
  }
  return(workflow_obj)
}


#' ComfyUI: set Seed
#'
#' @description
#' `cfy_set_seed` lets you set the seed to use for the present comfyUI workflow
#' @export
cfy_set_seed <- function(workflow_obj,seed=NA) {
  
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

#' ComfyUI: set Checkpoint
#'
#' @description
#' `cfy_set_checkpoint` lets you set the Checkpoint Model to use for the present comfyUI workflow
#' @export
cfy_set_checkpoint <- function(workflow_obj, checkpoint) {

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

#' ComfyUI: set LORA 
#'
#' @description
#' `cfy_set_lora` lets you set the LORA to use for the present comfyUI workflow
#' @export
cfy_set_lora <- function(workflow_obj, lora_name) {
  
  for (i in seq_along(workflow_obj[["comfyui_workflow"]])) {
    name <- names(workflow_obj[["comfyui_workflow"]][i])
    
    if ("inputs" %in% names(workflow_obj[["comfyui_workflow"]][[i]])) {
      if ("lora_name" %in% names(workflow_obj[["comfyui_workflow"]][[i]][["inputs"]]) ) {
        workflow_obj[["comfyui_workflow"]][[name]][["inputs"]][["lora_name"]] <- lora_name
      }
    }
  }
  return(workflow_obj) 
}

#' ComfyUI: set LORA Model Strength
#'
#' @description
#' `cfy_set_lora_model_strength` lets you set the strength of the LORAL model for the present comfyUI workflow
#' @export
cfy_set_lora_model_strength <- function(workflow_obj, strength_model) {

    for (i in seq_along(workflow_obj[["comfyui_workflow"]])) {
    name <- names(workflow_obj[["comfyui_workflow"]][i])
    
    if ("inputs" %in% names(workflow_obj[["comfyui_workflow"]][[i]])) {
      if ("lora_name" %in% names(workflow_obj[["comfyui_workflow"]][[i]][["inputs"]]) ) {
        workflow_obj[["comfyui_workflow"]][[name]][["inputs"]][["strength_model"]] <- strength_model
      }
    }
  }
  return(workflow_obj) 
  
}

#' ComfyUI: set Strength of the CLIP for LORA
#'
#' @description
#' `cfy_set_lora_clip_strength` lets you set the strength of the CLIP Modification by the LORA for the present comfyUI workflow
#' @export
cfy_set_lora_clip_strength <- function(workflow_obj, strength_clip) {
  
  for (i in seq_along(workflow_obj[["comfyui_workflow"]])) {
    name <- names(workflow_obj[["comfyui_workflow"]][i])
    
    if ("inputs" %in% names(workflow_obj[["comfyui_workflow"]][[i]])) {
      if ("lora_name" %in% names(workflow_obj[["comfyui_workflow"]][[i]][["inputs"]]) ) {
        workflow_obj[["comfyui_workflow"]][[name]][["inputs"]][["strength_clip"]] <- strength_clip
      }
    }
  }
  return(workflow_obj) 
  
  
}

#' ComfyUI: set cfg (guidance)
#'
#' @description
#' `cfy_set_cfg` lets you set the guidance (cfg) for the present comfyUI workflow
#' @export
cfy_set_cfg <- function(workflow_obj, cfg) {
  
  if (is.character(cfg)) { cfg <- as.integer(cfg) }
  if (is.na(cfg)) { cli::cli_abort("cfg needs to be defined as an integer") }
    #print(cfg)
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

#' ComfyUI: set number of steps for image generation
#'
#' @description
#' `cfy_set_steps` lets you set the number of steps to generate an image with the present comfyUI workflow
#' @export
cfy_set_steps <- function(workflow_obj, steps=20) {
  
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

#' ComfyUI: set custom workflow
#'
#' @description
#' `cfy_set_custom_workflow` lets you load a specific json file that contains a custom comfyui workflow
#' @export
cfy_set_custom_workflow <- function(workflow_obj, comfyui_workflow_json_filepath) {
  
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

#' ComfyUI: set a negative prompt for a simple ComfyUI workflow
#'
#' @description
#' `cfy_set_simple_workflow_negative_prompt` lets you set the negative prompt for a simple ComfyUI workflow
#' @export
cfy_set_simple_workflow_negative_prompt <- function(workflow_obj, negative_prompt) {
  
  if (!"comfyui_workflow" %in% names(workflow_obj)) {
    cli::cli_abort("Error: there is no current comfyui workflow, cannot assign negative prompt")
  }
  
  if (workflow_obj[["comfyui_workflow_type"]]!="simple") {
    cli::cli_abort("Error: this function only applies to simple comfyui workflows.")
  }

  json_object <- gsub(x = workflow_obj[["comfyui_workflow"]], pattern="<<<NEGATIVE_PROMPT>>>",replacement = negative_prompt)
  return(workflow_obj)
}

#' ComfyUI: Set up a very simple ComfyUI workflow
#'
#' @description
#' `cfy_set_simple_workflow` lets you set most simple ComfyUI workflow possible
#' @export
cfy_set_simple_worfklow <- function(workflow_obj, 
                                        checkpoint=NA,
                                        steps=20,
                                        seed=sample(1:10000000000, 1),
                                        scheduler="normal", 
                                        sampler="euler") {
  
  available_models <- cfy_get_model_checkpoints(workflow_obj)
  
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


#' ComfyUI: Queue Prompt
#'
#' @description
#' `cfy_queue_prompt` lets you queue the prompt to generate the image with the current ComfyUI workflow.
#' @export
cfy_queue_prompt <- function(workflow_obj, prompt_json) {
  
  data_prep <- list("prompt"=prompt_json, "client_id"=workflow_obj[["client_id"]])
  
  req <- httr2::request(glue::glue("http://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/prompt"))
  
  result <- req |> 
    httr2::req_body_json(data = data_prep 
    ) |> httr2::req_perform() 
  
  if (result$status_code==200) {
    
    prompt_id_returned <- result |> httr2::resp_body_json()
    return(prompt_id_returned)
  }

}

#' ComfyUI: Get History
#'
#' @description
#' `cfy_get_history` lets you get the history of the images generated by the current ComfyUI workflow.
#' @export
cfy_get_history <- function(workflow_obj, prompt_id) {
  
  req <- httr2::request(glue::glue("http://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/history/{prompt_id}"))
  result <- req |> httr2::req_perform() 
  if (result$status_code==200) {
    history_nodes <- result |> httr2::resp_body_json()
    return(history_nodes)
  }
  
}

#' ComfyUI: Process Prompts
#'
#' @description
#' `cfy_process_prompt` lets you process the prompts to generate the image with the current ComfyUI workflow.
#' @export
cfy_process_prompts <- function(workflow_obj, prompt) {

  #comfy_workflow <- img_gen
  #prompt <- "a beautiful Japanese woman with sunglasses with pink frames and blue tint"
  #prompt_text <- gsub(x = workflow_obj[["comfyui_workflow"]], pattern="<<<POSITIVE_PROMPT>>>",replacement = prompt)
  
  workflow_obj <- workflow_obj |> cfy_set_positive_prompt(positive_prompt = prompt)
  
  #print(prompt_text)
  client_id <- workflow_obj[["client_id"]]
  
  ws <- websocket::WebSocket$new(glue::glue("ws://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/ws?clientID={workflow_obj[['client_id']]}"))

  prompt_id <- workflow_obj |> cfy_queue_prompt(workflow_obj[["comfyui_workflow"]])
  prompt_id <- prompt_id$prompt_id
  
  i <- 0
  moving <- ""
  cli::cli_progress_message(msg = "Image generating...{moving}")
  while(cfy_check_queue_prompt_status(workflow_obj, prompt_id)=="ongoing") {
    Sys.sleep(2)
    i <- i + 1
    moving <- paste0(rep(".",i),collapse="")
    cli::cli_progress_update()
  }
  
  pics_list <- cfy_get_pictures_list(workflow_obj, prompt_id)
  
  resulting_images <- list()
  
  for (one_image in pics_list) {
  resulting_images <- append(resulting_images,
                             cfy_get_image(workflow_obj, one_image))
  }
  
  ws$close()
  
  workflow_obj[["res"]] <- resulting_images
  workflow_obj[["res_object_type"]] <- lapply(pics_list, function(x) glue::glue("image / {file_ext(x)}"))
  # change output to resulting_images
  #return(workflow_obj)
  return(resulting_images)
}


#' ComfyUI: Get Model Checkpoints
#'
#' @description
#' `cfy_get_model_checkpoints` lets you get a list of available checkpoint models from ComfyUI.
#' @export
cfy_get_model_checkpoints <- function(workflow_obj) {
  
  ws <- websocket::WebSocket$new(glue::glue("ws://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/ws?clientID={workflow_obj[['client_id']]}"))
  req <- httr2::request(glue::glue("http://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/models/checkpoints"))
  result <- req |> httr2::req_perform() 
  ws$close
  if (result$status_code==200) {
    checkpoints <- result |> httr2::resp_body_json()
    return(unlist(checkpoints))
  }
}

#' ComfyUI: Get Available Model Checkpoints
#'
#' @description
#' `cfy_get_available_model_checkpoints` lets you get a list of available checkpoint models from ComfyUI.
#' @export
cfy_get_available_model_checkpoints <- function(comfyui_ip_addr="127.0.0.1", comfyui_port="8188") {
  
  client_id <- uuid::UUIDgenerate()
  ws <- websocket::WebSocket$new(glue::glue("ws://{comfyui_ip_addr}:{comfyui_port}/ws?clientID={client_id}"))
  req <- httr2::request(glue::glue("http://{comfyui_ip_addr}:{comfyui_port}/models/checkpoints"))
  result <- req |> httr2::req_perform() 
  ws$close
  if (result$status_code==200) {
    checkpoints <- result |> httr2::resp_body_json()
    return(unlist(checkpoints))
  }

}

#' ComfyUI: Get Available Model LORAs
#'
#' @description
#' `cfy_get_available_model_loras` lets you get a list of available checkpoint models from ComfyUI.
#' @export
cfy_get_available_model_loras <- function(comfyui_ip_addr="127.0.0.1", comfyui_port="8188") {
  
  client_id <- uuid::UUIDgenerate()
  ws <- websocket::WebSocket$new(glue::glue("ws://{ip_addr}:{port}/ws?clientID={client_id}"))
  req <- httr2::request(glue::glue("http://{ip_addr}:{port}/models/loras"))
  result <- req |> httr2::req_perform() 
  ws$close
  if (result$status_code==200) {
    checkpoints <- result |> httr2::resp_body_json()
    return(unlist(checkpoints))
  }
  
}

#' ComfyUI: Unload model
#'
#' @description
#' `cfy_unload_model` lets you unload a model stored in VRAM to free memory.
#' @export
cfy_unload_model <- function(workflow_obj) {
  
  ws <- websocket::WebSocket$new(glue::glue("ws://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/ws?clientID={workflow_obj[['client_id']]}"))
  req <- httr2::request(glue::glue("http://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/free"))
  result <- req |> httr2::req_body_json(list("unload_models"=TRUE,"free_memory"=TRUE)) |>
    httr2::req_perform() 
  ws$close
  
}


#' ComfyUI: Get LORA Models list
#'
#' @description
#' `cfy_get_model_loras` lets you get a list of available LORA models from ComfyUI.
#' @export
cfy_get_model_loras <- function(workflow_obj) {
  
  ws <- websocket::WebSocket$new(glue::glue("ws://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/ws?clientID={workflow_obj[['client_id']]}"))
  req <- httr2::request(glue::glue("http://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/models/loras"))
  result <- req |> httr2::req_perform() 
  ws$close
  if (result$status_code==200) {
    loras <- result |> httr2::resp_body_json()
    return(unlist(loras))
  }
}

#' ComfyUI: Get Generated Image
#'
#' @description
#' `cfy_get_image` lets you get a generated image from ComfyUI.
#' @export
cfy_get_image <- function(workflow_obj, image_filename) {
  #print(image_filename)
  req <- httr2::request(glue::glue("http://{workflow_obj[['ip_addr']]}:{workflow_obj[['port']]}/view?filename={image_filename}"))
  result <- req |> httr2::req_perform() 
  if (result$status_code==200) {
    result_img <- result |> httr2::resp_body_raw()
    tempfile_for_img <- tempfile()
    result_img_raw <- magick::image_read(path = result_img)
    
  return(result_img_raw)
  }
}


#' ComfyUI: Check the queued prompts statuses
#'
#' @description
#' `cfy_check_queue_prompt_status` lets you check the status of the queued prompts from ComfyUI.
#' @export
cfy_check_queue_prompt_status <- function(workflow_obj, prompt_id) {
  
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
  
#' ComfyUI: Get Pictures List
#'
#' @description
#' `cfy_get_pictures_list` lets you get a list pictures already generated from ComfyUI.
#' @export
cfy_get_pictures_list <- function(workflow_obj, prompt_id) {  
  history <- workflow_obj |> cfy_get_history(prompt_id = prompt_id)
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
