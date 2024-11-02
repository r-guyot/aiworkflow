


server_address <- "127.0.0.1:8188"
client_id <- uuid::UUIDgenerate()

ws <- websocket::WebSocket$new(glue::glue("ws://{server_address}/ws?clientID={client_id}"))

ws$onMessage(function(event) {
  cat(event$data)
})

prompt_text = '
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
            "sampler_name": "euler",
            "scheduler": "normal",
            "seed": 8566257,
            "steps": 20
        }
    },
    "4": {
        "class_type": "CheckpointLoaderSimple",
        "inputs": {
            "ckpt_name": "realcartoonXL_v7.safetensors"
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
            "text": "a beautiful Japanese girl wearing sunglasses"
        }
    },
    "7": {
        "class_type": "CLIPTextEncode",
        "inputs": {
            "clip": [
                "4",
                1
            ],
            "text": "bad hands"
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

queue_prompt <- function(comfy_workflow, prompt_text) {
  
  #comfy_workflow <- list()
  #comfy_workflow[["client_id"]] <- client_id
  prompt_json <- fromJSON(txt=prompt_text,simplifyVector = F)
  
  data_prep <- list("prompt"=prompt_json, "client_id"=comfy_workflow[["client_id"]])
  
  #comfy_workflow[["ip"]] <- "127.0.0.1"
  #comfy_workflow[["port"]] <- "8188"
  
  req <- httr2::request(glue::glue("http://{comfy_workflow[['ip']]}:{comfy_workflow[['port']]}/prompt"))
  
  result <- req |> 
    httr2::req_body_json(data = data_prep 
    ) |> httr2::req_perform() 
  
  if (result$status_code==200) {
    
    prompt_id_returned <- result |> httr2::resp_body_json()
    return(prompt_id_returned)
  }

}

get_history <- function(comfy_workflow, prompt_id) {
  
  req <- httr2::request(glue::glue("http://{comfy_workflow[['ip']]}:{comfy_workflow[['port']]}/history/{prompt_id}"))
  result <- req |> httr2::req_perform() 
  if (result$status_code==200) {
    history_nodes <- result |> httr2::resp_body_json()
    return(history_nodes)
  }
  
}


get_images <- function(comfy_workflow, prompt) {
  
  prompt_id <- comfy_workflow |> queue_prompt(prompt_text)
  prompt_id <- prompt_id$prompt_id
  #need to implement some sort of status check
  history <- comfy_workflow |> get_history(prompt_id = prompt_id)
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
