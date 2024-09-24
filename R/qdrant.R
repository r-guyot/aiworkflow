#' Get Qdrant connection
#'
#' @description
#' `get_qdrant_connection` establishes a connection with a qdrant instance.
#''
#' @details
#' Creates a connection object for qdrant.
#' If you are using the API key to connect to Qdrant, you need to specify it in the api_key parameter.
#'
#' @param endpoint the URL pointing to the qdrant instance. Defaults to http://localhost
#' @param port the port to use to connect to the qdrant instance. Defaults to 6333
#' @param api_key optional for security. Defaults to NA. If given, it will use an API key to connect to the Qdrant.
#' 
#' @examples
#' conn <- get_qdrant_connection()
#' @export
get_qdrant_connection <- function(endpoint="http://localhost", port=6333, api_key=NA_character_) {
  
  conn <- list(endpoint=endpoint, port=port, type="qdrant_connection")
  
  if (!is.na(api_key)) {
    conn[["api_key"]] <- api_key
  }
  
  if (qdrant_check_connection_validity(conn, silent = FALSE)) {
    return(conn)
  } else {
    stop("Qdrant instance could not be reached.")
  }
}

#' Qdrant: Check if the Connection is valid
#'
#' @description
#' `qdrant_check_connection_validity` checks if the qdrant connection proposed is valid.
#''
#' @details
#' Does a simple test to confirm if the qdrant connection is working as expected.
#'
#' @importFrom httr2 req_headers
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom cli cli_alert
#' @importFrom glue glue
#'
#' @param conn a connection object created by get_qdrant_connection()
#' @param silent boolean, defaults to TRUE. If FALSE, prints a message when the connection is established.
#' 
#' @examples
#' conn <- get_qdrant_connection()
#' qdrant_check_connection_validity(conn,silent=FALSE)
#' @export
qdrant_check_connection_validity <- function(conn,silent=TRUE) {
  
  #check_conn <- qdrant_list_all_collections(conn)
  
  endpoint <- conn[["endpoint"]]
  port <- conn[["port"]]
  if ("api_key" %in% names(conn)) {
    api_key <- conn[["api_key"]]
  }
  
  req <- httr2::request(base_url = glue::glue("{endpoint}:{port}/collections"))
  req <- req |> httr2::req_headers("Accept"="application/json")
  req <- req |> add_api_key_header(conn = conn)
  res <- req |> httr2::req_perform()
  
  if (res$status_code==200) {
    if (silent==FALSE) { cli::cli_alert("Connection to Qdrant confirmed") }
    if (silent==FALSE & "api_key" %in% names(conn)) { cli_alert("Connection secured with API key") }
    return(TRUE)
  } else {
    return(FALSE)
  }

}

#' Adds API key header to the qdrant connection for security
#'
#' @description
#' `add_api_key_header` adds a security header to connect to Qdrant instance with an API key.
#' 
#' @importFrom httr2 req_headers
#'
#' @details
#' Adds a security header to connect to Qdrant instance with an API key.
#' The API key value is provided by the connection object obtained with get_qdrant_connection().
#' This function returns the request object along with the necessary header.
#'
#' @param conn a connection object created by get_qdrant_connection()
#' @param req a httr2 request object that is prepared for Qdrant REST API calls.
add_api_key_header <- function(req, conn) {
  
  if ("api_key" %in% names(conn)) {
    api_key <- conn[["api_key"]]
    req <- req |> httr2::req_headers("api-key"=api_key)
    return(req)
  } else {
    return(req)
  }
  
}


#' Qdrant: List all collections
#'
#' @description
#' `qdrant_list_all_collections` establishes a connection with a qdrant instance and lists all collections.
#''
#' @details
#' Lists all the collections available on the qdrant instance. 
#' Each collection can have different size of vector embeddings
#'
#' @importFrom httr2 req_headers
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom httr2 resp_body_json
#' @importFrom cli cli_alert
#' @importFrom glue glue
#' 
#' @param conn a connection object created by get_qdrant_connection()
#' 
#' @examples
#' conn <- get_qdrant_connection()
#' qdrant_list_all_collections(conn)
#' 
#' @export
qdrant_list_all_collections <- function(conn) {
  
  if (qdrant_check_connection_validity(conn)) {
    
    endpoint <- conn[["endpoint"]]
    port <- conn[["port"]]
    
  req <- httr2::request(base_url = glue::glue("{endpoint}:{port}/collections"))
  req <- req |> httr2::req_headers("Accept"="application/json")
  req <- req |> add_api_key_header(conn = conn)
  res <- req |> httr2::req_perform()
  if (res$status_code==200) {
    final_res <- res |> httr2::resp_body_json()
    return(final_res)
  }
  } else {
    cli_alert("qdrant connection not recognized. Use the get_qdrant_connection() function.")
    return(NA_character_)
  }
  
}



#' Qdrant: Check collection existence
#'
#' @description
#' `qdrant_check_collection_existence` establishes a connection with a qdrant instance and checks if a collection exists.
#''
#' @details
#' Confirms the existence of a qdrant collection.
#' Returns TRUE or FALSE 
#' 
#' @importFrom httr2 req_headers
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom httr2 req_method
#' @importFrom httr2 resp_body_json
#' @importFrom cli cli_alert
#' @importFrom glue glue
#' 
#' @param conn a connection object created by get_qdrant_connection()
#' @param collection_name the name of a collection for which you want to check the existence. Defaults to NA.
#' 
#' @examples
#' conn <- get_qdrant_connection()
#' qdrant_check_collection_existence(conn, collection_name="hi_there")
#' 
#' @export
qdrant_check_collection_existence <- function(conn, collection_name=NA_character_) {
  
  if (qdrant_check_connection_validity(conn)) {
  
  if (!is.na(collection_name)) {
    req <- httr2::request(base_url = glue::glue("{conn[['endpoint']]}:{conn[['port']]}/collections/{collection_name}/exists"))
    req <- req |> httr2::req_method(method = "GET")
    req <- req |> add_api_key_header(conn = conn)
    res <- req |> httr2::req_perform()
    if (res$status_code==200) {
      final_res <- res |> httr2::resp_body_json()
      return(final_res$result$exists)
    }
  } else {
    cli_alert("You need to provide a collection name")
  }
  } else { stop("Connection to Qdrant could not be established.")}
}

#' Qdrant: Get collection details
#'
#' @description
#' `qdrant_get_collection_details` establishes a connection with a qdrant instance and returns the details of a specific collection.
#''
#' @details
#' Returns the details of a specific qdrant collection.
#'
#' @param conn a connection object created by get_qdrant_connection()
#' @param collection_name the name of a collection for which you want to check the details. Defaults to NA.
#' 
#' @importFrom httr2 req_headers
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom httr2 req_method
#' @importFrom httr2 resp_body_json
#' @importFrom cli cli_alert
#' @importFrom glue glue
#' 
#' @examples
#' conn <- get_qdrant_connection()
#' qdrant_create_new_collection(conn, collection_name="story_of_my_life")
#' qdrant_get_collection_details(conn, collection_name="story_of_my_life")
#' qdrant_delete_collection(conn, collection_name="story_of_my_life")
#' 
#' @export
qdrant_get_collection_details <- function(conn, collection_name=NA_character_) {
  
  if (qdrant_check_connection_validity(conn)) {
  
  if (!is.na(collection_name)) {
    req <- httr2::request(base_url = glue::glue("{conn[['endpoint']]}:{conn[['port']]}/collections/{collection_name}"))
    req <- req |> httr2::req_method(method = "GET")
    req <- req |> add_api_key_header(conn = conn)
    res <- req |> httr2::req_perform()
    if (res$status_code==200) {
      final_res <- res |> httr2::resp_body_json()
      return(final_res)
    }
  } else {
    cli_alert("You need to provide a collection name")
  }
  } else { stop("Connection to Qdrant could not be established.")}

    
}


#' Qdrant: Create new collection
#'
#' @description
#' `qdrant_create_new_collection` establishes a connection with a qdrant instance and creates a new collection.
#''
#' @details
#' Creates a new collection for vector embeddings. 
#' You can customize the size of embeddings needed and the type of distance for similarity search.
#' If a collection already exists with the same name it will throw an error.
#' 
#' @importFrom httr2 req_headers
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom httr2 req_method
#' @importFrom httr2 resp_body_json
#' @importFrom httr2 resp_body_raw
#' @importFrom glue glue
#' @importFrom cli cli_alert
#' 
#' 
#' @param conn a connection object created by get_qdrant_connection()
#' @param collection_name the name of the collection you want to create. Defaults to NA.
#' @param vectors a R list object that needs to contain 'size' and 'distance'. Defaults to size=384 and distance='Cosine' 
#' This needs to be adapted to the embedding model you use. Different models have different resulting vector sizes.
#' 
#' @examples
#' conn <- get_qdrant_connection()
#' qdrant_create_new_collection(conn, collection_name="story_of_my_life")
#' qdrant_delete_collection(conn, collection_name="story_of_my_life")
#' 
#' @export
qdrant_create_new_collection <- function(conn, collection_name=NA_character_,vectors=list(size=384,distance="Cosine")) {
  
  if (qdrant_check_connection_validity(conn)) {
  
  if (qdrant_check_collection_existence(conn = conn,collection_name = collection_name)) {
    stop(glue::glue("The collection {collection_name} already exists on this qdrant instance!"))
  }
  
  if (!is.na(collection_name)) {
  req <- httr2::request(base_url = glue::glue("{conn[['endpoint']]}:{conn[['port']]}/collections/{collection_name}"))
  req <- req |> httr2::req_method(method = "PUT")

  default_collection <- list(vectors=vectors)
  default_collection_json <- jsonlite::toJSON(default_collection,auto_unbox = T)
  
  req <- req |> httr2::req_body_raw(default_collection_json)
  req <- req |> add_api_key_header(conn = conn)
  req <- req |> httr2::req_perform()
  if (req$status_code==200) {
    cli_alert("Collection {collection_name} was just created on the qdrant instance.")
  }
  } else {
    cli_alert("You need to provide a collection name")
  }
  } else { stop("Connection to Qdrant could not be established.")}

    
}

#' Qdrant: Delete collection
#'
#' @description
#' `qdrant_delete_collection` establishes a connection with a qdrant instance and deletes an existing collection.
#''
#' @details
#' Deletes an existing collection on qdrant. 
#' If the collection does not exists, it will throw an error.
#' If the deletion is successful it will display a confirmation message
#'
#' @param conn a connection object created by get_qdrant_connection()
#' @param collection_name the name of the collection you want to delete. Defaults to NA.
#' 
#' @importFrom httr2 req_headers
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom httr2 req_method
#' @importFrom httr2 resp_body_json
#' @importFrom httr2 resp_body_raw
#' @importFrom glue glue
#' 
#' @examples
#' conn <- get_qdrant_connection()
#' qdrant_create_new_collection(conn, collection_name="story_of_alice")
#' qdrant_delete_collection(conn, collection_name="story_of_alice")
#' 
#' @export
qdrant_delete_collection <- function(conn, collection_name=NA_character_) {
  
  if (qdrant_check_connection_validity(conn)) {
  
  if (!qdrant_check_collection_existence(conn = conn,collection_name = collection_name)) {
    stop(glue::glue("The collection {collection_name} does not exist on this qdrant instance!"))
  }
  
  if (!is.na(collection_name)) {
    req <- httr2::request(base_url = glue::glue("{conn[['endpoint']]}:{conn[['port']]}/collections/{collection_name}"))
    req <- req |> httr2::req_method(method = "DELETE")
    req <- req |> add_api_key_header(conn = conn)
    res <- req |> httr2::req_perform()
    
    if (res$status_code==200) {
      final_res <- res |> httr2::resp_body_json()
      if (final_res$result==TRUE) {
      cli_alert("Collection {collection_name} was just deleted on the qdrant instance.")
      }
    }
  } else {
    cli_alert("You need to provide a collection name")
  }
    
  } else { stop("Connection to Qdrant could not be established.")}

}


#' Qdrant: Upsert points
#'
#' @description
#' `qdrant_upsert_points` establishes a connection with a qdrant instance and insert points (vector) in a collection.
#''
#' @details
#' Inserts vector entries into an existing collection.
#'
#' @param conn a connection object created by get_qdrant_connection()
#' @param collection_name the name of the collection you want to use. Defaults to NA.
#' @param points a list containing vector embeddings. A very specific format is expected.
#' Here is the format that is expected (example of a vector of size 3):
#' list(points=list(list(id=1,
#'                       payload=list(text="hi there"),
#'                       vector=list(0.1,0.5,0.6))))
#' Note that 'payload' can contain a lot more data than 'text'. 
#' You can add more variables if needed within the payload list.
#' @param generate_id boolean, defaults to TRUE. If TRUE, will automatically create a unique UUID
#' for your vector, as it is needed by qdrant. If you don't use this, you have to provide an id by yourself.
#' 
#' @importFrom httr2 req_headers
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom httr2 req_method
#' @importFrom httr2 resp_body_json
#' @importFrom httr2 resp_body_raw
#' @importFrom glue glue
#' 
#' @examples
#' conn <- get_qdrant_connection()
#' qdrant_create_new_collection(conn, collection_name="test_db",
#' vectors=list(size=3,distance="Cosine"))
#' new_vectors <- list(points=list(list(payload=list(text="hi there"),
#'                       vector=list(0.1,0.5,0.6))))
#' qdrant_upsert_points(conn, points=new_vectors,
#' collection_name="test_db",generate_id=TRUE)
#' qdrant_delete_collection(conn, "test_db")
#' 
#' @export
qdrant_upsert_points <- function(conn, points=list(), collection_name, generate_id=TRUE) {
  
  if (qdrant_check_connection_validity(conn)) {
  
  if (!qdrant_check_collection_existence(conn = conn,collection_name = collection_name)) {
    stop(glue::glue("The collection {collection_name} does not exist on this qdrant instance"))
  }
  
  if (!identical(points, list())) {
    
    req <- httr2::request(base_url = glue::glue("{conn[['endpoint']]}:{conn[['port']]}/collections/{collection_name}/points"))
    
    if (generate_id==TRUE) {
      
      points[["points"]] <- lapply(points[["points"]], function(item) {
        text <- item[["payload"]][["text"]]
        #print(text)
        id <- generate_uuid_from_text(text)
        item[["id"]] <- id  # Add the new ID to the item
        #print(id)
        return(item)   # Return the modified item
      })
      
    }
    
    points_json <- jsonlite::toJSON(points,auto_unbox = T)
    req <- req |> httr2::req_method(method = "PUT")
    req <- req |> httr2::req_body_raw(points_json)
    req <- req |> add_api_key_header(conn = conn)
    req <- req |> httr2::req_headers("Content-Type"="application/json")
    res <- req |> httr2::req_perform()
    
    if (res$status_code==200) {
      final_res <- res |> httr2::resp_body_json()
      
     if (final_res$result[["status"]]=="acknowledged") {
        cli_alert("Vectors were added in the {collection_name} collection on the qdrant instance.")
      }
    }
    
  } else {
    cli_alert("You cannot upsert an empty list.")
  }
  
  } else { stop("Connection to Qdrant could not be established.")}
  
    
}




#' Generate Numeric List
#'
#' @description
#' `generate_numeric_list` generates a random list to simulate what vector embeddings look like
#'
#' @details
#' Generates a list of numerical values of a specific length.
#' It can be used to simulate what vector embeddings look like.
#'
#' @importFrom stats runif
#'
#' @param decimals the number of decimals needed for each figure. Defaults to 1.
#' @param length the length of the list you want to produce
#' 
#' @examples
#' generate_numeric_list(decimals=1, length=384)
#' 
#' @export
generate_numeric_list <- function(decimals = 1,length) {
  # Set the number of values to 384
  n <- length
  
  # Generate n random values between 0 and 1
  random_values <- stats::runif(n)
  
  # Round the values to the specified number of decimal points
  rounded_values <- round(random_values, decimals)
  
  # Return the list of rounded values
  return(as.list(rounded_values))
}

#' Generate UUID from text
#'
#' @description
#' `generate_uuid_from_text` generates a unique UUID for a given piece of text.
#'
#' @importFrom digest digest
#'
#' @details
#' Generates a UUID for a piece of text. The same text will give the same id.
#' This is useful to generate an id for inserting vectors into qdrant.
#'
#' @param text a piece of text to generate a UUID. Defaults to NA.
#' 
#' @examples
#' generate_uuid_from_text("hi there")
#' 
#' @export
generate_uuid_from_text <- function(text=NA_character_) {
  
  if (!is.na(text)) {
  text <- as.character(text)
  hash <- digest::digest(text, algo = "sha256", serialize = FALSE)
  uuid <- paste0(substr(hash, 1, 8), "-", 
                 substr(hash, 9, 12), "-",
                 "4", substr(hash, 14, 16), "-",
                 substr(hash, 17, 20), "-",
                 substr(hash, 21, 32))
  return(uuid)
  } else {
    cli_alert("No text provided to generate a UUID.")
  }
}

#' Qdrant: Retrieve a specific point (vector)
#'
#' @description
#' `qdrant_retrieve_point` retrieves a specific vector (point)
#''
#' @details
#' Retrieves the value of a specific vector/point, based on its id.
#' It will return a list with all the info available from qdrant.
#'
#' @param conn a connection object created by get_qdrant_connection()
#' @param collection_name the name of the collection you want to use. Defaults to NA.
#' @param id the id of the point to retrieve. Defaults to NA.
#' 
#' 
#' @importFrom httr2 req_headers
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom httr2 req_method
#' @importFrom httr2 resp_body_json
#' @importFrom httr2 resp_body_raw
#' @importFrom glue glue
#' 
#' @examples
#' conn <- get_qdrant_connection()
#' qdrant_create_new_collection(conn, collection_name="test_db",
#' vectors=list(size=3,distance="Cosine"))
#' new_vectors <- list(points=list(list(id=1, payload=list(text="hi there"),
#'                       vector=list(0.1,0.5,0.6))))
#' qdrant_upsert_points(conn, points=new_vectors,collection_name="test_db",generate_id=FALSE)
#' qdrant_retrieve_point(conn, collection_name="test_db", id=1)
#' qdrant_delete_collection(conn, "test_db")
#' 
#' @export
qdrant_retrieve_point <- function(conn, collection_name=NA_character_, id=NA_character_) {
  
  if (qdrant_check_connection_validity(conn)) {
  
  if (!qdrant_check_collection_existence(conn = conn,collection_name = collection_name)) {
    stop(glue::glue("The collection {collection_name} does not exist on this qdrant instance"))
  }
  
  if (!is.na(id) & !is.na(collection_name)) {
    
    req <- httr2::request(base_url = glue::glue("{conn[['endpoint']]}:{conn[['port']]}/collections/{collection_name}/points/{id}"))
    req <- req |> httr2::req_method(method = "GET")
    req <- req |> add_api_key_header(conn = conn)
    res <- req |> httr2::req_perform()
    if (res$status_code==200) {
      final_res <- res |> httr2::resp_body_json()
      return(final_res)
    }
    
  } else {
    if (is.na(id)) { cli_alert("You need to provide an id to retrieve a specific point.") }
    if (is.na(collection_name)) { cli_alert("You need to provide a collection name to retrieve a specific point.") }
  }
  
  } else { stop("Connection to Qdrant could not be established.")}

    
}


#' Qdrant: Delete points (vectors)
#'
#' @description
#' `qdrant_delete_points` deletes specific vectors (points)
#''
#' @details
#' This will delete specific vectors based on their ids.
#'
#' @param conn a connection object created by get_qdrant_connection()
#' @param collection_name the name of the collection you want to use. Defaults to NA.
#' @param ids a list of ids to delete. Defaults to an empty list. 
#' The value of "ids" needs to be in this format: list(1,2,3) if you want to delete vectors 1,2 and 3.
#' 
#' 
#' @importFrom httr2 req_headers
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom httr2 req_method
#' @importFrom httr2 resp_body_json
#' @importFrom httr2 resp_body_raw
#' @importFrom glue glue
#' 
#' @examples
#' conn <- get_qdrant_connection()
#' qdrant_create_new_collection(conn, collection_name="test_db",
#' vectors=list(size=3,distance="Cosine"))
#' new_vectors <- list(points=list(
#' list(id=1, payload=list(text="hi there"),vector=list(0.1,0.5,0.6)),
#' list(id=2, payload=list(text="well well well"),vector=list(0.6,0.1,0.3))
#' ))
#' qdrant_upsert_points(conn, points=new_vectors,collection_name="test_db")
#' qdrant_delete_points(conn, collection_name="test_db", ids=list(1,2))
#' qdrant_delete_collection(conn, "test_db")
#' @export
qdrant_delete_points <- function(conn, collection_name=NA_character_,ids=list()) {
  
  if (qdrant_check_connection_validity(conn)) {
  
  if (!qdrant_check_collection_existence(conn = conn,collection_name = collection_name)) {
    stop(glue::glue("The collection {collection_name} does not exist on this qdrant instance"))
  }
  
  if (!identical(ids,list()) & !is.na(collection_name)) {
    
    req <- httr2::request(base_url = glue::glue("{conn[['endpoint']]}:{conn[['port']]}/collections/{collection_name}/points/delete"))
    req <- req |> httr2::req_method(method = "POST")
    req <- req |> add_api_key_header(conn = conn)
    req <- req |> httr2::req_headers("Content-Type"="application/json")
    ids_to_delete <- list(points=ids)
    ids_to_delete_json <- jsonlite::toJSON(ids_to_delete, auto_unbox = T)
    req <- req |> httr2::req_body_raw(ids_to_delete_json)
    
    res <- req |> httr2::req_perform()
    
    if (res$status_code==200) {
      cli_alert("Successfully deleted points.")
    }
    
  } else {
    if (identical(ids,list())) { cli_alert("You need to provide an id to retrieve a specific point.") }
    if (is.na(collection_name)) { cli_alert("You need to provide a collection name to retrieve a specific point.") }
  }
  
  } else { stop("Connection to Qdrant could not be established.")}

    
}

points <- list(points=list(
  list(payload=list(text="yo man"),vector=generate_numeric_list(length = 384)),
  list(payload=list(text="yo man arg"),vector=generate_numeric_list(length = 384))
))

#' Qdrant: Search points (vectors)
#'
#' @description
#' `qdrant_search_points` searches for similar vectors (points)
#''
#' @details
#' This will run a search for similar vectors on qdrant.
#'
#' @param conn a connection object created by get_qdrant_connection()
#' @param collection_name the name of the collection you want to use. Defaults to NA.
#' @param vector a vector of embedding values that is used to initiate the search. Defaults to an empty list.
#' Note that this vector is simply a list containing numerical values for your embeddings.
#' If you use an embedding of size 384, this should be a simple list with 384 numerical values in it.
#' @param limit the maximum amount of vectors to return after the search. Defaults to 5.
#' @param with_payload if TRUE, will display the payload of the resulting vectors. Defaults to TRUE.
#' @param with_vector if TRUE, will display the full vector values in the results. Defaults to FALSE.
#' @param score_threshold a threshold for the minimum similarity score to return results. Defaults to NA. 
#' 
#' @examples
#' conn <- get_qdrant_connection()
#' qdrant_create_new_collection(conn, collection_name="test_db",
#' vectors=list(size=3,distance="Cosine"))
#' new_vectors <- list(points=list(
#' list(id=1, payload=list(text="hi there"),vector=list(0.1,0.5,0.6)),
#' list(id=2, payload=list(text="well well well"),vector=list(0.6,0.1,0.3))
#' ))
#' qdrant_upsert_points(conn, points=new_vectors, collection_name="test_db")
#' qdrant_search_points(conn, collection_name="test_db", vector=list(0.2,0.5,0.6))
#' qdrant_delete_collection(conn, "test_db")
#' @export
qdrant_search_points <- function(conn, 
                                 collection_name=NA_character_, 
                                 vector=list(),
                                 limit=5, 
                                 with_payload=TRUE,
                                 with_vector=FALSE,
                                 score_threshold=NA_real_) {
  
  if (qdrant_check_connection_validity(conn)) {
  
  if (!qdrant_check_collection_existence(conn = conn,collection_name = collection_name)) {
    stop(glue::glue("The collection {collection_name} does not exist on this qdrant instance"))
  }
  
  if (!is.na(collection_name) & !identical(vector,list())) {

    req <- httr2::request(base_url = glue::glue("{conn[['endpoint']]}:{conn[['port']]}/collections/{collection_name}/points/search"))
    req <- req |> httr2::req_method(method = "POST")
    req <- req |> add_api_key_header(conn = conn)
    req <- req |> httr2::req_headers("Content-Type"="application/json")
    
    search_list <- list(vector=vector,
                        limit=limit,
                        with_payload=with_payload,
                        with_vector=with_vector)
    
    if (!is.na(score_threshold)) {
      search_list[["score_threshold"]] <- score_threshold
    }
    
    search_json <- jsonlite::toJSON(search_list,auto_unbox = T)
    req <- req |> httr2::req_body_raw(search_json)
  
    res <- req |> httr2::req_perform()
    
    if (res$status_code==200) {
      final_res <- res |> httr2::resp_body_json()
      return(final_res)
    }
    
  } else {
    if (identical(vector,list())) { cli_alert("You need to provide a vector to retrieve a specific point.") }
    if (is.na(collection_name)) { cli_alert("You need to provide a collection name to retrieve a specific point.") }
  } 
  
  } else { stop("Connection to Qdrant could not be established.")}

    
}

#' Convert embeddings to Qdrant format
#'
#' @description
#' `convert_embeddings_to_qdrant_format` converts embeddings to the expected qdrant format for upserting.
#''
#' @details
#' Embeddings generated by the generate_document_embeddings() function have a different structure.
#' To make such embeddings directly usable with the qdrant function we can use this to make it seamless.
#'
#' @param input embeddings resulting from the generate_document_embeddings() function.
#' 
#' @examples
#' sentence <- "hi there this is a great day"
#' tmp_path <- tempfile()
#' writeLines(sentence, tmp_path)
#' conn <- get_ollama_connection()
#' doc_embeddings <- generate_document_embeddings(ollama_connection = conn,document_path = tmp_path)
#' embeddings_for_qdrant <- doc_embeddings |> convert_embeddings_to_qdrant_format()
#' 
#' @export
convert_embeddings_to_qdrant_format <- function(input) {
  points_list <- lapply(input, function(x) {
    list(
      payload = list(
        text = x$text
      ),
      vector = as.list(x$embeddings)
    )
  })
  
  return(list(points = points_list))
}
