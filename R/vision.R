resize_image_and_export_to_base64 <- function(input_image_path, max_dimension, force_jpg=F) {
  
  img_ext <- tools::file_ext(x = input_image_path)

  if (force_jpg==TRUE & (tolower(img_ext)!="jpg" & tolower(img_ext)!="jpeg")) {
    cli::cli_abort("Only JPG files accepted for images.")
  } else {
  
    tryCatch(
    img <- magick::image_read(path = input_image_path)
    , error=function(e) cli::cli_alert("Error {e}"))
    
    # resize picture according to max_dimension if specified
    if (!is.na(max_dimension)) {
    img <- magick::image_resize(image = img, geometry = glue::glue("{max_dimension}x{max_dimension}"))
    } 
    img_raw <- magick::image_write(img, format = "jpg", quality = 85)
    
    base64_image <- base64enc::base64encode(img_raw)
    return(base64_image)
  }
}

resize_images_and_export_to_base64 <- function(input_image_path_vector, max_dimension, force_jpg=F) {
  
  res <- sapply(input_image_path_vector, resize_image_and_export_to_base64, max_dimension, force_jpg)
  return(unname(res))
  
}

