


resize_image_and_export_to_base64 <- function(input_image_path, max_dimension, force_jpg=T) {
  
  img_ext <- tools::file_ext(x = input_image_path)

  if (force_jpg==TRUE & (img_ext!="jpg" & img_ext!="jpeg")) {
    cli::cli_abort("Only JPG files accepted for images.")
  } else {
  
    img <- magick::image_read(path = input_image_path)
    img <- magick::image_resize(image = img, geometry = max_dimension)
    img <- magick::image_resize(image = img, geometry = glue::glue("x{max_dimension}"))
    
    img_raw <- magick::image_write(img, format = "jpg", quality = 85)
    base64_image <- base64enc::base64encode(img_raw)
    return(base64_image)
  }
}

resize_images_and_export_to_base64 <- function(input_image_path_vector, max_dimension, force_jpg=T) {
  
  res <- sapply(input_image_path_vector, resize_image_and_export_to_base64, max_dimension, force_jpg)
  return(unname(res))
  
}
