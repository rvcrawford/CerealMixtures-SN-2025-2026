# downscale and output image as PNG file to share or view more easily

image_reduce <- function(x, directory = NULL, scale_factor = 0.1){
  require(EBImage)
  require(tiff)
  library(stringr)
  img_array <- readTIFF(x, native = FALSE, convert = TRUE)
  
  filename <- str_split_i(x, pattern = "/", i = -1)
  no_suffix <- str_split_i(filename, pattern = "\\.", i = 1)
  pth <- str_sub(x, end = -(nchar(filename)+1))
  
  if (is.null(directory)){
    path_to_use = pth
  } else {
    path_to_use = directory
  }
  
  # Convert to EBImage object
  img <- Image(img_array, colormode = "Color")
  
  rgb_im <- EBImage::rgbImage(img[,,1], img[,,2], img[,,3])
  
  # Resize and save
  im_small <- EBImage::resize(rgb_im, w = dim(rgb_im)[1] * scale_factor)
  img_name <- paste(path_to_use, no_suffix,"_downscale", scale_factor*100,"_pct", ".png", sep = "")
  EBImage::writeImage(im_small, img_name)
 
}
