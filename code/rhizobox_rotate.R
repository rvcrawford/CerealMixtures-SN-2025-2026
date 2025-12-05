crop_and_rotate <- function(x, directory = NULL, display_scale = 0.25){
  require(EBImage)
  require(tiff)
  require(stringr)
  require(exifr)
  require(lubridate)
  # obtain file metadata to get date
  get_metadata <- read_exif(x)
  
  filename <- str_split_i(x, pattern = "/", i = -1)
  no_suffix <- str_split_i(filename, pattern = "\\.", i = 1)
  pth <- str_sub(x, end = -(nchar(filename)+1))
  
  if (is.null(directory)){
    path_to_use = pth
  } else {
    path_to_use = directory
  }
  
  img_array <- readTIFF(x, native = FALSE, convert = TRUE)
  print(dim(img_array))
  
  # Convert to EBImage object
  img <- Image(img_array, colormode = "Color")
  
  # img <- readImage(x, all = TRUE)
  # find center coordinates
  dims_orig <- dim(img)
  
  cx_original <- dims_orig[[1]]/2
  cy_original <- dims_orig[[2]]/2
  
  rgb_im <- EBImage::rgbImage(img[,,1], img[,,2], img[,,3])
  img_display <- resize(rgb_im, w = dims_orig[1] * display_scale,
                        h = dims_orig[2] * display_scale)
  dims_display <- dim(img_display)
  
  cx_display <- dims_display[[1]]/2
  cy_display <- dims_display[[2]]/2
  repeat{
    dev.new(height = 7, width = 8, units = "in")
    plot(img_display)
    cat("Beginning in what should be the top left corner if the\n rhizobox were upright, click clockwise around the box\n")
    pts <- locator(4)
    
    points(pts$x, pts$y, col = "red", pch = 19, cex = 2)
    text(pts$x, pts$y, labels = 1:4, col = "yellow", pos = 3, cex = 1.5)
    happiness_question <- readline("are you happy with these points (y/n): ")
    if (happiness_question == "y"){
      dev.off()
      break
    } else if (happiness_question != "n"){
      cat("Only y or n please!")
      dev.off()
    }
    
  }
  
  # Scale points back to full resolution
  scale_factor <- 1 / display_scale
  pts$x <- pts$x * scale_factor
  pts$y <- pts$y * scale_factor
  
  # Free display image
  rm(img_display)
  gc()
  
  # Calculate angles from multiple edges 
  
  # load in image again
  img_full <- readImage(x, all = TRUE)
  
  top_angle <- atan2(pts$y[2] - pts$y[1], pts$x[2] - pts$x[1])
  bottom_angle <- atan2(pts$y[3] - pts$y[4], pts$x[3] - pts$x[4])
  left_side_angle <- atan2(pts$y[4] - pts$y[1], pts$x[4] - pts$x[1]) - pi/2
  right_side_angle <- atan2(pts$y[3] - pts$y[2], pts$x[3] - pts$x[2]) - pi/2
  
  # Average angles with weird math
  angles <- c(top_angle, bottom_angle, left_side_angle, right_side_angle)
  
  just_angle <- atan2(mean(sin(angles)), mean(cos(angles)))
  angle <- - atan2(mean(sin(angles)), mean(cos(angles)))* 180/pi
  
  cat(sprintf("Rotating by %.2f degrees\n", angle))
  
  # img2 <- img[xmin:xmax, ymin:ymax,]
  
  # Rotate all channels
  img_rotated <- rotate(img, angle, bg.col = "black")
  
  rm(img)
  gc()
  
  dims_rotated <- dim(img_rotated)
  cx_rotated <- dims_rotated[[1]]/2
  cy_rotated <- dims_rotated[[2]]/2
  
  # point translation
  points_translated_x <- pts$x - cx_original
  points_translated_y <- pts$y - cy_original 
  
  points_translated <- data.frame(x = points_translated_x, y = points_translated_y)
  points_translated$x_trans <- points_translated$x * cos(just_angle) - points_translated$y * sin(just_angle) + cx_rotated
  points_translated$y_trans <- points_translated$x * sin(just_angle) + points_translated$y * cos(just_angle) + cy_rotated
  
  # perform crop 
  img_cropped <- img_rotated[min(points_translated$x_trans):max(points_translated$x_trans), min(points_translated$y_trans):max(points_translated$y_trans),]
  rm(img_rotated)
  gc()
  
  # saved image path
  create_date_time <- get_metadata$FileModifyDate |> lubridate::ymd_hms()
  
  # CR = cropped, rotated
  img_name <- paste(path_to_use, no_suffix, "_CR_", create_date_time, ".tif", sep = "")
  cat("Attempting to save to:", img_name, "\n")
  print(path_to_use)
  print(no_suffix)
  print(create_date_time)
  cat(str(img_cropped))
  
  # set color mode so images are written in correct stack order
  colorMode(img_cropped) <- 2
  writeImage(img_cropped, img_name)
  rm(img_cropped)
  gc()
}
