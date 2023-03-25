
process_shoe <- function(shoe, flip_vertical=FALSE, flip_horizontal=TRUE){
  
  # Creating data frame
  data <- shoe %>% 
    raster::as.data.frame()
  
  data <- data %>%
    mutate(y = n():1) %>%
    pivot_longer(!y, names_to = "x", names_prefix = "V")
  
  # Converting x to character
  data_xchr <-  data %>% mutate(x = parse_number(x))
  
  # Reduce resolution of the image
  data_lowres <- data_xchr %>% 
    mutate(y = round(y, -1), x = round(x, -1)) %>%
    group_by(x, y) %>%
    summarize(value = mean(value)) %>% ungroup()
  
  # Remove white pixels
  data_lowres <- data_lowres %>%
    filter(value<0.8)
  
  # Filter the picture of image
  data_sub <- data_lowres %>%
    filter(between(x, 250, 1750) &  between(y, 500, 4300))
  
  # PCA to align
  pca <- data_sub %>% dplyr::select(x,y) %>% prcomp()
  data_aligned <- data_sub %>%
    mutate(x = pca$x[,2], y = pca$x[,1])
  
  if(flip_vertical==FALSE & flip_horizontal==TRUE){
    data_aligned <- data_sub  %>%
      mutate(x = round(pca$x[,2], -1), y = round(pca$x[,1], -1))
  }
  else{
    data_aligned <- data_sub  %>%
      mutate(x = -1 * round(pca$x[,2], -1), y = -1 * round(pca$x[,1], -1))
  }
  
  return(data_aligned)
  
}