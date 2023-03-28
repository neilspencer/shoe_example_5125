process_shoe <- function(dat, flip_vertical = FALSE, flip_horizontal = FALSE){
  
  # take data as input. Must be read with readTIFF(dat, convert = TRUE)
  # data file must be image.
  # flip = "none" by default, optional "vertical", horizontal"
  # "vertical" performs vertical flip, "horizontal" performs horizontal flip
  
  # convert to dataframe through raster package
  dat_df <- dat %>% raster::as.data.frame() %>%
    mutate(y = n():1) %>% #specify a row to pivot data
    pivot_longer(!y, names_to = "x", names_prefix = "V") %>% #pivot data
    mutate(x = parse_number(x)) %>% #convert x into number
    mutate(y = round(y, -1), x = round(x, -1)) %>%
    group_by(x, y) %>%
    summarize(value = mean(value), .groups = "drop") %>%
    ungroup() %>%
    filter(value < 0.8) %>%
    filter(between(x, 250, 1750) &  between(y, 500, 4300))
  
  pca <- dat_df %>% dplyr::select(x,y) %>% prcomp() #calculates PCA
  
  shoe_df <- dat_df %>% #zoom in on the shoe
    mutate(x = round(pca$x[,2], -1), y = round(pca$x[,1], -1))
  
  if(flip_vertical == FALSE & flip_horizontal == FALSE){
    return(shoe_df) #default
    
  } else if (flip_vertical == TRUE & flip_horizontal == FALSE) {
    shoe_df <- shoe_df %>% mutate(x = x, y = -1 * y) #perform vertical flip
    return(shoe_df)
    
  } else if (flip_vertical == FALSE & flip_horizontal == TRUE){
    shoe_df <- shoe_df %>% mutate(x = -1 * x, y = y) #perform horizontal flip
    return(shoe_df)
  } else {
    shoe_df <- shoe_df %>% mutate(x = -1 * x, y = -1 * y) #perform horizontal flip
    return(shoe_df)
  }
}