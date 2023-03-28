process_shoe <- function(shoe, 
                         flip_vertical = FALSE,
                         flip_horizontal = FALSE){
  shoedata <- shoe %>% raster::as.data.frame() 
  shoedata <- shoedata %>% dplyr::mutate(y = n():1)
  data_long <- shoedata %>% pivot_longer(!y, names_to = "x", names_prefix = "V")
  data_long <- data_long %>% mutate(x = parse_number(x))
  data_sub <- data_long %>% 
    mutate(y = round(y, -1), x = round(x, -1)) %>%
    group_by(x, y) %>%
    dplyr::summarize(value = mean(value)) %>% ungroup()
  data_sub <- data_sub %>%  filter(value < 0.8)
  data_sub_trimmed <- data_sub %>% filter( between(x, 250, 1750) &  between(y, 500, 4300))
  pca <- data_sub_trimmed %>% dplyr::select(x,y) %>% prcomp()
  shoe_aligned <- data_sub_trimmed  %>%
    mutate(x = round(pca$x[,2], -1), y = round(pca$x[,1], -1))
  
  if(flip_vertical){
    shoe_aligned <- shoe_aligned %>% mutate(y  = -y)
  }
  if(flip_horizontal){
    shoe_aligned <- shoe_aligned %>% mutate(x  = -x)
  }
  return(shoe_aligned)
}