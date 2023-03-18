shoe1= readTIFF("C:/Users/endkg/Desktop/shoe_1.tiff",convert = TRUE)

process_shoe=function(data,x,y,flip_vertical=FALSE,flip_horizontal = FALSE){
  if(is.matrix(data)==T){
    shoe=data
  }
  
  else {stop}
  
  newshoe=shoe %>% raster::as.data.frame() %>% mutate(y=n():1) %>% 
    pivot_longer(!y, names_to = "x", names_prefix = "V") %>% mutate(x=parse_number(x))
  subnewshoe=newshoe %>% mutate(y=round(y,-1),x=round(x,-1)) %>% group_by(x,y) %>% summarise(value=mean(value)) %>% ungroup() %>% filter(value<0.8) %>% filter( between(x, 250, 1750) &  between(y, 500, 4300))
  pca_shoe=subnewshoe %>% dplyr::select(x,y) %>% prcomp()
  if(flip_vertical==FALSE & flip_horizontal == FALSE){
    alignshoe=subnewshoe %>% mutate(x = round(pca_shoe$x[,2], -1), y =    round(pca_shoe$x[,1], -1))
  }
  else if(flip_vertical==TRUE & flip_horizontal == FALSE){
    alignshoe=subnewshoe %>% mutate(x = 1 * round(pca_shoe$x[,2], -1), y = -1 * round(pca_shoe$x[,1], -1))}
  else if(flip_vertical==FALSE & flip_horizontal == TRUE){
    alignshoe=subnewshoe %>% mutate(x = -1*round(pca_shoe$x[,2], -1), y =    round(pca_shoe$x[,1], -1))}
  else{
    alignshoe=subnewshoe %>% mutate(x = -1 * round(pca_shoe$x[,2], -1), y = -1 * round(pca_shoe$x[,1], -1))
  }
  return(alignshoe)}

shoe1 %>% process_shoe() %>% ggplot(aes(x = x, y = y, fill = value)) + 
  geom_raster() + 
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed()

shoe1 %>% process_shoe(flip_vertical = TRUE) %>% ggplot(aes(x = x, y = y, fill = value)) + 
  geom_raster() + 
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed()

shoe1 %>% process_shoe(flip_horizontal = TRUE) %>% ggplot(aes(x = x, y = y, fill = value)) + 
  geom_raster() + 
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed()

shoe1 %>% process_shoe(flip_horizontal = TRUE,
                       flip_vertical = TRUE) %>% 
  ggplot(aes(x = x, y = y, fill = value)) + 
  geom_raster() + 
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed()