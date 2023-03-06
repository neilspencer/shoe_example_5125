# load the packages we need
library(tiff)
library(tidyverse)

# run the code creating the process_shoe() function.
source("process_shoe.R")


shoe1 <- readTIFF("shoe_1.tiff", convert = TRUE)

# plotting output
shoe1 %>% process_shoe() %>% ggplot(aes(x = x, y = y, fill = value)) + 
  geom_raster() + 
  scale_fill_gradient(low = "black", high = "white") +
  coord_fixed()

