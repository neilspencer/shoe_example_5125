process_shoe <-
  function(shoe_matrix,
           flip_horizontal = FALSE,
           flip_vertical = FALSE) {

    shoedata <- shoe_matrix %>% raster::as.data.frame()
    shoedata <- shoedata %>% mutate(y = n():1)


    data_long <- shoedata %>%
      pivot_longer(!y, names_to = "x", names_prefix = "V") %>%
      mutate(x = parse_number(x))


    data_sub <- data_long %>%
      mutate(y = round(y,-1), x = round(x,-1)) %>%
      group_by(x, y) %>%
      summarize(value = mean(value)) %>%
      ungroup() %>%
      filter(value < 0.8)


    data_sub_trimmed <- data_sub %>%
      filter(between(x, 250, 1750) & between(y, 500, 4300))

    pca <- data_sub_trimmed %>%
      dplyr::select(x, y) %>%
      prcomp()
    data_aligned <- data_sub_trimmed %>%
      mutate(x = round(pca$x[, 2],-1),
             y = round(pca$x[, 1],-1))

    if (flip_horizontal) {
      data_aligned$x <- -data_aligned$x
    }
    if (flip_vertical) {
      data_aligned$y <- -data_aligned$y
    }
    return(data_aligned)
  }
