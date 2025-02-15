lc_classout <- function(x) {
  
  df <- data.frame(
    p_classes = round(as.vector(prop.table(table(x$predclass))*100),2),
    n_classes = as.vector(table(x$predclass))
  ) %>% 
    mutate(mem_classes = paste(p_classes, "%\n(n=", n_classes, ")", sep = "")) %>% 
    mutate(mem_classes = case_when(p_classes < 5 ~ cell_spec(mem_classes, color = "red"),
                                   p_classes < 10 ~ cell_spec(mem_classes, color = "orange"),
                                   TRUE ~ cell_spec(mem_classes, color = "black")))
  
  df2 <- t(df$mem_classes)
  df2 <- as.data.frame(df2)
  # names(df) <- classnr = 1:length(x$P)
  df2$Modell <- as.character(length(x$P))
  df2 <- dplyr::select(df2, Modell, everything())
  names(df2) <- c("Modell", rep(1:length(x$P)))
  df2
}