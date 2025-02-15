# Funktion zur Berechnung der Klassifikationsgenauigkeit und des totalen
# Klassifikationsfehlers für eine LCA auf Basis des Pakets poLCA.

classtable <- function(x, captioninfo = NULL) {
  
  # Entnommen aus: http://daob.nl/wp-content/uploads/2015/07/ESRA-course-slides.pdf
  # captioninfo wird in der Tabellenüberschrift ans Ende ergänzt und eignet sich
  # zusätzliche Infos für die Tabelle anzugeben. Die Anzah der Klassen wird
  # automatisch in die Überschrift eingefügt.
  
  posteriors <- x$posterior
  classes <- x$predclass
  class_size <- x$P
  
  classtable <- data.frame(posteriors, classes) %>% 
    group_by(classes) %>% 
    summarise_all(., function(x) {sum(x)})
  
  # % of classification error within class
  ct_out <- classtable %>% 
    summarise_at(vars(-classes), function(x) {
      round(x / sum(x), 2)
    }) %>% 
    mutate(class = 1:nrow(classtable))
  # Total classification error
  total_error <- 1 - sum(diag(as.matrix(classtable[,-1]))) / sum(as.matrix(classtable[,-1]))
  
  # Entropy
  entropy <- function(x) {-x * log(x)}
  
  error_prior <- entropy(x$P)
  error_post <- mean(apply(x$posterior, 1, entropy), na.rm = T)
  
  R2_entropy <- (error_prior - error_post) / error_prior
  
  # adding to table...
  # entropy
  ct_out$R2entropy <- R2_entropy
  # class sizes
  ct_out$size <- class_size*100
  
  dplyr::select(ct_out, class, size, R2entropy, everything()) %>%
    kable(., "html",
          digits = 2,
          caption = paste("Table: Entropy, class size, classification error and average latent class posterior probability for a LCA-Modell with",
                          length(ct_out)-3, "classes", captioninfo),
          col.names = c("True class", "class size (%)", "R² Entropy",
                        seq(1, length(ct_out)-3, 1)),
          align = c("l", "r", "c", rep("c", length(ct_out)-3))
    ) %>%
    kableExtra::kable_classic() %>%
    add_header_above(., c(" " = 3, "Predicted class membership" = length(ct_out)-3)) %>%
    footnote(general = "Predicted class membership represents the percentage overlap between true class and predicted class membership.",
             general_title = "Notes.",
             paste("Total classification error: ", round(total_error,4)),
             footnote_as_chunk = T)
}
