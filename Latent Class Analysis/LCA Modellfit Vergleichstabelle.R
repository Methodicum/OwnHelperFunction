########################################################################
#### LCA: Modellgüte Vergleichstabellenvorlage für einzelnes Modell ####
########################################################################

lc_out <- function(x) {
  data.frame(Modell = as.character(length(x$P)),
             LL = x$llik,
             chi = x$Chisq,
             df = x$resid.df,
             p = 1-pchisq(q = x$Chisq, df = x$resid.df),
             BIC = x$bic,
             AIC = (-2*x$llik) + 2 * x$npar,
             LR_Ratio = x$Gsq)
}


# Beispiel für Liste mit mehreren Modellen:
# lc_out_table <- rbind(
#   lc_out(lc_1),
#   lc_out(lc_2),
#   lc_out(lc_3),
#   lc_out(lc_4),
#   lc_out(lc_5),
#   lc_out(lc_6),
#   lc_out(lc_7),
#   lc_out(lc_8),
#   lc_out(lc_9),
#   lc_out(lc_10)
# ) %>% 
#   mutate(LR_lag = lag(LR_Ratio),
#          df_lag = lag(df)) %>% 
#   mutate(delta_LR = LR_lag - LR_Ratio,
#          delta_DF = df_lag - df) %>% 
#   mutate(p_LR = 1-pchisq(delta_LR, delta_DF))