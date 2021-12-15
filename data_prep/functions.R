save_model_output <- function(model, file_name) {
  coefs <- data.frame(coef(summary(model))) %>%
    rownames_to_column(var = "Predictor") %>%
    rename(SE = Std..Error, 
           z = z.value, 
           p = Pr...z..)
  write_csv(coefs, file_name)
}