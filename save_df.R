save_df <- function(df, directory = "./"){
  name_df <- deparse(substitute(df))
  save(list = name_df, file = paste0(directory, name_df, ".RData"), envir = parent.frame())
}
