#' @title Counting duplicates times of each row
#'
#' @details
#'
#' This is a convenience function to extract duplicate lines in dataframes
#'
#' @return
#'
#' A column of numbers indicating duplicated times
#'


count_duplicates <- function(data, ...) {
  
  uq_names <- as.list(substitute(list(...)))[-1L]
  df_name <- deparse(substitute(data))
  
  if (length(uq_names) == 0) {
    # if called on an entire data.frame with no specified variable names
    var_names <- names(data)
    nms <- rlang::syms(var_names)
    message("No variable names specified - using all columns.\n")
  } else {
    # nms <- rlang::quos(X2:X3) %>%
    #   rlang::quos_auto_name()
    # var_names <- names(nms)
    nms <- rlang::enquos(...)
  }
  
  df <- data %>%
    dplyr::select(!!! nms)
  
  x <- do.call('paste', c(df, sep = '\r'))
  ox <- order(x)
  rl <- rle(x[ox])
  cbind(df[ox[cumsum(rl$lengths)], , drop = FALSE], dupe_count = rl$lengths)
  
}
