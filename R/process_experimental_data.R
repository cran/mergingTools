#' Process raw experimental data
#'
#' \code{process_raw_experiments} splits the data into separate dataframes.
#' The number of different dataframes is the number of subexperiments and the number of variables on each one is the number of PMCs.
#' The validation data is on the form of several subexperiments that don't need to be merged together.
#' The intent of the data is to provide information on how each HEM behaves when put against another HEM.
#' The validation data them is a collection of subexperiments where each HEM is put against each other.
#'
#'
#' @param data Validation data.
#' @param n_pmcs Number of PMCS used on the experiment
#' @return Returns a list of dataframes as stated in the description.
#' @keywords HRM MUCH
#' @importFrom dplyr %>%
#' @import stats
#' @export
#' @examples
#' n_pmcs <- 6
#' data_much <- mergingTools::process_raw_experiments(data = data_much_raw_vignette,
#'                                                    n_pmcs = n_pmcs)
process_raw_experiments <- function(data = NULL, n_pmcs = NULL) {

  # Change the column names like 1.1, 1.2, to clean names 1.1 -> 1
  clean_names <- stringr::str_replace(colnames(data), "\\..*", replacement = "") %>%
    stringr::str_extract("\\d.*")

  # Transform code to HEM name
  new_cols <- mergingTools::code2hem(clean_names)
  colnames(data) <- new_cols

  # Split data frame into n_subexp dataframes
  ind <- 1:ncol(data)
  ind <- split(ind, ceiling(seq_along(ind) / n_pmcs))
  data_splitted <- list()
  n_subexp <- ncol(data) / n_pmcs
  . <- NULL
  for (i in 1:n_subexp) {
    data_splitted[[i]] <- data[, ind[[i]]] %>%
      tidyr::drop_na() %>%
      dplyr::select_if(colMeans(.) > 100)
  }

  return(data_splitted)
}
