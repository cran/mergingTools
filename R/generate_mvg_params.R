#' Generate multivariate Gaussian distribution parameters
#'
#' \code{generate_mvg_params} takes as input the experimental data along its correlation matrix to estimate the Sigma matrix and the vector of means to compute the multivariate Gaussian distribution
#'
#' @param splitted_data Splitted experimental data.
#' @param cor_matrix Correlation matrix
#' @return Returns the parameters of the multivariate Gaussian distributions as well as a list of concatenated HEM readings
#' @importFrom dplyr %>%
#' @keywords MUCH
#' @export
#' @examples
#' n_pmcs <- 6
#' data_much <- mergingTools::process_raw_experiments(data = data_much_raw_vignette,
#'                                                    n_pmcs = n_pmcs)
#' cor_matrix <- mergingTools::correlation_matrix(splitted_data = data_much)
#' dep_lvl <- 0.85
#' # Remove the HEMs which are linearly dependant on other HEMs
#' cor_matrix_independent <- mergingTools::get_independent_matrix(cor_matrix = cor_matrix,
#'                                                                dep_lvl = dep_lvl)
#' mvg_params <- mergingTools::generate_mvg_params(splitted_data = data_much,
#'                                                 cor_matrix = cor_matrix_independent)
generate_mvg_params <- function(splitted_data = NULL, cor_matrix = NULL) {
  hem_list <- list()
  hems <- colnames(cor_matrix)
  sd_hem <- vector(length = length(hems))
  mean_hem <- vector(length = length(hems))
  names(sd_hem) <- hems
  names(mean_hem) <- hems
  for (hem in hems) {
    df_with_hem <- purrr::map(splitted_data, ~
    sum(colnames(.x) %in% hem))
    index <- which(df_with_hem == 1)
    conc_vect <- c()

    for (ind in index) {
      hem_df_sc <- splitted_data[[ind]] %>%
        dplyr::select(dplyr::all_of(hem))

      m <- (hem_df_sc[[hem]] - mean(hem_df_sc[[hem]]))^2
      zscore <- (m - mean(m)) / sd(m)
      ind_outlier <- which(zscore >= 3)
      hem_df_sc <- hem_df_sc[-ind_outlier,]
      conc_vect <- c(conc_vect, hem_df_sc)
    }
    sd_hem[hem] <- sd(conc_vect)
    mean_hem[hem] <- mean(conc_vect)
    hem_list[[hem]] <- conc_vect
  }

  sd_diag <- diag(sd_hem)

  # Now compute the Sigma matrix
  sigma_matrix <- sd_diag %*% cor_matrix %*% sd_diag
  return(list(sigma_matrix = sigma_matrix, means = mean_hem, hems = hem_list))
}
