#' Simulate the MVG and merge the HEMs
#'
#' \code{simulate_and_merge} takes as input the parameters of the multivariate Gaussian distribution and simulates multiple samples.
#' The number of samples is determined by \code{n_sims}.
#' On each simulation, each HEM of the experimental data is sorted from lowest to highest independently.
#' Then, the simulated MGD is used as the model to arrange the experimental data.
#' For instance, if on the MGD the 10th highest value of HEM 1 is paired with the 2nd highest and the 4th highest values of HEMs 2 and 3 respectively,
#' Then, we modify the experimental data to copy this arrangement.
#' After merging and arranging, the 10th highest value of HEM 1 on the experimental data will be paired with the 2nd highest and the 4th highest values of HEMs 2 and 3 respectively.
#' The algorithm does multiple simulations of the MGD and keeps the sample that gives the lowest error on the correlation matrix when the data is merged w.r.t. the correlation matrix of the experimental data.
#'
#' @param mvg_params parameters from the multivariate Gaussian distribution
#' @param n_runs Number of rows for the output merged data
#' @param n_sims Number of simulations for the multivariate Gaussian distribution to find the optimal merge
#' @param cor_matrix Correlation matrix
#' @return Returns a merged dataframe as stated in the description
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
#' n_sims <- 10
#' n_runs <- 1000
#' merged_data <- mergingTools::simulate_and_merge(mvg_params = mvg_params,
#'                                                 n_runs = n_runs,
#'                                                 n_sims = n_sims,
#'                                                 cor_matrix = cor_matrix_independent)
simulate_and_merge <- function(mvg_params = NULL, n_runs = NULL, n_sims = NULL, cor_matrix = NULL) {
  # Parameters for the multivariate normal distribution
  mu <- mvg_params$means
  sigma <- mvg_params$sigma_matrix
  data_list <- mvg_params$hems
  exp_data <- purrr::map_df(data_list, ~ sample(.x, size = n_runs, replace = T))
  mse_cor <- c()
  link_list <- list()
  for (i in 1:n_sims) {
    # Generate normal distro
    link <- MASS::mvrnorm(n_runs, mu, sigma) %>% as.data.frame()

    # Now, order the random data based on the copulas for the multivariate normal distribution
    data_much_merge <- purrr::map2_df(exp_data, link, ~ sort(.x)[order(order(.y))])
    much_cor_matrix <- stats::cor(data_much_merge)
    mse_cor[i] <- sum((cor_matrix - much_cor_matrix)^2) / length(much_cor_matrix)
    link_list[[i]] <- link
  }

  # Select the simulation with lowest mse
  min_index <- which(mse_cor == min(mse_cor))
  data_much_merge <- purrr::map2_df(exp_data, link_list[[min_index]], ~ sort(.x)[order(order(.y))])
  return(data_much_merge)
}
