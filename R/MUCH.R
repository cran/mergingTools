#' MUCH Merge
#'
#' \code{MUCH_merge} merges HEMs coming from separate subexperiments into one single dataset.
#' A Subexperiment is a measurement of a group of HEMs the size of the allowed PMCs by the machine.
#' For MUCH you need to measure each HEM against the rest of them. For instance, let's say that one has 2 PMCs and 3 HEMs to measure.
#' The input data coming from the experiments should look like this:
#' \tabular{llllll}{
#' \strong{1}  \tab \strong{2}  \tab \strong{1}  \tab  \strong{3} \tab  \strong{2} \tab  \strong{3}\cr
#' 30 \tab 15 \tab 20 \tab 54 \tab 15 \tab 24 \cr
#' 35 \tab 16 \tab 25 \tab 32 \tab 10 \tab 29 \cr
#' 32 \tab 14 \tab 30 \tab 45 \tab 9  \tab 32 \cr
#' }
#'  where the numbers on top are the codes for the HEMs on the T2080. In this case we have three subexperiments:
#' subexp1 = (\strong{1}, \strong{2}), subexp2 = (\strong{1}, \strong{3}, \strong{6}), subexp3 = (\strong{2}, \strong{3});
#' The data will be processed into:
#' \tabular{llllll}{
#' \strong{H1}  \tab \strong{H2}  \tab \strong{H1}  \tab  \strong{H3} \tab  \strong{H2} \tab  \strong{H3}\cr
#' 30 \tab 15 \tab 20 \tab 54 \tab 15 \tab 24\cr
#' 35 \tab 16 \tab 25 \tab 32 \tab 10 \tab 29\cr
#' 32 \tab 14 \tab 30 \tab 45 \tab 9  \tab 32\cr
#' }
#' The processing transforms the code of the HEMs to the reference name on the T2080 manual.
#' Now with this data, \code{MUCH_merge} computes the correlation matrix of all HEMs and with it it constructs a multivariate Gaussian distribution (MVG).
#' Then \code{MUCH_merge} uses the order statistics of the MVG to arrange the experimental data.
#' Therefore the final input will look like this:
#' \tabular{lll}{
#' \strong{H1}  \tab \strong{H2}  \tab \strong{H3}  \cr
#' 30 \tab 15 \tab 45 \cr
#' 35 \tab 16 \tab 54 \cr
#' 32 \tab 14 \tab 32 \cr
#' }
#'
#' Take into consideration that the input data has readings on the same HEM for multiple subexperiments.
#' Therefore one must select the number of runs \code{n_runs} that they want the final output to have.
#'
#'
#' @param splitted_data Sample data as list of dataframes.
#' @param n_runs Number of rows for the output merged data
#' @param n_sims Number of simulations for the multivariate Gaussian distribution to find the optimal merge
#' @param dep_lvl Dependency level allowed. A number between 0 and 1 indicating the maximum correlation allowed between HEMs.
#' @return Returns a merged dataframe as stated in the description
#' @keywords MUCH
#' @export
#' @examples
#' n_pmcs <- 6
#' data_much <- mergingTools::process_raw_experiments(data = data_much_raw_vignette,
#'                                                    n_pmcs = n_pmcs)
#' merged_data <- mergingTools::MUCH_merge(splitted_data = data_much,
#'                                         n_runs = 1000,
#'                                         n_sims = 10,
#'                                         dep_lvl = 0.85)

MUCH_merge <- function(splitted_data = NULL, n_runs = NULL, n_sims = NULL, dep_lvl = NULL) {
  # Compute the correlation matrix for all HEMs
  cor_matrix <- mergingTools::correlation_matrix(splitted_data = splitted_data)

  # Remove the HEMs which are linearly dependant on other HEMs
  cor_matrix_independent <- mergingTools::get_independent_matrix(cor_matrix = cor_matrix, dep_lvl = dep_lvl)

  # Compute the parameters for the multivariate Gaussian distribution
  mvg_params <- mergingTools::generate_mvg_params(splitted_data = splitted_data, cor_matrix = cor_matrix_independent)

  # Simulate several MVG and merge best on the optimal one
  merged_data <- mergingTools::simulate_and_merge(mvg_params = mvg_params, n_runs = n_runs, n_sims = n_sims, cor_matrix = cor_matrix_independent)

  return(merged_data)
}
