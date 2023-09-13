#' HRM merge
#'
#' \code{HRM_merge} merges HEMs coming from separate subexperiments into one single dataset. A Subexperiment is a measurement of a group of HEMs the size of the allowed PMCs by the machine. For HRM there is a designated anchor HEM that will be included on each subexperiment.
#' For more details check the reference paper below.
#' The input data must be in a specific format:
#' \tabular{llllll}{
#' \strong{1}  \tab \strong{2}  \tab \strong{3}  \tab  \strong{1} \tab  \strong{4} \tab  \strong{5}\cr
#' 30 \tab 15 \tab 20 \tab 40 \tab 10 \tab 12\cr
#' 35 \tab 16 \tab 25 \tab 25 \tab 13 \tab 15\cr
#' 32 \tab 14 \tab 30 \tab 21 \tab 11 \tab 17\cr
#' }
#'  where the numbers on top are the codes for the HEMs on the T2080. In this case we have two subexperiments: subexp1 = (\strong{1}, \strong{2}, \strong{3}), subexp2 = (\strong{1}, \strong{4}, \strong{5});
#'  and H1 is the anchor HEM that should be included in each subexperiment. The data will be processed into:
#' \tabular{llllll}{
#' \strong{H1}  \tab \strong{H2}  \tab \strong{H3}  \tab  \strong{H1} \tab  \strong{H4} \tab  \strong{H5}\cr
#' 30 \tab 15 \tab 20 \tab 40 \tab 10 \tab 12\cr
#' 35 \tab 16 \tab 25 \tab 25 \tab 13 \tab 15\cr
#' 32 \tab 14 \tab 30 \tab 21 \tab 11 \tab 17\cr
#' }
#' The processing transforms the code of the HEMs to the reference name on the T2080 manual.
#' \code{merge_hems} sorts each subexperiment by the anchor HEM, and the substitutes the anchor HEM by its quantiles. If one inputs the example dataset, \code{merge_hems} will return:
#' \tabular{lllll}{
#' \strong{H1} \tab \strong{H2} \tab \strong{H3} \tab \strong{H4} \tab \strong{H5}\cr
#' 21 \tab 15 \tab 20 \tab 11 \tab 17\cr
#' 31 \tab 14 \tab 30 \tab 13 \tab 15\cr
#' 40 \tab 16 \tab 25 \tab 10 \tab 12\cr
#' }
#'
#' @param data Sample data.
#' @param anchor_hem Reference HEM for merging
#' @return Returns a merged dataframe as stated in the description
#' @keywords HRM
#' @importFrom dplyr %>%
#' @import stats
#' @export
#' @references S. Vilardell, I. Serra, R. Santalla, E. Mezzetti, J. Abella and F. J. Cazorla, "HRM: Merging Hardware Event Monitors for Improved Timing Analysis of Complex MPSoCs," in IEEE Transactions on Computer-Aided Design of Integrated Circuits and Systems, vol. 39, no. 11, pp. 3662-3673, Nov. 2020, <doi:10.1109/TCAD.2020.3013051>.
#' @examples
#' library(mergingTools)
#' n_pmcs <- 6
#' data_hrm <- mergingTools::process_raw_experiments(data = data_hrm_raw_vignette, n_pmcs = n_pmcs)
#' merged_data <- mergingTools::HRM_merge(data = data_hrm, anchor_hem = "PROCESSOR_CYCLES")
HRM_merge <- function(data = NULL, anchor_hem = NULL) {
  data_merged <- data %>%
    purrr::map(~ .x %>% dplyr::arrange(!!rlang::sym(anchor_hem))) %>%
    purrr::map(~ .x %>% dplyr::select(-{{ anchor_hem }})) %>%
    purrr::reduce(cbind)
  anchor_data <- purrr::map(data, ~ .x %>% dplyr::select({{ anchor_hem }})) %>%
    unlist() %>%
    stats::quantile(probs = seq(0, 1, length.out = nrow(data_merged)), type = 2) %>%
    unname()
  merged_data <- cbind(anchor_data, data_merged)
  names(merged_data)[1] <- anchor_hem
  return(merged_data)
}
