#' Compute correlation matrix
#'
#' @param splitted_data Sample data.
#' @return Returns a merged dataframe as stated in the description
#' @keywords HRM
#' @importFrom dplyr %>%
#' @export
#' @references S. Vilardell, I. Serra, R. Santalla, E. Mezzetti, J. Abella and F. J. Cazorla, "HRM: Merging Hardware Event Monitors for Improved Timing Analysis of Complex MPSoCs," in IEEE Transactions on Computer-Aided Design of Integrated Circuits and Systems, vol. 39, no. 11, pp. 3662-3673, Nov. 2020, <doi:10.1109/TCAD.2020.3013051>.
#' @examples
#' n_pmcs <- 6
#' data_much <- mergingTools::process_raw_experiments(data = data_much_raw_vignette,
#'                                                    n_pmcs = n_pmcs)
#' cor_matrix <- mergingTools::correlation_matrix(splitted_data = data_much)
correlation_matrix <- function(splitted_data = NULL) {
  hems <- purrr::map(splitted_data, ~ names(.x)) %>%
    purrr::reduce(c) %>%
    unique()
  hem_pairs <- t(utils::combn(hems, m = 2))

  # Initalise matrices
  cor_matrix <- diag(
    x = 1,
    nrow = length(hems),
    ncol = length(hems)
  )


  # Give matrices columns and row names to ease the indexing
  colnames(cor_matrix) <- hems
  rownames(cor_matrix) <- hems


  # Here we select a pair of hems, get the dataframe where they are measured together and compute the true correlation
  for (i in 1:nrow(hem_pairs)) {
    h1 <- hem_pairs[i, 1]
    h2 <- hem_pairs[i, 2]

    # Get the dataframes containing h1 and h2
    df_with_h1h2 <- purrr::map(splitted_data, ~ sum(colnames(.x) %in% c(h1, h2)))
    index <- which(df_with_h1h2 == 2)
    conc_df <- data.frame()

    # Concatenate all data from h1 and h2 removing outliers
    for (ind in index) {
      h1h2_df_sc <- splitted_data[[ind]] %>%
        dplyr::select(dplyr::all_of(h1), dplyr::all_of(h2))
      m1 <- (h1h2_df_sc[[h1]] - mean(h1h2_df_sc[[h1]]))^2
      m2 <- (h1h2_df_sc[[h2]] - mean(h1h2_df_sc[[h2]]))^2
      m <- m1+m2
      zscore <- (m-mean(m))/sd(m)
      ind_outlier <- which(zscore >= 3)
      h1h2_df_sc <- h1h2_df_sc[-ind_outlier,]
      conc_df <- rbind(conc_df, h1h2_df_sc)
    }

    # scale and compute correlation
    x <- scale(conc_df[[h1]])
    y <- scale(conc_df[[h2]])
    cor_matrix[h1, h2] <- cor(x, y)
  }
  cor_matrix[lower.tri(cor_matrix)] <- t(cor_matrix)[lower.tri(cor_matrix)]
  return(cor_matrix)
}
