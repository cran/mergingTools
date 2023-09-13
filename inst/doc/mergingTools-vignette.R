## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(mergingTools)

## ----include=FALSE------------------------------------------------------------
library(dplyr)
library(purrr)
library(DT)
library(devtools)
devtools::load_all()

## ----echo=FALSE---------------------------------------------------------------
# Data
DT::datatable(data_hrm_raw_vignette,
              extensions = 'FixedColumns',
              options = list(scrollX = TRUE,
              scrollCollapse = TRUE))


## -----------------------------------------------------------------------------
n_pmcs <- 6
data_hrm <- mergingTools::process_raw_experiments(data = data_hrm_raw_vignette, 
                                                  n_pmcs = n_pmcs)

## ----echo=FALSE---------------------------------------------------------------
DT::datatable(data_hrm[[1]],
              extensions = 'FixedColumns',
              options = list(scrollX = TRUE,
              scrollCollapse = TRUE))
DT::datatable(data_hrm[[2]],
              extensions = 'FixedColumns',
              options = list(scrollX = TRUE,
              scrollCollapse = TRUE))
DT::datatable(data_hrm[[3]],
              extensions = 'FixedColumns',
              options = list(scrollX = TRUE,
              scrollCollapse = TRUE))

## -----------------------------------------------------------------------------
anchor_hem <- "PROCESSOR_CYCLES"
data_arranged <- data_hrm %>%
  purrr::map(~ .x %>% arrange(!!sym(anchor_hem)))

## ----echo=FALSE---------------------------------------------------------------
DT::datatable(data_arranged[[1]],
              extensions = 'FixedColumns',
              options = list(scrollX = TRUE,
              scrollCollapse = TRUE))
DT::datatable(data_arranged[[2]],
              extensions = 'FixedColumns',
              options = list(scrollX = TRUE,
              scrollCollapse = TRUE))
DT::datatable(data_arranged[[3]],
              extensions = 'FixedColumns',
              options = list(scrollX = TRUE,
              scrollCollapse = TRUE))

## ----message=FALSE, warning=FALSE---------------------------------------------
data_merged <- data_arranged %>%
  purrr::map(~ .x %>% select(-anchor_hem)) %>%
  purrr::reduce(cbind)

## -----------------------------------------------------------------------------
DT::datatable(data_merged,
              extensions = 'FixedColumns',
              options = list(scrollX = TRUE,
              scrollCollapse = TRUE))

## -----------------------------------------------------------------------------
anchor_data <- purrr::map(data_arranged, ~ .x %>% dplyr::select(anchor_hem)) %>%
  unlist() %>%
  stats::quantile(probs = seq(0, 1, length.out = nrow(data_merged)), type = 2) %>%
  unname()
merged_data <- cbind(anchor_data, data_merged)
names(merged_data)[1] <- anchor_hem

## ----echo=FALSE---------------------------------------------------------------
DT::datatable(merged_data,
              extensions = 'FixedColumns',
              options = list(scrollX = TRUE,
              scrollCollapse = TRUE))

## -----------------------------------------------------------------------------
# Data
n_pmcs <- 6
data_much <- mergingTools::process_raw_experiments(data = data_much_raw_vignette, 
                                                  n_pmcs = n_pmcs)
length(data_much)

## -----------------------------------------------------------------------------
# Compute the correlation matrix for all HEMs
cor_matrix <- mergingTools::correlation_matrix(splitted_data = data_much)

## ----echo=FALSE---------------------------------------------------------------
DT::datatable(cor_matrix ,
              extensions = 'FixedColumns',
              options = list(scrollX = TRUE,
              scrollCollapse = TRUE)) %>% 
              formatRound(columns = colnames(cor_matrix), digits=3)

## -----------------------------------------------------------------------------
dep_lvl <- 0.85
# Remove the HEMs which are linearly dependant on other HEMs
cor_matrix_independent <- mergingTools::get_independent_matrix(cor_matrix = cor_matrix, dep_lvl = dep_lvl)

## ----echo=FALSE---------------------------------------------------------------
DT::datatable(cor_matrix_independent ,
              extensions = 'FixedColumns',
              options = list(scrollX = TRUE,
              scrollCollapse = TRUE)) %>% 
  formatRound(columns = colnames(cor_matrix_independent), digits=3)

## -----------------------------------------------------------------------------
# Compute the parameters for the multivariate Gaussian distribution
mvg_params <- mergingTools::generate_mvg_params(splitted_data = data_much, cor_matrix = cor_matrix_independent)

## -----------------------------------------------------------------------------
# Simulate several MVGD and merge best on the optimal one
n_sims <- 100
n_runs <- 1000
merged_data <- mergingTools::simulate_and_merge(mvg_params = mvg_params, n_runs = n_runs, n_sims = n_sims, cor_matrix = cor_matrix_independent)

## ----echo=FALSE---------------------------------------------------------------
DT::datatable(merged_data ,
              extensions = 'FixedColumns',
              options = list(scrollX = TRUE,
              scrollCollapse = TRUE))

