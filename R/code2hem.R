#' Code to HEM name
#'
#' \code{code2hem} returns the name of the HEM corresponding to the code of the T2080 manual.
#' When gathering experimental data the captured HEMs will be coded with a number.
#' This functions transforms the number into the reference name in the T2080 manual.
#' @param cod HEM code on the T2080 manual.
#' @return Returns the name of the HEM that corresponds to the input code.
#' @keywords HEM
#' @export
#' @examples
#' code <- 1
#' hem <- code2hem(code)
#' hem
#'
code2hem <- function(cod = NULL) {
  pmcs_n_codes <- mergingTools::T2080_code2name$hems
  names(pmcs_n_codes) <- mergingTools::T2080_code2name$code
  hem <- pmcs_n_codes[cod]
  return(hem)
}
