## Visualize as html- or pdf-table with linear model ---
##
## @param x a [tdcmm] model
##
## @return LaTeX or html-table
##
## @family tdcmm visualize
#
#' @export
visualize_regress_table <- function(x, design = design_lmu(), digits = 2, cap = "Linear Regression for", footnote = NULL) {

  tab <- x

  tab_format <- tab %>%
    rename(TOL = TOL) %>%
    dplyr::mutate(across(-1, ~round(.x, digits)),
                  p = format.pval(p, eps = .001, nsmall = 3),
                  p = gsub("0\\.","\\.", p)) %>%
    dplyr::mutate(across(any_of(c("beta", "TOL")), ~sub("^(-?)0.", "\\1.", sprintf("%.3f", .x)))) %>%
    dplyr::mutate(VIF = as.character(VIF)) %>%
    dplyr::mutate(across(any_of(c("beta", "VIF", "TOL")), ~dplyr::if_else(row_number()==1, "---", .x))) %>%
    kableExtra::kable(caption = cap,
                      align = c("l", rep("r", NCOL(tab) - 1)),
                      booktabs = TRUE,
                      longtable = FALSE,
                      linesep = "") %>%
    kableExtra::kable_styling(latex_options = c("repeat_header",
                                                "full_width = F")) %>%
    kableExtra::kable_styling(full_width = FALSE) %>%
    kableExtra::add_header_above(c("", "unstnd." = 4, "std.", "sig." =  2, "Multikoll." = 2), line = TRUE, line_sep = 3, bold = F) %>%
    kableExtra::footnote(footnote,
                         general_title = "",
                         threeparttable = TRUE)

  return(tab_format)

}
