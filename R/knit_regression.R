
# Visualize as html- or pdf-table with linear model ---
#
# @param x a [tdcmm] model
#
# @return html-, pdf- or word-table
#
# @family tdcmm visualize
#
#' @export
knit_regress_table <- function(x,
                                    design = design_uzh(),
                                    digits = 2,
                                    cap = "Linear Regression for",
                                    footnote = "footers irgendwann"
) {
  tab <- x

  ColNum_unst <- x |>
    dplyr::select(dplyr::any_of(c("B", "SE B", "LL", "UL"))) |>
    ncol()

  ColNum_std <- x |>
    dplyr::select(dplyr::any_of(c("beta"))) |>
    ncol()

  ColNum_sig <- x |>
    dplyr::select(dplyr::any_of(c("t", "p"))) |>
    ncol()

  ColNum_multicol <- x |>
    dplyr::select(dplyr::any_of(c("VIF", "TOL"))) |>
    ncol()

  kableHeader <- c(" ")

  if(ColNum_unst > 1) {
    kableHeader <- c(kableHeader, `unstd.` = ColNum_unst)
  } else if (ColNum_unst == 1){
    kableHeader <- c(kableHeader, " ")
  }

  if(ColNum_std > 1) {
    kableHeader <- c(kableHeader, `std.` = ColNum_std)
  } else if (ColNum_std == 1){
    kableHeader <- c(kableHeader, " ")
  }

  if(ColNum_sig > 1) {
    kableHeader <- c(kableHeader, `sig.` = ColNum_sig)
  } else if (ColNum_sig == 1){
    kableHeader <- c(kableHeader, " ")
  }


  if(ColNum_multicol > 1) {
    kableHeader <- c(kableHeader, `Multicoll.` = ColNum_multicol)
  } else if (ColNum_multicol == 1){
    kableHeader <- c(kableHeader, " ")
  }

  if(sum(c(ColNum_unst > 1, ColNum_std > 1, ColNum_sig > 1, ColNum_multicol > 1)) == 0) {
    kableHeader <- NULL
  }


  tab_format <- tab |>
    dplyr::mutate(dplyr::across(-1, ~round(.x, digits)),
                  dplyr::across(dplyr::any_of("p"), ~format.pval(.x, eps = .001, nsmall = 3)),
                  dplyr::across(dplyr::any_of("p"), ~gsub("0\\.","\\.", .x))) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("beta", "TOL")), ~sub("^(-?)0.", "\\1.", sprintf("%.3f", .x)))) |>
    dplyr::mutate(dplyr::across(dplyr::any_of("VIF"), as.character)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("beta", "VIF", "TOL")), ~dplyr::if_else(row_number()==1, "-", .x)))

  if(knitr::is_html_output() | knitr::is_latex_output()) {

  tab_knit <- tab_format |>
    kableExtra::kable(caption = cap,
                      align = c("l", rep("r", NCOL(tab) - 1)),
                      booktabs = TRUE,
                      longtable = FALSE,
                      linesep = "") |>
    kableExtra::kable_styling(latex_options = c("repeat_header",
                                                "full_width = F")) |>
    kableExtra::kable_styling(full_width = FALSE) |>
    kableExtra::add_header_above(header = kableHeader,
                                 line = TRUE, line_sep = 3, bold = F) |>
    kableExtra::footnote(footnote,
                         general_title = "",
                         threeparttable = TRUE)
  }

  if (knitr::pandoc_to("docx")){
  tab_knit <- tab_format |>
    flextable::flextable()

  }

  return(tab_format)

}

