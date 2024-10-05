#' Colorbrewer-inspired design with focus on LMU (lmu.de) green
#'
#' @return a list with `main_color_1`, a vector of 12 `main_colors`, a
#' corresponding `main_contrast_1` (the color of text to write on top of the
#' main color) and a corresponding `main_contrasts`, the `main_size` (for
#' lines), a `comparison_linetype`, `comparison_color`, and `comparison_size`
#' for all lines that act as comparative lines, and a [ggplot2] `theme`
#' @export
design_lmu <- function() {
  return(list(main_color_1 = "#00883A",
              main_colors = c("#00883A",
                              "#a6cee3",
                              "#1f78b4",
                              "#b2df8a",
                              #"#33a02c", #replaced by main_color_1
                              "#fb9a99",
                              "#e31a1c",
                              "#fdbf6f",
                              "#ff7f00",
                              "#cab2d6",
                              "#6a3d9a",
                              "#ffff99",
                              "#b15928"),
              main_contrast_1 = "#FFFFFF",
              main_contrasts = c("#FFFFFF",
                                 "#525252",
                                 "#FFFFFF",
                                 "#525252",
                                 "#525252",
                                 "#525252",
                                 "#525252",
                                 "#525252",
                                 "#525252",
                                 "#FFFFFF",
                                 "#525252",
                                 "#FFFFFF"),
              main_size = 0.95,
              comparison_linetype = "dashed",
              comparison_color = "#525252",
              comparison_size = 0.85,
              theme = function(...) {
                return(ggplot2::theme_minimal() +
                         ggplot2::theme(
                           axis.line = ggplot2::element_line(color = "#8f8f8f"),
                           axis.ticks = ggplot2::element_line(color = "#6b6b6b"),
                           axis.title = ggplot2::element_text(),
                           axis.title.x = ggplot2::element_text(vjust = -.5),
                           axis.title.y = ggplot2::element_text(vjust = 2.5),
                           strip.text = ggplot2::element_text(face = "bold"),
                           ...
                         ))
              }))
}

#' Colorbrewer-inspired design with focus on UZH (uzh.ch) blue
#'
#' @return a list with `main_color_1`, a vector of 12 `main_colors`, a
#' corresponding `main_contrast_1` (the color of text to write on top of the
#' main color) and a corresponding `main_contrasts`, the `main_size` (for
#' lines), a `comparison_linetype`, `comparison_color`, and `comparison_size`
#' for all lines that act as comparative lines, and a [ggplot2] `theme`
#' @export
design_uzh <- function() {
  return(list(main_color_1 = "#0028A5",
              main_colors = c("#0028A5",
                              "#BACBFF",
                              "#4AC9E3",
                              "#A5D233",
                              "#FFC845",
                              "#FC4C02",
                              "#BF0D3E",
                              "#7596FF",
                              "#B7E9F4",
                              "#DBEDAD",
                              "#FFE9B5",
                              "#FEB799"),
              main_contrast_1 = "#FFFFFF",
              main_contrasts = c("#FFFFFF",
                                 "#525252",
                                 "#FFFFFF",
                                 "#525252",
                                 "#FFFFFF",
                                 "#FFFFFF",
                                 "#FFFFFF",
                                 "#525252",
                                 "#525252",
                                 "#525252",
                                 "#525252",
                                 "#525252"),
              main_size = 0.95,
              comparison_linetype = "dashed",
              comparison_color = "#525252",
              comparison_size = 0.85,
              theme = function(...) {
                return(ggplot2::theme_minimal() +
                         ggplot2::theme(
                           axis.line = ggplot2::element_line(color = "#8f8f8f"),
                           axis.ticks = ggplot2::element_line(color = "#6b6b6b"),
                           axis.title = ggplot2::element_text(),
                           axis.title.x = ggplot2::element_text(vjust = -.5),
                           axis.title.y = ggplot2::element_text(vjust = 2.5),
                           strip.text = ggplot2::element_text(face = "bold"),
                           ...
                         ))
              }))
}

#' viridis-inspired design
#'
#' @return a list with `main_color_1`, a vector of 12 `main_colors`, a
#' corresponding `main_contrast_1` (the color of text to write on top of the
#' main color) and a corresponding `main_contrasts`, the `main_size` (for
#' lines), a `comparison_linetype`, `comparison_color`, and `comparison_size`
#' for all lines that act as comparative lines, and a [ggplot2] `theme`
#' @export
design_viridis <- function() {
  return(list(main_color_1 = "#440154FF",
              main_colors = c("#440154FF",
                              "#482173FF",
                              "#433E85FF",
                              "#38598CFF",
                              "#2D708EFF",
                              "#25858EFF",
                              "#1E9B8AFF",
                              "#2BB07FFF",
                              "#51C56AFF",
                              "#85D54AFF",
                              "#C2DF23FF",
                              "#FDE725FF"),
              main_contrast_1 = "#FFFFFF",
              main_contrasts = c("#FFFFFF",
                                 "#FFFFFF",
                                 "#FFFFFF",
                                 "#FFFFFF",
                                 "#FFFFFF",
                                 "#FFFFFF",
                                 "#525252",
                                 "#525252",
                                 "#525252",
                                 "#525252",
                                 "#525252",
                                 "#525252"),
              main_size = 0.95,
              comparison_linetype = "dashed",
              comparison_color = "#525252",
              comparison_size = 0.85,
              theme = function(...) {
                return(ggplot2::theme_minimal() +
                         ggplot2::theme(
                           axis.line = ggplot2::element_line(color = "#8f8f8f"),
                           axis.ticks = ggplot2::element_line(color = "#6b6b6b"),
                           axis.title = ggplot2::element_text(),
                           axis.title.x = ggplot2::element_text(vjust = -.5),
                           axis.title.y = ggplot2::element_text(vjust = 2.5),
                           strip.text = ggplot2::element_text(face = "bold"),
                           ...
                         ))
              }))
}

#' Gray design
#'
#' @return a list with `main_color_1`, a vector of 12 `main_colors`, a
#' corresponding `main_contrast_1` (the color of text to write on top of the
#' main color) and a corresponding `main_contrasts`, the `main_size` (for
#' lines), a `comparison_linetype`, `comparison_color`, and `comparison_size`
#' for all lines that act as comparative lines, and a [ggplot2] `theme`
#' @export
design_gray <- function() {
  return(list(main_color_1 = "#000000",
              main_colors = c("#000000",
                              "#cccccc",
                              "#111111",
                              "#bbbbbb",
                              "#222222",
                              "#aaaaaa",
                              "#333333",
                              "#999999",
                              "#444444",
                              "#888888",
                              "#555555",
                              "#777777"),
              main_contrast_1 = "#FFFFFF",
              main_contrasts = c("#FFFFFF",
                                 "#525252",
                                 "#FFFFFF",
                                 "#525252",
                                 "#FFFFFF",
                                 "#525252",
                                 "#FFFFFF",
                                 "#525252",
                                 "#FFFFFF",
                                 "#525252",
                                 "#FFFFFF",
                                 "#FFFFFF"),
              main_size = 0.95,
              comparison_linetype = "dashed",
              comparison_color = "#525252",
              comparison_size = 0.85,
              theme = function(...) {
                return(ggplot2::theme_minimal() +
                         ggplot2::theme(
                           axis.line = ggplot2::element_line(color = "#8f8f8f"),
                           axis.ticks = ggplot2::element_line(color = "#6b6b6b"),
                           axis.title = ggplot2::element_text(),
                           axis.title.x = ggplot2::element_text(vjust = -.5),
                           axis.title.y = ggplot2::element_text(vjust = 2.5),
                           strip.text = ggplot2::element_text(face = "bold"),
                           ...
                         ))
              }))
}

#' Grey design
#'
#' @return a list with `main_color_1`, a vector of 12 `main_colors`, a
#' corresponding `main_contrast_1` (the color of text to write on top of the
#' main color) and a corresponding `main_contrasts`, the `main_size` (for
#' lines), a `comparison_linetype`, `comparison_color`, and `comparison_size`
#' for all lines that act as comparative lines, and a [ggplot2] `theme`
#' @export
design_grey <- function() {
  return(design_gray())
}
