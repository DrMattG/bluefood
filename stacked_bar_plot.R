# ============================================================
# Stacked bar plot with split legends
# ============================================================

#' Extract a ggplot2 legend as a grob
#'
#' Utility function to extract the legend ("guide-box") from a ggplot
#' object. Useful for manual legend placement using grid or gridExtra.
#'
#' @param p A ggplot object.
#'
#' @return A grob object containing the legend.
#'
#' @keywords internal
#' @import ggplot2
#' @import grid
#'
#' @examples
#' \dontrun{
#' leg <- g_legend(my_plot)
#' grid::grid.draw(leg)
#' }
g_legend <- function(p) {
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  idx <- which(vapply(gt$grobs, function(x) x$name, "") == "guide-box")
  gt$grobs[[idx]]
}


#' Create a stacked bar plot with split legends
#'
#' This function generates a flipped stacked bar plot ("spine plot")
#' showing category counts, alongside two separately extracted legend
#' blocks. It is designed for repeated use across different categorical
#' variables while keeping visual style consistent.
#'
#' @param data A data frame.
#' @param column Bare column name containing comma-separated categories.
#' @param levels Character vector giving the full factor order.
#' @param legend_split A list of character vectors defining how levels
#'   should be split across legend blocks (e.g. list(group1, group2)).
#' @param legend_titles Character vector of legend titles, one per legend
#'   block.
#' @param palette_base A vector of base colours used to generate a palette.
#' @param ylab Character string for the y-axis label.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{spine}{The flipped stacked bar plot (ggplot object).}
#'   \item{legends}{A list of legend grobs.}
#' }
#'
#' @details
#' The function:
#' \itemize{
#'   \item separates comma-delimited values
#'   \item counts category occurrences
#'   \item enforces explicit factor ordering
#'   \item creates a stacked bar plot
#'   \item splits the legend into multiple blocks
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import ggtext
#'
#' @examples
#' \dontrun{
#' out <- stacked_split_legend_plot(
#'   data = my_data,
#'   column = Study.species,
#'   levels = species_levels,
#'   legend_split = list(group1, group2),
#'   legend_titles = c("Species", ""),
#'   palette_base = my_pal
#' )
#' }
stacked_split_legend_plot <- function(
    data,
    column,
    levels,
    legend_split,
    legend_titles = c("", ""),
    palette_base,
    ylab = "Study count"
) {
  
  col <- rlang::enquo(column)
  
  # ---- prepare data ----
  df <- data %>%
    tidyr::separate_rows(!!col, sep = ", ") %>%
    dplyr::count(!!col) %>%
    dplyr::mutate(
      !!col := factor(!!col, levels = levels)
    )
  
  pal <- taylor::color_palette(palette_base, n = nrow(df))
  df$palette <- as.vector(pal)
  
  # ---- base plot ----
  base_plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = 3, y = n, fill = !!col)
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.text = ggtext::element_markdown()
    ) +
    ggplot2::scale_fill_manual(values = df$palette) +
    ggplot2::ylab(ylab)
  
  spine_plot <- base_plot +
    ggplot2::coord_flip() +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_x_discrete(as.character(0:1))
  
  # ---- legend plots ----
  legend_plots <- purrr::map2(
    legend_split,
    legend_titles,
    ~ base_plot +
      ggplot2::scale_fill_manual(
        values = df$palette[df[[rlang::as_name(col)]] %in% .x],
        limits = .x
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(title = .y)
      )
  )
  
  legends <- purrr::map(legend_plots, g_legend)
  
  list(
    spine = spine_plot,
    legends = legends
  )
}


# ============================================================
# Example: Study species
# ============================================================

library(tidyverse)
library(taylor)
library(grid)
library(gridExtra)

# Example palette
my_pal <- c("#2c454a", "#577c84", "#a8bdbe",
            "#e2b8a2", "#ff9d78", "#e55634")

# Example usage
species_out <- stacked_split_legend_plot(
  data = data,
  column = Study.species,
  levels = c(
    "Atlantic cod (Gadus morhua)",
    "Gilthead sea bream (Sparus aurata)",
    "Sea bass (Dicentrus labrax)",
    "Atlantic salmon (Salmo salar)",
    "Chinook salmon (Oncorhynchus tshawytscha)",
    "Chum salmon (Oncorhynchus keta)",
    "Coho salmon (Oncorhynchus kisutch)",
    "Pink salmon (Oncorhynchus gorbuscha)",
    "Salmon (unspecified)"
  ),
  legend_split = list(
    c("Atlantic cod (Gadus morhua)",
      "Gilthead sea bream (Sparus aurata)",
      "Sea bass (Dicentrus labrax)",
      "Atlantic salmon (Salmo salar)"),
    c("Chinook salmon (Oncorhynchus tshawytscha)",
      "Chum salmon (Oncorhynchus keta)",
      "Coho salmon (Oncorhynchus kisutch)",
      "Pink salmon (Oncorhynchus gorbuscha)",
      "Salmon (unspecified)")
  ),
  legend_titles = c("Study species", ""),
  palette_base = my_pal
)

# Draw example
gridExtra::grid.arrange(
  species_out$spine,
  species_out$legends[[1]],
  species_out$legends[[2]],
  ncol = 3,
  widths = c(1.5, 1, 1)
)
