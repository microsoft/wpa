#' @title
#' Internal function for plotting the hourly activity patterns.
#'
#' @description
#' This is used within `plot_flex_index()` and `workpatterns_rank()`.
#'
#' @param data Data frame containing three columns:
#'   - `patternRank`
#'   - `Hours`
#'   - `Freq`
#'
#' @param start_hour Numeric value to specify expected start hour.
#' @param end_hour Numeric value to specify expected end hour.
#'
#' @param legend Data frame containing the columns:
#'   - `patternRank`
#'   - Any column to be used in the grey label box, supplied to `legend_label`
#'
#' @param legend_label String specifying column to display in the grey label
#' box
#'
#' @param legend_text String to be used in the bottom legend label.
#'
#' @param rows Number of rows to show in plot.
#' @noRd
#'
#' @export

plot_hourly_pat <- function(
    data,
    start_hour,
    end_hour,
    legend,
    legend_label,
    legend_text = "Observed activity",
    rows,
    title,
    subtitle,
    caption,
    ylab = paste("Top", rows, "activity patterns")
){

  ## 00, 01, 02, etc.
  hours_col <- stringr::str_pad(seq(0,23), width = 2, pad = 0)

  data %>%
    utils::head(rows)  %>%
    tidyr::pivot_longer(
      cols = hours_col,
      names_to = "Hours",
      values_to = "Freq"
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = Hours, y = patternRank, fill = Freq)) +
    ggplot2::geom_tile(height = .5) +
    ggplot2::ylab(ylab) +
    ggplot2::scale_y_reverse(expand = c(0, 0), breaks = seq(1, rows)) +
    wpa::theme_wpa_basic() +
    ggplot2::scale_x_discrete(position = "top")+
    ggplot2::theme(
      axis.title.x = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank()
    ) +
    # Not operational if not binary
    scale_fill_continuous(
      guide = "legend",
      low = "white",
      high = "#1d627e",
      breaks = 0:1,
      name = "",
      labels = c("", legend_text)
    ) +
    ggplot2::annotate(
      "text",
      y = legend$patternRank,
      x = 26.5,
      label = legend[[legend_label]],
      size = 3
    )+
    ggplot2::annotate("rect",
                      xmin = 25,
                      xmax = 28,
                      ymin = 0.5,
                      ymax = rows + 0.5,
                      alpha = .2) +
    ggplot2::annotate("rect",
                      xmin = 0.5,
                      xmax = start_hour + 0.5,
                      ymin = 0.5,
                      ymax = rows + 0.5,
                      alpha = .1,
                      fill = "gray50") +
    ggplot2::annotate("rect",
                      xmin = end_hour + 0.5,
                      xmax = 24.5,
                      ymin = 0.5,
                      ymax = rows + 0.5,
                      alpha = .1,
                      fill = "gray50") +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    )
}
