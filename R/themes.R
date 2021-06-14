# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Main theme for 'wpa' visualisations
#'
#' @description
#' A theme function applied to 'ggplot' visualisations in 'wpa'.
#' Install and load 'extrafont' to use custom fonts for plotting.
#'
#' @param font_size Numeric value that prescribes the base font size
#' for the plot. The text elements are defined relatively to this
#' base font size. Defaults to 12.
#'
#' @param font_family Character value specifying the font family
#' to be used in the plot. The default value is `"Segoe UI"`. To ensure
#' you can use this font, install and load {extrafont} prior to
#' plotting. There is an initialisation process that is described by:
#' <https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2>
#'
#' @import ggplot2
#'
#' @family Themes
#'
#' @return
#' Returns a ggplot object with the applied theme.
#'
#' @export
theme_wpa <- function(font_size = 12, font_family = "Segoe UI"){

  bg_colour <- "#FFFFFF"
  bg_colour2 <- "#CCCCCC" # light grey
  text_colour <- "grey40"

  text_small_dark <- element_text(size = font_size - 2, colour = text_colour, face = "plain")
  text_small_light <- element_text(size = font_size - 2, colour = "#FFFFFF", face = "plain")
  text_normal <- element_text(size = font_size + 2, colour = text_colour, face = "plain")
  text_italic <- element_text(size = font_size + 2, colour = text_colour, face = "italic")

  text_bold <- element_text(size = font_size + 2, colour = text_colour, face = "bold")
  text_title <- element_text(size = font_size + 8, colour = text_colour, face = "bold")

  theme_minimal(base_family = font_family) +
    theme(plot.background = element_blank(),
          # plot.background = element_rect(fill = bg_colour),
          text = text_normal,
          plot.title = text_title,
          plot.subtitle = text_italic,

          axis.title = text_normal,
          axis.text = text_small_dark,

          legend.title = text_small_dark,
          legend.text = text_small_dark) +

    theme(axis.line = element_line(colour = "grey20"),
          axis.ticks = element_blank(),

          legend.position = "bottom",
          legend.title = element_blank(),

          panel.grid = element_blank(),
          strip.background = element_rect(fill = bg_colour2, colour = bg_colour2),
          strip.text = text_small_dark)
}

#' @title Basic theme for 'wpa' visualisations
#'
#' @description
#' A theme function applied to 'ggplot' visualisations in 'wpa'.
#' Based on `theme_wpa()` but has no font requirements.
#'
#' @param font_size Numeric value that prescribes the base font size
#' for the plot. The text elements are defined relatively to this
#' base font size. Defaults to 12.
#'
#' @import ggplot2
#'
#' @family Themes
#'
#' @return
#' Returns a ggplot object with the applied theme.
#'
#' @export
theme_wpa_basic <- function(font_size = 12){

  bg_colour <- "#FFFFFF"
  bg_colour2 <- "#CCCCCC" # light grey
  text_colour <- "grey40"

  text_tiny_dark <- element_text(size = font_size - 4, colour = text_colour, face = "plain")
  text_small_dark <- element_text(size = font_size - 2, colour = text_colour, face = "plain")
  text_small_light <- element_text(size = font_size - 2, colour = "#FFFFFF", face = "plain")
  text_normal <- element_text(size = font_size + 0, colour = text_colour, face = "plain")
  text_italic <- element_text(size = font_size + 0, colour = text_colour, face = "italic")

  text_bold <- element_text(size = font_size + 0, colour = text_colour, face = "bold")
  text_title <- element_text(size = font_size + 2, colour = text_colour, face = "bold")

  theme_minimal() +
    theme(plot.background = element_blank(),
          # plot.background = element_rect(fill = bg_colour),
          text = text_normal,
          plot.title = text_title,
          plot.subtitle = text_normal,
          plot.caption = text_tiny_dark,

          axis.title = text_normal,
          axis.text = text_small_dark,

          legend.title = text_small_dark,
          legend.text = text_small_dark) +

    theme(axis.line = element_line(colour = "grey20"),
          axis.ticks = element_blank(),

          legend.position = "bottom",
          # legend.title = element_blank(),

          panel.grid = element_blank(),
          strip.background = element_rect(fill = bg_colour2, colour = bg_colour2),
          strip.text = text_small_dark)
}


