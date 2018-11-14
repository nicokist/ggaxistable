library(ggplot2)
library(purrr)
library(stringr)
library(dplyr)

#' Creates a table formatted as a ggplot labeller object.
#'
#' \code{table_labeller} returns a function which can be used as the \code{label} parameter in \code{scale_x_discrete} or \code{scale_y_discrete}.
#'
#'  This function is not usually used directly, but is called from \code{axis_table}.
#'
#' @param .data Data containing both the x/y aesthetic of the original data, as well as the columns which should be included in the table.
#' @param axis_col Character. The name or index of the column used for the x or y aesthetic.
#' @param selected_cols Character. The columns which should be used in the table.
#' @param col_widths Numeric vector. The desired width (in characters) of each column.
table_labeller <- function(.data, axis_col, selected_cols = colnames(.data), col_widths = NULL) {
  stopifnot(axis_col %in% colnames(.data))
  chosen_table <- dplyr::mutate_all(.data[selected_cols], as.character)
  if (is.null(col_widths)) {
    element_lengths <- purrr::map(chosen_table, stringr::str_length)

    ## make sure that the column names are also included before calculating column widths.
    for (i in seq_along(element_lengths)) {
      element_lengths[[i]] <- c(element_lengths[[i]], stringr::str_length(selected_cols)[i])
    }

    col_widths <- purrr::map_int(element_lengths, function(x) max(x, na.rm = TRUE)) + 4
    # rightmost column doesn't need the extra spacing
    col_widths[length(col_widths)] <- col_widths[length(col_widths)] - 4
  }
  stopifnot(ncol(chosen_table) == length(col_widths))

  #  format_cell <- function() {}

  for (i in seq_along(chosen_table)) {
    name <- colnames(chosen_table)[i]
    # Truncate and then pad every cell to make it look good in the fixed width tables.
    chosen_table[name] <- stringr::str_pad(stringr::str_trunc(chosen_table[[name]],
      width = col_widths[i],
      ellipsis = "~"
    ),
    width = col_widths[i],
    side = "right"
    )
  }

  # The next bit is tricky, it's hard to get just one bold line in the labels.
  # This is all necessary because sometimes you want different rows
  # to have the same label.
  formatted_rows <- c(purrr::pmap_chr(chosen_table, paste0))
  names(formatted_rows) <- .data[[axis_col]]

  col_header <- parse(text = paste0('bold("', paste0(stringr::str_pad(selected_cols, col_widths, "right"), collapse = ""), '")'))


  env <- list(formatted_rows = formatted_rows, col_header = col_header)

  z <- function(x, environment = env) {
    return(ifelse(x == "HEADER_placeholder", col_header, formatted_rows[x]))
  }
  z
}



#' Adds a table to a ggplot axis.
#'
#' \code{axis_table}
#'
#' Make sure you add
#' \code{+ theme(axis.text.x = element_text(family = "mono", color = "black", hjust=0))}
#'
#' to the plot if you're adding a table to y axis or
#' \code{+ theme(axis.text.y = element_text(family = "mono", color = "black", angle=-90, vjust=0))}
#'
#' for the x axis.
#' This will ensure your table is oriented correctly and that a fixed-width font is used.
#'
#' @param .data Data containing both the y aesthetic of the original data, as well as the columns which should be included in the table.
#' @param y A character vector containing the same column as is used for the y aesthetic in the plot (use only one of \code{x} and \code{y})
#' @param x A character vector containing the same column as is used for the x aesthetic in the plot (use only one of \code{x} and \code{y}).
#' @param selected_cols A character vector containing the columns in \code{.data} to be used in the axis table.
#' @param col_widths A numeric vector containing the desired widths (in characters) of each column.
#' @return A specially-formed scale_discrete function containing the table to be added to the plot.
#' @export
#'
#' @examples
#' see github page f
#'


axis_table <- function(.data, x = NULL, y = NULL, selected_cols = colnames(.data), col_widths = NULL) {
  if (is.null(x) & is.null(y)) stop("axis_table needs either an x or a y parameter.")
  if (!is.null(x) & !is.null(y)) stop("axis_table needs either an x or a y parameter but not both.")
  if (!is.null(x)) {
    axis <- x
    scale_xy_discrete <- ggplot2::scale_x_discrete
  }
  if (!is.null(y)) {
    axis <- y
    scale_xy_discrete <- ggplot2::scale_y_discrete
  }

  my_labeller <- table_labeller(.data, axis_col = axis, selected_cols = selected_cols, col_widths = col_widths)
  scale_xy_discrete(labels = my_labeller, limits = c(levels(as.factor(.data[[axis]])), "HEADER_placeholder"), drop = FALSE)
}
