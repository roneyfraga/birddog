#' Create CCT or Entropy Visualization Plots
#'
#' @param data Data from calculate_cct or calculate_entropy function
#' @param group_name Specific group to visualize
#' @param start_year Starting year for x-axis
#' @param end_year Ending year for x-axis
#' @param method Character string indicating the method: "cct" or "entropy"
#'
#' @return A plotly object with combined plots
#' @keywords internal
indexes_plots <- function(data, group_name, start_year, end_year, method = "cct") {
  
  tryCatch({
    # Filter data for specific group
    data |>
      dplyr::filter(.data$group == group_name) |>
      dplyr::arrange(.data$year) ->
      group_data
    
    if (nrow(group_data) == 0) {
      stop("No data available for group: ", group_name, call. = FALSE)
    }
    
    # Calculate appropriate number of breaks for x-axis
    n_years <- end_year - start_year + 1
    n_breaks <- ifelse(n_years > 20, 10, max(1, floor(n_years / 2)))
    
    # Set labels based on method
    if (method == "cct") {
      y_label <- "CCT - Absolute Value"
      y_label_diff <- "CCT - Differences"
      plot_title <- paste("Group:", group_name)
    } else {
      y_label <- "Entropy - Absolute Value"
      y_label_diff <- "Entropy - Differences"
      plot_title <- paste("Entropy - Group:", group_name)
    }
    
    # Plot 1: Absolute values
    p1 <- ggplot2::ggplot(group_data, ggplot2::aes(x = .data$year, y = .data$index)) +
      ggplot2::geom_line(color = "steelblue", linewidth = 1) +
      ggplot2::scale_y_continuous(limits = c(0, max(group_data$index, na.rm = TRUE) * 1.1)) +
      ggplot2::scale_x_continuous(
        limits = c(start_year, end_year),
        breaks = seq(start_year, end_year, by = max(1, floor(n_years / n_breaks)))
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
          angle = 90, vjust = 0.5, hjust = 1, size = 12
        ),
        axis.text.y = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 14),
        plot.title = ggplot2::element_text(size = 20)
      ) +
      ggplot2::labs(x = "Year", y = y_label, title = plot_title)
    
    # Plot 2: Differences
    group_data |>
      dplyr::mutate(diff = c(NA, base::diff(.data$index))) |>
      stats::na.omit() |>
      dplyr::mutate(color_flag = ifelse(.data$diff < 0, "blue", "red")) ->
      diff_data
    
    p2 <- ggplot2::ggplot(
      diff_data,
      ggplot2::aes(x = .data$year, y = .data$diff)
    ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          xend = .data$year, yend = .data$diff, y = 0, color = .data$color_flag
        ),
        linewidth = 0.8
      ) +
      ggplot2::geom_point(ggplot2::aes(color = .data$color_flag), size = 1) +
      ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
      ggplot2::scale_x_continuous(
        limits = c(start_year, end_year),
        breaks = seq(start_year, end_year, by = max(1, floor(n_years / n_breaks)))
      ) +
      ggplot2::scale_color_identity() +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
          angle = 90, vjust = 0.5, hjust = 1, size = 12
        ),
        axis.text.y = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 14)
      ) +
      ggplot2::labs(x = "Year", y = y_label_diff)
    
    # Combine plots using plotly with error handling
    tryCatch({
      plotly::subplot(
        plotly::ggplotly(p2),
        plotly::ggplotly(p1),
        nrows = 2,
        shareX = TRUE,
        titleY = TRUE
      )
    }, error = function(e) {
      stop("Error creating plotly subplot: ", e$message, call. = FALSE)
    })
    
  }, error = function(e) {
    stop("Error in indexes_plots for group ", group_name, ": ", e$message, call. = FALSE)
  })
}
