#' Calculate and Visualize Group Attributes from Scientific Networks
#'
#' This function analyzes publication growth rates and other attributes for research groups
#' identified in scientific networks. It calculates growth rates using exponential models,
#' creates horizon plots for visualization, and generates summary tables.
#'
#' @param groups A list containing network data with publications by year and group information.
#'        Must include elements: `network`, `pubs_by_year`, and `aggregate`.
#' @param growth_rate_period Numeric vector of years to use for growth rate calculation
#'        (default: 2010:2024).
#' @param horizon_plot Logical indicating whether to include horizon plots in the output table
#'        (default: TRUE).
#' @param show_results Logical indicating whether to print results to console (default: TRUE).
#' @param assign_result Character string specifying a variable name to assign the results to
#'        in the global environment (default: NULL).
#'
#' @return A list with two components:
#' \itemize{
#'   \item `attributes_table`: A gt table showing group attributes including growth rates
#'   \item `regression`: A list of model summaries for each group's growth rate calculation
#' }
#'
#' @details The function performs the following steps:
#' 1. Calculates growth rates using exponential models for each group
#' 2. Processes publication age and doubling time metrics
#' 3. Optionally creates horizon plots for each group's publication trend
#' 4. Generates a comprehensive summary table
#'
#' @examples
#' \dontrun{
#' # Assuming groups is output from sniff_groups()
#' groups_attributes <- sniff_groups_attributes(groups,
#'   growth_rate_period = 2010:2022,
#'   horizon_plot = TRUE
#' )
#'
#' # View the results table
#' print(groups_attributes$attributes_table)
#'
#' # Access model summaries
#' groups_attributes$regression
#' }
#'
#' @importFrom dplyr filter arrange select mutate left_join rename pull slice_head
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom ggplot2 ggplot aes scale_x_continuous scale_y_continuous
#' @importFrom stats lm nls coef
#' @export
sniff_groups_attributes <- function(groups,
                                    growth_rate_period = 2010:2022,
                                    horizon_plot = TRUE,
                                    show_results = TRUE,
                                    assign_result = NULL) {
  # Input validation
  if (!is.list(groups)) {
    stop("groups must be a list containing network data")
  }

  required_components <- c("network", "pubs_by_year", "aggregate")
  if (!all(required_components %in% names(groups))) {
    stop(paste("groups must contain:", paste(required_components, collapse = ", ")))
  }

  if (!is.numeric(growth_rate_period) || length(growth_rate_period) < 2) {
    stop("growth_rate_period must be a numeric vector with at least 2 years")
  }

  groups$network |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble() |>
    dplyr::select(DB) |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull() |>
    {
      \(x) gsub("_.*$", "", x)
    }() ->
    DB

  switch(DB,
    "wos" = "Web of Science",
    "openalex" = "OpenAlex",
    DB
  ) ->
    DB

  groups$pubs_by_year$year <- as.numeric(groups$pubs_by_year$year)

  groups_growth <- calculate_growth_rates(groups$pubs_by_year, growth_rate_period)

  data.frame(coefficient = groups_growth$growth_rates) |>
    tibble::rownames_to_column(var = "group") |>
    tibble::as_tibble() ->
    groups_growth_df

  groups$aggregate |>
    dplyr::mutate(average_age = paste0(floor(average_age), "+", round(average_age %% 1 * 12, 0), "m")) |>
    dplyr::left_join(groups_growth_df, by = "group") |>
    dplyr::rename(publications = quantity_papers) |>
    dplyr::mutate(
      growth_rate_percentage_year = (exp(coefficient) - 1) * 100,
      doubling_time_years = ifelse(growth_rate_percentage_year <= 0, NA, log(2) / coefficient)
    ) |>
    dplyr::mutate(coefficient = round(coefficient, digits = 4)) |>
    dplyr::mutate(growth_rate_percentage_year = round(growth_rate_percentage_year, digits = 1)) |>
    dplyr::mutate(doubling_time_years = round(doubling_time_years, digits = 1)) ->
    growth_rate_data

  groups$pubs_by_year |>
    dplyr::filter(!is.na(group)) |>
    dplyr::arrange(group, year) |>
    dplyr::filter(publications < min(growth_rate_period)) ->
    growth_rate_horizon

  # plot_horizon_group(growth_rate_horizon, group_name = 'g01')

  growth_rate_data |>
    dplyr::mutate(y = paste0(round(doubling_time_years, 0), "y")) |>
    dplyr::mutate(m = paste0(round((doubling_time_years %% 1) * 12, 0), "m")) |>
    dplyr::mutate(doubling_time = paste0(y, "+", ifelse(m != "0m", m, ""))) |>
    dplyr::mutate(doubling_time = stringr::str_squish(doubling_time)) |>
    dplyr::mutate(doubling_time = gsub("\\+$", "", doubling_time)) |>
    dplyr::select(group, publications, average_age, growth_rate_percentage_year, doubling_time) |>
    dplyr::mutate(horizon = group) ->
    growth_rate_data_gt

  if (horizon_plot == TRUE) {
    growth_rate_data_gt |>
      gt::gt() |>
      gt::tab_header(title = "Groups Attributes") |>
      gt::cols_label(
        group = gt::md("Group"),
        publications = gt::md("Publications"),
        average_age = gt::md("Average age"),
        growth_rate_percentage_year = gt::md("Growth rate"),
        doubling_time = gt::md("Doubling time"),
        horizon = gt::md("Horizon plot")
      ) |>
      gt::text_transform(
        locations = gt::cells_body(columns = "horizon"),
        fn = function(column) {
          column |>
            purrr::map(\(x) plot_horizon_group(growth_rate_horizon, group_name = x, year_range = c(min(growth_rate_period), max(growth_rate_period)))) |>
            gt::ggplot_image(aspect_ratio = 3)
        }
      ) |>
      gt::tab_source_note(source_note = gt::md(paste0("**Source**: ", DB, ". Data extracted, organized and estimated by the authors."))) |>
      gt::tab_footnote(footnote = "Average publication year: For example, '2016+7m' means that the articles were published, on average, in 2016 plus seven months.", locations = gt::cells_column_labels(columns = average_age)) |>
      gt::tab_footnote(footnote = "y = years, m = months. Calculated by ln(2)/b1 where b1 is the econometric model coefficient.", locations = gt::cells_column_labels(columns = doubling_time)) |>
      gt::tab_footnote(footnote = paste0("Growth rate percentage year. Calculated by exp(b1)-1 where b1 is the econometric model coefficient. Time span, ", min(growth_rate_period), " until ", max(growth_rate_period), "."), locations = gt::cells_column_labels(columns = growth_rate_percentage_year)) |>
      gt::tab_footnote(footnote = paste0("Publications between ", min(growth_rate_period), " and ", max(growth_rate_period), ". Chart type horizon plot."), locations = gt::cells_column_labels(columns = horizon)) ->
      attributes_gt
  } else {
    growth_rate_data_gt |>
      dplyr::select(-horizon) |>
      gt::gt() |>
      gt::tab_header(title = "Groups Attributes") |>
      gt::cols_label(
        group = gt::md("Group"),
        publications = gt::md("Publications"),
        average_age = gt::md("Average age"),
        growth_rate_percentage_year = gt::md("Growth rate"),
        doubling_time = gt::md("Doubling time"),
      ) |>
      gt::tab_source_note(source_note = gt::md(paste0("**Source**: ", DB, ". Data extracted, organized and estimated by the authors."))) |>
      gt::tab_footnote(footnote = "Average publication year: For example, '2016+7m' means that the articles were published, on average, in 2016 plus seven months.", locations = gt::cells_column_labels(columns = average_age)) |>
      gt::tab_footnote(footnote = "y = years, m = months. Calculated by ln(2)/b1 where b1 is the econometric model coefficient.", locations = gt::cells_column_labels(columns = doubling_time)) |>
      gt::tab_footnote(footnote = paste0("Growth rate percentage year. Calculated by exp(b1)-1 where b1 is the econometric model coefficient. Time span, ", min(growth_rate_period), " until ", max(growth_rate_period), "."), locations = gt::cells_column_labels(columns = growth_rate_percentage_year)) ->
      attributes_gt
  }

  result <- list(
    attributes_table = attributes_gt,
    regression = groups_growth$model_summary
  )

  if (!is.null(assign_result)) {
    assign(x = assign_result, value = result$attributes_table, envir = parent.frame())
  }

  result
}

#' Calculate Exponential Growth Rates for Research Groups
#'
#' @description
#' Internal function to calculate exponential growth rates of publications for different research groups.
#' Fits nonlinear growth models to publication time series data.
#'
#' @param pubs_by_year A data frame containing publication counts by year and group.
#' Must contain the columns: `group`, `year`, and `publications`.
#' @param growth_rate_period Numeric vector specifying the years to include in growth rate calculation.
#' Must contain at least 2 years.
#'
#' @return A list with four components:
#' \describe{
#'   \item{growth_rates}{Named vector of growth rate coefficients (b1) for each group}
#'   \item{failures}{Character vector of groups where model fitting failed}
#'   \item{success_rate}{Proportion of groups successfully modeled}
#'   \item{model_summary}{List of model summaries for each group}
#' }
#'
#' @details
#' This internal function:
#' \enumerate{
#'   \item Validates input data structure
#'   \item Filters data for specified growth period
#'   \item Fits exponential growth models (publications ~ b0*exp(b1*year)) to each group's data
#'   \item Handles modeling failures gracefully
#'   \item Returns growth rates and model diagnostics
#' }
#'
#' The function first fits a linear model to log-transformed data to get starting values,
#' then fits a nonlinear exponential model. For groups with insufficient data or modeling
#' problems, NA values are returned.
#'
#' @keywords internal
#' @noRd
calculate_growth_rates <- function(pubs_by_year, growth_rate_period) {

  # pubs_by_year = groups$pubs_by_year
  # growth_rate_period = growth_rate_period

  # Input validation
  if (!is.data.frame(pubs_by_year)) {
    stop("groups_growth must be a data frame")
  }

  required_cols <- c("group", "year", "publications")
  if (!all(required_cols %in% names(pubs_by_year))) {
    stop(paste("groups_growth must contain columns:", paste(required_cols, collapse = ", ")))
  }

  if (!is.numeric(growth_rate_period) || length(growth_rate_period) < 2) {
    stop("growth_rate_period must be a numeric vector with at least 2 years")
  }

  # Initialize results
  grupos <- cgn(sort(unique(pubs_by_year$group)))
  res <- setNames(rep(NA_real_, length(grupos)), grupos)
  m2_summary <- setNames(rep(NA_real_, length(grupos)), grupos)
  model_failures <- character(0)

  # Check if there's enough data to model
  if (nrow(pubs_by_year) == 0 || length(grupos) == 0) {
    warning("No data available for modeling")
    return(list(growth_rates = res, failures = model_failures))
  }

  for (i in seq_along(grupos)) {
    current_group <- grupos[i]

    tryCatch(
      {
        # Prepare data for current group
        pubs_by_year |>
          dplyr::filter(group == current_group) |>
          dplyr::arrange(year) |>
          dplyr::filter(year %in% growth_rate_period) ->
          d

        # Check if we have enough data points
        if (nrow(d) < 3) {
          stop("Insufficient data points (need at least 3 years)")
        }

        # Check for zero publications which would break log()
        if (any(d$publications <= 0)) {
          stop("Cannot handle zero or negative publication counts")
        }

        d |>
          dplyr::mutate(
            trend = 1:dplyr::n(),
            lnp = log(publications)
          ) ->
          d

        # Linear model for initial parameters
        m1 <- lm(lnp ~ trend, data = d)
        beta0 <- m1$coefficients[[1]]
        beta1 <- m1$coefficients[[2]]

        # growth_rate_period

        # Nonlinear model with multiple fallbacks
        m2 <- tryCatch(
          {
            stats::nls(publications ~ b0 * exp(b1 * (year - min(growth_rate_period))),
              start = list(b0 = exp(beta0), b1 = beta1),
              data = d,
              control = stats::nls.control(maxiter = 200, warnOnly = TRUE)
            )
          },
          error = function(e) {
            # Try with different algorithm if first attempt fails
            tryCatch(
              {
                stats::nls(publications ~ b0 * exp(b1 * (year - min(growth_rate_period))),
                  start = list(b0 = mean(d$publications), b1 = 0.1), # More robust starting values
                  data = d,
                  algorithm = "port",
                  control = stats::nls.control(maxiter = 200, warnOnly = TRUE)
                )
              },
              error = function(e) {
                NULL # Return NULL if both attempts fail
              }
            )
          }
        )

        if (is.null(m2)) {
          stop("Both modeling attempts failed")
        }

        res[current_group] <- coef(m2)[["b1"]]
        m2_summary[current_group] <- list(utils::capture.output(summary(m2)))
      },
      error = function(e) {
        warning("Failed to calculate growth rate for group ", current_group, ": ", e$message)
        model_failures <<- c(model_failures, current_group)
      }
    )
  }

  # Return results and information about failures
  list(
    growth_rates = res,
    failures = model_failures,
    success_rate = (length(grupos) - length(model_failures)) / length(grupos),
    model_summary = m2_summary
  )
}

#' Create Horizon Plot for Research Group Publication Trends
#'
#' @description
#' Internal function to generate horizon plots visualizing publication trends for 
#' individual research groups. Horizon plots provide a compact visualization of 
#' time series data by splitting and layering values into colored bands.
#'
#' @param data A data frame containing publication data with columns:
#'   \itemize{
#'     \item `group`: Research group identifier
#'     \item `year`: Publication year
#'     \item `publications`: Number of publications
#'   }
#' @param group_name Character string specifying which group to plot (length 1)
#' @param year_range Numeric vector of length 2 specifying the year range to plot (default: c(2010, 2024))
#' @param palette Character string specifying color palette (default: "BluGrn")
#' @param horizon_scale Numeric value controlling number of horizon bands (default: 5)
#' @param show_x_axis Logical indicating whether to show x-axis labels (default: FALSE)
#' @param base_size Base font size for plot (default: 11)
#' @param use_ggthemes Logical indicating whether to use ggthemes (default: TRUE)
#'
#' @return A ggplot object containing the horizon plot. Returns a "no data available" plot
#' if no data exists for the specified group.
#'
#' @details
#' This internal function:
#' \itemize{
#'   \item Validates input data structure
#'   \item Filters data for the specified group and year range
#'   \item Creates a horizon plot using ggHoriPlot
#'   \item Applies consistent theming
#'   \item Handles missing data gracefully
#' }
#' 
#' The function uses `ggHoriPlot::geom_horizon()` for the visualization and falls back
#' to `ggplot2::theme_minimal()` if ggthemes is not available.
#'
#' @keywords internal
#' @noRd
#' @importFrom ggplot2 ggplot aes geom_blank annotate theme_void theme_minimal
#' @importFrom ggplot2 labs scale_x_continuous scale_y_continuous theme
#' @importFrom ggplot2 element_blank element_text element_line unit rel
#' @importFrom dplyr filter arrange
plot_horizon_group <- function(data,
                               group_name,
                               year_range = c(2010, 2024),
                               palette = "BluGrn",
                               horizon_scale = 5,
                               show_x_axis = FALSE,
                               base_size = 11,
                               use_ggthemes = TRUE) {
  # Input validation
  stopifnot(
    is.data.frame(data),
    "group" %in% names(data),
    "year" %in% names(data),
    "publications" %in% names(data),
    is.character(group_name),
    length(group_name) == 1,
    is.numeric(year_range),
    length(year_range) == 2
  )

  # Filter and prepare data
  data |>
    dplyr::filter(group == group_name, !is.na(year), !is.na(publications)) |>
    dplyr::arrange(year) ->
    plot_data

  if (nrow(plot_data) == 0) {
    warning("No data available for group: ", group_name)
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No data available") +
      ggplot2::theme_void())
  }

  # Create base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = year, y = publications)) +
    ggHoriPlot::geom_horizon(ggplot2::aes(fill = ggplot2::after_stat(Cutpoints)),
    # ggHoriPlot::geom_horizon(ggplot2::aes(fill = ..Cutpoints..),
      origin = "min",
      horizonscale = horizon_scale
    ) +
    ggHoriPlot::scale_fill_hcl(palette = palette, reverse = TRUE) +
    ggplot2::labs(title = paste("Group:", group_name)) +
    ggplot2::scale_x_continuous(
      limits = year_range,
      breaks = seq(year_range[1], year_range[2], by = 5),
      expand = c(0, 0.4)
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0))

  # Apply theme
  if (use_ggthemes && requireNamespace("ggthemes", quietly = TRUE)) {
    p <- p + ggthemes::theme_few(base_size = base_size)
  } else {
    p <- p + ggplot2::theme_minimal(base_size = base_size)
  }

  # Common theme elements
  p <- p + ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    legend.position = "none",
    plot.title = ggplot2::element_text(size = ggplot2::rel(0.9), hjust = 0.5),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank()
  )

  # X-axis customization
  if (show_x_axis) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1,
        size = ggplot2::rel(0.8)
      ),
      axis.ticks.x = ggplot2::element_line(),
      panel.spacing.y = ggplot2::unit(0, "lines")
    )
  } else {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank()
    )
  }

  return(p)
}
