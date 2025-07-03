#' Plot decomposition of time series data
#'
#' This function generates a plot of the decomposition of time series data into trend,
#' seasonal, and irregular components. Supporting either classical or STL decomposition.
#'
#' @param obj A decompose table object from the feasts library
#' @param var The series variable name (e.g., "trend", "seasonal", etc.)
#' @param outliers Logical; should outliers be shown in the plot? Default is FALSE
#' @return A plotly subplot of the decomposition components


plot_decomposition <- function(obj, var, outliers = FALSE) {
    d <- obj
    obj_attr <- attributes(obj)
    intervals <- unlist(obj_attr$interval)
    interval <- names(which(intervals == 1))
    index <- as.character(obj_attr$index)
    if (interval %in% c("week", "month", "quarter")) {
        d$date <- as.Date(d[[index]])
    } else {
        d$date <- d[[index]]
    }

    color <- "#0072B5"



    if (outliers) {
        if (obj_attr$method == "STL") {
            sdv <- sd(d$remainder)

            d$sd3 <- ifelse(d$remainder >= 3 * sdv | d$remainder <= -3 * sdv, d[[var]], NA)
            d$sd2 <- ifelse(d$remainder >= 2 * sdv & d$remainder < 3 * sdv | d$remainder <= -2 * sdv & d$remainder > -3 * sdv, d[[var]], NA)
        } else {
            sdv <- sd(d$random, na.rm = TRUE)

            d$sd3 <- ifelse(d$random >= 3 * sdv | d$random <= -3 * sdv, d[[var]], NA)
            d$ sd2 <- ifelse(d$random >= 2 * sdv & d$random < 3 * sdv | d$random <= -2 * sdv & d$random > -3 * sdv, d[[var]], NA)
        }
    }


    series <- d |>
        plotly::plot_ly(x = ~date, y = ~ get(var), type = "scatter", mode = "lines", line = list(color = color), name = "Actual", showlegend = FALSE) |>
        plotly::layout(yaxis = list(title = "Actial"))


    trend <- d |>
        plotly::plot_ly(x = ~date, y = ~trend, type = "scatter", mode = "lines", line = list(color = color), name = "Trend", showlegend = FALSE) |>
        plotly::layout(yaxis = list(title = "Trend"))


    if (obj_attr$method == "STL") {
        if (interval != "year") {
            seasonal <- d |>
                plotly::plot_ly(x = ~date, y = ~season_year, type = "scatter", mode = "lines", line = list(color = color), name = "Seasonal", showlegend = FALSE) |>
                plotly::layout(yaxis = list(title = "Seasonal"))
        } else {
            seasonal <- NULL
        }

        seasonal_adj <- d |>
            plotly::plot_ly(x = ~date, y = ~season_adjust, type = "scatter", mode = "lines", line = list(color = color), name = "Seasonal Adjusted", showlegend = FALSE) |>
            plotly::layout(yaxis = list(title = "Seasonal Adjusted"))

        irregular <- d |> plotly::plot_ly(
            x = ~date, y = ~remainder,
            type = "scatter", mode = "lines",
            line = list(color = color), name = "Irregular", showlegend = FALSE
        )
    } else {
        if (interval != "year") {
            seasonal <- d |>
                plotly::plot_ly(x = ~date, y = ~seasonal, type = "scatter", mode = "lines", line = list(color = color), name = "Seasonal", showlegend = FALSE) |>
                plotly::layout(yaxis = list(title = "Seasonal"))
        } else {
            seasonal <- NULL
        }

        seasonal_adj <- d |>
            plotly::plot_ly(x = ~date, y = ~season_adjust, type = "scatter", mode = "lines", line = list(color = color), name = "Seasonal Adjusted", showlegend = FALSE) |>
            plotly::layout(yaxis = list(title = "Seasonal Adjusted"))

        irregular <- d |>
            plotly::plot_ly(
                x = ~date, y = ~random,
                type = "scatter", mode = "lines",
                line = list(color = color), name = "Irregular", showlegend = FALSE
            ) |>
            plotly::layout(yaxis = list(title = "Irregular"))
    }


    if (outliers) {
        series <- series |>
            plotly::add_trace(x = ~date, y = ~sd2, marker = list(color = "orange")) |>
            plotly::add_trace(x = ~date, y = ~sd3, marker = list(color = "red"))
        irregular <- irregular |>
            plotly::add_segments(
                x = min(d$date),
                xend = max(d$date),
                y = 2 * sdv,
                yend = 2 * sdv,
                name = "2SD",
                line = list(color = "orange", dash = "dash")
            ) |>
            plotly::add_segments(
                x = min(d$date),
                xend = max(d$date),
                y = -2 * sdv,
                yend = -2 * sdv,
                name = "-2SD",
                line = list(color = "orange", dash = "dash")
            ) |>
            plotly::add_segments(
                x = min(d$date),
                xend = max(d$date),
                y = 3 * sdv,
                yend = 3 * sdv,
                name = "3SD",
                line = list(color = "red", dash = "dash")
            ) |>
            plotly::add_segments(
                x = min(d$date),
                xend = max(d$date),
                y = -3 * sdv,
                yend = -3 * sdv,
                name = "-3SD",
                line = list(color = "red", dash = "dash")
            ) |>
            plotly::layout(yaxis = list(title = "Irregular"))
    }
    if (is.null(seasonal)) {
        p <- plotly::subplot(series, trend, seasonal_adj, irregular,
            nrows = 4, titleY = TRUE, shareX = TRUE
        )
    } else {
        p <- plotly::subplot(series, trend, seasonal, seasonal_adj, irregular,
            nrows = 5, titleY = TRUE, shareX = TRUE
        )
    }
    capitalize_first <- function(word) {
        if (!is.character(word) || length(word) != 1) {
            stop("Input must be a single character string")
        }
        return(paste0(toupper(substr(word, 1, 1)), tolower(substr(word, 2, nchar(word)))))
    }

    p <- p |>
        plotly::layout(xaxis = list(title = paste("Decomposition Method: ",
            obj_attr$method,
            "; Frequency: ",
            capitalize_first(interval),
            sep = ""
        )))

    return(p)
}


#' Compute and plot ACF of a time series
#'
#' @param ts Time series data
#' @param var Variable name to be plotted
#' @param lag_max Maximum number of lags
#' @param frequency Frequency at which seasonal component is expected (default = NULL)
#'
#' @return A plotly object

plot_acf <- function(ts, var, lag_max, frequency, alpha = 0.05) {
    a <- ts |> feasts::ACF(!!rlang::sym(var), lag_max = lag_max)
    color <- "#0072B5"
    pi_upper <- qnorm(1 - alpha / 2) / sqrt(nrow(ts))
    pi_upper
    p <- plotly::plot_ly(type = "bar")

    if (!is.null(frequency)) {
        s <- seq(from = frequency, by = frequency, to = nrow(a))
        a$seasonal <- NA
        a$non_seasonal <- a$acf
        a$non_seasonal[s] <- NA
        a$seasonal[s] <- a$acf[s]

        p <- p |>
            plotly::add_trace(x = a$lag, y = a$non_seasonal, name = "Non-seasonal", marker = list(
                color = color,
                line = list(
                    color = "rgb(8,48,107)",
                    width = 1.5
                )
            )) |>
            plotly::add_trace(x = a$lag, y = a$seasonal, name = "Seasonal", marker = list(color = "red", line = list(
                color = "rgb(8,48,107)",
                width = 1.5
            )))
    } else {
        p <- p |> plotly::add_trace(x = a$lag, y = a$acf, name = "Lags", marker = list(
            color = color,
            line = list(
                color = "rgb(8,48,107)",
                width = 1.5
            )
        ))
    }

    p <- p |>
        plotly::layout("ACF Plot", yaxis = list(title = "ACF"), xaxis = list(title = "Lags")) |>
        plotly::add_segments(x = ~ min(a$lag), xend = ~ max(a$lag), y = pi_upper, yend = pi_upper, line = list(color = "black", dash = "dash"), name = "95% CI", showlegend = TRUE, legendgroup = "ci") |>
        plotly::add_segments(x = ~ min(a$lag), xend = ~ max(a$lag), y = -pi_upper, yend = -pi_upper, line = list(color = "black", dash = "dash"), name = "95% CI", showlegend = FALSE, legendgroup = "ci")


    return(p)
}



#' Plot a time series against its lagged value with regression line and metrics
#'
#' @param ts A data frame containing a single time series column.
#' @param var The name of the variable to plot.
#' @param lag The number of lags to consider.
#'
#' @return A `plotly` object showing the relationship between the original
#'         variable and its lagged value, along with a regression line and metrics.
#'

plot_lag <- function(ts, var, lag) {
    d <- ts |>
        dplyr::mutate(lag = dplyr::lag(x = !!rlang::sym(var), n = lag))

    # Create the regression formula
    formula <- as.formula(paste(var, "~ lag"))

    # Fit the linear model
    model <- lm(formula, data = d)

    # Extract model coefficients
    intercept <- coef(model)[1]
    slope <- coef(model)[2]

    # Format regression formula text
    reg_formula <- paste0(
        "y = ", round(intercept, 2),
        ifelse(slope < 0, " - ", " + "),
        abs(round(slope, 2)), paste("*lag", lag, sep = "")
    )

    # Get adjusted R-squared
    adj_r2 <- summary(model)$adj.r.squared
    adj_r2_label <- paste0("Adjusted R² = ", round(adj_r2, 3))

    # Add predicted values to data
    d$predicted <- predict(model, newdata = d)

    # Create plot
    p <- plot_ly(d,
        x = ~lag, y = ~ get(var), type = "scatter", mode = "markers",
        name = "Actual"
    ) %>%
        add_lines(
            x = ~lag, y = ~predicted, name = "Regression Fitted Line",
            line = list(color = "red", dash = "dash")
        ) %>%
        layout(
            title = paste(var, "vs Lag", lag, sep = " "),
            xaxis = list(title = paste("Lag", lag, sep = " ")),
            yaxis = list(title = var),
            annotations = list(
                list(
                    x = 0.05, y = 0.95, xref = "paper", yref = "paper",
                    text = reg_formula,
                    showarrow = FALSE,
                    font = list(size = 12)
                ),
                list(
                    x = 0.05, y = 0.88, xref = "paper", yref = "paper",
                    text = adj_r2_label,
                    showarrow = FALSE,
                    font = list(size = 12)
                )
            )
        )

    return(p)
}
