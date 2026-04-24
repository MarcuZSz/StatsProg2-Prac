# Exercise 1

library(stringr)

string_starts_with <- function(string, prefix) {
  str_sub(string, 1, str_length(prefix)) == prefix
}

repeat_to_match_length <- function(x, y) {
  rep(y, length.out = length(x))
}

# Exercise 2 + 3 

library(palmerpenguins)
library(tidyverse)

filter_penguin_species <- function(data, species_name) {
  data %>%
    filter(species == species_name)
}

make_penguin_scatter <- function(data, x_var, y_var, title) {
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point() +
    labs(
      title = title,
      x = x_var,
      y = y_var
    )
}

plot_penguin_measurements <- function(data,
                                      species_name,
                                      x_var,
                                      y_var,
                                      title = NULL) {
  valid_species <- c("Adelie", "Chinstrap", "Gentoo")
  
  if (!species_name %in% valid_species) {
    stop(
      paste0(
        "`species_name` must be one of: ",
        paste(valid_species, collapse = ", "),
        ". You supplied: ", species_name
      ),
      call. = FALSE
    )
  }
  
  if (!x_var %in% names(data)) {
    stop(
      paste0(
        "`x_var` must be a column in `data`. You supplied: ", x_var
      ),
      call. = FALSE
    )
  }
  
  if (!y_var %in% names(data)) {
    stop(
      paste0(
        "`y_var` must be a column in `data`. You supplied: ", y_var
      ),
      call. = FALSE
    )
  }
  
  if (!is.numeric(data[[x_var]])) {
    stop(
      paste0(
        "`x_var` must refer to a numeric column. `", x_var, "` is not numeric."
      ),
      call. = FALSE
    )
  }
  
  if (!is.numeric(data[[y_var]])) {
    stop(
      paste0(
        "`y_var` must refer to a numeric column. `", y_var, "` is not numeric."
      ),
      call. = FALSE
    )
  }
  
  filtered_data <- filter_penguin_species(data, species_name)
  
  if (is.null(title)) {
    title <- paste("Penguin plot for", species_name)
  }
  
  make_penguin_scatter(
    data = filtered_data,
    x_var = x_var,
    y_var = y_var,
    title = title
  )
}

plot_penguin_measurements(
  data = penguins,
  species_name = "Adelie",
  x_var = "bill_length_mm",
  y_var = "bill_depth_mm",
  title = "Bill Length vs Bill Depth for Adelie Penguins"
)

plot_penguin_measurements(
  data = penguins,
  species_name = "Gentoo",
  x_var = "flipper_length_mm",
  y_var = "body_mass_g",
  title = "Flipper Length vs Body Mass for Gentoo Penguins"
)

# Error testing

plot_penguin_measurements(
  data = penguins,
  species_name = "Penguin",
  x_var = "bill_length_mm",
  y_var = "bill_depth_mm"
)

plot_penguin_measurements(
  data = penguins,
  species_name = "Adelie",
  x_var = "fluffiness",
  y_var = "bill_depth_mm"
)

plot_penguin_measurements(
  data = penguins,
  species_name = "Adelie",
  x_var = "island",
  y_var = "bill_depth_mm"
)

# Exercise 4

fibonacci_iterative <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || n < 0 || n != as.integer(n)) {
    stop("`n` must be a single non-negative integer.", call. = FALSE)
  }
  
  if (n == 0) return(0)
  if (n == 1) return(1)
  
  a <- 0
  b <- 1
  
  for (i in 2:n) {
    temp <- a + b
    a <- b
    b <- temp
  }
  
  b
}

fibonacci_recursive <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || n < 0 || n != as.integer(n)) {
    stop("`n` must be a single non-negative integer.", call. = FALSE)
  }
  
  if (n == 0) return(0)
  if (n == 1) return(1)
  
  fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)
}

fibonacci_iterative(30)
fibonacci_recursive(30)

system.time(replicate(1000, fibonacci_iterative(30)))
system.time(replicate(100, fibonacci_recursive(30)))

# Exercise 5

# Task 1

summarise_species <- function(species_name, data = penguins) {
  species_data <- data[data$species == species_name, ]
  
  if (nrow(species_data) == 0) {
    stop(
      paste0("`species_name` must be one of: ",
             paste(unique(data$species), collapse = ", "),
             ". You supplied: ", species_name),
      call. = FALSE
    )
  }
  
  mean_bill <- mean(species_data$bill_length_mm, na.rm = TRUE)
  mean_mass <- mean(species_data$body_mass_g, na.rm = TRUE)
  
  cat(species_name, "— mean bill:", mean_bill,
      "mean mass:", mean_mass, "\n")
}

# Task 2

load_penguin_data <- function() {
  penguins
}

clean_penguin_data <- function(data) {
  data[!is.na(data$bill_length_mm) & !is.na(data$body_mass_g), ]
}

filter_penguin_species <- function(data, species_name) {
  filtered_data <- data[data$species == species_name, ]
  
  if (nrow(filtered_data) == 0) {
    stop(
      paste0("`species_name` must be one of: ",
             paste(unique(data$species), collapse = ", "),
             ". You supplied: ", species_name),
      call. = FALSE
    )
  }
  
  filtered_data
}

summarise_penguin_data <- function(data) {
  cat("n =", nrow(data), "\n")
  cat("mean bill length:", mean(data$bill_length_mm), "\n")
  cat("mean body mass:", mean(data$body_mass_g), "\n")
}

plot_penguin_data <- function(data, species_name) {
  ggplot(data, aes(x = bill_length_mm, y = body_mass_g)) +
    geom_point() +
    labs(title = paste("Bill length vs. body mass —", species_name))
}

penguin_report <- function(species_name) {
  data <- load_penguin_data()
  data <- clean_penguin_data(data)
  data <- filter_penguin_species(data, species_name)
  
  summarise_penguin_data(data)
  plot_penguin_data(data, species_name)
}

penguin_report("Gentoo")

# Exercise 6 (skip reflection log)

# Exercise 7

library(dplyr)

validate_starwars_inputs <- function(data,
                                     plot_type,
                                     x_var,
                                     y_var = NULL,
                                     group_var = NULL) {
  plot_type <- match.arg(plot_type, c("histogram", "scatter", "bar"))
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }
  
  if (nrow(data) == 0) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }
  
  required_cols <- c(x_var, y_var, group_var)
  required_cols <- required_cols[!is.null(required_cols)]
  
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "These columns are missing from `data`: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  if (!is.numeric(data[[x_var]])) {
    stop(
      paste0("`x_var` must refer to a numeric column. `", x_var, "` is not numeric."),
      call. = FALSE
    )
  }
  
  if (plot_type == "scatter") {
    if (is.null(y_var)) {
      stop("For `plot_type = \"scatter\"`, you must supply `y_var`.", call. = FALSE)
    }
    
    if (!is.numeric(data[[y_var]])) {
      stop(
        paste0("`y_var` must refer to a numeric column. `", y_var, "` is not numeric."),
        call. = FALSE
      )
    }
  }
  
  if (plot_type == "bar" && is.null(group_var)) {
    stop("For `plot_type = \"bar\"`, you must supply `group_var`.", call. = FALSE)
  }
  
  plot_type
}

clean_starwars_data <- function(data,
                                x_var,
                                y_var = NULL,
                                group_var = NULL) {
  required_cols <- c(x_var, y_var, group_var)
  required_cols <- required_cols[!is.null(required_cols)]
  
  cleaned_data <- data %>%
    filter(if_all(all_of(required_cols), ~ !is.na(.x)))
  
  if (nrow(cleaned_data) == 0) {
    stop("No rows remain after removing missing values.", call. = FALSE)
  }
  
  cleaned_data
}

summarise_starwars_groups <- function(data,
                                      group_var,
                                      value_var,
                                      top_n = 10) {
  data %>%
    group_by(group = .data[[group_var]]) %>%
    summarise(
      n = n(),
      mean_value = mean(.data[[value_var]], na.rm = TRUE),
      median_value = median(.data[[value_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(n)) %>%
    slice_head(n = top_n)
}

plot_starwars_data <- function(data,
                               plot_type,
                               x_var,
                               y_var = NULL,
                               group_var = NULL,
                               top_n = 10) {
  plot_type <- match.arg(plot_type, c("histogram", "scatter", "bar"))
  
  if (plot_type == "histogram") {
    return(
      ggplot(data, aes(x = .data[[x_var]])) +
        geom_histogram(bins = 30, fill = "steelblue", color = "white") +
        labs(
          title = paste("Distribution of", x_var),
          x = x_var,
          y = "Count"
        )
    )
  }
  
  if (plot_type == "scatter") {
    if (is.null(group_var)) {
      return(
        ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
          geom_point(alpha = 0.8) +
          labs(
            title = paste(x_var, "vs", y_var),
            x = x_var,
            y = y_var
          )
      )
    }
    
    return(
      ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], color = .data[[group_var]])) +
        geom_point(alpha = 0.8) +
        labs(
          title = paste(x_var, "vs", y_var, "by", group_var),
          x = x_var,
          y = y_var,
          color = group_var
        )
    )
  }
  
  summary_data <- summarise_starwars_groups(
    data = data,
    group_var = group_var,
    value_var = x_var,
    top_n = top_n
  )
  
  ggplot(summary_data, aes(x = reorder(group, mean_value), y = mean_value)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = paste("Mean", x_var, "by", group_var),
      x = group_var,
      y = paste("Mean", x_var)
    )
}

starwars_report <- function(data = dplyr::starwars,
                            plot_type = c("histogram", "scatter", "bar"),
                            x_var,
                            y_var = NULL,
                            group_var = NULL,
                            top_n = 10) {
  plot_type <- validate_starwars_inputs(
    data = data,
    plot_type = match.arg(plot_type),
    x_var = x_var,
    y_var = y_var,
    group_var = group_var
  )
  
  cleaned_data <- clean_starwars_data(
    data = data,
    x_var = x_var,
    y_var = y_var,
    group_var = group_var
  )
  
  summary_data <- NULL
  if (!is.null(group_var)) {
    summary_data <- summarise_starwars_groups(
      data = cleaned_data,
      group_var = group_var,
      value_var = x_var,
      top_n = top_n
    )
  }
  
  plot_obj <- plot_starwars_data(
    data = cleaned_data,
    plot_type = plot_type,
    x_var = x_var,
    y_var = y_var,
    group_var = group_var,
    top_n = top_n
  )
  
  list(
    clean_data = cleaned_data,
    summary = summary_data,
    plot = plot_obj
  )
}

# plots

report_bar <- starwars_report(
  data = dplyr::starwars,
  plot_type = "bar",
  x_var = "mass",
  group_var = "species",
  top_n = 8
)

report_bar$summary
report_bar$plot


report_hist <- starwars_report(
  data = dplyr::starwars,
  plot_type = "histogram",
  x_var = "height"
)

report_hist$plot

report_scatter <- starwars_report(
  data = dplyr::starwars,
  plot_type = "scatter",
  x_var = "height",
  y_var = "mass",
  group_var = "gender"
)

report_scatter$plot

# edge cases

starwars_report(
  data = dplyr::starwars,
  plot_type = "histogram",
  x_var = "midichlorians"
)

starwars_report(
  data = dplyr::starwars,
  plot_type = "histogram",
  x_var = "species"
)

starwars_report(
  data = dplyr::starwars[0, ],
  plot_type = "histogram",
  x_var = "height"
)
