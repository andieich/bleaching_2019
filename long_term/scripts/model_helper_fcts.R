# check how much of distribution is above 0
above_0 <- function(x) {
  length(which(x > 0)) / length(x)
}

#get summary
brms_summary <- function(x) {
  posterior::summarise_draws(
    x,
    "mean",
    "median",
    "sd",
    ~ quantile(.x, probs = c(0.025, 0.975))
  )
}

#make colours less intense
make_pastel <- function(cols, n = 0.4) {
  names <- names(cols)
  cols <- col2rgb(cols)
  # transform to HSV space
  cols <- rgb2hsv(cols)
  cols <- hsv(cols[1, ], cols[2, ] * n, cols[3, ])
  names(cols) <- names
  return(cols)
}

# colours
top_3_corals <- c("Pocillopora", "Montipora", "Acropora")
cols_genera <- c(brewer.pal(11, "Set3")[c(1, 3, 5)])
names(cols_genera) <- c(top_3_corals)


# theme for ggplots
theme_andi <- function() {
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),
    axis.line = element_line(colour = "black")
  )
}


# predict data from model to calculate difference in coral cover between two years
predict_before_after <- function(.model, .data, .get_summary = FALSE, ...) {
  # Get a list of quoted dots
  group_vars <- enquos(...)

  # get years
  years <- .data$year %>% unique()

  # check if only 2 year in data
  if (length(years) != 2) {
    stop(paste("Only two years allowed but", length(years != 2), "provided"))
  }

  year_1 <- sym(as.character(years[1]))
  year_2 <- sym(as.character(years[2]))

  #make new df
  new_data <- .data %>%
    dplyr::select(!!!group_vars, year) %>%
    distinct() %>%
    mutate(row = 1:n())

  # predict data

  post_pred <- fitted(.model, newdata = new_data, summary = F) %>%
    # convert the results to a data frame
    data.frame() %>%
    # rename the columns
    set_names(pull(new_data, row)) %>%
    # add a numeric index for the MCMC draws
    mutate(draw = 1:n()) %>%
    # convert to the long format
    pivot_longer(-draw) %>%
    # convert the row column from the character format to the numeric format
    mutate(row = as.double(name)) %>%
    # join the nd predictor grid to the output
    left_join(new_data, by = "row") %>%
    # drop two of the columns which are now unnecessary
    dplyr::select(-name, -row) %>%
    # convert to a wider format so we can compute the contrast
    pivot_wider(names_from = year, values_from = value) %>%
    # compute the ATE contrast
    mutate(tau = !!year_2 - !!year_1) %>%
    # compute the average ATE value within each MCMC draw
    group_by(draw, !!!group_vars) %>%
    summarise(ate = mean(tau)) %>%
    # remove the draw index column
    ungroup() %>%
    dplyr::select(ate, !!!group_vars)

  if (.get_summary) {
    post_pred <- post_pred %>%
      group_by(!!!group_vars) %>%
      brms_summary()
  }

  return(post_pred)
}

# predict data from model to calculate relative difference in coral cover between two years
predict_before_after_rel <- function(.model, .data, .get_summary = FALSE, ...) {
  # Get a list of quoted dots
  group_vars <- enquos(...)

  # get years
  years <- .data$year %>% unique()

  # check if only 2 year in data
  if (length(years) != 2) {
    stop(paste("Only two years allowed but", length(years != 2), "provided"))
  }

  year_1 <- sym(as.character(years[1]))
  year_2 <- sym(as.character(years[2]))

  #make new df
  new_data <- .data %>%
    dplyr::select(!!!group_vars, year) %>%
    distinct() %>%
    mutate(row = 1:n())

  # predict data

  post_pred <- fitted(.model, newdata = new_data, summary = F) %>%
    # convert the results to a data frame
    data.frame() %>%
    # rename the columns
    set_names(pull(new_data, row)) %>%
    # add a numeric index for the MCMC draws
    mutate(draw = 1:n()) %>%
    # convert to the long format
    pivot_longer(-draw) %>%
    # convert the row column from the character format to the numeric format
    mutate(row = as.double(name)) %>%
    # join the nd predictor grid to the output
    left_join(new_data, by = "row") %>%
    # drop two of the columns which are now unnecessary
    dplyr::select(-name, -row) %>%
    # convert to a wider format so we can compute the contrast
    pivot_wider(names_from = year, values_from = value) %>%
    # compute the ATE contrast
    mutate(rel_change = (!!year_2 - !!year_1) / !!year_1) %>%
    # compute the average ATE value within each MCMC draw
    group_by(draw, !!!group_vars) %>%
    summarise(rel_change = mean(rel_change)) %>%
    # remove the draw index column
    ungroup() %>%
    dplyr::select(rel_change, !!!group_vars)

  if (.get_summary) {
    post_pred <- post_pred %>%
      group_by(!!!group_vars) %>%
      brms_summary()
  }

  return(post_pred)
}
