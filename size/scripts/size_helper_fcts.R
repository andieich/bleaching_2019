#function to read and set up data exported with TagLab.
read_data <- function(files) {
    dat <- read.csv(files) %>%
        mutate(file = basename(files))

    #  Unique genet ID, two clumns per year (ID and area). Since four years are considered, set up names accordingly

    names(dat) <- c(
        "genet_ID",
        "spec_1",
        "ID_1",
        "area_1",
        "spec_2",
        "ID_2",
        "area_2",
        "spec_3",
        "ID_3",
        "area_3",
        "spec_4",
        "ID_4",
        "area_4",
        "file"
    )

    dat <- dat %>%
        #add filename
        mutate(file = substr(file, 1, nchar(file) - 4)) %>%
        #Make uniqe genet ID by adding qudrat ID
        mutate(genet_ID = paste(file, genet_ID, sep = "_")) %>%
        dplyr::select(-file)

    return(dat)
}

# One csv file is exported per quadrat and 20 quadrats and files exist per site. This function combines these data using the read_data() function.

combine_data <- function(path) {
    #Uses read_data() to read and combine all csv files in a folder (path)

    files <- list.files(path, full.names = T, pattern = ".csv")

    data <- do.call(rbind, lapply(files, read_data))

    return(data)
}


# This function extracts information from the "raw" TagLab files. This fucntion uses df containing data for a pair of years (before vs after)
get_infos <- function(data, buffer = 0.1) {
    # gets infos from the TagLab genet tables and assigns categories to the change in area
    # No change (wit a buffer of 10%, i.e.+ 5% area change and - 5% area) ==> No change
    # Area shrinks ==> partial mortality
    # Area increases ==> growth
    # Genet not in second year ==> complete mortality
    # new coral ==> born

    #divide buffer in + and - direction
    thresh <- buffer / 2

    # make new columns for state category and the relative change
    data$state <- NA
    data$rel_change <- NA

    #loop through data to get info
    for (i in 1:nrow(data)) {
        #if genet only in second year = born
        if (data$area_1[i] == 0 & data$area_2[i] > 0) {
            data$state[i] <- "born"
            #In this case, assign genus name also to year 1 althought that colony did not yet exist in the first year. This is done do make the df more consistent and easier plotting
            data$spec_1[i] <- data$spec_2[i]
            data$area_1[i] <- data$area_2[i]

            #if area is 0 in second year abut > 0 before => dead
        } else if (data$area_2[i] == 0 & data$area_1[i] > 0) {
            data$state[i] <- "died"

            # If colony exists in both years
        } else if (data$area_1[i] > 0 & data$area_2[i] > 0) {
            #calculate the relative change compared to the area in the first year
            data$rel_change[i] <- (data$area_2[i] - data$area_1[i]) /
                data$area_1[i]

            # if the relative change is more negative than the defined threshold ==> "shrunk"
            if (data$rel_change[i] < -thresh) {
                data$state[i] <- "shrunk"

                # if it is larger than the threshold ==> "grew"
            } else if (data$rel_change[i] > thresh) {
                data$state[i] <- "grew"

                # if within the threshold = no change
            } else {
                data$state[i] <- "no change"
            }

            # give error if none of the above cases applies
        } else {
            stop("Cannot assign a state!")
        }
    }

    return(data)
}


# The TagLab data contains multiple entries in cells if a genet split into several colonies or if colonies merged. This is the case for the colum conatining the genus name (e.g. "Pocillopora Pocillopora Pocillopora" and for the area, e.g. "2.1 4.3 7.8".) The functions selects only on genus name and sums up the areas. This function is called from another function (combine_multiple_entries()) which loops over the rows in several columns

# Input:
# cell: The cell of a data frame that should be combined
# row_num: The row number, not necessary for manipulations but helps to understand what's going on if there is a problem
#type: either "character" (for genus/species names) or "numeric" for areas.

combine_multiple_entries_cell <- function(cell, row_num, type) {
    # get the cell content and split by empty spaces
    content <- str_split(cell, pattern = "  +") %>%
        unlist()

    # only continue if there the cell contains values/characters
    content <- content[nchar(content) > 0]

    # combine areas
    if (type == "numeric") {
        #set to numeric
        content <- content %>%
            as.numeric()

        # Trigger error if the cell contans non numerical calues (they will be NA after as.numeric())
        if (length(content) > 1 & any(is.na(content))) {
            stop(paste("Non numerical value in row", row_num))

            #otherwise, calculate sum
        } else {
            content <- content %>%
                sum()
        }

        # if cell is character get the unique values
    } else if (type == "character") {
        content <- unique(content)

        # if the cell contains now more than one value, there are different species/genus names in the cell. Trigger a warning
        if (length(content) > 1) {
            warning(paste("There's multiple species names in row", row_num))
        }
    } else {
        #Error if not character or number
        stop("type not defined or not numeric or character")
    }

    # if cell is formatted as character but doenst contain any information, set to NA
    if (identical(nchar(content), integer(0))) {
        content <- NA
    }
    return(content)
}


# This fucntioon loopes over the colums in the df for the genus/species names in both years and areas in both years and uses above combine_multiple_entries_cell() function to combine the information

combine_multiple_entries <- function(data) {
    # calls the combine_multiple_entries_cell() function for each row

    for (row_i in 1:nrow(data)) {
        data$area_1[row_i] <- combine_multiple_entries_cell(
            cell = data$area_1[row_i],
            type = "numeric",
            row_num = row_i
        ) %>%
            as.numeric()

        data$area_2[row_i] <- combine_multiple_entries_cell(
            cell = data$area_2[row_i],
            type = "numeric",
            row_num = row_i
        ) %>%
            as.numeric()

        data$area_3[row_i] <- combine_multiple_entries_cell(
            cell = data$area_3[row_i],
            type = "numeric",
            row_num = row_i
        ) %>%
            as.numeric()

        data$area_4[row_i] <- combine_multiple_entries_cell(
            cell = data$area_4[row_i],
            type = "numeric",
            row_num = row_i
        ) %>%
            as.numeric()

        data$spec_1[row_i] <- combine_multiple_entries_cell(
            cell = data$spec_1[row_i],
            type = "character",
            row_num = row_i
        )

        data$spec_2[row_i] <- combine_multiple_entries_cell(
            cell = data$spec_2[row_i],
            type = "character",
            row_num = row_i
        )

        data$spec_3[row_i] <- combine_multiple_entries_cell(
            cell = data$spec_3[row_i],
            type = "character",
            row_num = row_i
        )
        data$spec_4[row_i] <- combine_multiple_entries_cell(
            cell = data$spec_4[row_i],
            type = "character",
            row_num = row_i
        )
    }

    # make sure areas are set to numeric
    data <- data %>%
        mutate_at(vars(starts_with("area")), as.numeric) %>%
        mutate_at(vars(starts_with("area")), ~ replace(., is.na(.), 0))

    return(data)
}

# gglot theme
theme_andi <- function() {
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.line = element_line(colour = "black")
    )
}


#Function to predict & plot model data

#define colours

cols_state <- c(
    "complete mortality" = "#D55E00",
    "growth" = "#009E73",
    "partial mortality" = "#E69F00",
    "no change" = "grey60"
)

x_seq <- log(c(0, 2, 10, 50, 250, 1000) + 1)
x_seq_trans <- round(exp(x_seq) - 1, 0)

plot_model <- function(data, model, title = "", sep_by_site = T) {
    # To plot raw data as ticks, dummy code states (one column per state, 0/1 for true false), => pivor longer to have one column for state and one for true/false, => select only true (=1)
    dat_dummy <- data %>%
        dplyr::select(spec_1, log_area, state) %>%
        mutate(
            state = recode(
                state,
                "died" = "complete mortality",
                "grew" = "growth",
                "shrunk" = "partial mortality"
            )
        ) %>%
        fastDummies::dummy_cols(
            select_columns = "state",
            remove_selected_columns = T
        ) %>%
        pivot_longer(3:6, names_to = "state", values_to = "dummy") %>%
        mutate(state = gsub("state_", "", state)) %>%
        filter(dummy != 0) %>%
        mutate(area = exp(log_area) - 1)

    #predict data with model along sequence of area

    dat_pred <- model %>%
        epred_draws(
            newdata = expand_grid(
                log_area = seq(0, 7.3, length = 200),
                site = unique(data$site)
            ),
            re_formula = NA
        )

    if (sep_by_site) {
        #summarise predictions
        dat_predS <- dat_pred %>%
            group_by(log_area, site, .category) %>%
            summarise(
                median = median(.epred, na.rm = T),
                l95 = quantile(.epred, 0.025, na.rm = T),
                u95 = quantile(.epred, 0.975, na.rm = T),

                l90 = quantile(.epred, 0.05, na.rm = T),
                u90 = quantile(.epred, 0.95, na.rm = T),

                l80 = quantile(.epred, 0.1, na.rm = T),
                u80 = quantile(.epred, 0.9, na.rm = T),
            ) %>%
            ungroup() %>%
            mutate(area = exp(log_area) - 1)
    } else {
        #summarise predictions
        dat_predS <- dat_pred %>%
            group_by(log_area, .category) %>%
            summarise(
                median = median(.epred, na.rm = T),
                l95 = quantile(.epred, 0.025, na.rm = T),
                u95 = quantile(.epred, 0.975, na.rm = T),

                l90 = quantile(.epred, 0.05, na.rm = T),
                u90 = quantile(.epred, 0.95, na.rm = T),

                l80 = quantile(.epred, 0.1, na.rm = T),
                u80 = quantile(.epred, 0.9, na.rm = T),
            ) %>%
            ungroup() %>%
            mutate(area = exp(log_area) - 1)
    }

    #and plot
    plot <- dat_predS %>%
        mutate(
            .category = recode(
                .category,
                "died" = "complete mortality",
                "grew" = "growth",
                "shrunk" = "partial mortality"
            )
        ) %>%
        ggplot(aes(x = log_area)) +
        geom_hline(yintercept = 0.5, linetype = "11", colour = "grey") +
        geom_ribbon(
            aes(x = log_area, ymin = l95, ymax = u95, fill = .category),
            alpha = .2
        ) +
        geom_ribbon(
            aes(x = log_area, ymin = l90, ymax = u90, fill = .category),
            alpha = .2
        ) +
        geom_ribbon(
            aes(x = log_area, ymin = l80, ymax = u80, fill = .category),
            alpha = .2
        ) +
        geom_line(aes(y = median, col = .category)) +
        geom_point(
            data = dat_dummy,
            aes(y = dummy + 0.05, col = state),
            shape = "|",
            stroke = 0.1,
            alpha = 0.5,
            position = position_dodgev(height = 0.11)
        ) +
        scale_colour_manual(values = cols_state, name = NULL) +
        scale_fill_manual(values = cols_state, name = NULL) +
        scale_y_continuous(name = "Probability", breaks = seq(0, 1, 0.2)) +
        scale_x_continuous(breaks = x_seq, labels = x_seq_trans) +
        labs(
            y = "Probability",
            x = expression("Colony area" ~ cm^2),
            title = title
        ) +
        theme_andi() +
        theme(
            legend.position = "bottom",
            plot.title = element_text(face = "bold", hjust = 0.5)
        ) #

    if (sep_by_site) {
        plot <- plot +
            facet_wrap2(~site, axes = "all")
    }

    print(plot)

    exp <- list("plot" = plot, "data" = dat_predS, "data_raw" = dat_pred)

    return(exp)
}

add_annotation <- function(x_val, col) {
    x_pos <- log(x_val + 1)

    list(
        geom_point(
            x = x_pos,
            y = .5,
            size = .4,
            color = col,
            inherit.aes = FALSE
        ),
        geom_segment(
            x = x_pos,
            xend = x_pos,
            y = .5,
            yend = .5 + .2,
            linewidth = .2,
            color = col,
            inherit.aes = FALSE
        ),
        annotate(
            "text",
            x = x_pos,
            y = .5 + .2,
            label = x_val,
            color = col,
            hjust = 0.5,
            vjust = -0.15
        )
    )
}
