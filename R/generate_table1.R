library(data.table)
library(tidyverse)
library(table1)

#' @description
#' Generate a Table 1 summarizing cohort characteristics.
#' @param
#' * patient_level_data: n x p dataframe containing p characteristic variables
#'                       that will be summarized across n patients.
#'
#' * var_dictionary: List of up to p sublists where the name of each sublist
#'                   corresponds to a variable name in patient_level_data and
#'                   contains any or all of the arguments below:
#'
#'                 rename: Variable name to be displayed in Table 1, if different
#'                         than name in patient_level_data. If unspecified, the
#'                         name in patient_level_data is used.
#'                 type: Variable type, either 'continuous' or 'categorical'.
#'                       Continuous variables are summarized with mean and sd.
#'                       Categorical variables are summarized with proportions.
#'                       If unspecified, variables with < log(n) unique values
#'                       are taken as categorical.
#'                 unit: Variable unit. If unspecified, no unit is reported.
#'
#'     If NULL, then the Table 1 uses the names in patient_level_data.
#'
#' * stratifying_var: A string with the name of stratifying variable.
#'                    Must be one of the p column names of patient_level_data.
#'                    If NULL, no stratifying variable will be used.
#'
#' * num_digits: Number of digits after the decimal place for mean and SD
#'               of continuous variables (default is 3).
#'
#' * pct_digits: Number of digits after the decimal place for percentages
#'               (default is 1).
#'
#' * my_table_style: Style of the table.
#'                   The same parameter as topclass for table 1.
#' @return
#' Table 1 which summarized the cohort.

generate_table1 <- function(patient_level_data, var_dictionary = NULL,
                            stratifying_var = NULL,
                            num_digits = 3, pct_digits = 1,
                            my_table_style =
                              "Rtable1-grid Rtable1-shade Rtable1-times") {

  # Obtain original variable names, renames, types, and units.
  var_names <- colnames(patient_level_data)
  var_renames <- determine_var_name(patient_level_data)
  var_types <- determine_var_type(patient_level_data)
  var_units <- lapply(seq_len(length(var_names)), function(xx) NA)
  names(var_units) <- colnames(patient_level_data)

  if (!is.null(var_dictionary)) {

    # TODO(not priority): Very sloppy, clean this code.
    var_dict_renames <- tryCatch(
      expr = as.matrix(unlist(sapply(var_dictionary, "[[", "rename"))),
      error = function(x) var_renames
    )
    var_renames_update <- match(
      rownames(var_dict_renames),
      names(var_renames)
    )
    if (!all(is.na(var_renames_update))) {
      var_renames[var_renames_update] <- var_dict_renames
    }

    var_dict_types <- tryCatch(
      expr = as.matrix(unlist(sapply(var_dictionary, "[[", "type"))),
      error = function(x) var_types
    )
    var_types_update <- match(
      rownames(var_dict_types),
      names(var_types)
    )
    if (!all(is.na(var_types_update))) {
      var_types[var_types_update] <- var_dict_types
    }

    var_dict_units <- tryCatch(
      expr = as.matrix(unlist(sapply(var_dictionary, "[[", "unit"))),
      error = function(x) var_units
    )

    var_units_update <- match(
      rownames(var_dict_units),
      names(var_units)
    )

    if (!all(is.na(var_units_update))) {
      var_units[var_units_update] <- var_dict_units
    }
  }


  # Determine which variables are categorical and make them factor variables.
  for (name in var_names) {
    if (var_types[name] == "categorical") {
      categories <- unique(patient_level_data[, name])
      categories <- categories[!is.na(categories)]
      res <- tryCatch(as.numeric(categories),
        error = function(e) NULL,
        warning = function(w) NULL
      )

      if (is.null(res)) {
        patient_level_data[, name] <- factor(patient_level_data[, name])
      } else {
        categories <- unique(patient_level_data[, name])
        order <- str_order(categories,
          decreasing = FALSE,
          na_last = TRUE,
          numeric = TRUE
        )
        levels <- categories[order]

        patient_level_data[, name] <- factor(patient_level_data[, name],
          levels = levels
        )
      }
    } else {
      patient_level_data[, name] <- as.numeric(patient_level_data[, name])
    }
  }

  # Adjust variable name with unit which will appear on table 1.
  for (name in var_names) {
    if (!is.na(var_units[name])) {
      var_renames[name] <- paste0(var_renames[name], " (", var_units[name], ")")
    }
  }

  # Create labels based on variable renames and stratification for tabel1().
  if (is.null(stratifying_var)) {
    labels <- list(variables = var_renames)

    strata <- c(list(Total = patient_level_data))
  } else {

    # Rename stratifying variable if desired.
    stratifying_var_rename <- var_renames[stratifying_var][[1]]

    # Add column of stratifying variable with new name.
    # TODO(not priority): Fix sloppy code.
    patient_level_data[, stratifying_var_rename] <- patient_level_data[, stratifying_var]

    # Remove stratifying variable from variables displayed in Table 1.
    var_renames[which(names(var_renames) == stratifying_var)] <- NULL

    labels <- list(
      variables = var_renames,
      groups = list("", stratifying_var_rename)
    )

    strata <- c(
      list(
        Total = patient_level_data
      ),
      split(patient_level_data, patient_level_data[, stratifying_var_rename])
    )
  }

  # Code below uses the table1() example documentation:
  # https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html#using-abbreviated-code-to-specify-a-custom-renderer
  # Last accessed: 28/06/2021.

  # Create render functions for categorical, continuous variables, and mising.
  my_render_categorical <- function(x) {
    c("", sapply(stats.default(x), function(y) {
      with(y, sprintf(paste0("%d (%0.", pct_digits, "f%%)"), FREQ, PCT))
    })) # %0.0f% to change the digit.
  }

  my_render_continuous <- function(x) {
    with(
      stats.default(x),
      sprintf(paste0("%0.", num_digits, "f (%0.", num_digits, "f)"), MEAN, SD)
    )
  }

  my_render_missing <- function(x) {
    with(
      stats.default(is.na(x))$Yes,
      c(Missing = sprintf(paste0("%0d (%0.", pct_digits, "f%%)"), FREQ, PCT))
    )
  }

  my_render <- function(x, name, ...) {
    render.default(
      x,
      render.continuous = my_render_continuous,
      render.categorical = my_render_categorical,
      render.missing = my_render_missing
    )
  }

  if (is.null(stratifying_var)) {
    return(table1(strata, labels,
      render = my_render,
      topclass = my_table_style
    ))
  } else {
    level_count <- length(unique(patient_level_data[, stratifying_var_rename]))

    return(table1(strata, labels,
      groupspan = c(1, level_count),
      render = my_render,
      topclass = my_table_style
    ))
  }
}


#' @description
#' Generate the list of variable types based on the number of data points and
#' the unique value for each variable from patient_level_data.
#' @param
#' * patient_level_data : n x p dataframe containing p characteristic variables
#'                        that will be summarized across n patients.
#' @return
#' List of variable types.

determine_var_type <- function(patient_level_data) {
  max_num_categories <- log(nrow(patient_level_data))
  num_unique <- sapply(
    seq_len(ncol(patient_level_data)),
    function(xx) length(unique(patient_level_data[, xx]))
  )
  var_type <- as.list(ifelse(num_unique < max_num_categories,
    "categorical", "continuous"
  ))
  names(var_type) <- colnames(patient_level_data)

  return(var_type)
}


#' @description
#' Generate the list of variable names based on patient_level_data column names.
#' @param
#' * patient_level_data: n x p dataframe containing p characteristic variables
#'                       that will be summarized across n patients.
#' @return
#' List of variable names.

determine_var_name <- function(patient_level_data) {
  var_name <- as.list(colnames(patient_level_data))
  names(var_name) <- colnames(patient_level_data)

  return(var_name)
}


#' @description
#' Generate the list of variable's information which can be input(var_dictionary)
#' of generate_table1 based on data_dictionary.
#' @param
#' * data_dictionary: A dataframe contains variables' information.
#'                    Rownames are original names.
#'                    Three columns: type, unit and rename of original variables.
#' @return
#' List of variables's information.

data_dictionary_2_list <- function(data_dictionary) {

  # Make shift fix for now.
  data_dictionary$type <- ifelse(data_dictionary$type == "Numeric",
    "continuous", "categorical"
  )

  dictionary_list <- lapply(
    setNames(
      split(
        data_dictionary,
        seq(nrow(data_dictionary))
      ),
      rownames(data_dictionary)
    ),
    function(x) as.list(x)
  )
  dictionary_list <- lapply(dictionary_list, Filter, f = Negate(is.na))

  return(dictionary_list)
}


#' @description
#' Simulate_table1_data generates a dataframe for illustration.
#' @param
#' * num_data: number of data points.
#' @return
#' Dataframe contains continuous and categorical variables.
#' Some variables have missing values.

simulate_table1_data <- function(num_data = 10000) {
  my_dat <- cbind(
    rnorm(num_data),
    rpois(num_data, 10),
    rbinom(num_data, 1, 0.3),
    rbinom(num_data, 12, 0.3),
    rnorm(num_data, 160, 3),
    sample(
      x = c("A", "B", "C", "D", "E"),
      size = num_data,
      replace = TRUE,
      prob = rep(1 / 5, 5)
    )
  )

  my_dat[sample(1:num_data, 0.2 * num_data), 1] <- NA
  my_dat[sample(1:num_data, 0.3 * num_data), 3] <- NA

  colnames(my_dat) <- c(
    "Variable_1", "Variable_2", "Variable_3",
    "Variable_4", "Variable_5", "Variable_6"
  )

  my_dat <- as.data.frame(my_dat)

  return(my_dat)
}
