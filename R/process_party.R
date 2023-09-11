#' @title process_party
#'
#' @description
#' Recodes party affiliation items in an ISSP wave.
#'
#' @param data A wave of ISSP data.
#' @param pty_cols The names of the party affiliation columns in the ISSP wave.
#' @param use_attr Logical. Should country codes be matched to attributes of 
#' party affiliation columns (see details)?
#' @param attr_sep A string with the delimiter separating country codes in 
#' attributes of the party affiliation column (see details).
#' @param other_thresh The threshold below which to classify small party 
#' supporters as "Other", as a weighted proportion of the country-sample.
#' @param recode_usa Logical. Should US party strength be recoded into 
#' unordered categories (see details)?
#'
#' @details
#' The ISSP data has a row for every respondent and a column for party 
#' affiliation in every country. This makes recoding party affiliation 
#' complicated. The same values are often used to indicate missingness within 
#' countries and rows of party affiliation columns corresponding to other 
#' countries. Part of this function matches country codes to each country's 
#' party affiliation column and recodes the values used to indicate 
#' within-country missingness. This is relatively easy during later waves of 
#' the ISSP, where two-character ISO country codes are part of the names of 
#' party affiliation columns. However, during earlier waves, country 
#' information is stored in the attributes of party affiliation columns. The 
#' \code{use_attr} argument allows the user to specify whether to use the 
#' attributes of party affiliation columns to identify the country. If 
#' \code{use_attr = TRUE}, the \code{attr_sep} argument specifies the 
#' delimiter separating country codes in the attributes of party affiliation 
#' columns. The \code{attr_sep} argument should be set to "2" for 1993 and 
#' "_" for 2000. If \code{use_attr = FALSE}, the function assumes that the 
#' party affiliation column names in the ISSP wave contain country codes.
#' 
#' In many ISSP waves, the United States uses a standard ordinal party 
#' strength variable to measure party affiliation. This is distinct from the 
#' nominal party affiliation variables used in other countries. The 
#' \code{recode_usa} argument allows the user to specify whether to recode the 
#' US party strength variable into nominal party affiliation categories. If 
#' \code{recode_usa = TRUE}, the function recodes partisans, strong partisans, 
#' and party leaners as "Democrat" and "Republican", pure independents and 
#' those not answering the question as "None", and all other respondents as 
#' "Other".
#'
#' @export
#'
#' @importFrom dplyr case_when mutate across select starts_with last_col rename_with if_else filter anti_join left_join
#' @importFrom labelled var_label
#' @importFrom tidyr pivot_longer separate_wider_delim
#' @importFrom stringr str_remove
#' @importFrom srvyr as_survey_design survey_count
#'
process_party <- function(
    data,
    pty_cols,
    use_attr = FALSE,
    attr_sep = NULL,
    other_thresh = 0.01,
    recode_usa = TRUE
){

  # Used to filter out countries present in an ISSP wave but not answering the 
  # party affiliation question
  not_all_na <- function(x) any(!is.na(x))

  # Recodes International Automobile Identification codes used in 1993 and 2000 
  # to iso2c codes used in subsequent waves
  # This covers all non-iso2c-consistent codes used in the 1993 and 2000 data, 
  # and should be updated if using other waves before the adoption of iso2c
  hrmnse_ccode <- function(x) {
    y <- case_when(
      x == "D" ~ "DE",
      x == "CR" ~ "CZ",
      x == "A" ~ "AT",
      x == "AUS" ~ "AU",
      x == "CDN" ~ "CA",
      x == "RCH" ~ "CL",
      x == "E" ~ "ES",
      x == "SF" ~ "FI",
      x == "J" ~ "JP",
      x == "MEX" ~ "MX",
      x == "N" ~ "NO",
      x == "RP" ~ "PH",
      x == "P" ~ "PT",
      x == "RUS" ~ "RU",
      x == "S" ~ "SE",
      x == "SLO" ~ "SI",
      x == "USA" ~ "US",
      x == "H" ~ "HU",
      x == "I" ~ "IT",
      x == "IRL" ~ "IE",
      TRUE ~ x
    )
    y
  }

  # The ISSP data has a row for every respondent and a column for party 
  # affiliation in every country
  # This makes recoding party affiliation complicated
  # The same values are often used to indicate non-partisans and rows of party 
  # affiliation columns corresponding to other countries
  # This function matches country codes to each country's party affiliation 
  # column and recodes the latter,
  # allowing us to recode values otherwise used to indicate other country rows.
  # The function requires the country code to be the party affiliation column 
  # name
  set_na <- function(df){
    # Get a vector of country codes
    cntry_col <- unique(df$ccode)
    # Loop through the country codes
    for(cntry in cntry_col) {
      # Get the column names of the party affiliation columns for the country
      prty_cols <- colnames(df)[colnames(df) == cntry]
      # Loop through the party affiliation columns
      for(col in prty_cols){
        # Recode 0 and -2 to 99 when country codes match party affiliation 
        # column names
        # 0 and -2 are used to indicate other country rows in different ISSP 
        # waves
        df[df$ccode == cntry, col] <- sapply(
          df[df$ccode == cntry, col],
          function(x) if_else(x == 0 | x == -2, 99, x)
        )
      }
    }
    return(df)
  }

  input <- data |>
    # Select the columns needed for recoding
    select(id, cntry, ccode, year, weight, {{pty_cols}}) |>
    # Select countries that answered the party affiliation question
    select(where(not_all_na))

  # Rename party affiliation columns using country codes
  # If using ISSP 1993 or 2000, use the attributes of the party columns to 
  # identify the country
  if(use_attr) {
    # Create object containing party column attributes
    input |>
      select(starts_with("V")) |>
      var_label() |>
      unlist() -> variable_labels

    # Get country code from party column attributes
    variable_labels <- data.frame(
      name = names(variable_labels),
      labels = variable_labels
    ) |>
      separate_wider_delim(
        labels,
        delim = attr_sep,
        names = c("ccode", "excess")
      ) |>
      mutate(ccode = hrmnse_ccode(ccode)) |>
      select(-excess)

    # Rename party columns with country codes
    input <- input |>
      rename_with(
        ~ variable_labels$ccode,
        .cols = -c(id, cntry, ccode, year, weight)
      )

    # Else, the party affiliation column names in ISSP 2010 and 2020 already 
    # contain country codes
  } else {
    # Strip variable suffix in party affiliation columns, leaving country 
    # codes as names
    colnames(input) <- gsub("_PRTY", "", colnames(input))
  }

  # Create df with numeric party affiliation variables
  party_numbers <- input |>
    mutate(across(6:last_col(), as.numeric)) |>
    # Use the set_na function to distinguish missing obs within countries
    set_na() |>
    # Now we can set other country rows to missing in party affiliation columns
    mutate(across(6:last_col(), ~ if_else(.x == 0 | .x == -2, NA, .x))) |>
    # Create long column with party affiliation values
    pivot_longer(
      cols = 6:last_col(),
      names_to = "pty_var",
      values_to = "pty_val"
    ) |>
    # Unlike any other country, Austria provides different values for "Other 
    # Party" in 2010 and 2020: 94 and 8, respectively
    # We need to be able to identify these rows when coding nonpartisans below
    # Return the name of party affiliation variables to their original names 
    # in 2010 and 2020
    # That way, we can find rows where pty_var == "AT_PRTY" and pty_val == 94 
    # or 8
    mutate(
      pty_var = if_else(
        year == 2010 | year == 2020, paste0(pty_var, "_PRTY"), pty_var
      )
    )


  # Create object with keys identifying other country rows
  na_filter <- party_numbers |>
    filter(is.na(pty_val)) |>
    # Convert pty_val to character so we can join with party strings below
    mutate(
      # Convert pty_val to character so we can join with party strings below
      pty_val = as.character(pty_val),
      # Remove "_PRTY" suffix from pty_var to avoid duplication when joining
      pty_var = str_remove(pty_var, "_PRTY")
    )

  # Create df with nonpartisan and uncoded party affiliation values
  nonpartisans <- party_numbers |>
    mutate(
      pty_val = case_when(
        # 95 is the value used to indicate "Other" response options in all 
        # Environment Modules of the ISSP,
        # except for Austria in 2010 and 2020, India in 2020, and Britain in 
        # 2020
        pty_var == "AT_PRTY" & year == 2010 & pty_val == 94 |
          pty_var == "AT_PRTY" & year == 2020 & pty_val == 8 |
          pty_var == "IN_PRTY" & year == 2020 & pty_val == 6 |
          pty_var == "GB_PRTY" & year == 2020 & pty_val == 9 |
          # 95 is used for "Would not vote" in Austria in 2010,
          # so we need to stop this value being coded as "Other"
          pty_var != "AT_PRTY" & pty_val == 95 ~ 95,
        # This captures the nonpartisan value codes used in all Environment 
        # Modules of the ISSP
        # This picks up all nonpartisans after we have set other country rows 
        # to missing and uncoded party affiliation values as 95
        pty_val > 90 | pty_val < 0 ~ 99,
        TRUE ~ pty_val
      ),
      # Remove "_PRTY" suffix from pty_var to avoid duplication when joining
      pty_var = str_remove(pty_var, "_PRTY")
    ) |>
    # Subset to nonpartisan and uncoded party affiliation values
    filter(pty_val == 95 | pty_val == 99) |>
    # Recode nonpartisan values to "None" and uncoded party affiliation values 
    # to "Other"
    mutate(
      nonpty_val = if_else(
        pty_val == 95, "Other", "None"
      )
    ) |>
    select(-pty_val)

  # Create df with party affiliation values as strings
  party_strings <- input |>
    # Convert party affiliation columns to factors to get value labels
    mutate(across(6:last_col(), as_factor)) |>
    # Create long column with party affiliation strings
    pivot_longer(cols = 6:last_col(),
                 names_to = "pty_var",
                 values_to = "pty_val") |>
    # Convert to character so we can filter out other country rows below
    mutate(pty_val = as.character(pty_val))

  # Create df with party affiliation values as strings, excluding other 
  # country rows
  party_cleaned <- party_strings |>
    # Filter out other country rows
    anti_join(
      na_filter,
      by = c("id", "cntry", "ccode", "year", "weight",  "pty_var")
    ) |>
    # Join with nonpartisan and uncoded party affiliation values
    left_join(
      nonpartisans,
      by = c("id", "cntry", "ccode", "year", "weight", "pty_var")
    ) |>
    # Replace raw nonpartisan and uncoded party affiliation labels with 
    # strings from the nonpartisan df
    mutate(
      pty_val = if_else(
        is.na(nonpty_val), pty_val, nonpty_val
      )
    ) |>
    select(-nonpty_val, -pty_var)

  # If using the ordinal US party strength variable,
  # recode it into nominal party categories.
  if(recode_usa) {
    # Create df with recoded USA party affiliation values
    us_party <- party_numbers |>
      # Remove "_PRTY" suffix from pty_var to have consistent USA variable 
      # names across waves
      mutate(pty_var = str_remove(pty_var, "_PRTY")) |>
      # Filter to USA
      filter(ccode == "US" & pty_var == "US") |>
      # Code partisans, strong partisans, and party leaners as "Democrat" 
      # and "Republican"
      # Code pure independents and those not answering the question as "None"
      # Code all other respondents as "Other"
      mutate(
        us_pid = case_when(
          pty_val %in% 1:3 ~ "Democrat",
          pty_val %in% c(4, 99) ~ "None",
          pty_val %in% 5:7 ~ "Republican",
          TRUE ~ "Other"
        )
      ) |>
      select(-pty_var, -pty_val)

    # Join USA party affiliation values with party affiliation values 
    # for all other countries
    party_cleaned <- party_cleaned |>
      left_join(
        us_party,
        by = c("id", "cntry", "ccode", "year", "weight")
      ) |>
      # Replace raw USA party affiliation labels with strings from 
      # the USA party df
      mutate(
        pty_val = if_else(
          is.na(us_pid), pty_val, us_pid
        )
      ) |>
      select(-us_pid)

  }
  # We want to minimize the number of party affiliation categories
  # We will recode all categories with a frequency below a certain 
  # threshold as "Other"
  # Create df with party affiliation frequencies
  party_reduced <- party_cleaned |>
    # Use survey package to weight observations using 
    # post-stratification weights
    as_survey_design(weights = weight) |>
    # Count number of observations in each party affiliation category 
    # by country
    survey_count(ccode, pty_val) |>
    mutate(
      # Calculate frequency of each party affiliation category by country
      freq = n / sum(n),
      # Code party affiliation categories with a frequency below the 
      # threshold as "Other"
      new_pty = case_when(
        pty_val %in% c("Other", "None") ~ pty_val,
        freq < other_thresh ~ "Other",
        TRUE ~ pty_val
      ),
      .by = ccode
    ) |>
    select(ccode, pty_val, new_pty)

  # Create new country-party affiliation variable with reduced number 
  # of categories
  output <- party_cleaned |>
    left_join(
      party_reduced,
      by = c("ccode", "pty_val")
    ) |>
    mutate(
      cntry_pty = if_else(
        pty_val != new_pty,
        paste0(ccode, "_", new_pty),
        paste0(ccode, "_", pty_val)
      )
    ) |>
    select(-pty_val, -new_pty)

  return(output)

}
