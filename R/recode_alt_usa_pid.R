#' @title recode_alt_usa_pid
#' 
#' @description 
#' Provides two alternative recodings of the American party 
#' identification strength variable.
#' 
#' @param data A wave of ISSP data.
#' @param usa_pid A column containing responses to the
#' American party identification strength question.
#' 
#' @export
#' 
#' @importFrom dplyr filter mutate select case_when if_else
#' 
recode_alt_usa_pid <- function(
    data, 
    usa_pid
){
  
  output <- data |> 
    filter(ccode == "US") |> 
    select(id, ccode, {{ usa_pid }}) |> 
    mutate(
      {{usa_pid}} := as.numeric({{usa_pid}}),
      pid_no_lean = case_when(
        {{usa_pid}} %in% c(1:2) ~ "Democrat",
        {{usa_pid}} %in% c(3, 4, 5, 99) ~ "None",
        {{usa_pid}} %in% c(6:7) ~ "Republican",
        {{usa_pid}} == 0 ~ NA,
      TRUE ~ "Other"
      ),
      pid_no_lean = paste0(ccode, "_", pid_no_lean),
      pid_strength = if_else({{ usa_pid }} %in% c(1:7), {{ usa_pid }}, NA)
      ) |> 
    select(-{{ usa_pid }})

}
