#' Count the number of activated weapon system held by a country in each year
#'
#' @param data_frame A dataframe from with the function extracts information
#' @param country_name The name of a country
#'
#' @return A dataframe that has been filtered and modified
#'
#' @import readr
#' @import dplyr
#' @import stringr
#' @import forcats
#' @export
#'
#' @examples
#' count_sys_num(df_weapon_systems, 'RU')
#'
count_sys_num <- function(data_frame, country_name) {
  # count the number of activated weapon system held by a country in each year

  df_by_country <- data_frame %>%
    #consider only the rows which correspond to country_name
    filter(str_detect(country, pattern = country_name))

  # initiate the date frame by years
  year_list = c(as.character(1945:2024))

  weapon_sys_count = c()  # a list of activated weapon system in each year
  for (year in year_list) {

    count = 0  # the number of activated weapon system in a year
    for (i in 1:length(df_by_country$weapon_sys)) {

      # We use YYYY-01-01 as an indicator for a year
      year_date = as.Date(as.character(year), format='%Y')-42

      if ((df_by_country$start_year[i] <= year_date) & (
        df_by_country$end_year[i] >= year_date)) {

        count = count + 1
      }
    }
    weapon_sys_count = c(weapon_sys_count, count)
  }
  # Return a list of activated weapon system in each year
  weapon_sys_count
}
