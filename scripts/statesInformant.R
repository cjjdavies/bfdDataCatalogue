trinidadCrops <- FAOSTAT_data_1_30_2020_crops
summary(trinidadCrops)

trinidadCrops <- trinidadCrops %>% mutate_if(is.character, as.factor)
trinidadCrops$Year <- ISOdate(trinidadCrops$Year, 1, 1) %>% as.Date()

inver <- Invertromieall280311 %>% pivot_longer(
             cols = 14:69,
             names_to = "species",
             values_to = "count") %>%
  pivot_longer(cols = 11:13,
               names_to = "phenolic",
               values_to = "phenol ug/ml")

state_pops <- import("https://raw.githubusercontent.com/smach/SampleData/main/states.csv")

stateInformant <- create_informant(state_pops,
                 tbl_name = "states.csv", label = "US Census data")

stateInformant <- stateInformant %>%
  info_tabular(
    Description = "Table of US state populations from decennial censuses, with data from 2000, 2010, and 2020 as well as columns for percent changes and Census Bureau regions and divisions.",
    Updates = "Does not update (except once every 10 years)",
    Source = "US Census Bureau and the R tidycensus package",
    Stored = "[Sample Data GitHub repository](https://github.com/smach/SampleData)",
    `Used by` = "Do More with R"
  )

stateInformant <- stateInformant %>%
  info_columns(
    columns = "State",
    info = "Full text name of the state such as Hawaii or Alaska."
  )  %>%
  info_columns(
    columns = "State Code",
    info = "Two-letter state abbreviation such as HI or AK."
  )

stateInformant <- stateInformant %>%
  info_columns(
    columns = c("PctChange_2000", "PctChange_2010", "PctChange_2020"),
    info = c("Percent population change from prior decennial census. Format already multiplies the decimal by 100, so, for example, a 10.1% change is represented as the number 10.1.")
  )

stateInformant <- stateInformant %>%
  info_columns(
    columns = starts_with("PctChange"),
    info = "Percent population change from prior decennial census. Format already multiplies the decimal by 100, so, for example, a 10.1% change is represented as the number 10.1."
  )

stateInformant <- stateInformant %>%
  info_columns(
    columns = "PctChange_2020",
    info = " This column shows the percent change from the 2020 Census compared with 2010."
  )

stateInformant <- stateInformant %>%
  info_snippet(
    snippet_name = "median_2020",
    fn = ~ . %>% .$Pop_2020 %>% median()
  ) %>%
  info_columns(
    columns = "Pop_2020",
    info = "State population from the 2020 Census. Median value is {median_2020}"
  )

stateInformant <- stateInformant %>%
  info_snippet(
    "census_divisions",
    fn = snip_list("Division", limit = 10, sorting = "inseq")
  ) %>%
  info_columns(
    "Division",
    info = "Census Bureau divisions. Possible values: {census_divisions}"
  ) %>%
  incorporate()

get_informant_report(stateInformant, title = "US State Populations Data")
