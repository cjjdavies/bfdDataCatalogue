pacman::p_load(rio,
               dplyr,
               rvest,
               tidyr,
               purrr,
               glue)

scotsCourts <- import ("https://raw.githubusercontent.com/cjjdavies/scts-mi/refs/heads/main/mi_data.csv")

scotsCourts <- scotsCourts %>% mutate_at(("Court type"), as.factor)

scotsCourts <- scotsCourts %>%
  tidyr::unite("ID", "Court type":"Court stage", remove = FALSE)

scotsCourts <- scotsCourts %>%
  pivot_longer(cols = 4:45, names_to = "dates", values_to = "numbers") %>%
  mutate(newdate = as.Date(dates, format = "%d/%m/%Y"))

scotsCourts$newdate <- format(scotsCourts$newdate, "%b.%Y")

scotsCourts <- scotsCourts %>% pivot_wider(id_cols = c(ID,
                                        `Court type`,
                                        `Court stage`,
                                        `2023/24`,
                                        `2019/20  monthly average`),
                            names_from = newdate, values_from = numbers)

colnames(scotsCourts)[colnames(scotsCourts) == "2023/24"] <- "2023.24"
colnames(scotsCourts)[colnames(scotsCourts) == "2019/20  monthly average"] <- "2019.20.monthly.average"

scotsCourts[] <- lapply(scotsCourts, gsub, pattern = "%", fixed = TRUE, replacement = "")
scotsCourts[] <- lapply(scotsCourts, gsub, pattern = "N/A", fixed = TRUE, replacement = NA)

scotsCourts$`Court stage` <- gsub("3", replacement = "", scotsCourts$`Court stage`)

scotsCourts <- scotsCourts %>% mutate_at(c(4:47), as.numeric)

scotsCourts <- scotsCourts %>% mutate_if(is.character, as.factor)

scotsCourts <- scotsCourts %>%
  relocate(`2023.24`, `2019.20.monthly.average`, .after = Sep.23)

scotsCourtsN <- scotsCourts %>%
  filter(scotsCourts$`Court type` == "National") %>%
  select(-ID)

scotsCourtsN <- scotsCourtsN %>%
  pivot_longer(3:44,
               names_to = "monthYear",
               values_to = "Value")

scotsCourtsInformant <- create_informant(scotsCourtsN,
                                  tbl_name = "scotsCourts_dataset.csv",
                                  label = "Scottish Courts Data")

scotsCourtsInformant <- scotsCourtsInformant %>%
  info_tabular(
    Description = "Subset of data from the SCTS Monthly Criminal Management Information time series released by the Scottish Courts and Tribunals Service in response to the Covid-19 pandemic, showing volumes of court business across different court stages. Data are subsetted to National level only.",
    Updates = "Monthly",
    Source = "[Latest dataset can be downloaded from SCTS website](https://scotcourts.gov.uk/publications/#/)",
    Stored = "[Larger sample dataset can be accessed from GitHub repository, as mi_data.csv](https://github.com/cjjdavies/scts-mi/tree/main)"
  )

scotsCourtsInformant <- scotsCourtsInformant %>%
  info_columns(
    columns = "Court type",
    info = "Categorical variable indicating the type of court - currently subset to National level. Possible other values: High Court, Sheriff (Solemn), Sheriff (Summary), Justice of the Peace"
  ) %>%
  incorporate()

scotsCourtsInformant <- scotsCourtsInformant %>%
  info_snippet(
    "court_stage",
    fn = snip_list("Court stage")
  ) %>%
  info_columns(
    columns = "Court stage",
    info = "Categorical variable indicating the court stage. Values: {court_stage}"
  ) %>%
  incorporate()

scotsCourtsInformant <- scotsCourtsInformant %>%
  info_columns(
    columns = "2023.24",
    Info = "Total for the year to date (based on the monthYear column). Year is based on financial year and runs from April to March."
  ) %>%
  incorporate()

scotsCourtsInformant <- scotsCourtsInformant %>%
  info_columns(
    columns = "2019.20.monthly.average",
    Info = "12-month averages from April 2019 to March 2020 for each court stage, to enable comparisons before and after the pandemic."
  ) %>%
  incorporate()

scotsCourtsInformant <- scotsCourtsInformant %>%
  info_columns(
    columns = "monthYear",
    Info = "Month and year for which data are available, from April 2020 to September 2023."
  ) %>%
  incorporate()

scotsCourtsInformant <- scotsCourtsInformant %>%
  info_columns(
    columns = "Value",
    Info = "Volume or percentage, depending on court stage - see court stages."
  ) %>%
  incorporate()

scotsCourtsInformant <- get_informant_report(scotsCourtsInformant,
                                      title = "Scottish Courts and Tribunals Service Monthly Criminal Management Information")

export_report(
  scotsCourtsInformant,
  filename = "www/scotsCourtsInformantReport.html"
)

# hepCTableData <- readRDS("data/hepCTableData.Rds")
# library(DT)
# datatable(hepCTableData, escape = FALSE, filter = 'top', rownames = FALSE)
