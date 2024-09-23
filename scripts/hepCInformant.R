pacman::p_load(rio,
               dplyr,
               rvest,
               tidyr,
               purrr,
               glue)

hepC <- import ("https://raw.githubusercontent.com/cjjdavies/ML_intro/refs/heads/main/hepatitisC_dataset.csv")

hepC <- hepC %>%
  select(-1)

hepC <- hepC %>% mutate_if(is.character, as.factor)

hepCInformant <- create_informant(hepC,
                                  tbl_name = "hepatitisC_dataset.csv",
                                  label = "Hepatitis C Data")

hepCInformant <- hepCInformant %>%
  info_tabular(
    Description = "Sample dataset of measures used to identify potential cases of Hepatitis C in females and males, aged 19 to 77 years. Used for demonstrating machine learning techniques.",
    Updates = "Does not update",
    Source = "Data supplied by the University of Aberdeen Centre for Health Data Science",
    Stored = "[Sample data GitHub repository](https://github.com/cjjdavies/ML_intro/tree/main)"
  )

hepCInformant <- hepCInformant %>%
  info_snippet(
    "category_levels",
    fn = snip_list("Category", limit = 10, sorting = "inseq")
  ) %>%
  info_columns(
    columns = "Category",
    info = "A discrete (nominal) variable indicating disease state and blood donor status. Possible values: {category_levels}"
  ) %>%
  incorporate()

hepCInformant <- hepCInformant %>%
  info_snippet(
    "sex_levels",
    fn = snip_list("Sex", sorting = "inseq")
  ) %>%
  info_columns(
    columns = "Sex",
    info = "Biological sex of the patient, as a binary, categorical variable. Values: {sex_levels}"
  ) %>%
  incorporate()

hepCInformant <- hepCInformant %>%
  info_snippet(
    snippet_name = "alb_lowest",
    fn = snip_lowest(column = "ALB")
  ) %>%
  info_snippet(
    snippet_name = "alb_highest",
    fn = snip_highest(column = "ALB")
  ) %>%
  info_columns(
    columns = "ALB",
    Info = "Albumin is a plasma protein produced by the liver and used to diagnose for cirrhosis in patients with hepatitis C. Low levels in the blood may indicate cirrhosis. It is a continuous numerical variable, with a range of {alb_lowest} to {alb_highest} in the dataset."
  ) %>%
  incorporate()

hepCInformant <- hepCInformant %>%
  info_snippet(
    snippet_name = "alp_lowest",
    fn = snip_lowest(column = "ALP")
  ) %>%
  info_snippet(
    snippet_name = "alp_highest",
    fn = snip_highest(column = "ALP")
  ) %>%
  info_columns(
    columns = "ALP",
    Info = "Alkaline phosphatase is an enzyme made in the liver and bones. High levels in blood tests may indicate liver disease. It is a continous numerical variable, with a range of {alp_lowest} to {alp_highest} in the dataset."
  ) %>%
  incorporate()

hepCInformant <- hepCInformant %>%
  info_snippet(
    snippet_name = "alt_lowest",
    fn = snip_lowest(column = "ALT")
  ) %>%
  info_snippet(
    snippet_name = "alt_highest",
    fn = snip_highest(column = "ALT")
  ) %>%
  info_columns(
    columns = "ALT",
    Info = "Alanine amino-transferase is an enzyme involved in the conversion of alanine to glutamate and pyruvate. High levels in blood tests may indicate liver damage. It is a continuous numerical variable, with a range of {alt_lowest} to {alt_highest} in the dataset."
  ) %>%
  incorporate()

hepCInformant <- hepCInformant %>%
  info_snippet(
    snippet_name = "ast_lowest",
    fn = snip_lowest(column = "AST")
  ) %>%
  info_snippet(
    snippet_name = "ast_highest",
    fn = snip_highest(column = "AST")
  ) %>%
  info_columns(
    columns = "AST",
    Info = "Aspartate amino-transferase is an enzyme present in various parts of the body. High concentrations in the blood may indicate liver damage or distress. It is a continuous numerical variable, with a range of {ast_lowest} to {ast_highest} in the dataset."
  ) %>%
  incorporate()

hepCInformant <- hepCInformant %>%
  info_snippet(
    snippet_name = "bil_lowest",
    fn = snip_lowest(column = "BIL")
  ) %>%
  info_snippet(
    snippet_name = "bil_highest",
    fn = snip_highest(column = "BIL")
  ) %>%
  info_columns(
    columns = "BIL",
    Info = "Bilirubin is a by-product of the catabolic process involved in the normal breakdown of red blood cells and other porphyrin-based cells. It is process by the liver and other organs prior to excretion. High levels (>1.2mg/ dL) in blood tests indicate this process is not occurring as it should, and may be indicative of a liver disorder. It is a continuous numerical variable, with a range of {bil_lowest} to {bil_highest} in the dataset."
  ) %>%
  incorporate()

hepCInformant <- hepCInformant %>%
  info_snippet(
    snippet_name = "che_lowest",
    fn = snip_lowest(column = "CHE")
  ) %>%
  info_snippet(
    snippet_name = "che_highest",
    fn = snip_highest(column = "CHE")
  ) %>%
  info_columns(
    columns = "CHE",
    Info = "Choline esterase is an enzyme used to identify the presence of liver cirrhosis. Reduced levels in the blood may indicate cirrhosis. It is a continuous numerical variable, with a range of {che_lowest} to {che_highest} in the dataset."
  ) %>%
  incorporate()

hepCInformant <- hepCInformant %>%
  info_snippet(
    snippet_name = "chol_lowest",
    fn = snip_lowest(column = "CHOL")
  ) %>%
  info_snippet(
    snippet_name = "chol_highest",
    fn = snip_highest(column = "CHOL")
  ) %>%
  info_columns(
    columns = "CHOL",
    Info = "May refer to cholesterol, which is broken down by the liver. If the liver is not functioning properly it may not be able to break down cholesterol efficiently. Diets high in cholesterol may cause nonalcoholoic fatty liver disease. It is a continuous numerical variable, with a range of {chol_lowest} to {chol_highest} in the dataset."
  ) %>%
  incorporate()

hepCInformant <- hepCInformant %>%
  info_snippet(
    snippet_name = "crea_lowest",
    fn = snip_lowest(column = "CREA")
  ) %>%
  info_snippet(
    snippet_name = "crea_highest",
    fn = snip_highest(column = "CREA")
  ) %>%
  info_columns(
    columns = "CREA",
    Info = "May refer to creatine or creatinine; creatine is an amino acid made by the liver and used by the muscles and brain in the metabolism of energy. Impaired liver function may reduce the production of creatine, and therefore creatinine. It is a continuous numerical variable, with a range of {crea_lowest} to {crea_highest} in the dataset."
  ) %>%
  incorporate()

hepCInformant <- hepCInformant %>%
  info_snippet(
    snippet_name = "ggt_lowest",
    fn = snip_lowest(column = "GGT")
  ) %>%
  info_snippet(
    snippet_name = "ggt_highest",
    fn = snip_highest(column = "GGT")
  ) %>%
  info_columns(
    columns = "GGT",
    Info = "Gamma-glutamyl-transferase in an enzyme found mostly in the liver, but is present in small quantities throughout the body. An impaired liver may 'leak' GGT into the bloodstream; high levels in blood tests may be indicative of liver damage or disease. It is a continuous numerical variable, with a range of {ggt_lowest} to {ggt_highest} in the dataset."
  ) %>%
  incorporate()

hepCInformant <- hepCInformant %>%
  info_snippet(
    snippet_name = "prot_lowest",
    fn = snip_lowest(column = "PROT")
  ) %>%
  info_snippet(
    snippet_name = "prot_highest",
    fn = snip_highest(column = "PROT")
  ) %>%
  info_columns(
    columns = "PROT",
    Info = "May refer to protein C or C-reactive protein, a protein dependent on vitamin K, and made in the liver. A deficiency of protein C may indicate liver disease, particularly alcoholic liver disease. It is a continuous numerical variable, with a range of {prot_lowest} to {prot_highest} in the dataset."
  ) %>%
  incorporate()

hepCInformant <- hepCInformant %>%
  info_snippet(
    snippet_name = "age_lowest",
    fn = snip_lowest(column = "Age")
  ) %>%
  info_snippet(
    snippet_name = "age_highest",
    fn = snip_highest(column = "Age")
  ) %>%
  info_columns(
    columns = "Age",
    Info = "Age range from {age_lowest} to {age_highest}."
  ) %>%
  incorporate()

hepCInformant <- get_informant_report(hepCInformant,
                     title = "Hepatitis C Health Process Measures Data")


extract_report_info <- function(report_file, report_path = "www") {
  report_file <- paste0(report_path, "/", report_file)
  my_html <- read_html(report_file)
  Title <- html_element(my_html, ".gt_heading") %>%
    html_text2()
  metadata <- html_elements(my_html, ".gt_from_md") %>%
    html_text2()
  Description <- get_table_metadata_fields("DESCRIPTION", metadata)
  Source <- get_table_metadata_fields("SOURCE", metadata)
  Updates <- get_table_metadata_fields("UPDATES", metadata)
  Stored <- get_table_metadata_fields("STORED", metadata)
  Columns <- html_elements(my_html, "code:nth-child(1)") %>%
    html_text2() %>%
    paste(. , collapse = ", ")

  Title = glue("<a title='{Title}' href='https://github.com/cjjdavies/bfdDataCatalogue/docs/{report_file}'>{Title}</a>")
  report_info <- data.frame(Title = Title, Description = Description, Source = Source, Updates = Updates, Stored = Stored, Columns = Columns)
  return(report_info)

}

get_table_metadata_fields <- function(field, char_vector) {
  mytext <- char_vector[grepl(field, char_vector)]
  mytext <- gsub(field, "", mytext, fixed = TRUE)
  #mytext <- gsub("n", "", mytext, fixed = TRUE)
  return(mytext)
}

extract_report_info("hepCInformantReport.html", report_path = "www")

table_data <- purrr::map_df(dir("www"), extract_report_info)

saveRDS(table_data, "data/hepCTableData.Rds")

hepCTableData <- readRDS("data/hepCTableData.Rds")
library(DT)
datatable(hepCTableData, escape = FALSE, filter = 'top', rownames = FALSE)
