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

  Title = glue("<a title='{Title}' href='{report_file}'>{Title}</a>")
  report_info <- data.frame(Title = Title, Description = Description, Source = Source, Updates = Updates, Stored = Stored, Columns = Columns)
  return(report_info)

}

get_table_metadata_fields <- function(field, char_vector) {
  mytext <- char_vector[grepl(field, char_vector)]
  mytext <- gsub(field, "", mytext, fixed = TRUE)
  #mytext <- gsub("n", "", mytext, fixed = TRUE)
  return(mytext)
}

extract_report_info("*.html", report_path = "www")

table_data <- purrr::map_df(dir("www"), extract_report_info)

saveRDS(table_data, "data/table_data.Rds")
