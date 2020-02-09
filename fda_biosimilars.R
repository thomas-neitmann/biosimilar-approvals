library(magrittr)
library(rvest)
library(stringr)
website <- "https://www.fda.gov/drugs/biosimilars/biosimilar-product-information"

table <- read_html(website) %>%
  html_node("table") %>%
  html_table()

fda_biosimilars <- within(table, {
  biosimilar_brand_name <- str_extract(`Biosimilar Name`, "[A-Z]{1}[a-z]{1,}")
  biosimilar_generic_name <- str_sub(
    `Biosimilar Name`,
    str_locate(`Biosimilar Name`, "\\(")[, 1L] + 1L,
    nchar(`Biosimilar Name`) - 1L
  ) %>% str_to_lower() %>% str_replace(" -", "-")
  
  reference_brand_name <- str_extract(`Reference Product`, "[A-Z]{1}[a-z]{1,}")
  reference_generic_name <- str_sub(
    `Biosimilar Name`,
    str_locate(`Biosimilar Name`, "\\(")[, 1L] + 1L,
    nchar(`Biosimilar Name`) - 1L
  )
  
  approval_month_char <- str_extract(`Approval Date`, "[A-Z]{1}[a-z]{1,}")
  approval_month <- match(approval_month_char, month.name)
  approval_year <- as.numeric(str_extract(`Approval Date`, "[0-9]{4}"))
  
  rm(approval_month_char, list = colnames(table))
}) %>% .[, c(6:3, 1, 2)]

fda_biosimilars_by_year <- fda_biosimilars %>%
  group_by(approval_year) %>%
  summarise(n = n()) %>%
  mutate(agency = "FDA")

save(fda_biosimilars, file = "data/fda_biosimilars.RData")
save(fda_biosimilars_by_year, file = "data/fda_biosimilars_by_year.RData")
