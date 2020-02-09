library(dplyr)
library(rvest)

base_url <- "https://www.ema.europa.eu"
url <- file.path(base_url, "en/medicines/field_ema_web_categories%253Aname_field/Human/ema_group_types/ema_medicine/field_ema_med_status/authorised-36/ema_medicine_types/field_ema_med_biosimilar/search_api_aggregation_ema_medicine_types/field_ema_med_biosimilar/ema_group_types/ema_medicine")

n_records <- url %>%
  read_html() %>%
  html_node(".ecl-u-mb-md-none") %>%
  html_text() %>%
  stringr::str_extract("[0-9]{1,}") %>%
  as.integer()
n_records_per_page <- 25L
n_pages <- ceiling(n_records / n_records_per_page)

biosimilar_urls <- lapply(0:(n_pages-1L), function(p) {
  url %>%
    paste0(., "?page=", p) %>%
    read_html() %>%
    html_nodes(".ecl-list-item__link") %>%
    html_attr("href") %>%
    paste0(base_url, .)
}) %>% do.call(c, .)

biosimilars_list <- lapply(biosimilar_urls, function(url) {
  print(url)
  html <- read_html(url)

  status <- html %>%
    html_node(".ema-status-title") %>%
    html_text()

  reference_product <- html %>%
    html_node(".field-type-text-with-summary") %>%
    html_text() %>%
    stringr::str_split("\\.") %>%
    .[[1L]] %>%
    grep("The reference medicine for .{1,} is .{1,}", ., value = TRUE) %>%
    stringr::word(-1L)

  tables <- html_nodes(html, "table")

  product_details <- tables[[1L]] %>%
    html_table() %>%
    `colnames<-`(c("name", "value")) %>%
    tidyr::pivot_wider()

  publication_details <- tables[[2L]] %>%
    html_table() %>%
    `colnames<-`(c("name", "value")) %>%
    tidyr::pivot_wider()

  cbind(product_details, publication_details, status, url, reference_product,
        stringsAsFactors = FALSE)
})

ema_biosimilars <- biosimilars_list %>%
  bind_rows() %>%
  rename(
    brand_name = Name,
    agency_product_number = `Agency product number`,
    active_substance = `Active substance`,
    company = `Marketing-authorisation holder`,
    generic_name = `International non-proprietary name (INN) or common name`,
    therapeutic_area = `Therapeutic area (MeSH)`,
    atc_code = `Anatomical therapeutic chemical (ATC) code`,
    approval_date = `Date of issue of marketing authorisation valid throughout the European Union`,
    applicant = `Marketing-authorisation applicant`,
    refusal_date = `Date of refusal of marketing authorisation`,
    source = url
  ) %>%
  mutate(
    approval_date = as.Date(approval_date, format = "%d/%m/%Y"),
    approval_year = as.numeric(format(approval_date, "%Y")),
    refusal_date = as.Date(refusal_date),
    company = ifelse(is.na(company), applicant, company)
  ) %>%
  select(
    -c(Biosimilar, Revision, `Contact address`, `Additional monitoring`,
       `Date of opinion`, applicant)
  )

ema_biosimilars_by_year <- ema_biosimilars %>%
  filter(status != "Refused") %>%
  group_by(approval_year) %>%
  summarise(n = n()) %>%
  mutate(agency = "EMA")

save(ema_biosimilars, file = "data/ema_biosimilars.RData")
save(ema_biosimilars_by_year, file = "data/ema_biosimilars_by_year.RData")

breaks <- with(ema_approvals_by_year, min(approval_year):max(approval_year))
ggplot(ema_approvals_by_year, aes(approval_year, n)) +
  geom_col() +
  scale_x_continuous(breaks = breaks, labels = function(x) paste0("'", substr(x, 3L, 4L)))
