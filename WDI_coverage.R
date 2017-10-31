library(curl)
library(plyr)
library(readr)
library(dplyr)
library(tidyr)
library(readxl)

WDI_bulk_url <- "http://databank.worldbank.org/data/download/WDI_csv.zip"
WDI_local_filename <- "WDI_csv.zip"
WDI_last_modified <- "WDI_csv.lastmodified"
WDI_local_path <- "WDI_csv"

download_if_modified <- function() {
  h <- new_handle()
  
  if (file.exists(WDI_last_modified)) {
    last_modified <- read_file(WDI_last_modified)
    handle_setheaders(h, .list = list("If-Modified-Since" = last_modified))
  }
  
  tryCatch({
    message("Downloading WDI bulk file if changed...")
    curl_download(WDI_bulk_url, WDI_local_filename, quiet = T, handle = h)
    unzip(WDI_local_filename, exdir = WDI_local_path)
    message("WDI bulk file has been updated")

    last_modified = handle_data(h)$modified
    last_modified = strftime(
      last_modified,
      format = "%a, %d %b %Y %H:%M:%S",
      tz = "GMT", usetz=T
    )
    write_file(last_modified, WDI_last_modified)
  }, error = function(e) {
      if (handle_data(h)$status_code == 304) {
        message("WDI bulk file has not been updated, using cached copy.")
      }   
    }
  )
  
  h
}

h <- download_if_modified()

wdi_data <- read_csv(file.path(WDI_local_path, "WDIData.csv"))
wdi_series <- read_csv(file.path(WDI_local_path, "WDISeries.csv"))
wdi_countries <- read_csv(file.path(WDI_local_path, "WDICountry.csv"))

wdi_data <- wdi_data %>%
  select(-starts_with("X")) %>% # Remove column read_csv created from trailing , in file
  select(-`Indicator Name`, -`Country Name`)

wdi_long <- wdi_data %>% gather("year", "value", `1960`:`2016`, convert=T)

summ_country_indicator <- wdi_long %>%
  group_by(`Indicator Code`, `Country Code`) %>%
  summarise(
    value.count = sum(!is.na(value)),
    latest.year = max(year[!is.na(value)]),
    latest.year.finite = ifelse(is.finite(latest.year), latest.year, NA)
  )

summ_indicator <- summ_country_indicator %>%
  group_by(`Indicator Code`) %>%
  summarise(
    years.min = min(value.count),
    years.max = max(value.count),
    years.p10 = quantile(value.count, 0.10),
    years.p25 = quantile(value.count, 0.25),
    years.median = quantile(value.count, 0.50),
    years.p75 = quantile(value.count, 0.75),
    countries.maxyears = sum(value.count == max(value.count)),
    latest.year.min = min(latest.year),
    latest.year.max = max(latest.year),
    latest.year.p10 = quantile(latest.year, 0.10),
    latest.year.p25 = quantile(latest.year, 0.25),
    latest.year.median = quantile(latest.year, 0.50),
    latest.year.p75 = quantile(latest.year, 0.75),
    latest.year.noninf.p10 = quantile(latest.year.finite, 0.10, na.rm=T),
    latest.year.noninf.p25 = quantile(latest.year.finite, 0.25, na.rm=T),
    latest.year.noninf.median = quantile(latest.year.finite, 0.50, na.rm=T),
    latest.year.noninf.p75 = quantile(latest.year.finite, 0.75, na.rm=T),
  )

wdi_series_codes <- wdi_series %>%
  select(`Series Code`) %>%
  mutate(
    code_parts = strsplit(`Series Code`, ".", fixed=T),
    code_part_1 = sapply(code_parts, first),
    code_part_2 = sapply(code_parts, function(x) x[2])
  )

# Treemap ####
library(ggplot2)
library(treemapify)

ggplot(wdi_series_codes, aes(area = 1, fill = code_part_1, subgroup = code_part_1)) +
  geom_treemap(color="white") +
  geom_treemap_subgroup_text(place = "centre", colour = "white", size = 16, min.size = 14, fontface="bold") +
  theme(legend.position = "none")

# Time & Region coverage ####

country_count <- wdi_long %>% pull(`Country Code`) %>% unique %>% length

indicator_decade <- wdi_long %>%
#  left_join(wdi_countries %>% select(`Country Code`, Region)) %>%
#  filter(!is.na(Region)) %>%
  mutate(decade = floor(year/10)*10) %>%
  group_by(decade) %>%
  group_by(`Indicator Code`, add = TRUE) %>%
  filter(!is.na(value)) %>%
  summarise(countries = length(unique(`Country Code`))) %>%
  ungroup %>%
  complete(decade, `Indicator Code`, fill = list(countries = 0)) %>%
  spread(decade, countries)

regions <- list(
  "LCN" = "Latin America & Caribbean",
  "SAS" = "South Asia",
  "SSF" = "Sub-Saharan Africa",
  "ECS" = "Europe & Central Asia",
  "MEA" = "Middle East & North Africa",
  "EAS" = "East Asia & Pacific",
  "NAC" = "North America"
)

indicator_region <- wdi_long %>%
  filter(year >= 2010) %>%
  left_join(wdi_countries %>% select(`Country Code`, Region)) %>%
  filter(!is.na(Region)) %>%
  group_by(Region) %>%
  group_by(`Indicator Code`, add = TRUE) %>%
  filter(!is.na(value)) %>%
  summarise(countries = length(unique(`Country Code`))) %>%
  ungroup %>%
  complete(Region, `Indicator Code`, fill = list(countries = 0)) %>%
  spread(Region, countries)
names(indicator_region)[match(regions, names(indicator_region))] <- names(regions)

region_country_count <- indicator_region %>%
  select(-`Indicator Code`) %>%
  summarise_all(max)

indicator_meta <- indicator_decade %>%
  full_join(indicator_region) %>%
  mutate_all(funs(coalesce(., 0))) %>%
  left_join(wdi_series %>% select(`Series Code`, `Indicator Name`), by = c("Indicator Code" = "Series Code"))

library(htmltools)
make_decade <- function(decade, countries, country_count) {
  prop <- countries / country_count
  src <- paste0(
    "images/",
    if (prop == 0.0) "zero" else if (prop < 0.2) "low" else if (prop > 0.8) "high" else "medium",
    "_time.png"
  )
  tags$img(src = src, title = paste0(decade, "s: ", countries, " countries available"), width=16, height=16)
}

make_region <- function(region_code, region, countries, region_country_count) {
  prop <- countries / region_country_count
  src <- paste0(
    "images/",
    if (prop == 0.0) "zero" else if (prop < 0.2) "low" else if (prop > 0.8) "high" else "medium",
    "_",
    region_code,
    ".png"
  )
  tags$img(src = src, title = paste0(region, ": ", countries, " countries available (out of ", region_country_count, ")"), width=16, height=16)
}

make_indicator <- function(df) {
  tags$tr(
    tags$td(df$`Indicator Name`),
    tags$td(tags$a(href=paste0("//data.worldbank.org/indicator/", df$`Indicator Code`), df$`Indicator Code`)),
    tags$td(
      make_decade(1960, df$`1960`, country_count),
      make_decade(1970, df$`1970`, country_count),
      make_decade(1980, df$`1980`, country_count),
      make_decade(1990, df$`1990`, country_count),
      make_decade(2000, df$`2000`, country_count),
      make_decade(2010, df$`2010`, country_count)
    ),
    tags$td(
      lapply(names(regions), function(code) { make_region(code, regions[[code]], df[[code]], region_country_count[[code]]) })
    )
  )
}

make_indicator_group <- function(df) {
  group <- tags$tr(tags$th(colspan = 4, df$`Indicator Group`[1]), class="group")
  head <- tags$tr(tags$th("Indicator"),tags$th("Code"),tags$th("Time coverage"),tags$th("Region coverage"), class="headers")
  rows <- alply(df, 1, make_indicator)
  c(list(group), list(head), rows)
}

make_indicators_table <- function(meta) {
  groups <- dlply(meta, .(`Indicator Group`), make_indicator_group)
  tags$table(unlist(groups, recursive = FALSE), class="indicators")
}

bytopic <- indicator_meta %>%
  left_join(wdi_series %>% select(`Series Code`, `Topic`), by = c("Indicator Code"="Series Code")) %>%
  rename(`Indicator Group` = Topic) %>%
  arrange(`Indicator Group`, `Indicator Name`)

ind_table <- make_indicators_table(bytopic)
page <- tags$html(
  tags$link(rel="stylesheet", type="text/css", href="theme.css"),
  tags$body(ind_table)
)
write_file(as.character(page), "docs/index.html")


poverty <- read_xlsx("poverty_indicators.xlsx") %>% select(`Indicator Group`,`Indicator Code`)
poverty <- poverty %>%
  left_join(indicator_meta) %>%
  mutate_all(function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(`Indicator Group` = factor(`Indicator Group`, unique(`Indicator Group`)))
ind_table <- make_indicators_table(poverty)
page <- tags$html(
  tags$link(rel="stylesheet", type="text/css", href="theme.css"),
  tags$body(ind_table)
)
write_file(as.character(page), "test.html")
