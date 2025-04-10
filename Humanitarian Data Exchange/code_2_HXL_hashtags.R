
# R packages
library(jsonlite)
library(httr)
library(dplyr)
library(readxl)
library(writexl)       

# HXL steps
# 1. Grab a spreadsheet of humanitarian data
## disease_outbreaks 
### Last update at "https://api.github.com/repos/jatorresmunguia/disease_outbreak_news/contents/Last%20update"
url_api <- "https://api.github.com/repos/jatorresmunguia/disease_outbreak_news/contents/Last%20update"
last_file <- fromJSON(content(GET(url_api), as = "text"))$name[grepl(fromJSON(content(GET(url_api), as = "text"))$name, pattern = paste0("^outbreaks"))]

rdata_file <- last_file[grepl(".csv$", last_file)]
file_name <- basename(rdata_file)
last_version <- read.csv(paste0("https://raw.githubusercontent.com/jatorresmunguia/disease_outbreak_news/refs/heads/main/Last%20update", "/", rdata_file),
                         header = TRUE)

last_version <- last_version |>
  select(-c(icd11c1, icd11c2, icd11c3, icd11l1, icd11l2, icd11l3)) 
  
rm(url_api, file_name, last_file, rdata_file)

### Add some extra geographical attributes 
#### Regional classification of the WHO
##### Available at: https://cdn.who.int/media/docs/default-source/air-pollution-documents/air-quality-and-health/un-agencies-region-classification-for-country.xlsx?sfvrsn=289af35f_3
###### URL del archivo Excel
url_who_class <- "https://cdn.who.int/media/docs/default-source/air-pollution-documents/air-quality-and-health/un-agencies-region-classification-for-country.xlsx?sfvrsn=289af35f_3"

###### Descargar el archivo temporalmente
temp_file <- tempfile(fileext = ".xlsx")
GET(url_who_class, write_disk(temp_file, overwrite = TRUE))

###### Leer el archivo Excel
who_class <- read_excel(temp_file)

###### adding geographic information from WHO
last_version <- who_class |>
  select(`ISO Country code`, `WHO Region name2`) |>
  rename(
    iso3 = `ISO Country code`,
    who_region = `WHO Region name2`
  ) |>
  right_join(last_version, by = "iso3")

rm(url_who_class, temp_file, who_class)

### Add some extra geographical attributes 
#### Regional classification of the unsd
##### Available at: https://unstats.un.org/unsd/methodology/m49/overview/
###### URL del archivo Excel
###### Leer el archivo Excel
unsd_class <- read_excel(path = "UNSD — Methodology.xlsx")

###### adding geographic information from unsd
last_version <- unsd_class |>
  select(`ISO-alpha3 Code`, `Region Name`, `Sub-region Name`) |>
  rename(
    iso3 = `ISO-alpha3 Code`,
    unsd_region = `Region Name`,
    unsd_subregion = `Sub-region Name`
  ) |>
  right_join(last_version, by = "iso3")

rm(unsd_class)

# 2. Insert a new row between the headers and the data
last_version <- last_version |>
  arrange(desc(Year), iso3, icd104c) |> # Ordering data by Year, iso3, icd104c
  mutate(id_outbreak = paste0(Year, iso3, icd104c)) |> # ID to identify each outbreak Assigning an ID to each report.
  glimpse()
  
last_version <- last_version |>
  add_row(.before = 1) 

# 3. Add some HXL hashtags
## List all the columns by name and their corresponding HXL hashtag
cols2hxl <- c(id_outbreak = "#Year+iso3+icd4",
              Country = "#country+name", 
              iso2 = "#country+code+iso2", 
              iso3 = "#country+code+iso3", 
              unsd_region = "#region+unsd",
              unsd_subregion = "#subregion+unsd",
              who_region = "#region+who",
              Year = "#date+year", 
              icd10n = "#disease+name+icd", 
              icd103n = "#disease+name+icd3", 
              icd104n = "#disease+name+icd4", 
              icd10c = "#disease+code+icd", 
              icd103c = "#disease+code+icd3", 
              icd104c = "#disease+code+icd4", 
              Disease = "#disease+name", 
              DONs = "#news+id", 
              Definition = "#x_disease+definition")

last_version <- last_version |>
  mutate(across(all_of(names(cols2hxl)), 
                ~ replace(., 1, cols2hxl[cur_column()]))) |>
  select(id_outbreak, Year, icd10n, icd103n, icd104n, icd10c, icd103c, icd104c, Disease, Definition, Country, iso2, iso3, unsd_region, unsd_subregion, who_region, DONs)

rm(cols2hxl)

# Write Excel file
write_xlsx(x = list(Data = last_version), path = "data_2_share/disease_outbreaks_HDX.xlsx")
 
