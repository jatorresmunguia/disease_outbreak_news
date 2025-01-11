################################################################################
### **'A global dataset of pandemic- and epidemic-prone disease outbreaks'** ###  
################################################################################

# Packages
library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)
library(clock)
library(writexl)
library(tidyverse)
library(purrr)
library(glue)

## Default Language: English
Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "English")

## Moving last code
### Defining folders
current_file <- "code_to_extract_dons.R"
history_folder <- "code_to_extract_dons_history"

last_update_dir <- "Last update/"
last_files <- list.files(path = last_update_dir, pattern = paste0("^", "outbreaks", ".*"), full.names = TRUE)
rdata_file <- last_files[grepl("\\.RData$", last_files)]
update_date <- dmy(sub(".*?(\\d+).*", "\\1", rdata_file))

if (!dir.exists(history_folder)) {
  dir.create(history_folder)
}

new_filename <- file.path(paste0(history_folder, "/code_to_extract_dons_", update_date, ".R"))

if (file.exists(current_file)) {
  file.copy(current_file, new_filename)
} else {
  stop("File code_to_extract_dons.R was not found!")
}

## New code for updated data

data <- data.frame("Outbreak" = NA,
                   "Date" = NA,
                   "Year" = NA,
                   "Month" = NA,
                   "Day" = NA,
                   "Description" = NA,
                   "Link" = NA)

rD <- rsDriver(browser = "firefox", port = 2511L,  # between 0 and 65535
               chromever = NULL)

remDr <- rD[["client"]]

# Navegar a la página
remDr$navigate("https://www.who.int/emergencies/disease-outbreak-news")

# Dynamically go page by page
for(page_input in 1){ # Only recent ones to update, manually check the website
  
  Sys.sleep(5)
  
  pagination_input <- remDr$findElement(using = 'css', value = "input.k-textbox")
  
  pagination_input$clearElement()
  
  # Input a new page number (e.g., go to next page)
  pagination_input$sendKeysToElement(list(as.character(page_input)))
  
  # Simulate pressing "Enter" to navigate
  pagination_input$sendKeysToElement(list(key = "enter"))
  
  # Wait for the new page to load
  Sys.sleep(5)
  
  # Obtener el código fuente de la página ya cargada dinámicamente 
  page_source <- remDr$getPageSource()[[1]]
  
  # Parsear el código fuente con rvest
  page <- read_html(page_source)
  
  newslink <- page |>
    html_nodes("a.sf-list-vertical__item") |>
    html_attr("href")
  
  for(news in 1:length(newslink)){
    data[(page_input-1)*20+news, "Link"] <- newslink[news]
    
    # Extracting the title from each DON
    data[(page_input-1)*20+news, "Outbreak"] <- read_html(newslink[news]) |>
      html_nodes("div.sf-item-header-wrapper") |>
      html_element("h1.don-title") |>
      html_text(trim = TRUE) 
    
    # Extracting the information on date from each DON
    data[(page_input-1)*20+news, "Date"] <- read_html(newslink[news]) |>
      html_nodes("div.sf-item-header-wrapper") |>
      html_element("span.timestamp") |>
      html_text(trim = TRUE) 
    
    # Extracting the Description from each DON
    data[(page_input-1)*20+news, "Description"] <- read_html(newslink[news]) |>
      html_nodes("article.sf-detail-body-wrapper") |>
      html_text(trim = TRUE)
  }
  
}

data <- data |> 
  mutate(Date = as.Date(Date, format = "%d %B %Y")) |>
  mutate(Year = get_year(Date), 
         Month = get_month(Date), 
         Day = get_day(Date))|>
  glimpse()

## Previous update
# Define a function to load and move files
load_and_move_files_fn <- function(file_prefix) {
  
  # Get the list of files matching the prefix
  last_files <- list.files(path = "Last update/", pattern = paste0("^", file_prefix, ".*"), full.names = TRUE)
  
  # Check if there are both .RData and .csv files
  rdata_file <- last_files[grepl("\\.RData$", last_files)]
  csv_file <- last_files[grepl("\\.csv$", last_files)]
  
  if (length(rdata_file) == 1 & length(csv_file) == 1) {
    
    # Load the .RData file into a temporary environment
    temp_env <- new.env()
    load(rdata_file, envir = temp_env)
    
    # Get the name of the first (and only) object in the environment
    object_name <- ls(temp_env)[1]
    
    # Assign the object to prev_dons_raw or prev_dons_unique as appropriate
    if (file_prefix == "dons_raw") {
      prev_dons_raw <<- temp_env[[object_name]]
    } 
    if (file_prefix == "dons_unique") {
      prev_dons_unique <<- temp_env[[object_name]]
    }
    if (file_prefix == "dons_all") {
      prev_dons_all <<- temp_env[[object_name]]
    }
    if (file_prefix == "covid_unique") {
      prev_covid_unique <<- temp_env[[object_name]]
    }
    if (file_prefix == "outbreaks") {
      prev_outbreaks <<- temp_env[[object_name]]
    }
    rm(temp_env)
    
    # Extract the date from the filename
    update_date <- sub(".*?(\\d+).*", "\\1", rdata_file)
    
    # Create a new folder in "Previous updates/" with the format "update_YYYYMMDD"
    new_subfolder <- file.path("Previous updates", paste0("update_", update_date))
    dir.create(new_subfolder, showWarnings = FALSE)
    
    # Move the files to the corresponding subfolder
    file.rename(rdata_file, file.path(new_subfolder, basename(rdata_file)))
    file.rename(csv_file, file.path(new_subfolder, basename(csv_file)))
    
    # Check the new location
    if (file.exists(file.path(new_subfolder, basename(rdata_file))) &
        file.exists(file.path(new_subfolder, basename(csv_file)))) {
      message(paste0("Both files for ", file_prefix, " have been moved to '", new_subfolder, "'."))
    } else {
      message(paste0("Error, one or both files for ", file_prefix, " were not moved to the new subfolder!"))
    }
    
  } else {
    message(paste0("Error, there should be exactly one .RData and one .csv file for ", file_prefix, " in the 'Last update' directory!"))
  }
}

# Load files for dons_unique and dons_raw
load_and_move_files_fn("dons_unique")
load_and_move_files_fn("dons_raw")
load_and_move_files_fn("dons_all")
load_and_move_files_fn("covid_unique")
load_and_move_files_fn("outbreaks")

rm(load_and_move_files_fn)

## Working on raw DONs from previous updates
# Date format
prev_dons_raw <- prev_dons_raw |>
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) |>
  glimpse()

max_dons_id_prev <- max(as.numeric(gsub(x = prev_dons_raw$ID, pattern = "DON", replacement = "")))
max_dons_date_prev <- max(prev_dons_raw$Date)

## Working with raw DONs from last updates 
# filter update to avoid duplicating dons
last_dons_raw1 <- data |> 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) |>
  filter(Date > max_dons_date_prev)|>
  glimpse()

last_dons_raw1 <- last_dons_raw1 |> 
  arrange(desc(Year), desc(Month), desc(Day), desc(Link)) |> # Ordering data by date
  mutate(rowid = (max_dons_id_prev+length(Year)):(max_dons_id_prev+1)) |>
  mutate(ID = paste0("DON", str_pad(rowid, 4, pad = "0"))) |> # ID to identify each DON. Assigning an ID to each report. Oldest one is DON0001
  glimpse()

## Raw DONs together ##
last_dons_raw <- last_dons_raw1 |> # Last DONs raw
  select(-c(rowid, Outbreak, Year, Month, Day)) |> 
  bind_rows(prev_dons_raw) |> # Previous DONs raw
  glimpse()

max_dons_date_last <- gsub(
  x = format(max(as.Date(last_dons_raw1$Date)), 
             format = "%d-%m-%Y"),
  pattern = "-", replacement = ""
)

### Disease names ###
## Setting homogeneous names ##
# International Classification of Diseases ##
icd <- readxl::read_xlsx(path = "classification/icd1011.xlsx")

# Extracting disease name, I keep working with the last_dons_raw1, DONs to update
## DONs related to multiple diseases?
last_dons_raw2 <- last_dons_raw1 |>
  # seasonal influenza, rhinovirus, RSV, and human metapneumovirus in China
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Trends of acute respiratory infection, including human metapneumovirus, in the Northern Hemisphere"), 4, 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(icd104n = case_when(
    Outbreak == "Trends of acute respiratory infection, including human metapneumovirus, in the Northern Hemisphere" & row_number() == 1 ~ "Human metapneumovirus pneumonia",
    Outbreak == "Trends of acute respiratory infection, including human metapneumovirus, in the Northern Hemisphere" & row_number() == 2 ~ "Influenza with other manifestations, seasonal influenza virus identified",
    Outbreak == "Trends of acute respiratory infection, including human metapneumovirus, in the Northern Hemisphere" & row_number() == 3 ~ "Respiratory syncytial virus pneumonia",
    Outbreak == "Trends of acute respiratory infection, including human metapneumovirus, in the Northern Hemisphere" & row_number() == 4 ~ "Acute bronchitis due to rhinovirus",
    TRUE ~ NA
  )) |>
  # In Congo Influenza A (H1N1, pdm09), rhinoviruses, SARS-COV-2, Human coronaviruses, parainfluenza viruses, and Human Adenovirus
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Undiagnosed disease - Democratic Republic of the Congo"), 6, 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(icd104n = case_when(
    Outbreak == "Undiagnosed disease - Democratic Republic of the Congo" & row_number() == 1 ~ "Influenza with other manifestations, seasonal influenza virus identified",
    Outbreak == "Undiagnosed disease - Democratic Republic of the Congo" & row_number() == 2 ~ "Other viral infections of unspecified site",
    Outbreak == "Undiagnosed disease - Democratic Republic of the Congo" & row_number() == 3 ~ "Severe acute respiratory syndrome [SARS]",
    Outbreak == "Undiagnosed disease - Democratic Republic of the Congo" & row_number() == 4 ~ "Coronavirus infection, unspecified site",
    Outbreak == "Undiagnosed disease - Democratic Republic of the Congo" & row_number() == 5 ~ "Parainfluenza virus pneumonia",
    Outbreak == "Undiagnosed disease - Democratic Republic of the Congo" & row_number() == 6 ~ "Adenoviral pneumonia",
    TRUE ~ icd104n
  )) |>
  ungroup() |>
  glimpse()

last_dons_raw3 <- last_dons_raw2 |>
  mutate(icd104n = case_when(
    Outbreak == "Acute respiratory infections complicated by malaria (previously undiagnosed disease) - Democratic Republic of the Congo" ~ "Plasmodium falciparum malaria, unspecified",
    grepl(Outbreak, pattern = "Marburg virus disease") ~ "Marburg virus disease",
    Outbreak == "Oropouche virus disease - Region of the Americas" ~ "Oropouche virus disease",
    TRUE ~ icd104n
  )) 

# Merge with icd
last_dons_raw4 <- last_dons_raw3 |>
  mutate(icd104n_lower = tolower(icd104n)) |>
  select(!c(icd104n)) |> 
  plyr::join(
    icd %>% mutate(icd104n_lower = tolower(icd104n)),
    by = "icd104n_lower",
    type = "left",
    match = "all"
  ) |>
  select(-icd104n_lower) |>
    glimpse()

# iso codes #
iso <- readxl::read_xlsx(path = "classification/isocodes.xlsx")

# Extracting country name #
## DONs related to multiple countries?
# Country names as in ISO
last_dons_raw5 <- last_dons_raw4 |> 
  # seasonal influenza, rhinovirus, RSV, and human metapneumovirus in China
  mutate(Country = case_when(
    Outbreak == "Trends of acute respiratory infection, including human metapneumovirus, in the Northern Hemisphere" ~ "China",
    grepl(Outbreak, pattern = "Rwanda") ~ "Rwanda",
    grepl(Outbreak, pattern = "Democratic Republic of the Congo") ~ "Congo Democratic Republic of the",
    TRUE ~ NA
  )) |>
  # Oropouche virus disease - Region of the Americas
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Oropouche virus disease - Region of the Americas"), 11, 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 1 ~ "Bolivia (Plurinational State of)",
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 2 ~ "Brazil",
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 3 ~ "Canada",
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 4 ~ "Cayman Islands",
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 5 ~ "Colombia",
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 6 ~ "Cuba",
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 7 ~ "Ecuador",
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 8 ~ "Guyana",
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 9 ~ "Panama",
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 10 ~ "Peru",
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 11 ~ "United States of America",
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

## Adding iso country names and codes
last_dons_raw6 <- last_dons_raw5 |> 
  mutate(Country_lower = tolower(Country)) |>
  select(!c(Country)) |> 
  plyr::join(
    iso %>% mutate(Country_lower = tolower(Country)),
    by = "Country_lower",
    type = "left",
    match = "all"
  ) |>
  select(-Country_lower) |> 
  # Update Country, iso2, and iso3 for Palestine
  mutate(
    iso2 = case_when(Country == "Gaza" ~ "PS",
                     TRUE ~ iso2),
    Country = case_when(Country == "Gaza" ~ "Palestine State of", 
                        TRUE ~ Country),
    iso3 = case_when(Country == "Palestine State of" ~ "PSE", 
                     TRUE ~ iso3)
  ) |>
  glimpse()

# Only DONs related to diseases, i.e. deleting those with information on events, rules, recommendations, etc.
## All DONs during this month refer to disease outbreaks

## creating a key to identify unique outbreaks by disease, year, and country
last_dons_raw7 <- last_dons_raw6 |>
  mutate(key = paste0(iso3, Year, icd104c)) |>
  group_by(key) |>
  mutate(DONs = paste(ID, collapse = ", ")) |> # All DONs by outbreak
  ungroup()

#### All DON's (including duplicates if there are multiple diseases of countries reported in one single DON)
# joining previous DONs 
last_dons_all <- last_dons_raw7 |>
  select(Country, iso2, iso3, Year, icd10n, icd103n, icd104n, icd10c, icd103c, icd104c, 
         icd11c1, icd11c2, icd11c3, icd11l1, icd11l2, icd11l3, Disease, DONs, Definition) |>
  rbind(prev_dons_all)

#### Unique cases per country per year ####
# joining last unique outbreaks with all previous DONs
last_dons_unique1 <- last_dons_raw7 |>
  select(Country, iso2, iso3, Year, icd10n, icd103n, icd104n, icd10c, icd103c, icd104c, 
         icd11c1, icd11c2, icd11c3, icd11l1, icd11l2, icd11l3, Disease, DONs, Definition) |>
  distinct(Country, icd104n, Year, .keep_all = TRUE) 

# Update Country, iso2, and iso3 for Bonaire Sint Eustatius and Saba
last_dons_unique2 <- last_dons_unique1 |>
  mutate(
    Country = case_when(iso2 == "BQ" ~ "Bonaire Sint Eustatius and Saba",
                        TRUE ~ Country),
    iso2 = case_when(iso2 == "BQ" ~ "BQ",
                     TRUE ~ iso2),
    iso3 = case_when(iso2 == "BQ" ~ "BES",
                     TRUE ~ iso3)
  ) |>
  # Update Country, iso2, and iso3 for Kosovo
  mutate(
    iso2 = case_when(Country == "Kosovo" ~ "XK", 
                     TRUE ~ iso2),
    iso3 = case_when(Country == "Kosovo" ~ "XXK",
                     TRUE ~ iso3)
  ) |>
  # Update Country, iso2, and iso3 for Palestine (to include Gaza)
  mutate(
    iso2 = case_when(Country == "Gaza" ~ "PS",
                     TRUE ~ iso2),
    Country = case_when(Country == "Gaza" ~ "Palestine State of", 
                        TRUE ~ Country),
    iso3 = case_when(Country == "Palestine State of" ~ "PSE", 
                     TRUE ~ iso3)
  )

last_dons_unique <- last_dons_unique2 |>
  rbind(prev_dons_unique) |>
  distinct(Country, icd104n, Year, .keep_all = TRUE) # Apply again distinct()! 

## Save results in 
save(last_dons_raw, file = paste0("Last update/", "dons_raw_", max_dons_date_last, ".RData"))
save(last_dons_all, file = paste0("Last update/", "dons_all_", max_dons_date_last, ".RData"))
save(last_dons_unique, file = paste0("Last update/", "dons_unique_", max_dons_date_last, ".RData"))

write.csv(last_dons_raw, file = paste0("Last update/", "dons_raw_", max_dons_date_last, ".csv"))
write.csv(last_dons_all, file = paste0("Last update/", "dons_all_", max_dons_date_last, ".csv"))
write.csv(last_dons_unique, file = paste0("Last update/", "dons_unique_", max_dons_date_last, ".csv"))

#### Corona Dashboard ####
# Download "Daily COVID-19 cases and deaths by date reported to WHO" 
# and locate it in covid_who_dashboard/
# or download directly from the url
covid_dashboard <- read.csv("https://srhdpeuwpubsa.blob.core.windows.net/whdh/COVID/WHO-COVID-19-global-daily-data.csv")

# Date format
covid_dashboard <- covid_dashboard |> 
  mutate(Date_reported = as.Date(Date_reported, format = "%Y-%m-%d")) |>
  mutate(Year = get_year(Date_reported), 
         Month = get_month(Date_reported), 
         Day = get_day(Date_reported)) |>
  glimpse()

# Filter to last date
covid_dashboard <- covid_dashboard |> 
  filter(Date_reported < max(as.Date(last_dons_raw1$Date))) |> # last_dons_raw1: subset to last update
  aggregate(New_cases ~ Country_code + Country + Year, sum) |>
  glimpse()

covid_dashboard <- covid_dashboard |>
  filter(New_cases > 0)

covid_dashboard <- covid_dashboard |>
  mutate(icd10n = "Provisional assignment of new diseases of uncertain etiology or emergency use") |> 
  mutate(icd103n = "Emergency use of U07") |> 
  mutate(icd104n = "COVID-19, virus identified") |> 
  mutate(icd10c = "U00-U49") |> 
  mutate(icd103c = "U07") |> 
  mutate(icd104c = "U071") |> 
  mutate(icd11c1 = "25") |> 
  mutate(icd11c2 = "RA01") |> 
  mutate(icd11c3 = "25RA01") |> 
  mutate(icd11l1 = "Codes for special purposes") |> 
  mutate(icd11l2 = "International provisional assignment of new diseases of uncertain aetiology and emergency use") |> 
  mutate(icd11l3 = "COVID-19") |> 
  mutate(Disease = "COVID-19") |> 
  mutate(DONs = "Coronavirus dashboard") |> 
  mutate(Definition = "Infectious disease caused by the SARS-CoV-2 virus.") |>
  mutate(iso2 = Country_code)

covid_dashboard <- covid_dashboard |>
  select(iso2, Year, icd10n, icd103n, icd104n, icd10c, icd103c, icd104c, 
         icd11c1, icd11c2, icd11c3, icd11l1, icd11l2, icd11l3, 
         Disease, DONs, Definition) |>
  left_join(iso, by = "iso2")

## Bonaire Sint Eustatius and Saba together in the iso 
covid_dashboard <- covid_dashboard |>
  mutate(
    Country = case_when(
      iso2 %in% c("XA", "XB", "XC") ~ "Bonaire Sint Eustatius and Saba",
      TRUE ~ Country  # Keep the original value if the condition is not met
    ),
    iso2 = case_when(
      iso2 %in% c("XA", "XB", "XC") ~ "BQ",
      TRUE ~ iso2  # Keep the original value if the condition is not met
    ),
    iso3 = case_when(
      iso2 == "BQ" ~ "BES", 
      TRUE ~ iso3
    )  # Update iso3 to "BES" if it is "BQ"
  )

# Select columns from covid_dashboard that match the column names in last_dons_unique
covid_unique <- covid_dashboard |>
  select(all_of(colnames(last_dons_unique))) |>
  # Filter out rows where Country is NA
  filter(!is.na(Country)) 

levels(factor(covid_unique$Country)) # Number of countries
levels(factor(covid_unique$icd104n)) # Only one disease: COVID-19, virus identified

save(covid_unique, file = paste0("Last update/", "covid_unique_", max_dons_date_last, ".RData"))
write.csv(covid_unique, file = paste0("Last update/", "covid_unique_", max_dons_date_last, ".csv"))

#### Final dataset of outbreaks ####
# Combine last_dons_unique and covid_unique into one dataset
outbreaks <- last_dons_unique |>
  bind_rows(covid_unique)

# Assign new row numbers to the combined dataset
rownames(outbreaks) <- 1:nrow(outbreaks)

# Update Country, iso2, and iso3 for Bonaire Sint Eustatius and Saba
outbreaks <- outbreaks |>
  mutate(
    Country = case_when(iso2 == "BQ" ~ "Bonaire Sint Eustatius and Saba",
                        TRUE ~ Country),
    iso2 = case_when(iso2 == "BQ" ~ "BQ",
                     TRUE ~ iso2),
    iso3 = case_when(iso2 == "BQ" ~ "BES",
                     TRUE ~ iso3)
  )

# Update Country, iso2, and iso3 for Kosovo
outbreaks <- outbreaks |>
  mutate(
    iso2 = case_when(Country == "Kosovo" ~ "XK", 
                     TRUE ~ iso2),
    iso3 = case_when(Country == "Kosovo" ~ "XXK",
                     TRUE ~ iso3)
  )

# Update Country, iso2, and iso3 for Palestine (to include Gaza)
outbreaks <- outbreaks |>
  mutate(
    iso2 = case_when(Country == "Gaza" ~ "PS",
                     TRUE ~ iso2),
    Country = case_when(Country == "Gaza" ~ "Palestine State of", 
                        TRUE ~ Country),
    iso3 = case_when(Country == "Palestine State of" ~ "PSE", 
                     TRUE ~ iso3)
  )

# Filter out rows where Country is NA
outbreaks <- outbreaks |>
  filter(!is.na(Country))

# Check the number of unique countries and diseases
nlevels(factor(outbreaks$Country))
nlevels(factor(outbreaks$icd104n))

# Trim whitespace from all character columns
outbreaks <- outbreaks |>
  mutate(across(where(is.character), ~ trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")))

### saving new outbreaks dataset
save(outbreaks, file = paste0("Last update/", "outbreaks_", max_dons_date_last, ".RData"))
write.csv(outbreaks, file = paste0("Last update/", "outbreaks_", max_dons_date_last, ".csv"), row.names = FALSE)
