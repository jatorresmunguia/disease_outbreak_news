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

## Default Language: English
Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "English")

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
for(page_input in 1:9){ # Only recent ones to update, manually check the website
  
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

max_dons_date_last <- gsub(x = format(max(as.Date(last_dons_raw1$Date)), format = "%d-%m-%Y"), pattern = "-", replacement = "")

### Disease names ###
## Setting homogeneous names ##
# International Classification of Diseases ##
icd <- readxl::read_xlsx(path = "classification/icd1011.xlsx")

# Extracting disease name 
last_dons_raw2 <- last_dons_raw1 |>
  mutate(Disease = str_split(Outbreak, "-| – | - | ｰ |- | – | – | – ", simplify = TRUE)[, 1]) |>
  glimpse()

last_dons_raw3 <- last_dons_raw2 |>
  mutate(icd104n = case_when(
    grepl(Disease, pattern = "Chandipura") ~ "Other specified mosquito-borne viral fevers",
    grepl(Disease, pattern = "[H|h]epatitis E") ~ "Acute hepatitis E",
    grepl(Disease, pattern = "[A|a]cute hepatitis of unknown") ~ "Acute viral hepatitis, unspecified",
    grepl(Disease, pattern = "[A|a]nthrax") ~ "Cutaneous anthrax",
    grepl(Disease, pattern = "Klebsiella pneumoniae") ~ "Bacterial pneumonia, unspecified",
    grepl(Outbreak, pattern = "[A|a]vian [I|i]nfluenza") ~ "Influenza due to identified zoonotic or pandemic influenza virus",
    grepl(Disease, pattern = "[B|o]otulism") ~ "Botulism",
    grepl(Disease, pattern = "[C|c]holera") ~ "Classical cholera",
    grepl(Outbreak, pattern = "poliovirus type") ~ "Acute poliomyelitis, unspecified",
    grepl(Disease, pattern = "[C|c]rimean") ~ "Crimean-Congo haemorrhagic fever",
    grepl(Disease, pattern = "[D|d]engue") ~ "Dengue, unspecified",
    grepl(Outbreak, pattern = "polio virus 2") ~ "Acute poliomyelitis, unspecified",
    grepl(Disease, pattern = "[D|d]engue") ~ "Dengue, unspecified",
    grepl(Disease, pattern = "[D|d]iphtheria") ~ "Diphtheria, unspecified",
    grepl(Disease, pattern = "[E|e]bola") ~ "Ebola virus disease",
    grepl(Disease, pattern = "[E|e]nterovirus") ~ "Enterovirus infection, unspecified site",
    grepl(Disease, pattern = "Guillain") ~ "Guillain-Barré syndrome",
    grepl(Outbreak, pattern = "[I|i]nfluenza A") ~ "Influenza due to identified zoonotic or pandemic influenza virus",
    grepl(Disease, pattern = "Japanese encephalitis") ~ "Japanese encephalitis",
    grepl(Disease, pattern = "[L|l]assa") ~ "Lassa fever",
    grepl(Disease, pattern = "[L|l]egio") ~ "Legionnaires disease",
    grepl(Disease, pattern = "Leptospirosis") ~ "Leptospirosis, unspecified",
    grepl(Disease, pattern = "[M|m]arburg") ~ "Marburg virus disease",
    grepl(Disease, pattern = "[M|m]easles") ~ "Measles",
    grepl(Disease, pattern = "[M|m]eningi") ~ "Bacterial meningitis, unspecified",
    grepl(Disease, pattern = "Pneumococcal meningitis") ~ "Pneumococcal meningitis",
    grepl(Disease, pattern = "[M|m]iddle [E|e]ast [R|r]espiratory") ~ "Middle East respiratory syndrome coronavirus [MERS-CoV]",
    grepl(Outbreak, pattern = "[M|m]onkey") ~ "Monkeypox",
    grepl(Outbreak, pattern = "[M|m]pox") ~ "Monkeypox",
    grepl(Disease, pattern = "[S|s]almonella") ~ "Salmonella infection, unspecified",
    grepl(Disease, pattern = "[M|m]yocarditis") ~ "Other and unspecified infectious diseases",
    grepl(Disease, pattern = "[N|n]ipah") ~ "Other viral infections of unspecified site",
    grepl(Disease, pattern = "[O|o]ropo") ~ "Oropouche virus disease",
    grepl(Disease, pattern = "fungal meningitis") ~ "Meningitis due to other specified causes",
    grepl(Disease, pattern = "Psittacosis") ~ "Chlamydia psittaci infection",
    grepl(Disease, pattern = "[R|r]abies") ~ "Urban rabies",
    grepl(Disease, pattern = "[R|r]ift") ~ "Rift Valley fever",
    grepl(Outbreak, pattern = "Multi-country outbreak of Salmonella Typhimurium linked to chocolate products – Europe and the United States of America") ~ "Salmonella infection, unspecified",
    grepl(Outbreak, pattern = "hepatitis of unknown") ~ "Acute viral hepatitis, unspecified",
    grepl(Disease, pattern = "Western [E|e]quine [E|e]ncephalitis") ~ "Western equine encephalitis",
    grepl(Outbreak, pattern = "Wild poliovirus type 1") ~ "Acute paralytic poliomyelitis, wild virus, indigenous",
    grepl(Disease, pattern = "[Y|y]ellow") ~ "Yellow fever, unspecified")) |>
  glimpse()
    
## DONs related to multiple diseases
# Suspected triple outbreak of typhoid fever, shigellosis and cholera - Congo
last_dons_raw4 <- last_dons_raw3 |>
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Suspected triple outbreak of typhoid fever, shigellosis and cholera - Congo"), 4, 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(icd104n = case_when(
    Outbreak == "Suspected triple outbreak of typhoid fever, shigellosis and cholera - Congo" & row_number() == 1 ~ "Typhoid fever",
    Outbreak == "Suspected triple outbreak of typhoid fever, shigellosis and cholera - Congo" & row_number() == 2 ~ "Classical cholera",
    Outbreak == "Suspected triple outbreak of typhoid fever, shigellosis and cholera - Congo" & row_number() == 3 ~ "Shigellosis due to Shigella dysenteriae",
    Outbreak == "Suspected triple outbreak of typhoid fever, shigellosis and cholera - Congo" & row_number() == 4 ~ "Shigellosis due to Shigella flexneri",
    TRUE ~ icd104n
  )) |>
  ungroup() |>
  glimpse()
  
# Geographical expansion of cases of dengue and chikungunya beyond the historical areas of transmission in the Region of the Americas
last_dons_raw5 <- last_dons_raw4 |>
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Geographical expansion of cases of dengue and chikungunya beyond the historical areas of transmission in the Region of the Americas"), 2, 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(icd104n = case_when(
    Outbreak == "Geographical expansion of cases of dengue and chikungunya beyond the historical areas of transmission in the Region of the Americas" & row_number() == 1 ~ "Dengue, unspecified",
    Outbreak == "Geographical expansion of cases of dengue and chikungunya beyond the historical areas of transmission in the Region of the Americas" & row_number() == 2 ~ "Chikungunya virus disease",
    TRUE ~ icd104n
  )) |>
  ungroup() |>
  glimpse()

# Increased incidence of scarlet fever and invasive Group A Streptococcus infection - multi-country
last_dons_raw6 <- last_dons_raw5 |>
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Increased incidence of scarlet fever and invasive Group A Streptococcus infection - multi-country"), 2, 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(icd104n = case_when(
    Outbreak == "Increased incidence of scarlet fever and invasive Group A Streptococcus infection - multi-country" & row_number() == 1 ~ "Sepsis due to streptococcus, group A",
    Outbreak == "Increased incidence of scarlet fever and invasive Group A Streptococcus infection - multi-country" & row_number() == 2 ~ "Scarlet fever",
    TRUE ~ icd104n
  )) |>
  ungroup() |>
  glimpse()

# Malaria - Pakistan
last_dons_raw7 <- last_dons_raw6 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Malaria - Pakistan"), 2, 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(icd104n = case_when(
    Outbreak == "Malaria - Pakistan" & row_number() == 1 ~ "Plasmodium falciparum malaria, unspecified",
    Outbreak == "Malaria - Pakistan" & row_number() == 2 ~ "Plasmodium vivax malaria with other complications",
    TRUE ~ icd104n
  )) |>
  ungroup() |>
  glimpse()

# Upsurge of respiratory illnesses among children-Northern China
last_dons_raw8 <- last_dons_raw7 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Upsurge of respiratory illnesses among children-Northern China"), 4, 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(icd104n = case_when(
    Outbreak == "Upsurge of respiratory illnesses among children-Northern China" & row_number() == 1 ~ "Influenza with other manifestations, seasonal influenza virus identified",
    Outbreak == "Upsurge of respiratory illnesses among children-Northern China" & row_number() == 2 ~ "Pneumonia due to Mycoplasma pneumoniae",
    Outbreak == "Upsurge of respiratory illnesses among children-Northern China" & row_number() == 3 ~ "Respiratory syncytial virus pneumonia",
    Outbreak == "Upsurge of respiratory illnesses among children-Northern China" & row_number() == 4 ~ "Severe acute respiratory syndrome [SARS]",
    TRUE ~ icd104n
  )) |>
  ungroup() |>
  glimpse()

# Merge with icd
last_dons_raw9 <- last_dons_raw8 |> 
  select(-Disease) |> 
  plyr::join(icd, type = "left", by = "icd104n", match = "all") |>
  glimpse()

# iso codes #
iso <- readxl::read_xlsx(path = "classification/isocodes.xlsx")

# Extracting country name #
last_dons_raw10 <- last_dons_raw9 |>
  mutate(Country = map_chr(str_split(Outbreak, "-| – | - | ｰ |- | – | – | – "), ~ .x[length(.x)]))

# Country names as in ISO
last_dons_raw11 <- last_dons_raw10 |> 
  mutate(Country = case_when(
    grepl(Country, pattern = "Democratic Republic of the Congo") ~ "Congo Democratic Republic of the",
    grepl(Country, pattern = "Saudi Arabia") ~ "Saudi Arabia",
    grepl(Country, pattern = "Leste") ~ "Timor-Leste",
    grepl(Country, pattern = "China") ~ "China",
    grepl(Country, pattern = "Netherlands") ~ "Netherlands",
    grepl(Country, pattern = "Tanzania") ~ "Tanzania United Republic of",
    grepl(Country, pattern = "United Arab Emirates") ~ "United Arab Emirates",
    grepl(Country, pattern = "Oman") ~ "Oman",
    grepl(Country, pattern = "United Kingdom of Great Britain and Northern Ireland") ~ "United Kingdom of Great Britain and Northern Ireland",
    TRUE ~ Country
    )) |>
  glimpse()

## DONs related to multiple countries
# Oropouche virus disease - Region of the Americas
last_dons_raw12 <- last_dons_raw11 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Oropouche virus disease - Region of the Americas"), 5, 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 1 ~ "Bolivia (Plurinational State of)",
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 2 ~ "Brazil",
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 3 ~ "Colombia",
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 4 ~ "Cuba",
    Outbreak == "Oropouche virus disease - Region of the Americas" & row_number() == 5 ~ "Peru",
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Mpox – African Region
last_dons_raw13 <- last_dons_raw12 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Mpox – African Region"), 5, 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    Outbreak == "Mpox – African Region" & row_number() == 1 ~ "Burundi",
    Outbreak == "Mpox – African Region" & row_number() == 2 ~ "Kenya",
    Outbreak == "Mpox – African Region" & row_number() == 3 ~ "Rwanda",
    Outbreak == "Mpox – African Region" & row_number() == 4 ~ "Uganda",
    Outbreak == "Mpox – African Region" & row_number() == 5 ~ "Côte d'Ivoire",
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Dengue - Global situation
Dengue_Global_situation_countries_20240530 <- c(
  "Benin", "Burkina Faso", "Cabo Verde", "Côte d'Ivoire", "Eritrea", "Ethiopia", 
  "Ghana", "Liberia", "Mali", "Mauritania", "Mauritius", "Niger", "Sao Tome and Principe", 
  "Senegal", "Seychelles", "Antigua and Barbuda", "Argentina", "Aruba", "Barbados", "Belize", 
  "Bolivia (Plurinational State of)", "Bonaire Sint Eustatius and Saba", "Brazil", 
  "Virgin Islands (British)", "Colombia", "Costa Rica", "Cuba", "Curaçao", "Dominican Republic", 
  "Ecuador", "El Salvador", "French Guiana", "Grenada", "Guadeloupe", "Guatemala", "Guyana", 
  "Honduras", "Jamaica", "Martinique", "Mexico", "Montserrat", "Nicaragua", "Panama", 
  "Paraguay", "Peru", "Puerto Rico", "Saint Barthélemy", "Saint Kitts and Nevis", 
  "Saint Martin (French part)", "Saint Vincent and the Grenadines", "Sint Maarten (Dutch part)", 
  "Suriname", "Trinidad and Tobago", "Turks and Caicos Islands", "United States of America", 
  "Uruguay", "Afghanistan", "Pakistan", "Sudan", "Réunion", "Bangladesh", "Maldives", 
  "Myanmar", "Nepal", "Sri Lanka", "Thailand", "Australia", "Cambodia", "China", "Fiji", 
  "French Polynesia", "Lao People's Democratic Republic", "Malaysia", "Micronesia (Federated States of)", 
  "New Caledonia", "Palau", "Samoa", "Singapore", "Solomon Islands", "Tokelau", "Vanuatu", 
  "Viet Nam", "Wallis and Futuna", "Bahamas", "Cayman Islands", "Chile", "Saint Lucia", 
  "Mayotte", "Angola", "Kenya", "Virgin Islands (U.S.)", "France", "Tonga", "Tuvalu"
  )

last_dons_raw14 <- last_dons_raw13 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Dengue - Global situation") & Date == "2024-05-30", length(Dengue_Global_situation_countries_20240530), 1)) |>
  uncount(repeated_row) |>
  group_by(ID, Date) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Dengue - Global situation") & Date == "2024-05-30" & row_number() <= length(Dengue_Global_situation_countries_20240530) ~ Dengue_Global_situation_countries_20240530[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Yellow fever – African Region (AFRO)
Yellow_fever_African_Region_AFRO_countries_2024_03_20 <- c(
  "Burkina Faso", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", 
  "Congo Democratic Republic of the", "Guinea", "Niger", "Nigeria", "South Sudan", "Togo", "Uganda"
)

last_dons_raw15 <- last_dons_raw14 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Yellow fever") & Date == "2024-03-20", length(Yellow_fever_African_Region_AFRO_countries_2024_03_20), 1)) |>
  uncount(repeated_row) |>
  group_by(ID, Date) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Yellow fever") & Date == "2024-03-20" & row_number() <= length(Yellow_fever_African_Region_AFRO_countries_2024_03_20) ~ Yellow_fever_African_Region_AFRO_countries_2024_03_20[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Psittacosis – European region
Psittacosis_European_region <- c(
  "Austria", "Denmark", "Germany", "Sweden", "Netherlands"
  )

last_dons_raw16 <- last_dons_raw15 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Psittacosis – European region"), length(Psittacosis_European_region), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Psittacosis – European region") & row_number() <= length(Psittacosis_European_region) ~ Psittacosis_European_region[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Dengue - Global situation
Dengue_Global_situation_countries_20231221 <- c(
  "Côte d'Ivoire", "Mauritania", "Seychelles", "Argentina", "Barbados", 
  "Belize", "Bolivia (Plurinational State of)", "Brazil", "Virgin Islands (British)", 
  "Colombia", "Costa Rica", "Dominican Republic", "Ecuador", "El Salvador", 
  "Grenada", "Guadeloupe", "Guatemala", "Guyana", "Honduras", "Jamaica", 
  "Mexico", "Montserrat", "Nicaragua", "Panama", "Paraguay", "Peru", 
  "Puerto Rico", "Saint Lucia", "Suriname", "Uruguay", 
  "Venezuela (Bolivarian Republic of)", "Bangladesh", "Maldives", "Nepal", 
  "Sri Lanka", "Thailand", "Antigua and Barbuda", "Aruba", "French Guiana", 
  "Martinique", "Saint Barthélemy", "Saint Kitts and Nevis", 
  "Saint Martin (French part)", "Saint Vincent and the Grenadines", 
  "Trinidad and Tobago", "Ethiopia", "Sao Tome and Principe", "Bahamas", 
  "Pakistan", "Sudan", "Sint Maarten (Dutch part)", "Turks and Caicos Islands", 
  "Togo", "Cayman Islands", "Bermuda", "Dominica", "Anguilla", "Cabo Verde", 
  "Mauritius"
)

last_dons_raw17 <- last_dons_raw16 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Dengue - Global situation") & Date == "2023-12-21", length(Dengue_Global_situation_countries_20231221), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Dengue - Global situation") & Date == "2023-12-21" & row_number() <= length(Dengue_Global_situation_countries_20231221) ~ Dengue_Global_situation_countries_20231221[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Acute hepatitis of unknown aetiology in children - Multi-country
Acute_hepatitis_unknown_aetiology <- c(
  "Argentina", "Austria", "Belgium", "Bulgaria", "Canada", "Cyprus", "Denmark", "France", "Greece", "Indonesia", 
  "Ireland", "Israel", "Italy", "Japan", "Maldives", "Mexico", "Moldova Republic of", "Netherlands", "Norway",
  "Palestine State of", "Panama", "Poland", "Portugal", "Korea Republic of", "Romania", "Serbia", "Singapore",
  "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom of Great Britain and Northern Ireland", "United States of America"
)

last_dons_raw18 <- last_dons_raw17 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Acute hepatitis of unknown aetiology in children - Multi-country"), length(Acute_hepatitis_unknown_aetiology), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Acute hepatitis of unknown aetiology in children - Multi-country") & row_number() <= length(Acute_hepatitis_unknown_aetiology) ~ Acute_hepatitis_unknown_aetiology[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Cholera – Global situation  
Cholera_Global_situation_2022_12_16 <- c(
  "Burkina Faso", "Niger", "Nigeria", "Benin", "Cameroon", "South Sudan", "Ethiopia", "Yemen", "Congo Democratic Republic of the",
  "Burundi", "Kenya", "Somalia", "Tanzania United Republic of", "Malawi", "Zambia", "Zimbabwe", "Mozambique", 
  "Lebanon", "Iraq", "Syrian Arab Republic", "Iran (Islamic Republic of)", "Pakistan", "Afghanistan", "India", "Nepal", "Bangladesh", "China", "Philippines"
)

last_dons_raw19 <- last_dons_raw18 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Cholera – Global situation") & Date == "2022-12-16", length(Cholera_Global_situation_2022_12_16), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Cholera – Global situation") & Date == "2022-12-16" & row_number() <= length(Cholera_Global_situation_2022_12_16) ~ Cholera_Global_situation_2022_12_16[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse() 

# Cholera – Global situation  
Cholera_Global_situation_2023_02_11 <- c(
  "Burundi", "Cameroon", "Congo Democratic Republic of the", "Ethiopia", "Kenya", "Malawi", "Mozambique", "Nigeria", "Zambia", 
  "Haiti", "Dominican Republic", 
  "Afghanistan", "Lebanon", "Pakistan", "Somalia", "Syrian Arab Republic", 
   "Bangladesh", "Philippines"
)

last_dons_raw20 <- last_dons_raw19 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Cholera – Global situation") & Date == "2023-02-11", length(Cholera_Global_situation_2023_02_11), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Cholera – Global situation") & Date == "2023-02-11" & row_number() <= length(Cholera_Global_situation_2023_02_11) ~ Cholera_Global_situation_2023_02_11[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse() 

# Dengue – the Region of the Americas
Dengue_Region_Americas <- c(
  "Brazil", "Peru", "Bolivia (Plurinational State of)", "Argentina", "Nicaragua", "Colombia", 
  "Mexico", "Paraguay", "Honduras", "Ecuador", "Venezuela (Bolivarian Republic of)", 
  "Guatemala", "Panama", "Costa Rica"
)

last_dons_raw21 <- last_dons_raw20 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Dengue – the Region of the Americas"), length(Dengue_Region_Americas), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Dengue – the Region of the Americas") & row_number() <= length(Dengue_Region_Americas) ~ Dengue_Region_Americas[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Enterovirus-Echovirus 11 Infection - the European Region
Enterovirus_Echovirus_European_Region <- c(
  "France", "Croatia", "Italy", "Spain", "Sweden", "United Kingdom of Great Britain and Northern Ireland"
)

last_dons_raw22 <- last_dons_raw21 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Enterovirus-Echovirus 11 Infection - the European Region"), length(Enterovirus_Echovirus_European_Region), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Enterovirus-Echovirus 11 Infection - the European Region") & row_number() <= length(Enterovirus_Echovirus_European_Region) ~ Enterovirus_Echovirus_European_Region[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Geographical expansion of cases of dengue and chikungunya beyond the historical areas of transmission in the Region of the Americas
Chikungunya_Americas <- c(
  "Argentina", "Bolivia (Plurinational State of)", "Brazil", "Paraguay", "Peru"
)

last_dons_raw23 <- last_dons_raw22 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Geographical expansion of cases of dengue and chikungunya") & icd104n == "Chikungunya virus disease", length(Chikungunya_Americas), 1)) |>
  uncount(repeated_row) |>
  group_by(ID, icd104n) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Geographical expansion of cases of dengue and chikungunya beyond") & icd104n == "Chikungunya virus disease" & row_number() <= length(Chikungunya_Americas) ~ Chikungunya_Americas[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

Dengue_Americas <- c(
  "Bolivia (Plurinational State of)", "Paraguay", "Peru"
)

last_dons_raw24 <- last_dons_raw23 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Geographical expansion of cases of dengue and chikungunya beyond the historical areas of transmission in the Region of the Americas") & icd104n == "Dengue, unspecified", length(Dengue_Americas), 1)) |>
  uncount(repeated_row) |>
  group_by(ID, icd104n) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Geographical expansion of cases of dengue and chikungunya beyond the historical areas of transmission in the Region of the Americas") & icd104n == "Dengue, unspecified" & row_number() <= length(Dengue_Americas) ~ Dengue_Americas[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Iatrogenic Botulism- European Region
Botulism_European_region <- c(
  "Turkey", "Austria", "Germany", "Switzerland"
)

last_dons_raw25 <- last_dons_raw24 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Iatrogenic Botulism- European Region"), length(Botulism_European_region), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Iatrogenic Botulism- European Region") & row_number() <= length(Botulism_European_region) ~ Botulism_European_region[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Increased incidence of scarlet fever and invasive Group A Streptococcus infection - multi-country
Streptococcus_infection_multi <- c(
  "France", "Ireland", "Netherlands", "Sweden", "United Kingdom of Great Britain and Northern Ireland"
)

last_dons_raw26 <- last_dons_raw25 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Increased incidence of scarlet fever and invasive Group A Streptococcus infection - multi-country") & icd104n == "Sepsis due to streptococcus, group A", length(Streptococcus_infection_multi), 1)) |>
  uncount(repeated_row) |>
  group_by(ID, icd104n) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Increased incidence of scarlet fever and invasive Group A Streptococcus infection - multi-country") & icd104n == "Sepsis due to streptococcus, group A" & row_number() <= length(Streptococcus_infection_multi) ~ Streptococcus_infection_multi[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

scarlet_multi <- c(
  "France", "United Kingdom of Great Britain and Northern Ireland"
)

last_dons_raw27 <- last_dons_raw26 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Increased incidence of scarlet fever and invasive Group A Streptococcus infection - multi-country") & icd104n == "Scarlet fever", length(scarlet_multi), 1)) |>
  uncount(repeated_row) |>
  group_by(ID, icd104n) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Increased incidence of scarlet fever and invasive Group A Streptococcus infection - multi-country") & icd104n == "Scarlet fever" & row_number() <= length(scarlet_multi) ~ scarlet_multi[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Multi-country monkeypox outbreak in non-endemic countries
Multi_monkeypox_2022_05_21 <- c(
  "Cameroon", "Canada", "Central African Republic", "Congo", 
  "Costa Rica", "Ghana", "Nigeria"
)

last_dons_raw28 <- last_dons_raw27 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Multi-country monkeypox outbreak in non-endemic countries") & Date == "2022-05-21", length(Multi_monkeypox_2022_05_21), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Multi-country monkeypox outbreak in non-endemic countries") & Date == "2022-05-21" & row_number() <= length(Multi_monkeypox_2022_05_21) ~ Multi_monkeypox_2022_05_21[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Multi-country monkeypox outbreak in non-endemic countries: Update
Multi_monkeypox_2022_05_29 <- c(
  "Argentina", "Canada", "French Guiana", "United States of America", "United Arab Emirates", "Sudan",
  "Austria", "Belgium", "Czechia", "Denmark", "Finland", "France", "Germany", "Israel", "Italy", "Netherlands", 
  "Portugal", "Slovenia", "Spain", "Sweden", "Slovakia", "Switzerland", "United Kingdom of Great Britain and Northern Ireland"
)

last_dons_raw29 <- last_dons_raw28 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Multi-country monkeypox outbreak in non-endemic countries: Update") & Date == "2022-05-29", length(Multi_monkeypox_2022_05_29), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Multi-country monkeypox outbreak in non-endemic countries: Update") & Date == "2022-05-29" & row_number() <= length(Multi_monkeypox_2022_05_29) ~ Multi_monkeypox_2022_05_29[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Multi-country monkeypox outbreak: situation update
Multi_monkeypox_2022_06_04 <- c(
  "Argentina", "Canada", "Mexico", "United States of America", "Morocco", 
  "United Arab Emirates", "Austria", "Belgium", "Czechia", "Denmark", 
  "Finland", "France", "Germany", "Hungary", "Ireland", "Israel", 
  "Italy", "Malta", "Netherlands", "Norway", "Portugal", "Slovenia", 
  "Spain", "Sweden", "Switzerland", 
  "United Kingdom of Great Britain and Northern Ireland", "Australia"
)

last_dons_raw30 <- last_dons_raw29 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Multi-country monkeypox outbreak: situation update") & Date == "2022-06-04", length(Multi_monkeypox_2022_06_04), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Multi-country monkeypox outbreak: situation update") & Date == "2022-06-04" & row_number() <= length(Multi_monkeypox_2022_06_04) ~ Multi_monkeypox_2022_06_04[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Multi-country monkeypox outbreak: situation update
Multi_monkeypox_2022_06_10 <- c(
  "Argentina", "Canada", "Mexico", "United States of America", 
  "Morocco", "United Arab Emirates", "Austria", "Belgium", 
  "Czechia", "Denmark", "Finland", "France", "Germany", 
  "Hungary", "Ireland", "Israel", "Italy", "Latvia", "Malta", 
  "Netherlands", "Norway", "Portugal", "Slovenia", "Spain", 
  "Sweden", "Switzerland", "United Kingdom of Great Britain and Northern Ireland", "Australia"
)

last_dons_raw31 <- last_dons_raw30 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Multi-country monkeypox outbreak: situation update") & Date == "2022-06-10", length(Multi_monkeypox_2022_06_10), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Multi-country monkeypox outbreak: situation update") & Date == "2022-06-10" & row_number() <= length(Multi_monkeypox_2022_06_10) ~ Multi_monkeypox_2022_06_10[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Multi-country monkeypox outbreak: situation update
Multi_monkeypox_2022_06_17 <- c(
  "Cameroon", "Central African Republic", "Congo", "Congo Democratic Republic of the", 
  "Ghana", "Nigeria", "Argentina", "Brazil", "Canada", "Mexico", 
  "United States of America", "Venezuela (Bolivarian Republic of)", "Morocco", 
  "United Arab Emirates", "Austria", "Belgium", "Czechia", "Denmark", 
  "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", 
  "Iceland", "Ireland", "Israel", "Italy", "Malta", "Netherlands", 
  "Norway", "Poland", "Portugal", "Romania", "Slovenia", "Spain", 
  "Sweden", "Switzerland", "United Kingdom of Great Britain and Northern Ireland", "Australia"
)

last_dons_raw32 <- last_dons_raw31 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Multi-country monkeypox outbreak: situation update") & Date == "2022-06-17", length(Multi_monkeypox_2022_06_17), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Multi-country monkeypox outbreak: situation update") & Date == "2022-06-17" & row_number() <= length(Multi_monkeypox_2022_06_17) ~ Multi_monkeypox_2022_06_17[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Multi-country monkeypox outbreak: situation update
Multi_monkeypox_2022_06_27 <- c(
  "Benin", "Italy", "Congo", "Ghana", "Cameroon", "Central African Republic", 
  "Congo Democratic Republic of the", "Nigeria", "South Africa", "Argentina", 
  "Brazil", "Canada", "Mexico", "United States of America", 
  "Venezuela (Bolivarian Republic of)", "Lebanon", "Morocco", "United Arab Emirates", 
  "Austria", "Belgium", "Czechia", "Denmark", "Finland", "France", 
  "Georgia", "Germany", "Gibraltar", "Greece", "Hungary", "Iceland", 
  "Ireland", "Israel", "Luxembourg", "Malta", "Netherlands", 
  "Norway", "Poland", "Portugal", "Romania", "Serbia", "Slovenia", 
  "Spain", "Sweden", "Switzerland", "United Kingdom of Great Britain and Northern Ireland", "Australia", 
  "Korea Republic of", "Singapore"
)

last_dons_raw33 <- last_dons_raw32 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Multi-country monkeypox outbreak: situation update") & Date == "2022-06-27", length(Multi_monkeypox_2022_06_27), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Multi-country monkeypox outbreak: situation update") & Date == "2022-06-27" & row_number() <= length(Multi_monkeypox_2022_06_27) ~ Multi_monkeypox_2022_06_27[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Multi-country outbreak of Salmonella Typhimurium linked to chocolate products – Europe and the United States of America
Multi_salmonella_chocolate <- c(
  "Belgium", "France", "Germany", "Ireland", "Luxembourg", 
  "Netherlands", "Norway", "Spain", "Sweden", 
  "United States of America", "United Kingdom of Great Britain and Northern Ireland"
)

last_dons_raw34 <- last_dons_raw33 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Multi-country outbreak of Salmonella Typhimurium linked to chocolate products – Europe and the United States of America"), length(Multi_salmonella_chocolate), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Multi-country outbreak of Salmonella Typhimurium linked to chocolate products – Europe and the United States of America") & row_number() <= length(Multi_salmonella_chocolate) ~ Multi_salmonella_chocolate[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Outbreak of suspected fungal meningitis associated with surgical procedures performed under spinal anaesthesia – the United States of America and Mexico
fungal_mex_eu <- c(
  "Mexico", "United States of America"
)

last_dons_raw35 <- last_dons_raw34 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Outbreak of suspected fungal meningitis associated with surgical procedures performed under spinal anaesthesia – the United States of America and Mexico"), length(fungal_mex_eu), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Outbreak of suspected fungal meningitis associated with surgical procedures performed under spinal anaesthesia – the United States of America and Mexico") & row_number() <= length(fungal_mex_eu) ~ fungal_mex_eu[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Severe acute hepatitis of unknown aetiology in children - Multi-country
Acute_hepatitis_unknown_aetiology_2022_06_24 <- c(
  "Canada", "Mexico", "United States of America", "Panama", "Colombia", "Brazil", "Argentina",
  "Norway", "Sweden", "United Kingdom of Great Britain and Northern Ireland", "Netherlands", "Denmark", 
  "Latvia", "Ireland", "France", "Austria", "Poland", "Belgium", 
  "Moldova Republic of", "Portugal", "Spain", "Serbia", 
  "Bulgaria", "Greece", "Cyprus", "Palestine State of", 
  "Israel", "Qatar", "Italy", "Maldives", "Singapore", "Indonesia", "Japan"
)

last_dons_raw36 <- last_dons_raw35 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Severe acute hepatitis of unknown aetiology in children - Multi-country") & Date == "2022-06-24", length(Acute_hepatitis_unknown_aetiology_2022_06_24), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Severe acute hepatitis of unknown aetiology in children - Multi-country") & Date == "2022-06-24" & row_number() <= length(Acute_hepatitis_unknown_aetiology_2022_06_24) ~ Acute_hepatitis_unknown_aetiology_2022_06_24[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Severe acute hepatitis of unknown aetiology in children - Multi-country
Acute_hepatitis_unknown_aetiology_2022_07_12 <- c(
  "Canada", "Mexico", "United States of America", "Costa Rica", "Panama", "Colombia", "Brazil", "Argentina",
  "Norway", "Sweden", "United Kingdom of Great Britain and Northern Ireland", "Netherlands", "Denmark", 
  "Latvia", "Ireland", "France", "Austria", "Poland", 
  "Moldova Republic of", "Portugal", "Spain", "Serbia", 
  "Bulgaria", "Greece", "Cyprus", "Palestine State of", 
  "Israel", "Qatar", "Italy", "Maldives", "Singapore", "Indonesia", "Japan"
)

last_dons_raw37 <- last_dons_raw36 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Severe acute hepatitis of unknown aetiology in children - Multi-country") & Date == "2022-07-12", length(Acute_hepatitis_unknown_aetiology_2022_07_12), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Severe acute hepatitis of unknown aetiology in children - Multi-country") & Date == "2022-07-12" & row_number() <= length(Acute_hepatitis_unknown_aetiology_2022_07_12) ~ Acute_hepatitis_unknown_aetiology_2022_07_12[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Severe acute hepatitis of unknown origin in children - Multicountry
Acute_hepatitis_unknown_aetiology_2022_04_23 <- c(
  "United States of America", 
  "Norway", "Ireland", "United Kingdom of Great Britain and Northern Ireland", "Denmark", 
  "Netherlands", "Belgium", "Spain", "France", "Italy", "Romania", "Israel"
)

last_dons_raw38 <- last_dons_raw37 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Severe acute hepatitis of unknown origin in children - Multicountry") & Date == "2022-04-23", length(Acute_hepatitis_unknown_aetiology_2022_04_23), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Severe acute hepatitis of unknown origin in children - Multicountry") & Date == "2022-04-23" & row_number() <= length(Acute_hepatitis_unknown_aetiology_2022_04_23) ~ Acute_hepatitis_unknown_aetiology_2022_04_23[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Yellow fever - African Region (AFRO)
Yellow_fever_African_Region_AFRO_countries_2023_01_03 <- c(
  "Cameroon", "Central African Republic", "Chad", "Côte d'Ivoire", 
  "Congo Democratic Republic of the", "Ghana", "Kenya", "Niger", 
  "Nigeria", "Congo", "Sierra Leone", "Uganda"
)

last_dons_raw39 <- last_dons_raw38 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Yellow fever") & Date == "2023-01-03", length(Yellow_fever_African_Region_AFRO_countries_2023_01_03), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Yellow fever") & Date == "2023-01-03" & row_number() <= length(Yellow_fever_African_Region_AFRO_countries_2023_01_03) ~ Yellow_fever_African_Region_AFRO_countries_2023_01_03[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

# Yellow fever - African Region (AFRO)
Yellow_fever_African_Region_AFRO_countries_2022_09_02 <- c(
  "Cameroon", "Central African Republic", "Chad", "Côte d'Ivoire", 
  "Congo Democratic Republic of the", "Ghana", "Kenya", "Niger", 
  "Nigeria", "Congo", "Sierra Leone", "Uganda"
)

last_dons_raw40 <- last_dons_raw39 |> 
  mutate(repeated_row = ifelse(grepl(Outbreak, pattern = "Yellow fever") & Date == "2022-09-02", length(Yellow_fever_African_Region_AFRO_countries_2022_09_02), 1)) |>
  uncount(repeated_row) |>
  group_by(ID) |>
  mutate(Country = case_when(
    grepl(Outbreak, pattern = "Yellow fever") & Date == "2022-09-02" & row_number() <= length(Yellow_fever_African_Region_AFRO_countries_2022_09_02) ~ Yellow_fever_African_Region_AFRO_countries_2022_09_02[row_number()],
    TRUE ~ Country
  )) |>
  ungroup() |>
  glimpse()

## Adding iso country names and codes
last_dons_raw41 <- last_dons_raw40 |> 
  plyr::join(iso, type = "left", by = "Country", match = "all") |>
# Update Country, iso2, and iso3 for Palestine
  mutate(
    iso2 = case_when(Country == "Gaza" ~ "PS",
                     TRUE ~ iso2),
    Country = case_when(Country == "Gaza" ~ "Palestine State of", 
                        TRUE ~ Country),
    iso3 = case_when(Country == "Palestine State of" ~ "PSE", 
                     TRUE ~ iso3)
  )

# Only DONs related to diseases, i.e. deleting those with information on events, rules, recommendations, etc.
# Antimicrobial Resistance, Hypervirulent Klebsiella pneumoniae - Global situation
last_dons_raw42 <- last_dons_raw41 |> 
  filter(Outbreak != "Antimicrobial Resistance, Hypervirulent Klebsiella pneumoniae - Global situation")

## creating a key to identify unique outbreaks by disease, year, and country
last_dons_raw43 <- last_dons_raw42 |>
  mutate(key = paste0(iso3, Year, icd104c)) |>
  group_by(key) |>
  mutate(DONs = paste(ID, collapse = ", ")) |> # All DONs by outbreak
  ungroup()

#### All DON's (including duplicates if there are multiple diseases of countries reported in one single DON)
# joining previous DONs 
last_dons_all <- last_dons_raw43 |>
  select(Country, iso2, iso3, Year, icd10n, icd103n, icd104n, icd10c, icd103c, icd104c, 
         icd11c1, icd11c2, icd11c3, icd11l1, icd11l2, icd11l3, Disease, DONs, Definition) |>
  rbind(prev_dons_all)

#### Unique cases per country per year ####
# joining last unique outbreaks with all previous DONs
last_dons_unique1 <- last_dons_raw43 |>
  select(Country, iso2, iso3, Year, icd10n, icd103n, icd104n, icd10c, icd103c, icd104c, 
         icd11c1, icd11c2, icd11c3, icd11l1, icd11l2, icd11l3, Disease, DONs, Definition) |>
  distinct()

last_dons_unique <- last_dons_unique1 |>
  rbind(prev_dons_unique) |>
  distinct() # Apply again distinct()! 

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
covid_dashboard <- read.csv("covid19_who_dashboard/WHO-COVID-19-global-daily-data.csv")

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
covid_dashboard <- covid_dashboard %>%
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
covid_unique <- covid_dashboard %>%
  select(all_of(colnames(last_dons_unique))) %>%
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
outbreaks <- outbreaks %>%
  mutate(
    Country = case_when(iso2 == "BQ" ~ "Bonaire Sint Eustatius and Saba",
                        TRUE ~ Country),
    iso2 = case_when(iso2 == "BQ" ~ "BQ",
                     TRUE ~ iso2),
    iso3 = case_when(iso2 == "BQ" ~ "BES",
                     TRUE ~ iso3)
  )

# Update Country, iso2, and iso3 for Kosovo
outbreaks <- outbreaks %>%
  mutate(
    iso2 = case_when(Country == "Kosovo" ~ "XK", 
                     TRUE ~ iso2),
    iso3 = case_when(Country == "Kosovo" ~ "XXK",
                     TRUE ~ iso3)
  )

# Update Country, iso2, and iso3 for Palestine (to include Gaza)
outbreaks <- outbreaks %>%
  mutate(
    iso2 = case_when(Country == "Gaza" ~ "PS",
                     TRUE ~ iso2),
    Country = case_when(Country == "Gaza" ~ "Palestine State of", 
                        TRUE ~ Country),
    iso3 = case_when(Country == "Palestine State of" ~ "PSE", 
                     TRUE ~ iso3)
  )

# Filter out rows where Country is NA
outbreaks <- outbreaks %>%
  filter(!is.na(Country))

# Check the number of unique countries and diseases
nlevels(factor(outbreaks$Country))
nlevels(factor(outbreaks$icd104n))

# Trim whitespace from all character columns
outbreaks <- outbreaks %>%
  mutate(across(where(is.character), ~ trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")))

### saving new outbreaks dataset
save(outbreaks, file = paste0("Last update/", "outbreaks_", max_dons_date_last, ".RData"))
write.csv(outbreaks, file = paste0("Last update/", "outbreaks_", max_dons_date_last, ".csv"))
