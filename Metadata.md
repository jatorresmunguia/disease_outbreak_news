# Metadata 

The compiled database contains datasets and code in R language. Following the Fields of Research metadata schema from Figshare, the following files can be found in the repository:

### 1. **dons_raw_<DATE>.RData** and **dons_raw_<DATE>.csv** (previously **DONsRaw.RData** and **DONsRaw.csv**)

- **Categories**: Statistics, Epidemiology, Diseases
- **Item type**: Dataset
- **Keywords**: DONs, Coronavirus Dashboard, WHO, Global database
- **Description**: Dataset containing the raw data from all the DONs as extracted from www.who.int/emergencies/disease-outbreak-news. The dataset presents information from January 1996 to the <DATE> of last update. Each line represents a DON.

**Data records**:
- **ID**: DON unique identifier.
- **Description**: Name of the DON.
- **Date**: Date of registry.
- **Link**: Link to the website for more details.

### 2. **dons_all_<DATE>.RData** and **dons_all_<DATE>.csv** (previously **AllDONs.RData** and **AllDONs.csv**)

- **Categories**: Statistics, Epidemiology, Diseases
- **Item type**: Dataset
- **Keywords**: DONs, Coronavirus Dashboard, WHO, Global database
- **Description**: Dataset containing the DONs as extracted from www.who.int/emergencies/disease-outbreak-news. The dataset presents information from January 1996 to the <DATE> of last update. In this dataset, a row is added for each disease and country reported per DON. If a specific DON includes more than one disease for a country, an additional row is added for each disease. Similarly, if a DON reports a specific disease in multiple countries, a new row is added for each country where the disease was reported.

**Data records**:
- **ID**: DON unique identifier.
- **Description**: Name of the DON.
- **Date**: Date of registry.
- **Link**: Link to the website for more details.

### 3. **dons_unique_<DATE>.RData** and **dons_unique_<DATE>.csv** (previously **UniqueDONs.RData** and **UniqueDONs.csv**)

- **Categories**: Statistics, Epidemiology, Diseases
- **Item type**: Dataset
- **Keywords**: DONs, Coronavirus Dashboard, WHO, Global database
- **Description**: Dataset of unique disease outbreaks. An outbreak in this dataset is defined as the occurrence of at least one case of a specific disease -excluding COVID-19- in a country -or territory- during a particular year.

**Data records:**
- **Country**: Name of the country where the outbreak occurred.
- **iso2**: Alpha-2 country code from the ISO 3166.
- **iso3**: Alpha-3 country code from the ISO 3166.
- **Year**: Year of occurrence of the outbreak.
- **icd10n, icd103n, icd104n**: Name of disease types, subtypes, and names according to the ICD-10.
- **icd10c, icd103c, icd104c**: Codes for disease types, subtypes, and names according to the ICD-10.
- **icd11c1, icd11c2, icd11c3**: Codes for disease types, subtypes, and names according to the ICD-11.
- **icd11l1, icd11l2, icd11l3**: Names of disease types, subtypes, and names according to the ICD-11.
- **Disease**: Name of the disease.
- **DONs**: List of DONs reporting the outbreak.
- **Definition**: Definition of the disease according to the ICD-11.

### 4. **covid_unique_<DATE>.RData** and **covid_unique_<DATE>.csv** (previously **COVIDOutbreaks.RData** and **COVIDOutbreaks.csv**))

- **Categories**: Statistics, Epidemiology, Diseases
- **Item type**: Dataset
- **Keywords**: DONs, Coronavirus Dashboard, WHO, Global database
- **Description**: Dataset covering COVID-19 outbreaks between 2020 and March 2022. An outbreak in this dataset is defined as the occurrence of at least one case of COVID-19 in a country -or territory- during a particular year.

**Data records:**
- **Country**: Name of the country where the outbreak occurred.
- **iso2**: Alpha-2 country code from the ISO 3166.
- **iso3**: Alpha-3 country code from the ISO 3166.
- **Year**: Year of occurrence of the outbreak.
- **icd10n, icd103n, icd104n**: Name of disease types, subtypes, and names according to the ICD-10.
- **icd10c, icd103c, icd104c**: Codes for disease types, subtypes, and names according to the ICD-10.
- **icd11c1, icd11c2, icd11c3**: Codes for disease types, subtypes, and names according to the ICD-11.
- **icd11l1, icd11l2, icd11l3**: Names of disease types, subtypes, and names according to the ICD-11.
- **Disease**: Name of the disease.
- **DONs**: List of DONs reporting the outbreak.
- **Definition**: Definition of the disease according to the ICD-11.

### 5. **outbreaks_<DATE>.RData** and **outbreaks_<DATE>.csv** (previously **Outbreaks.RData** and **Outbreaks.csv**))

- **Categories**: Statistics, Epidemiology, Diseases
- **Item type**: Dataset
- **Keywords**: DONs, Coronavirus Dashboard, WHO, Global database
- **Description**: Dataset containing unique disease outbreaks from January 1996 to the <DATE> of last update. An outbreak in this dataset is defined as the occurrence of at least one case of a specific disease in a country -or territory- during a particular year.

**Data records:**
- **Country**: Name of the country where the outbreak occurred.
- **iso2**: Alpha-2 country code from the ISO 3166.
- **iso3**: Alpha-3 country code from the ISO 3166.
- **Year**: Year of occurrence of the outbreak.
- **icd10n, icd103n, icd104n**: Name of disease types, subtypes, and names according to the ICD-10.
- **icd10c, icd103c, icd104c**: Codes for disease types, subtypes, and names according to the ICD-10.
- **icd11c1, icd11c2, icd11c3**: Codes for disease types, subtypes, and names according to the ICD-11.
- **icd11l1, icd11l2, icd11l3**: Names of disease types, subtypes, and names according to the ICD-11.
- **Disease**: Name of the disease.
- **DONs**: List of DONs reporting the outbreak.
- **Definition**: Definition of the disease according to the ICD-11.
