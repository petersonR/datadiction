################################################### -
## Title: get epa data with RAQSPAI
## Author: Ryan Peterson
## Date Created: Mon Aug 29 15:22:52 2022
################################################### -

library(tidyverse)
library(here)

# install.packages("RAQSAPI", dependencies = T)
# keyring::key_set(service = "AQSDatamart", username = "ryan.a.peterson@cuanschutz.edu")

library(RAQSAPI) 
library(keyring) 

datamartAPI_user <- "ryan.a.peterson@cuanschutz.edu"
server <- "AQSDatamart"

aqs_credentials(username = datamartAPI_user, key = key_get(service = server, username = datamartAPI_user))

years <- 1991:2023

pb <- progress::progress_bar$new(total =length(years), format = " [:bar] :percent eta: :eta", width= 60)

daily_avg <- tibble()

for(y in years) {
  daily <- aqs_dailysummary_by_cbsa("44201", bdate = as.Date(paste0(y, "-01-01")), edate =  as.Date(paste0(y, "-12-31")), cbsa_code = 19740)
  
  daily_avg <- daily %>% 
    filter(pollutant_standard == "Ozone 8-Hour 2008") %>% 
    group_by(date_local) %>% 
    summarize(daily_avg = mean(arithmetic_mean), n_obs = n(), n_sites = length(unique(site_number))) %>% 
    bind_rows(daily_avg)
  
  Sys.sleep(1)
  pb$tick()
}

dts <- tibble(date_local = seq(min(as.Date(daily_avg$date_local)), max(as.Date(daily_avg$date_local)), by = 1))

try1 <- left_join(dts, mutate(daily_avg, date_local = as.Date(date_local)))

try1 %>% 
  arrange(date_local) %>% 
  write_csv(file = "~/OneDrive - The University of Colorado Denver/Websites/blog/peterson_blog/posts/did-denver-zero-fare-policy-work/ozone_data-91-23.csv")


