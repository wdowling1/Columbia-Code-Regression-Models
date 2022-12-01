## GIS Data cleaning

setwd("/Users/Will/Desktop/QMSS/GIS/Research Project")

install.packages("stringr")
library(stringr)

## DW Nominate Score file
# Padding Strings
dw_nom$district_code <- str_pad(dw_nom$district_code, 2, pad = "0")
dw_nom$state_icpsr <- str_pad(dw_nom$state_icpsr, 2, pad = "0")

# Adding GEOID column
dw_nom <- dw_nom %>% mutate(geoid = paste(state_icpsr, district_code, sep = ""))

# Filtering for contig US states
unique(dw_nom$state_abbrev)
dw_nom <- filter(dw_nom, state_abbrev != "AK" & state_abbrev != "PR" & state_abbrev != "MP" & state_abbrev != "DC" & state_abbrev != "HI" & state_abbrev != "USA" & state_abbrev != "VI" & state_abbrev != "GU" & state_abbrev != "AS")

# Filtering for 116 congress
dw_nom_116 <- filter(dw_nom, congress == 116)

# Save out
install.packages("xlsx")
library(xlsx)
openxlsx::write.xlsx(x = dw_nom, file = "DW Nominate Cumulative.xlsx")

openxlsx::write.xlsx(x = dw_nom_116, file = "116 DW Nominate Scores.xlsx")

## House Vote Totals File
# Padding Strings
house_df$district <- str_pad(house_df$district, 2, pad = "0")
house_df$state_ic <- str_pad(house_df$state_ic, 2, pad = "0")

# Adding GEOID column
house_df <- house_df %>% mutate(geoid = paste(state_ic, district, sep = ""))

# Filtering States
house_df <- filter(house_df, state_po != "AK" & state_po != "DC" & state_po != "HI")
house_df <- filter(house_df, party %in% c("DEMOCRAT", "REPUBLICAN", "INDEPENDENT"))

# Filtering for 116 congress (2018 and 2020 elections)
house_df <- filter(house_df, year %in% c("2018"))

# Selecting only those candidates who won their districts
house_df_top <- house_df %>%
  group_by(geoid) %>%
  slice(which.max(Vote_share))

# Creating two dfs: one for dem and one for rep
df_dem <- filter(df, party == "DEMOCRAT")
df_rep <- filter(df, party == "REPUBLICAN")

# Save out
openxlsx::write.xlsx(x = df, file = "116 House Election Results.xlsx")
openxlsx::write.xlsx(x = df_dem, file = "116 House Election Results_dem.xlsx")
openxlsx::write.xlsx(x = df_rep, file = "116 House Election Results_rep.xlsx")


## Merged Data
# Padding Strings
df$state_fips <- str_pad(df$state_fips, 3, pad = "0")
df$district_code <- str_pad(df$district_code, 3, pad = "0")

# Adding ID column
df <- df %>% mutate(id = paste(state_fips, stcong, endcong, district_code, sep = ""))


