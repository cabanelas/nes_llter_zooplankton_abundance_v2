################################################################################
#############     Zooplankton Abundance DATA Update EDI           ##############
#############             AUG-2024                 #############################
## by: Alexandra Cabanelas 
################################################################################
# Merging zooplankton abundance files 
# First, start by merging STAGED data sent to me by Harvey Aug 2024 and 
#STAGED summer 2023 (that has EN695) and STAGED data from GoogleDrive

# then, extract abundance UNSTAGED data from GoogleDrive
#and then combines STAGED and UNSTAGED data

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(here)
library(tidyverse)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##
latest <- read.csv(file.path("raw",
                             "LTER_cruises_Stage_Concentration_15Aug2024_FIXED.csv"),
                   header = T)
# data Harvey W. sent me 15AUG2024

staged23 <- read.csv(file.path("raw",
                               "LTER_ZooData_21July2023.csv"),
                     header = T)
# contains data for EN695 [the original included more cruises but those are other
#cruises were sent to me to the LTER_cruises_Stage_concentration_...above]

#merge
together <- rbind(latest,staged23)

#add TOTCNT column
together <- together %>%
  group_by(CRUISE_NAME, STATION, CAST) %>%
  mutate(TOTCNT = sum(ZOOPLANKTON_COUNT, na.rm = TRUE)) %>%
  ungroup() %>%
  relocate(TOTCNT, .after = ZOOPLANKTON_COUNT)
#select(CRUISE_NAME, STATION, CAST, ZOOPLANKTON_COUNT, TOTCNT, everything())

#write.csv(together, "output/abundance_staged_NESLTER_aug24.csv")


## ------------------------------------------ ##
#          STAGED  Google drive data -----
## ------------------------------------------ ##

file_list <- list.files(path = "raw/NES_noaa_staged_clean", 
                        pattern = "\\.csv$", 
                        full.names = TRUE)
# this is staged data from the google drive 

data_frames <- lapply(file_list, read.csv)

names(data_frames) <- gsub("\\.csv$", "", basename(file_list))
list2env(data_frames, envir = .GlobalEnv)
combined_data <- do.call(rbind, data_frames)
rownames(combined_data) <- NULL

#write.csv(combined_data, "output/zoop_abund_gdrive_extracted.csv")

## ------------------------------------------ ##
#          UNSTAGED  Google drive data -----
## ------------------------------------------ ##
file_list_unstaged <- list.files(path = "raw/NES_noaa_unstaged_clean", 
                                 pattern = "\\.csv$", full.names = TRUE)

data_frames_unstaged <- lapply(file_list_unstaged, read.csv)

names(data_frames_unstaged) <- gsub("\\.csv$", "", basename(file_list_unstaged))
list2env(data_frames_unstaged, envir = .GlobalEnv)
combined_data_unstaged <- do.call(rbind, data_frames_unstaged)
rownames(combined_data_unstaged) <- NULL

#write.csv(combined_data_unstaged, "output/all_unstaged_googleDrive.csv")

## ------------------------------------------ ##
#       Merging Unstaged and Staged df -----
## ------------------------------------------ ##
### These are the zp files off google drive
# merging the staged and unstaged files

## ------------------------------------------ ##
#            Data to merge -----
## ------------------------------------------ ##
staged <- read.csv(file.path("output","zoop_abund_gdrive_extracted.csv"),
                   header = T)
# staged data from google drive - created in merge_data.R script 

unstaged <- read.csv(file.path("output","all_unstaged_googleDrive.csv"),
                     header = T)

combined <- rbind(staged, unstaged)

# add missing values per cruise, station, cast 
combined_sorted <- combined %>%
  arrange(CRUISE_NAME, STATION, CAST) #rows are ordered by these columns

#fills missing values by looking both downwards and upwards within each group 
#filling gaps based on available values within each group
#this is because the rows from the unstaged data have missing values 
combined_filled <- combined_sorted %>%
  group_by(CRUISE_NAME, STATION, CAST) %>% 
  fill(LATITUDE, LONGITUDE, SAMPLE_SPLIT_FACTOR, EVENT_DATE, TIME, DAY, MONTH,
       YEAR, HOUR, MINUTE, DEPTH, NET_MAX_DEPTH, 
       .direction = "downup")

#data type (class) of each column
sapply(combined_filled, class)

# fix the ones that are logical
combined_filled <- combined_filled %>%
  mutate(across(ZOOPLANKTON_COUNT:ZOOSTG_999, as.integer))

# check if all specified columns are consistent within each group
inconsistencies <- combined_filled %>%
  group_by(CRUISE_NAME, STATION, CAST) %>%
  summarize( #checking for consistency within each group
    #counts the number of unique (non-missing) values within each group
    #if count = 1, all values for that column within the group are the same = consistency
    LATITUDE_consistent = n_distinct(LATITUDE, na.rm = TRUE) == 1,
    LONGITUDE_consistent = n_distinct(LONGITUDE, na.rm = TRUE) == 1,
    EVENT_DATE_consistent = n_distinct(EVENT_DATE, na.rm = TRUE) == 1,
    TIME_consistent = n_distinct(TIME, na.rm = TRUE) == 1,
    DAY_consistent = n_distinct(DAY, na.rm = TRUE) == 1,
    MONTH_consistent = n_distinct(MONTH, na.rm = TRUE) == 1,
    YEAR_consistent = n_distinct(YEAR, na.rm = TRUE) == 1,
    HOUR_consistent = n_distinct(HOUR, na.rm = TRUE) == 1,
    MINUTE_consistent = n_distinct(MINUTE, na.rm = TRUE) == 1,
    DEPTH_consistent = n_distinct(DEPTH, na.rm = TRUE) == 1,
    NET_MAX_DEPTH_consistent = n_distinct(NET_MAX_DEPTH, na.rm = TRUE) == 1
  ) %>%
  filter(
    !LATITUDE_consistent |
      !LONGITUDE_consistent |
      !EVENT_DATE_consistent |
      !TIME_consistent |
      !DAY_consistent |
      !MONTH_consistent |
      !YEAR_consistent |
      !HOUR_consistent |
      !MINUTE_consistent |
      !DEPTH_consistent |
      !NET_MAX_DEPTH_consistent
  )

# view the inconsistencies
print(inconsistencies)

#combined_filtered <- combined_filled %>%
#  filter(!is.na(ZOOPLANKTON_COUNT) & ZOOPLANKTON_COUNT != 0)

#replace NA values with 0 
combined_filled <- combined_filled %>%
  mutate(across(
    c(ZOOPLANKTON_COUNT:ZOOSTG_999, -TOTCNT),  # Include range but exclude ZOOCNT
    ~ replace_na(., 0)
  ))


# fix TOTCNT column
combined_filled <- combined_filled %>%
  group_by(CRUISE_NAME, STATION, CAST) %>%
  mutate(TOTCNT = if_else(is.na(TOTCNT), 
                          sum(ZOOPLANKTON_COUNT, na.rm = TRUE), 
                          TOTCNT)) %>%
  ungroup()

#write.csv(combined_filled, "output/unstaged_staged_merged_filled.csv")


# add data flags (Created manually)
## ------------------------------------------ ##
#            Data Flags -----
## ------------------------------------------ ##
# add data flags to combined_filled
flgs <- read.csv(file.path("raw","gdrive_ManualFlags.csv"),
                 header = T)
flgs_unique <- flgs[!duplicated(flgs[c("CRUISE_NAME", "STATION", "CAST")]), ]


combined_flgs <- merge(combined_filled, flgs_unique, 
                       by = c("CRUISE_NAME", "STATION", "CAST"), 
                       all.x = TRUE)

# rename columns
combined_flgs <- combined_flgs %>%
  rename(
    ZOO_STAGE_000 = ZOOSTG_000,
    ZOO_STAGE_024 = ZOOSTG_024,
    ZOO_STAGE_023 = ZOOSTG_023,
    ZOO_STAGE_022 = ZOOSTG_022,
    ZOO_STAGE_021 = ZOOSTG_021,
    ZOO_STAGE_020 = ZOOSTG_020,
    ZOO_STAGE_030 = ZOOSTG_030,
    ZOO_STAGE_029 = ZOOSTG_029,
    ZOO_STAGE_028 = ZOOSTG_028,
    ZOO_STAGE_013 = ZOOSTG_013,
    ZOO_STAGE_999 = ZOOSTG_999
  )

tog <- bind_rows(together, combined_flgs)

#write.csv(tog, "output/allCruisesTogether.csv")


# bad flowmeter
## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##
file_list_en627 <- list.files(path = "raw/bad_flow", 
                              pattern = "\\.csv$", full.names = TRUE)

data_frames_en627 <- lapply(file_list_en627, read.csv)

names(data_frames_en627) <- gsub("\\.csv$", "", basename(file_list_en627))
list2env(data_frames_en627, envir = .GlobalEnv)
data_en627 <- do.call(rbind, data_frames_en627)
rownames(data_en627) <- NULL

#write.csv(data_en627, "output/EN627_mergedStagedandUnstaged.csv")


# rename and fix columns to match the tog df
data_en627 <- data_en627 %>%
  rename(
    CRUISE_NAME = CRUNAM,
    STATION = STA,
    #CAST
    #LATITUDE 
    #LONGITUDE 
    #GEAR_VOLUME_FILTERED 
    #SAMPLE_SPLIT_FACTOR
    #EVENT_DATE
    #TIME
    #DAY
    #MONTH
    #YEAR
    #HOUR
    #MINUTE
    #DEPTH
    #NET_MAX_DEPTH
    VOLUME_100M3 = VOLSML,
    ZOO_ALIQUOT = ALQFCTR,
    TAXA_004 = PLKTAX_NUM,
    TAXA_NAME = PLKTAX_NAM, 
    #ITIS_TAXON 
    #ITIS_TSN
    #CONC_100M3
    ZOOPLANKTON_COUNT = ZOOCNT,
    #TOTCNT
    ZOO_STAGE_000 = ZOOSTG_000,
    ZOO_STAGE_024 = ZOOSTG_024,
    ZOO_STAGE_023 = ZOOSTG_023,
    ZOO_STAGE_022 = ZOOSTG_022,
    ZOO_STAGE_021 = ZOOSTG_021,
    ZOO_STAGE_020 = ZOOSTG_020,
    ZOO_STAGE_030 = ZOOSTG_030,
    ZOO_STAGE_029 = ZOOSTG_029,
    ZOO_STAGE_028 = ZOOSTG_028,
    ZOO_STAGE_013 = ZOOSTG_013,
    ZOO_STAGE_999 = ZOOSTG_999
  ) %>%
  select(-GERCOD, -BONNUM, -ZOOSTG_054, -ZOOSTG_051, -ZOOSTG_050)

# Add missing columns with NA values
missing_cols <- c("CAST", "LATITUDE", "LONGITUDE", "GEAR_VOLUME_FILTERED", 
                  "SAMPLE_SPLIT_FACTOR", "EVENT_DATE", "TIME", "DAY", "MONTH", 
                  "YEAR", "HOUR", "MINUTE", "DEPTH", "NET_MAX_DEPTH", 
                  "ITIS_TAXON", "ITIS_TSN", "CONC_100M3", "TOTCNT", 
                  "ADULT_CONC", "C5_CONC", "C4_CONC", "C3_CONC", "C2_CONC", 
                  "C1_CONC", "CRYTOPIA_CONC", "FURCILIA_CONC", 
                  "CALYPTOPIS_CONC", "NAUPLIUS_CONC", "UNKNOWN_CONC", 
                  "PRIMARY_FLAG", "SECONDARY_FLAG")

data_en627[missing_cols] <- NA

colnames(tog)
colnames(data_en627)

# Reorder columns to match `tog`
data_en627 <- data_en627 %>%
  select(all_of(colnames(tog)))


sapply(data_en627, class)

data_en627 <- data_en627 %>%
  mutate(across(ZOOPLANKTON_COUNT:ZOO_STAGE_999, as.integer))

#replace NA values with 0 
data_en627 <- data_en627 %>%
  mutate(across(
    c(ZOOPLANKTON_COUNT:ZOO_STAGE_999, -TOTCNT),  # Include range but exclude ZOOCNT
    ~ replace_na(., 0)
  ))


# fix TOTCNT column
data_en627 <- data_en627 %>%
  group_by(CRUISE_NAME, STATION) %>%
  mutate(TOTCNT = if_else(is.na(TOTCNT), 
                          sum(ZOOPLANKTON_COUNT, na.rm = TRUE), 
                          TOTCNT)) %>%
  ungroup()


unique(data_en627$STATION)

#fix Station column
data_en627 <- data_en627 %>%
  mutate(
    # Step 1: Standardize specific entries directly
    STATION = if_else(
      str_detect(STATION, regex("(?i)^lu?\\s*iic$")), 
      "u11c", 
      STATION
    ),
    # Step 2: Remove "L " prefix if it exists (using regex)
    #trim leading/trailing whitespace
    STATION = str_trim(str_replace(STATION, "^L\\s*", "")),
    # Step 3: Remove leading zeros
    STATION = str_replace(STATION, "^0*", "")
  )
unique(data_en627$STATION)


## LAST STEP IS TO MERGE ALL OF THEM
all_zp_data <- rbind(tog, data_en627)

#write.csv(all_zp_data, "output/all_zoop_count_data.csv")
