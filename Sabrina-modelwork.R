#Change to your filepath for the excel document
survivoR_filepath <- "C:/Users/sabri/OneDrive - Iowa State University/Documents/BayesSurvivoR/survivoR.xlsx"
#Reading in Dataset
library(readxl)
sheet_names <- excel_sheets(survivoR_filepath)
# Read each sheet into a list of data frames
df_list <- lapply(sheet_names, function(x) read_excel("survivoR.xlsx", sheet = x))
# Assign each data frame to the global environment using the sheet names
names(df_list) <- sheet_names
list2env(df_list, envir = .GlobalEnv)
sheet_names





#EDA before modeling

#Want everyone separated by castaway score
cast_detail<-df_list[["Castaway Details"]]
identifiers<-cast_detail$castaway_id
#Remove non-US castaways
uscast_detail <- cast_detail[grepl("^US", cast_detail$castaway_id), ]
uscast_detail

##################################################

#Separating occupations

unique(uscast_detail$occupation)
#Check how many unknown collars there are
sum(uscast_detail$collar == "Unknown")
#Create new df for unknown collars
unknown_collar <- uscast_detail[uscast_detail$collar == "Unknown", ]
#unknown_collar$occupation-- found the occupation list for the unknown collars
library(dplyr)
#Replace unknown with collar category
uscast_detail <- uscast_detail %>%
  mutate(collar = case_when(
      collar == "Unknown" & grepl("ivy league graduate|harvard law student|law student|aspiring writer", occupation, ignore.case = TRUE) ~ "White collar",
      collar == "Unknown" & grepl("student|dental student|retired navy seal|retired police officer|retired teacher|former navy fighter pilot|single mom|aspiring writer|ex-nfl player|retired mlb player|iraq war veteran|army veteran|air force veteran|ex-nfl player's wife;homemaker", occupation, ignore.case = TRUE) ~ "No collar",
      TRUE ~ collar #keep original collar if not unknown
      ))

library(stringr)
#Standardize to existing collar categories
uscast_detail <- uscast_detail %>%
  mutate(collar = str_to_title(collar))

#Visualize for prior
#Calculate the proportions of each collar category
collar_distribution <- uscast_detail %>%
  group_by(collar) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

library(ggplot2)
ggplot(collar_distribution, aes(x = collar, y = proportion, fill = collar)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribution of Collar Categories", x = "Collar", y = "Proportion")


#########################################################

#Creating ethnicity column

uscast_detail <- uscast_detail %>%
  rowwise() %>%
  mutate(ethnicity = paste(c(
        if (african) "African" else NULL,
        if (asian) "Asian" else NULL,
        if (latin_american) "Latin American" else NULL,
        if (native_american) "Native American" else NULL
      ),
      collapse = "; "
    ),
    ethnicity = ifelse(ethnicity == "", "White", ethnicity)
  ) %>%
  ungroup()
head(uscast_detail$ethnicity) #categories including dual-combinations of each

#########################################################

#Creating column of whether the castaway had a nickname used instead of real name (ex: Johnny Fairplay)

########################################################

#Creating an "age when playing" column
#Exists in Castaways

#######################################################

#Advantage movement: if they played their advantage for themselves