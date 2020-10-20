#### Preamble ####
# Purpose: The purpose of this code is to clean-up the 2017 GSS data obtained 
# from the U of T library. That data is available to U of T students, but it needs 
# to be put into a tidy format before it can be analysed. This code does that.
# The main issue is that the data are released with codes for variables, whereas,
# we want the variable. e.g. sex is 1 or 2, but we want sex is female or male. (This
# sounds trite in that case, but gets more difficult with more involved variables.)
# So we create a dictionary type dataset that has the variable names and their 
# possible values. In that we embed some R code that will do a replacement. We 
# then apply that dataset to the raw dataset. Finally we do all the usual cleaning.
# to the dataset. You will end up with a dataset called gss.csv.
# Authors: Rohan Alexander and Sam Caetano
# Contact: rohan.alexander@utoronto.ca
# Date: 7 October 2020
# License: MIT
# Pre-reqs: You need to have downloaded the data from the library. To do that: 
## 1. Go to: http://www.chass.utoronto.ca/
## 2. Data centre --> UofT users or http://dc.chass.utoronto.ca/myaccess.html
## 3. Click SDA @ CHASS, should redirect to sign in. Sign in.
## 4. Continue in English (you're welcome to use the French, but we probably can't
## help you too much).
## 5. Crtl F GSS, click
## 6. Click "Data" on the one you want. We used 2017, but you may want a different 
## wave. In particular the General Social Survey on social identity (cycle 27), 
## 2013 has some variables on voter participation if you're into that sort of 
## thing. You're welcome to pick any year but this code applies to 2017.
## 7. Click download
## 8. Select CSV data file, data definitions for STATA (gross, but stick with it for now).
## 9. Can select all variables by clicking button next to green colored "All". Then continue.
## 10. Create the files, download and save
# Check: 
## You WILL need to change the raw data name. Search for .csv - line 41
## You may need to adjust the filepaths depending on your system. Search for: read_


#### Workspace set-up ####
install.packages("readr")
install.packages("janitor")
install.packages("tidyverse")
install.packages("dplyr")
library(readr)
library(dplyr)
library(janitor)
library(tidyverse)

# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("AAMlH7mk.csv")
dict <- read_lines("gss_dict.txt", skip = 17) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("gss_labels.txt")


#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)))

# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables
gss <- raw_data %>% 
  select(CASEID, 
         agegr5,
         sex,
         brthcan,
         prv,
         region,
         edu10,
         b7,
         incm,
         incmhsd,
         a1,
         owncomp,
         a6,
         a8,
         a15,
         b23,
         hrsschl,
         c1,
         hrswork,
         hrshome,
         hrsothr,
         f1
         ) %>%
  mutate_at(vars(agegr5:f1), .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(agegr5:f1),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))

# Fix the names
gss <- gss %>% 
  clean_names() %>% 
  rename(age = agegr5,
         sex = sex,
         country_of_birth = brthcan,
         province = prv,
         region = region,
         education = edu10,
         job = b7,
         personal_income = incm,
         husehold_income = incmhsd,
         household_internet_connection = a1,
         own_computer = owncomp,
         ever_use_internet = a6,
         ever_use_computer = a8,
         ability_to_use_computer = a15,
         use_at_school = b23,
         using_hours_at_school = hrsschl,
         use_in_job = c1,
         using_hours_at_work = hrswork,
         hours_at_home = hrshome,
         hours_at_other = hrsothr,
         years_of_using_internet = f1) 

#### Clean up ####
gss <- gss %>% 
  mutate_at(vars(age:years_of_using_internet), 
            .funs = funs(ifelse(.=="Dont know"|.=="Not asked"|.=="Not stated", "NA", .))) 

gss <- gss %>% 
  mutate_at(vars(job, household_internet_connection:ever_use_computer, use_at_school, use_in_job), .funs = funs(case_when(
    .=="Yes"~1,
    .=="No"~0,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(age), 
            .funs = funs(case_when(.=="15 to 17"~"15-17",
                                   .=="18 to 19"~"18-19",
                                   .=="20 to 24"~"20-24",
                                   .=="25 to 29"~"25-29",
                                   .=="30 to 34"~"30-34",
                                   .=="35 to 39"~"35-39",
                                   .=="40 to 44"~"40-44",
                                   .=="45 to 49"~"45-49",
                                   .=="50 to 54"~"50-54",
                                   .=="55 to 59"~"55-59",
                                   .=="60 to 64"~"60-64",
                                   .=="65 to 69"~"65-69",
                                   .=="70 to 74"~"70-74",
                                   .=="75 to 79"~"75-79",
                                   .=="80 years and over"~"80+")))

#convert months to year
gss <- gss %>% 
  mutate_at(vars(years_of_using_internet), 
            .funs = funs(case_when(.=="Less than 6 months"~"<0.5",
                                   .=="6 to 12 months"~"0.5-1",
                                   .=="1 to 3 years"~"1-3",
                                   .=="4 to 6 years"~"4-6",
                                   .=="7 or more years"~"7+")))

gss <- gss %>% filter(!is.na(years_of_using_internet))

write_csv(gss, "gss.csv")