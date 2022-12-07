library(tidyverse)
library(dplyr)
library(haven)
require(ggplot2)
require(scales)
library("ggplot2")
library(RColorBrewer)
library(MatchIt)
library(randomForest)
library(xtable)
library(tinytex)
library(haven)
library(lmtest)
library(sandwich)
library(boot)

# IMPORT DATA
ugandaUG2021_raw_data_labelled <- read_dta("./UG_Round_3_web_version_2022.dta")
ugandaUG2021_raw_data <- zap_labels(ugandaUG2021_raw_data_labelled)

#### CREATE FUNCTIONS ####
convert_yes_no <- function(item, labs = c(2, 1)) {
  # Function to convert yes and no responses to standardized values of 0 for "no" and 1 for "yes"
  # ITEM - item to convert
  # LABS - current labeling of item, where LABS[1] is current label for "no" and LABS[2] is current label for "yes"
  
  no_lab <- labs[1]
  yes_lab <- labs[2]
  
  
  if (!is.na(item)) {
    if (item == no_lab) {
      item <- 0
    } else if (item == yes_lab) {
      item <- 1
    }
  }
  return(item)
}

convert_based_on_col <- function(df, condi_col, condi_val, conv_col, conv_val, reverse=FALSE, many=FALSE) {
  # Function that converts a value in one column to some value based on if another column equals some value
  # DF- data frame
  # CONDI_COL - column that condition is based on
  # CONDI_VAL - perform conversion if CONDI_COL takes this value
  # CONV_COL - column to convert
  # CONV_VAL - convert CONV_COL to this value if CONDI_COL takes value CONDI_VAL
  # REVERSE - if set to TRUE, convert values when the value in CONDI_COL doesn't equal CONDI_VAL
  # MANY - if set to TRUE, can use vectors for CONDI_VAL and CONV_VAL such that if CONDI_COL == CONDI_VAL[i], CONV_COL will be     converted to CONV_VAL[i]
  
  for (n in 1:nrow(df)){
    
    val <- df[n, condi_col]
    
    if (!is.na(val)){
      
      if (many == FALSE) {
        
        if (reverse == TRUE) {
          
          if (val != condi_val) {
            
            df[n, conv_col] <- conv_val 
          } 
        } else {
          if (val == condi_val) {
            
            df[n, conv_col] <- conv_val
          }
        }
      } else {
        
        for (i in 1:length(condi_val)) {
          
          if (reverse == TRUE) {
            
            if (val != condi_val[i]) {
              
              df[n, conv_col] <- conv_val[i]
            }
          } else {
            
            if (val == condi_val[i]) {
              
              df[n, conv_col] <- conv_val[i]
            }
          }
        }
        
      }
    }
  }
  
  return(df)
}

convert_cols_to_single <- function(df, cols_to_convert, new_col, new_vals, na_val=NA) {
  # Function to convert multiple yes/no columns regarding a single topic into a single column
  # DF - data frame
  # COLS_TO_CONVERT - columns to collapse into a single column
  # NEW_COL - name for new column
  # NEW_VALS - vector where the i-th value corresponds to value for NEW_COL in case where observation has positive response for COLS_TO_CONVERT[i]
  # NA_VAL - val to map NA to in new column
  
  df[, new_col] <- na_val
  
  for (n in 1: nrow(df)) {
    
    i <- 1
    
    for (col in cols_to_convert){
      
      if (!is.na(df[n, col])) {
        
        if (df[n,col] == 1) {
          
          df[n, new_col] <- new_vals[i]
        }
      }
      
      i <- i + 1
    }
    
  }
  
  return(df)
}

convert_yes_no_many_cols <- function(df, cols_to_conv, new_col_names=c(NA)) {
  # Function to convert many yes-no columns to standardized values for yes and no
  # DF - data frame
  # COLS_TO_CONV - columns to standardize
  # NEW_COL_NAMES - optional, provides names of new columns
  
  for (i in 1:length(cols_to_conv)) {
    if (is.na(new_col_names[1])) {
      col_name <- cols_to_conv[i]
    }
    else {
      col_name <- new_col_names[i]
    }
    df[,col_name] <- sapply(df[[cols_to_conv[i]]], convert_yes_no)
  }
  return(df)
}


#### end ####  

#### CLEANING 1: STANRARDISE YES-NO VARS ####
cols_to_stdze <- c("q1", "a3iv", "a7", "a10", "a19", "a20i_1", "a20i_2", "a20i_3", "b1", "b3", "b6", "b10", "c1a")
data <- convert_yes_no_many_cols(ugandaUG2021_raw_data, cols_to_stdze, cols_to_stdze)

#### end ####

#### CLEANING 2: TAX TYPES BINARIES #####

data <- data %>%
  mutate(a1_Local_service_tax=0, a1_Presumptive_tax=0, a1_Corporation_tax=0,a1_PAYE=0,
         a1_Property_tax=0,a1_Rental_tax=0,a1_VAT=0,a1_Stamp_duty=0,a1_Pension=0,
         a1_Customs_duties=0,a1_Local_excise_duty=0,a1_Withholding_tax=0,
         a1_Unofficial_levies=0,a1_Vehicle_Tax=0,a1_Trading_licence=0,a1_Third_Party=0,
         a1_Graduated_tax=0,a1_OTT=0,a1_Mobile_money_tax=0,a1_Income_tax=0,
         a1_official_levies=0,a1_occupation_tax=0,a1_utility_taxes=0,
         a1_transport_tax=0,a1_weighing_scale_levie=0,a1_roads_tax=0,a1_police_tax=0,
         a1_construction_tax=0,a1_loan_tax=0,a1_agricuiture_tax=0,a1_fishermen_tax=0,
         a1_national_id_tax=0,a1_environmental_levy=0,a1_electricity_vat=0,
         a1_drug_shop_tax=0,a1_driving_licence=0,a1_capital_gain_tax=0,
         a1_barter_trade=0,a1_gravity_tax=0,a1_fees_tax=0,a1_BOAT_LICENSE=0,
         a1_Gun_Tax=0,a1_Dont_Know=0,a1_None=0)

## specify the columns of interest
cola1 <- c("a1_1", "a1_2", "a1_3", "a1_4", "a1_5", "a1_6", "a1_7", "a1_8", "a1_9", "a1_10", "a1_11", "a1_12")

## assign the new column
data$a1_Local_service_tax <- ((rowSums(data[, cola1] == 1, na.rm=T) > 0) * 1)
data$a1_Presumptive_tax <- ((rowSums(data[, cola1] == 2, na.rm=T) > 0) * 1)
data$a1_Corporation_tax <- ((rowSums(data[, cola1] == 3, na.rm=T) > 0) * 1)
data$a1_PAYE <- ((rowSums(data[, cola1] == 4, na.rm=T) > 0) * 1)
data$a1_Property_tax <- ((rowSums(data[, cola1] == 5, na.rm=T) > 0) * 1)
data$a1_Rental_tax <- ((rowSums(data[, cola1] == 6, na.rm=T) > 0) * 1)
data$a1_VAT <- ((rowSums(data[, cola1] == 7, na.rm=T) > 0) * 1)
data$a1_Stamp_duty <- ((rowSums(data[, cola1] == 8, na.rm=T) > 0) * 1)
data$a1_Pension <- ((rowSums(data[, cola1] == 9, na.rm=T) > 0) * 1)
data$a1_Customs_duties <- ((rowSums(data[, cola1] == 10, na.rm=T) > 0) * 1)
data$a1_Local_excise_duty <- ((rowSums(data[, cola1] == 11, na.rm=T) > 0) * 1)
data$a1_Withholding_tax <- ((rowSums(data[, cola1] == 12, na.rm=T) > 0) * 1)
data$a1_Unofficial_levies <- ((rowSums(data[, cola1] == 13, na.rm=T) > 0) * 1)
data$a1_Vehicle_Tax <- ((rowSums(data[, cola1] == 20, na.rm=T) > 0) * 1)
data$a1_Trading_licence <- ((rowSums(data[, cola1] == 21, na.rm=T) > 0) * 1)
data$a1_Third_Party <- ((rowSums(data[, cola1] == 22, na.rm=T) > 0) * 1)
data$a1_Graduated_tax <- ((rowSums(data[, cola1] == 23, na.rm=T) > 0) * 1)
data$a1_OTT <- ((rowSums(data[, cola1] == 24, na.rm=T) > 0) * 1)
data$a1_Mobile_money_tax <- ((rowSums(data[, cola1] == 25, na.rm=T) > 0) * 1)
data$a1_Income_tax <- ((rowSums(data[, cola1] == 26, na.rm=T) > 0) * 1)
data$a1_official_levies <- ((rowSums(data[, cola1] == 29, na.rm=T) > 0) * 1)
data$a1_occupation_tax <- ((rowSums(data[, cola1] == 30, na.rm=T) > 0) * 1)
data$a1_utility_taxes <- ((rowSums(data[, cola1] == 31, na.rm=T) > 0) * 1)
data$a1_transport_tax <- ((rowSums(data[, cola1] == 32, na.rm=T) > 0) * 1)
data$a1_weighing_scale_levie <- ((rowSums(data[, cola1] == 33, na.rm=T) > 0) * 1)
data$a1_roads_tax <- ((rowSums(data[, cola1] == 34, na.rm=T) > 0) * 1)
data$a1_police_tax <- ((rowSums(data[, cola1] == 35, na.rm=T) > 0) * 1)
data$a1_construction_tax <- ((rowSums(data[, cola1] == 36, na.rm=T) > 0) * 1)
data$a1_loan_tax <- ((rowSums(data[, cola1] == 37, na.rm=T) > 0) * 1)
data$a1_agricuiture_tax <- ((rowSums(data[, cola1] == 38, na.rm=T) > 0) * 1)
data$a1_fishermen_tax <- ((rowSums(data[, cola1] == 39, na.rm=T) > 0) * 1)
data$a1_national_id_tax <- ((rowSums(data[, cola1] == 42, na.rm=T) > 0) * 1)
data$a1_environmental_levy <- ((rowSums(data[, cola1] == 43, na.rm=T) > 0) * 1)
data$a1_electricity_vat <- ((rowSums(data[, cola1] == 44, na.rm=T) > 0) * 1)
data$a1_drug_shop_tax <- ((rowSums(data[, cola1] == 45, na.rm=T) > 0) * 1)
data$a1_driving_licence <- ((rowSums(data[, cola1] == 46, na.rm=T) > 0) * 1)
data$a1_capital_gain_tax <- ((rowSums(data[, cola1] == 48, na.rm=T) > 0) * 1)
data$a1_barter_trade <- ((rowSums(data[, cola1] == 49, na.rm=T) > 0) * 1)
data$a1_gravity_tax <- ((rowSums(data[, cola1] == 50, na.rm=T) > 0) * 1)
data$a1_fees_tax <- ((rowSums(data[, cola1] == 51, na.rm=T) > 0) * 1)
data$a1_BOAT_LICENSE <- ((rowSums(data[, cola1] == 53, na.rm=T) > 0) * 1)
data$a1_Gun_Tax <- ((rowSums(data[, cola1] == 54, na.rm=T) > 0) * 1)
data$a1_Dont_Know <- ((rowSums(data[, cola1] == 99, na.rm=T) > 0) * 1)
data$a1_None <- ((rowSums(data[, cola1] == 999, na.rm=T) > 0) * 1)


data <- data %>%
  mutate(a2_Local_service_tax=0, a2_Presumptive_tax=0, a2_Corporation_tax=0,a2_PAYE=0,
         a2_Property_tax=0,a2_Rental_tax=0,a2_VAT=0,a2_Stamp_duty=0,a2_Pension=0,
         a2_Customs_duties=0,a2_Local_excise_duty=0,a2_Withholding_tax=0,
         a2_Unofficial_levies=0,a2_Vehicle_Tax=0,a2_Trading_licence=0,a2_Third_Party=0,
         a2_Graduated_tax=0,a2_OTT=0,a2_Mobile_money_tax=0,a2_Income_tax=0,
         a2_official_levies=0,a2_occupation_tax=0,a2_utility_taxes=0,
         a2_transport_tax=0,a2_weighing_scale_levie=0,a2_roads_tax=0,a2_police_tax=0,
         a2_construction_tax=0,a2_loan_tax=0,a2_agricuiture_tax=0,a2_fishermen_tax=0,
         a2_national_id_tax=0,a2_environmental_levy=0,a2_electricity_vat=0,
         a2_drug_shop_tax=0,a2_driving_licence=0,a2_capital_gain_tax=0,
         a2_barter_trade=0,a2_gravity_tax=0,a2_fees_tax=0,a2_BOAT_LICENSE=0,
         a2_Gun_Tax=0,a2_Dont_Know=0,a2_None=0)

## specify the columns of interest
cola2 <- c("a2_1", "a2_2", "a2_3", "a2_4", "a2_5", "a2_6", "a2_7", "a2_8", "a2_9", "a2_10", "a2_11", "a2_12")

## assign the new column
data$a2_Local_service_tax <- ((rowSums(data[, cola2] == 1, na.rm=T) > 0) * 1)
data$a2_Presumptive_tax <- ((rowSums(data[, cola2] == 2, na.rm=T) > 0) * 1)
data$a2_Corporation_tax <- ((rowSums(data[, cola2] == 3, na.rm=T) > 0) * 1)
data$a2_PAYE <- ((rowSums(data[, cola2] == 4, na.rm=T) > 0) * 1)
data$a2_Property_tax <- ((rowSums(data[, cola2] == 5, na.rm=T) > 0) * 1)
data$a2_Rental_tax <- ((rowSums(data[, cola2] == 6, na.rm=T) > 0) * 1)
data$a2_VAT <- ((rowSums(data[, cola2] == 7, na.rm=T) > 0) * 1)
data$a2_Stamp_duty <- ((rowSums(data[, cola2] == 8, na.rm=T) > 0) * 1)
data$a2_Pension <- ((rowSums(data[, cola2] == 9, na.rm=T) > 0) * 1)
data$a2_Customs_duties <- ((rowSums(data[, cola2] == 10, na.rm=T) > 0) * 1)
data$a2_Local_excise_duty <- ((rowSums(data[, cola2] == 11, na.rm=T) > 0) * 1)
data$a2_Withholding_tax <- ((rowSums(data[, cola2] == 12, na.rm=T) > 0) * 1)
data$a2_Unofficial_levies <- ((rowSums(data[, cola2] == 13, na.rm=T) > 0) * 1)
data$a2_Vehicle_Tax <- ((rowSums(data[, cola2] == 20, na.rm=T) > 0) * 1)
data$a2_Trading_licence <- ((rowSums(data[, cola2] == 21, na.rm=T) > 0) * 1)
data$a2_Third_Party <- ((rowSums(data[, cola2] == 22, na.rm=T) > 0) * 1)
data$a2_Graduated_tax <- ((rowSums(data[, cola2] == 23, na.rm=T) > 0) * 1)
data$a2_OTT <- ((rowSums(data[, cola2] == 24, na.rm=T) > 0) * 1)
data$a2_Mobile_money_tax <- ((rowSums(data[, cola2] == 25, na.rm=T) > 0) * 1)
data$a2_Income_tax <- ((rowSums(data[, cola2] == 26, na.rm=T) > 0) * 1)
data$a2_official_levies <- ((rowSums(data[, cola2] == 29, na.rm=T) > 0) * 1)
data$a2_occupation_tax <- ((rowSums(data[, cola2] == 30, na.rm=T) > 0) * 1)
data$a2_utility_taxes <- ((rowSums(data[, cola2] == 31, na.rm=T) > 0) * 1)
data$a2_transport_tax <- ((rowSums(data[, cola2] == 32, na.rm=T) > 0) * 1)
data$a2_weighing_scale_levie <- ((rowSums(data[, cola2] == 33, na.rm=T) > 0) * 1)
data$a2_roads_tax <- ((rowSums(data[, cola2] == 34, na.rm=T) > 0) * 1)
data$a2_police_tax <- ((rowSums(data[, cola2] == 35, na.rm=T) > 0) * 1)
data$a2_construction_tax <- ((rowSums(data[, cola2] == 36, na.rm=T) > 0) * 1)
data$a2_loan_tax <- ((rowSums(data[, cola2] == 37, na.rm=T) > 0) * 1)
data$a2_agricuiture_tax <- ((rowSums(data[, cola2] == 38, na.rm=T) > 0) * 1)
data$a2_fishermen_tax <- ((rowSums(data[, cola2] == 39, na.rm=T) > 0) * 1)
data$a2_national_id_tax <- ((rowSums(data[, cola2] == 42, na.rm=T) > 0) * 1)
data$a2_environmental_levy <- ((rowSums(data[, cola2] == 43, na.rm=T) > 0) * 1)
data$a2_electricity_vat <- ((rowSums(data[, cola2] == 44, na.rm=T) > 0) * 1)
data$a2_drug_shop_tax <- ((rowSums(data[, cola2] == 45, na.rm=T) > 0) * 1)
data$a2_driving_licence <- ((rowSums(data[, cola2] == 46, na.rm=T) > 0) * 1)
data$a2_capital_gain_tax <- ((rowSums(data[, cola2] == 48, na.rm=T) > 0) * 1)
data$a2_barter_trade <- ((rowSums(data[, cola2] == 49, na.rm=T) > 0) * 1)
data$a2_gravity_tax <- ((rowSums(data[, cola2] == 50, na.rm=T) > 0) * 1)
data$a2_fees_tax <- ((rowSums(data[, cola2] == 51, na.rm=T) > 0) * 1)
data$a2_BOAT_LICENSE <- ((rowSums(data[, cola2] == 53, na.rm=T) > 0) * 1)
data$a2_Gun_Tax <- ((rowSums(data[, cola2] == 54, na.rm=T) > 0) * 1)
data$a2_Dont_Know <- ((rowSums(data[, cola2] == 99, na.rm=T) > 0) * 1)
data$a2_None <- ((rowSums(data[, cola2] == 999, na.rm=T) > 0) * 1)


#### end ####

#### CLEANING 3: SOME NEW VARIABLES ####

data$num_taxes_know <- data$a1_Local_service_tax + data$a1_Presumptive_tax + data$a1_Corporation_tax + data$a1_PAYE + data$a1_Property_tax + data$a1_Rental_tax + data$a1_VAT + data$a1_Stamp_duty + data$a1_Pension + data$a1_Customs_duties + data$a1_Local_excise_duty + data$a1_Withholding_tax + data$a1_Unofficial_levies + data$a1_Vehicle_Tax + data$a1_Trading_licence + data$a1_Third_Party + data$a1_Graduated_tax + data$a1_OTT + data$a1_Mobile_money_tax + data$a1_Income_tax + data$a1_official_levies + data$a1_occupation_tax + data$a1_utility_taxes + data$a1_transport_tax + data$a1_weighing_scale_levie + data$a1_roads_tax + data$a1_police_tax + data$a1_construction_tax + data$a1_loan_tax + data$a1_agricuiture_tax + data$a1_fishermen_tax + data$a1_national_id_tax + data$a1_environmental_levy + data$a1_electricity_vat + data$a1_drug_shop_tax + data$a1_driving_licence + data$a1_capital_gain_tax + data$a1_barter_trade + data$a1_gravity_tax + data$a1_fees_tax + data$a1_BOAT_LICENSE + data$a1_Gun_Tax + data$a2_Local_service_tax + data$a2_Presumptive_tax + data$a2_Corporation_tax + data$a2_PAYE + data$a2_Property_tax + data$a2_Rental_tax + data$a2_VAT + data$a2_Stamp_duty + data$a2_Pension + data$a2_Customs_duties + data$a2_Local_excise_duty + data$a2_Withholding_tax + data$a2_Unofficial_levies + data$a2_Vehicle_Tax + data$a2_Trading_licence + data$a2_Third_Party + data$a2_Graduated_tax + data$a2_OTT + data$a2_Mobile_money_tax + data$a2_Income_tax + data$a2_official_levies + data$a2_occupation_tax + data$a2_utility_taxes + data$a2_transport_tax + data$a2_weighing_scale_levie + data$a2_roads_tax + data$a2_police_tax + data$a2_construction_tax + data$a2_loan_tax + data$a2_agricuiture_tax + data$a2_fishermen_tax + data$a2_national_id_tax + data$a2_environmental_levy + data$a2_electricity_vat + data$a2_drug_shop_tax + data$a2_driving_licence + data$a2_capital_gain_tax + data$a2_barter_trade + data$a2_gravity_tax + data$a2_fees_tax + data$a2_BOAT_LICENSE + data$a2_Gun_Tax

data$a4_c_1 <- ifelse(data$a4_1==1 & !is.na(data$a4_1) | data$a4_2==1 & !is.na(data$a4_2) | data$a4_3==1 & !is.na(data$a4_3), 1, 0)
data$a4_c_2 <- ifelse(data$a4_1==2 & !is.na(data$a4_1) | data$a4_2==2 & !is.na(data$a4_2) | data$a4_3==2 & !is.na(data$a4_3), 1, 0)
data$a4_c_3 <- ifelse(data$a4_1==3 & !is.na(data$a4_1) | data$a4_2==3 & !is.na(data$a4_2) | data$a4_3==3 & !is.na(data$a4_3), 1, 0)
data$a4_c_4 <- ifelse(data$a4_1==4 & !is.na(data$a4_1) | data$a4_2==4 & !is.na(data$a4_2) | data$a4_3==4 & !is.na(data$a4_3), 1, 0)
data$a4_c_5 <- ifelse(data$a4_1==5 & !is.na(data$a4_1) | data$a4_2==5 & !is.na(data$a4_2) | data$a4_3==5 & !is.na(data$a4_3), 1, 0)
data$a4_c_6 <- ifelse(data$a4_1==6 & !is.na(data$a4_1) | data$a4_2==6 & !is.na(data$a4_2) | data$a4_3==6 & !is.na(data$a4_3), 1, 0)
data$a4_c_7 <- ifelse(data$a4_1==7 & !is.na(data$a4_1) | data$a4_2==7 & !is.na(data$a4_2) | data$a4_3==7 & !is.na(data$a4_3), 1, 0)
data$a4_c_8 <- ifelse(data$a4_1==8 & !is.na(data$a4_1) | data$a4_2==8 & !is.na(data$a4_2) | data$a4_3==8 & !is.na(data$a4_3), 1, 0)
data$a4_c_9 <- ifelse(data$a4_1==9 & !is.na(data$a4_1) | data$a4_2==9 & !is.na(data$a4_2) | data$a4_3==9 & !is.na(data$a4_3), 1, 0)
data$a4_c_10 <- ifelse(data$a4_1==10 & !is.na(data$a4_1) | data$a4_2==10 & !is.na(data$a4_2) | data$a4_3==10 & !is.na(data$a4_3), 1, 0)
data$a4_c_11 <- ifelse(data$a4_1==11 & !is.na(data$a4_1) | data$a4_2==11 & !is.na(data$a4_2) | data$a4_3==11 & !is.na(data$a4_3), 1, 0)
data$a4_c_12 <- ifelse(data$a4_1==12 & !is.na(data$a4_1) | data$a4_2==12 & !is.na(data$a4_2) | data$a4_3==12 & !is.na(data$a4_3), 1, 0)
data$a4_c_13 <- ifelse(data$a4_1==13 & !is.na(data$a4_1) | data$a4_2==13 & !is.na(data$a4_2) | data$a4_3==13 & !is.na(data$a4_3), 1, 0)
data$a4_c_99 <- ifelse(data$a4_1==99 & !is.na(data$a4_1) | data$a4_2==99 & !is.na(data$a4_2) | data$a4_3==99 & !is.na(data$a4_3), 1, 0)

#### end ####

#### CLEANING 4: removing NA ####

data_no_NA <- data[!is.na(data$region), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$district), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$subcounty), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$parish), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$setting), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$gender), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$age), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$num_taxes_know), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$num_taxes_pay), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a4_c_1), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a4_c_2), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a4_c_3), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a4_c_4), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a4_c_5), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a4_c_6), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a4_c_7), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a4_c_8), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a4_c_9), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a4_c_10), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a4_c_11), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a4_c_12), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a4_c_13), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a4_c_99), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a5_1), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a5_2), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a5_3), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a5_6), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a5_7), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a7), ]    
data_no_NA <- data_no_NA[!is.na(data_no_NA$a15_1), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a15_2), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a15_3), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a15_4), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a15_5), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a15_6), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a15_7), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a15_8), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a19), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$a22a), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$b1), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$b6), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$b10), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$c1a), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$c1c), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$c1d), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$c1f_1), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$c1f_2), ]
data_no_NA <- data_no_NA[!is.na(data_no_NA$f2), ]

#### end ####

#### CLEANING 5: MAKE BINARY 1-0 ####

data_no_NA$a5_4 <- ifelse(data_no_NA$a5_4==1,1,0)

data_no_NA$a5_1 <- ifelse(data_no_NA$a5_1==1,1,0) 
data_no_NA$female <- ifelse(data_no_NA$gender==2,1,0) 
data_no_NA$urban <- ifelse(data_no_NA$setting==1,1,0)
data_no_NA$a15_1 <- ifelse(data_no_NA$a15_1==1,1,0)
data_no_NA$a15_6 <- ifelse(data_no_NA$a15_6==1,1,0)
data_no_NA$a15_7 <- ifelse(data_no_NA$a15_7==1,1,0)
data_no_NA$a15_8 <- ifelse(data_no_NA$a15_8==1,1,0)
data_no_NA$a5_7 <- ifelse(data_no_NA$a5_7==1,1,0)
data_no_NA$a15_2 <- ifelse(data_no_NA$a15_2==1,1,0)


data_no_NA$a19_1 <- ifelse(data_no_NA$a19==1,1,0)
data_no_NA$a19_0 <- ifelse(data_no_NA$a19==2,1,0)

data_no_NA$a22a_1 <- ifelse(data_no_NA$a22a==1,1,0)
data_no_NA$a22a_2 <- ifelse(data_no_NA$a22a==2,1,0)
data_no_NA$a22a_3 <- ifelse(data_no_NA$a22a==3,1,0)

data_no_NA$b6_1 <- ifelse(data_no_NA$b6==1,1,0)
data_no_NA$b6_0 <- ifelse(data_no_NA$b6==2,1,0)

data_no_NA$c1a_1 <- ifelse(data_no_NA$c1a==1,1,0)
data_no_NA$c1a_0 <- ifelse(data_no_NA$c1a==2,1,0)

data_no_NA$c1c_1 <- ifelse(data_no_NA$c1c==1,1,0)
data_no_NA$c1c_2 <- ifelse(data_no_NA$c1c==2,1,0)
data_no_NA$c1c_3 <- ifelse(data_no_NA$c1c==3,1,0)
data_no_NA$c1c_4 <- ifelse(data_no_NA$c1c==4,1,0)
data_no_NA$c1c_5 <- ifelse(data_no_NA$c1c==5,1,0)
data_no_NA$c1c_6 <- ifelse(data_no_NA$c1c==6,1,0)
data_no_NA$c1c_7 <- ifelse(data_no_NA$c1c==7,1,0)
data_no_NA$c1c_8 <- ifelse(data_no_NA$c1c==8,1,0)
data_no_NA$c1c_9 <- ifelse(data_no_NA$c1c==9,1,0)
data_no_NA$c1c_10 <- ifelse(data_no_NA$c1c==10,1,0)
data_no_NA$c1c_11 <- ifelse(data_no_NA$c1c==11,1,0)
data_no_NA$c1c_12 <- ifelse(data_no_NA$c1c==12,1,0)
data_no_NA$c1c_13 <- ifelse(data_no_NA$c1c==13,1,0)
data_no_NA$c1c_14 <- ifelse(data_no_NA$c1c==14,1,0)
data_no_NA$c1c_15 <- ifelse(data_no_NA$c1c==15,1,0)
data_no_NA$c1c_16 <- ifelse(data_no_NA$c1c==16,1,0)
data_no_NA$c1c_17 <- ifelse(data_no_NA$c1c==17,1,0)

data_no_NA$c1d_1 <- ifelse(data_no_NA$c1d==1,1,0)
data_no_NA$c1d_2 <- ifelse(data_no_NA$c1d==2,1,0)
data_no_NA$c1d_3 <- ifelse(data_no_NA$c1d==3,1,0)
data_no_NA$c1d_4 <- ifelse(data_no_NA$c1d==4,1,0)
data_no_NA$c1d_5 <- ifelse(data_no_NA$c1d==5,1,0)
data_no_NA$c1d_6 <- ifelse(data_no_NA$c1d==6,1,0)
data_no_NA$c1d_7 <- ifelse(data_no_NA$c1d==7,1,0)
data_no_NA$c1d_8 <- ifelse(data_no_NA$c1d==8,1,0)
data_no_NA$c1d_9 <- ifelse(data_no_NA$c1d==9,1,0)
data_no_NA$c1d_10 <- ifelse(data_no_NA$c1d==10,1,0)
data_no_NA$c1d_11 <- ifelse(data_no_NA$c1d==11,1,0)
data_no_NA$c1d_12 <- ifelse(data_no_NA$c1d==12,1,0)

data_no_NA$c1f_1_n <- ifelse(data_no_NA$c1f_1 != 6,data_no_NA$c1f_1,0)
data_no_NA$c1f_1_d <- ifelse(data_no_NA$c1f_1 == 6,1,0)

data_no_NA$c1f_2_n <- ifelse(data_no_NA$c1f_2 != 6,data_no_NA$c1f_2,0)
data_no_NA$c1f_2_d <- ifelse(data_no_NA$c1f_2 == 6,1,0)


#### end ####

#### CLEANING 6: MAKE INCOME PROXY ####

# make a new variable "a1prop" in the dataframe "data_no_NA" such that 
# it equals 1 if EITHER ONE OF a22b_1, a22b_2, or a22b_3 equals EITHER ONE OF 
# 5; and 0 otherwise.

# 5	Land and Building (Property tax)

# create an empty vector to hold the values of know_hi_taxes
a1prop <- vector()

# loop through the rows of the data_no_NA data frame
for (i in 1:nrow(data_no_NA)) {
  # check if any of the values in a22b_1_NNA, a22b_2_NNA, or a22b_3_NNA are in {3, 29, 36, 37}
  if (any(data_no_NA$a1_1[i] %in% c(5)) |
      any(data_no_NA$a1_2[i] %in% c(5)) |
      any(data_no_NA$a1_3[i] %in% c(5)) |
      any(data_no_NA$a1_4[i] %in% c(5)) |
      any(data_no_NA$a1_5[i] %in% c(5)) |
      any(data_no_NA$a1_6[i] %in% c(5)) |
      any(data_no_NA$a1_7[i] %in% c(5)) |
      any(data_no_NA$a1_8[i] %in% c(5)) |
      any(data_no_NA$a1_9[i] %in% c(5)) |
      any(data_no_NA$a1_10[i] %in% c(5)) |
      any(data_no_NA$a1_11[i] %in% c(5)) |
      any(data_no_NA$a1_12[i] %in% c(5)) |
      any(data_no_NA$a1_13[i] %in% c(5))) {
    # if any of the values are in the set, append 1 to the know_hi_taxes vector
    a1prop <- append(a1prop, 1)
  } else {
    # if none of the values are in the set, append 0 to the know_hi_taxes vector
    a1prop <- append(a1prop, 0)
  }
}

# add the know_hi_taxes vector as a new column to the data_no_NA data frame
data_no_NA$a1prop <- a1prop


#### end  ####

#### CLEANING 7: MAKE BUSINESS PROXY ####

# make a new variable "a1bus" in the dataframe "data_no_NA" such that 
# it equals 1 if EITHER ONE OF a22b_1, a22b_2, or a22b_3 equals EITHER ONE OF 
# 5; and 0 otherwise.

# 3	Corporation tax
# 10	Customs duties (Import duty, VAT on imports, etc.)
# 11	Local excise duty


# create an empty vector to hold the values of know_hi_taxes
a1bus <- vector()

# loop through the rows of the data_no_NA data frame
for (i in 1:nrow(data_no_NA)) {
  # check if any of the values in a22b_1_NNA, a22b_2_NNA, or a22b_3_NNA are in {3, 29, 36, 37}
  if (any(data_no_NA$a1_1[i] %in% c(3, 10, 11)) |
      any(data_no_NA$a1_2[i] %in% c(3, 10, 11)) |
      any(data_no_NA$a1_3[i] %in% c(3, 10, 11)) |
      any(data_no_NA$a1_4[i] %in% c(3, 10, 11)) |
      any(data_no_NA$a1_5[i] %in% c(3, 10, 11)) |
      any(data_no_NA$a1_6[i] %in% c(3, 10, 11)) |
      any(data_no_NA$a1_7[i] %in% c(3, 10, 11)) |
      any(data_no_NA$a1_8[i] %in% c(3, 10, 11)) |
      any(data_no_NA$a1_9[i] %in% c(3, 10, 11)) |
      any(data_no_NA$a1_10[i] %in% c(3, 10, 11)) |
      any(data_no_NA$a1_11[i] %in% c(3, 10, 11)) |
      any(data_no_NA$a1_12[i] %in% c(3, 10, 11)) |
      any(data_no_NA$a1_13[i] %in% c(3, 10, 11))) {
    # if any of the values are in the set, append 1 to the know_hi_taxes vector
    a1bus <- append(a1bus, 1)
  } else {
    # if none of the values are in the set, append 0 to the know_hi_taxes vector
    a1bus <- append(a1bus, 0)
  }
}

# add the know_hi_taxes vector as a new column to the data_no_NA data frame
data_no_NA$a1bus <- a1bus


#### end  ####


### CEM -- OUTCOME: I'd happily pay taxes without enforcement -->  a5_4 [1 agree, 0 neither/disagree] You would happily pay taxes without any enforcement

  # CEM Algorithm Parameters
  cutpoints <- list(age=c(15,20,25,35,45,55,65), num_taxes_know=c(2,4,6,8,10,12))
  
  
  #### 1. Belief tax is important for country  ####
  
  # a5_1 [1 agree, 0 disagree/neither] Tax is important for the country success and economy
  
  matches4_C <- matchit(a5_1 ~ urban + female + age
                        + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                        data=data_no_NA, method="cem", cutpoints=cutpoints)
  summary(matches4_C)

  Smatch_set4_C <- match.data(matches4_C)
  
  # Unweighted regression:
  Ufit4 <- lm(a5_4 ~ a5_1, data = match_set4_C, weights = weights)
  # Weighted regression:
  coeftest(Ufit4, vcov. = vcovHC)
  # Unmatched raw correlation:
  summary(lm(data=data_no_NA, formula=a5_4~a5_1))
  
  
  #### end ####
  
  #### 2. Confidence taxes are spent well ####
  
  # a15_1 [1 agree, 0 disagree/neither] You are confident that all taxes, fees and fines collected are spent wisely
  
  matches6_C <- matchit(a15_1 ~ urban + female + age
                        + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                        data=data_no_NA, method="cem", cutpoints=cutpoints)
  summary(matches6_C)
  
  match_set6_C <- match.data(matches6_C)
  
  Ufit6 <- lm(a5_4 ~ a15_1, data = match_set6_C, weights = weights)
  coeftest(Ufit6, vcov. = vcovHC)
  
  summary(lm(data=data_no_NA, formula=a5_4~a15_1))
  
  
  #### end ####

  #### 3. Knowledge of what taxes are spent on ####
  
  # a15_6 [1 agree, 0 disagree/neither] You feel that you have a good understanding on what the taxes, fees and fines are spent on
  
  matches7_C <- matchit(a15_6 ~ urban + female + age
                        + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                        data=data_no_NA, method="cem", cutpoints=cutpoints)
  summary(matches7_C)
  
  match_set7_C <- match.data(matches7_C)
  
  Ufit7 <- lm(a5_4 ~ a15_6, data = match_set7_C, weights = weights)
  coeftest(Ufit7, vcov. = vcovHC)
  
  summary(lm(data=data_no_NA, formula=a5_4~a15_6))
  
  
  #### end ####
  
  #### 4. Belief that the gov. taxes harmful products to be fair ####
  
  # a15_7 [1 agree, 0 neither/disagree] The reason why the government taxes harmful products such as tobacco is to foster fairness
  
  matches8_C <- matchit(a15_7 ~ urban + female + age
                        + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                        data=data_no_NA, method="cem", cutpoints=cutpoints)
  summary(matches8_C)
  
  match_set8_C <- match.data(matches8_C)
  
  Ufit8 <- lm(a5_4 ~ a15_7, data = match_set8_C, weights = weights)
  coeftest(Ufit8, vcov. = vcovHC)
  
  summary(lm(data=data_no_NA, formula=a5_4~a15_7))
  
  
  #### end ####
  
  #### 5. Belief citizen voices are listened to on tax policy ####
  
  # a15_8 [1 agree, 0 disagree/neither] Citizen voices are always considered in drafting and enacting the tax Bills
  
  matches9_C <- matchit(a15_8 ~ urban + female + age
                        + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                        data=data_no_NA, method="cem", cutpoints=cutpoints)
  summary(matches9_C)
  
  match_set9_C <- match.data(matches9_C)
  
  Ufit9 <- lm(a5_4 ~ a15_8, data = match_set9_C, weights = weights)
  coeftest(Ufit9, vcov. = vcovHC)
  
  summary(lm(data=data_no_NA, formula=a5_4~a15_8))
  
  
  #### end ####
  
  #### 6. Belief that taxes are a civic duty ####
  
  # a5_7 [1 agree, 0 disagree/neither] Paying tax is civic duty and should be paid regardless of whether a service provided by government is poor/delayed/not given
  
  matches12_C <- matchit(a5_7 ~ urban + female + age
                         + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                         data=data_no_NA, method="cem", cutpoints=cutpoints)
  summary(matches12_C)
  
  match_set12_C <- match.data(matches12_C)
  
  Ufit12 <- lm(a5_4 ~ a5_7, data = match_set12_C, weights = weights)
  coeftest(Ufit12, vcov. = vcovHC)
  
  summary(lm(data=data_no_NA, formula=a5_4~a5_7))
  
  
  #### end ####
  
  #### 7. Belief that current taxes are fair  ####
  
  # a15_2 [1 agree, 0 disagree/neither] The current tax rates are fair
  
  matches13_C <- matchit(a15_2 ~ urban + female + age
                         + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                         data=data_no_NA, method="cem", cutpoints=cutpoints)
  summary(matches13_C)
  
  match_set13_C <- match.data(matches13_C)
  
  Ufit13 <- lm(a5_4 ~ a15_2, data = match_set13_C, weights = weights)
  coeftest(Ufit13, vcov. = vcovHC)
  
  summary(lm(data=data_no_NA, formula=a5_4~a15_2))
  
  
  #### end ####
  
  #### 8. Perception of widespread tax evasion  ####
  
  # a22a_1 [1 high, 0 Medium/Low/Never/Don't know/Refused to answer] How would you rate individuals/business and organization level of tax evasion in Uganda?
  
  matches16_C <- matchit(a22a_1 ~ urban + female + age
                         + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                         data=data_no_NA, method="cem", cutpoints=cutpoints)
  summary(matches16_C)
  
  match_set16_C <- match.data(matches16_C)
  
  Ufit16 <- lm(a5_4 ~ a22a_1, data = match_set16_C, weights = weights)
  coeftest(Ufit16, vcov. = vcovHC)
  
  summary(lm(data=data_no_NA, formula=a5_4~a22a_1))
  
  
  #### end ####





### PSM -- OUTCOME: I'd happily pay taxes without enforcement -->  a5_4 [1 agree, 0 neither/disagree] You would happily pay taxes without any enforcement

  #### 1. Belief tax is important for country ####
  
  matches4_o3_L <- matchit(a5_1 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="logit")
  
  matches4_o3_F <- matchit(a5_1 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="randomforest")
  matches4_o3_Fd <- matchit(a5_1 ~ setting + female + age,
                            data_no_NA, method="nearest", distance="randomforest")
  
  matches4_o3_N <- matchit(a5_1 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="nnet", distance.options=list(size=10))
  
  summary(matches4_o3_L)
  summary(matches4_o3_F)
  summary(matches4_o3_Fd)
  summary(matches4_o3_N)
  
  match_set4_o3_L <- match.data(matches4_o3_L)
  match_set4_o3_F <- match.data(matches4_o3_F)
  match_set4_o3_Fd <- match.data(matches4_o3_Fd)
  match_set4_o3_N <- match.data(matches4_o3_N)
  
  summary(lm(a5_4 ~ a5_1, data = match_set4_o3_L, weights = weights))
  summary(lm(a5_4 ~ a5_1, data = match_set4_o3_F, weights = weights))
  summary(lm(a5_4 ~ a5_1, data = match_set4_o3_Fd, weights = weights))
  summary(lm(a5_4 ~ a5_1, data = match_set4_o3_N, weights = weights))
  # a5_4 [1 agree, 2 neither, 3 disagree] You would happily pay taxes without any enforcement
  # a5_1 [1 agree, 0 disagree/neither] Tax is important for the country success and economy
  
  # Logit/Forest/Net: Believing tax is important for the country makes people about 11-19 percentage
  #                 points happier paying for taxes without enforcement,
  #                 significant at <0.1%.
  
  summary(matches4_o3_F, un=FALSE)
  
  
  #### end ####
  
  #### 2. Confidence taxes are spent well ####
  
  matches6_o3_L <- matchit(a15_1 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="logit")
  
  matches6_o3_F <- matchit(a15_1 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="randomforest")
  matches6_o3_Fd <- matchit(a15_1 ~ setting + female + age,
                            data_no_NA, method="nearest", distance="randomforest")
  
  matches6_o3_N <- matchit(a15_1 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="nnet", distance.options=list(size=10))
  
  summary(matches6_o3_L)
  summary(matches6_o3_F)
  summary(matches6_o3_Fd)
  summary(matches6_o3_N)
  
  match_set6_o3_L <- match.data(matches6_o3_L)
  match_set6_o3_F <- match.data(matches6_o3_F)
  match_set6_o3_Fd <- match.data(matches6_o3_Fd)
  match_set6_o3_N <- match.data(matches6_o3_N)
  
  summary(lm(a5_4 ~ a15_1, data = match_set6_o3_L, weights = weights))
  summary(lm(a5_4 ~ a15_1, data = match_set6_o3_F, weights = weights))
  summary(lm(a5_4 ~ a15_1, data = match_set6_o3_Fd, weights = weights))
  summary(lm(a5_4 ~ a15_1, data = match_set6_o3_N, weights = weights))
  

  
  #### end ####
  
  #### 3. Knowledge of what taxes are spent on ####
  
  matches7_o3_L <- matchit(a15_6 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="logit")
  
  matches7_o3_F <- matchit(a15_6 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="randomforest")
  matches7_o3_Fd <- matchit(a15_6 ~ setting + female + age,
                            data_no_NA, method="nearest", distance="randomforest")
  
  matches7_o3_N <- matchit(a15_6 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="nnet", distance.options=list(size=10))
  
  summary(matches7_o3_L)
  summary(matches7_o3_F)
  summary(matches7_o3_Fd)
  summary(matches7_o3_N)
  
  match_set7_o3_L <- match.data(matches7_o3_L)
  match_set7_o3_F <- match.data(matches7_o3_F)
  match_set7_o3_Fd <- match.data(matches7_o3_Fd)
  match_set7_o3_N <- match.data(matches7_o3_N)
  
  summary(lm(a5_4 ~ a15_6, data = match_set7_o3_L, weights = weights))
  summary(lm(a5_4 ~ a15_6, data = match_set7_o3_F, weights = weights))
  summary(lm(a5_4 ~ a15_6, data = match_set7_o3_Fd, weights = weights))
  summary(lm(a5_4 ~ a15_6, data = match_set7_o3_N, weights = weights))

  
  #### end ####
  
  #### 4. Belief that the gov. taxes harmful products to be fair ####
  
  matches8_o3_L <- matchit(a15_7 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="logit")
  
  matches8_o3_F <- matchit(a15_7 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="randomforest")
  matches8_o3_Fd <- matchit(a15_7 ~ setting + female + age,
                            data_no_NA, method="nearest", distance="randomforest")
  
  matches8_o3_N <- matchit(a15_7 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="nnet", distance.options=list(size=10))
  
  summary(matches8_o3_L)
  summary(matches8_o3_F)
  summary(matches8_o3_Fd)
  summary(matches8_o3_N)
  
  match_set8_o3_L <- match.data(matches8_o3_L)
  match_set8_o3_F <- match.data(matches8_o3_F)
  match_set8_o3_Fd <- match.data(matches8_o3_Fd)
  match_set8_o3_N <- match.data(matches8_o3_N)
  
  summary(lm(a5_4 ~ a15_7, data = match_set8_o3_L, weights = weights))
  summary(lm(a5_4 ~ a15_7, data = match_set8_o3_F, weights = weights))
  summary(lm(a5_4 ~ a15_7, data = match_set8_o3_Fd, weights = weights))
  summary(lm(a5_4 ~ a15_7, data = match_set8_o3_N, weights = weights))

  
  #### end ####
  
  #### 5. Belief citizen voices are listened to on tax policy ####
  
  matches9_o3_L <- matchit(a15_8 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="logit")
  
  matches9_o3_F <- matchit(a15_8 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="randomforest")
  matches9_o3_Fd <- matchit(a15_8 ~ setting + female + age,
                            data_no_NA, method="nearest", distance="randomforest")
  
  matches9_o3_N <- matchit(a15_8 ~ setting + female + age
                           + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                           data_no_NA, method="nearest", distance="nnet", distance.options=list(size=10))
  
  summary(matches9_o3_L)
  summary(matches9_o3_F)
  summary(matches9_o3_Fd)
  summary(matches9_o3_N)
  
  match_set9_o3_L <- match.data(matches9_o3_L)
  match_set9_o3_F <- match.data(matches9_o3_F)
  match_set9_o3_Fd <- match.data(matches9_o3_Fd)
  match_set9_o3_N <- match.data(matches9_o3_N)
  
  summary(lm(a5_4 ~ a15_8, data = match_set9_o3_L, weights = weights))
  summary(lm(a5_4 ~ a15_8, data = match_set9_o3_F, weights = weights))
  summary(lm(a5_4 ~ a15_8, data = match_set9_o3_Fd, weights = weights))
  summary(lm(a5_4 ~ a15_8, data = match_set9_o3_N, weights = weights))

  
  #### end ####

  #### 6. Belief that taxes are a civic duty ####
  
  matches12_o3_L <- matchit(a5_7 ~ setting + female + age
                            + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                            data_no_NA, method="nearest", distance="logit")
  
  matches12_o3_F <- matchit(a5_7 ~ setting + female + age
                            + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                            data_no_NA, method="nearest", distance="randomforest")
  matches12_o3_Fd <- matchit(a5_7 ~ setting + female + age,
                             data_no_NA, method="nearest", distance="randomforest")
  
  matches12_o3_N <- matchit(a5_7 ~ setting + female + age
                            + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                            data_no_NA, method="nearest", distance="nnet", distance.options=list(size=10))
  
  summary(matches12_o3_L)
  summary(matches12_o3_F)
  summary(matches12_o3_Fd)
  summary(matches12_o3_N)
  
  match_set12_o3_L <- match.data(matches12_o3_L)
  match_set12_o3_F <- match.data(matches12_o3_F)
  match_set12_o3_Fd <- match.data(matches12_o3_Fd)
  match_set12_o3_N <- match.data(matches12_o3_N)
  
  summary(lm(a5_4 ~ a5_7, data = match_set12_o3_L, weights = weights))
  summary(lm(a5_4 ~ a5_7, data = match_set12_o3_F, weights = weights))
  summary(lm(a5_4 ~ a5_7, data = match_set12_o3_Fd, weights = weights))
  summary(lm(a5_4 ~ a5_7, data = match_set12_o3_N, weights = weights))
  
  
  #### end ####
  
  #### 7. Belief that current taxes are fair  ####
  
  matches13_o3_L <- matchit(a15_2 ~ setting + female + age
                            + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                            data_no_NA, method="nearest", distance="logit")
  
  matches13_o3_F <- matchit(a15_2 ~ setting + female + age
                            + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                            data_no_NA, method="nearest", distance="randomforest")
  matches13_o3_Fd <- matchit(a15_2 ~ setting + female + age,
                             data_no_NA, method="nearest", distance="randomforest")
  
  matches13_o3_N <- matchit(a15_2 ~ setting + female + age
                            + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                            data_no_NA, method="nearest", distance="nnet", distance.options=list(size=10))
  
  summary(matches13_o3_L)
  summary(matches13_o3_F)
  summary(matches13_o3_Fd)
  summary(matches13_o3_N)
  
  match_set13_o3_L <- match.data(matches13_o3_L)
  match_set13_o3_F <- match.data(matches13_o3_F)
  match_set13_o3_Fd <- match.data(matches13_o3_Fd)
  match_set13_o3_N <- match.data(matches13_o3_N)
  
  summary(lm(a5_4 ~ a15_2, data = match_set13_o3_L, weights = weights))
  summary(lm(a5_4 ~ a15_2, data = match_set13_o3_F, weights = weights))
  summary(lm(a5_4 ~ a15_2, data = match_set13_o3_Fd, weights = weights))
  summary(lm(a5_4 ~ a15_2, data = match_set13_o3_N, weights = weights))
  
  
  #### end ####
  
  #### 8. Perception of widespread tax evasion  ####
  
  # a22a_1 [1 high, 0 Medium/Low/Never/Don't know/Refused to answer] How would you rate individuals/business and organization level of tax evasion in Uganda?
  
  matches16_L <- matchit(a22a_1 ~ urban + female + age
                         + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                         data_no_NA, method="nearest", distance="logit")
  
  matches16_F <- matchit(a22a_1 ~ urban + female + age
                         + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                         data_no_NA, method="nearest", distance="randomforest")
  matches16_Fd <- matchit(a22a_1 ~ urban + female + age,
                         data_no_NA, method="nearest", distance="randomforest")
  
  matches16_N <- matchit(a22a_1 ~ urban + female + age
                         + num_taxes_know + a1prop + a1bus + a19_1 + b1 + c1a_1,
                         data_no_NA, method="nearest", distance="nnet", distance.options=list(size=10))
  
  summary(matches16_L)
  summary(matches16_F)
  summary(matches16_Fd)
  summary(matches16_N)
  
  match_set16_L <- match.data(matches16_L)
  match_set16_F <- match.data(matches16_F)
  match_set16_Fd <- match.data(matches16_Fd)
  match_set16_N <- match.data(matches16_N)
  
  summary(lm(a5_4 ~ a22a_1, data = match_set16_L, weights = weights))
  summary(lm(a5_4 ~ a22a_1, data = match_set16_F, weights = weights))
  summary(lm(a5_4 ~ a22a_1, data = match_set16_Fd, weights = weights))
  summary(lm(a5_4 ~ a22a_1, data = match_set16_N, weights = weights))
  
  #### end ####
  
  