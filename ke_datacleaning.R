library(tidyverse)
library(dplyr)
library(haven)

raw_data_labelled <- read_dta("IpsosKe_Taxation_data_Round5.dta")
raw_data <- zap_labels(raw_data_labelled)

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


# Standardize and rename yes-no variables
# EMP - dummy for employment
# BUSINESS_OWNER - dummy for owning a business
# PAST_OWNER - dummy for having owned business in past 5 years
# HAD_REG_PAST - dummy for if had regulation / permits
# NEEDED_REG_PAST - dummy for if past business needed regulation / permits
# NEEDED_LOAN_PAST - dummy for having needed loan to start past business
# GOT_LOAN_PAST - dummy for if respondent needed and actually received loan for past business
# BUSINESS_OPTIMISM - dummy for if respondent thinks current business will do well in future
# BUSINESS_TOO_BIG - dummy for if respondent thinks current business is getting too big to run
# HAVE_REG - dummy for if business owner has permit needed for current business
# NEEDED_LOAN - dummy for if loan needed to start current business
# GOT_LOAN - dummy for if got loan for current business
# FAVOURABLE_BUSINESS_CONDIS - dummy for agreement with "The economic conditions in Kenya are currently favorable for prosperity of any business"
# GOVT_CREATING_OPS - dummy for agreement with "The government is creating exciting opportunities for small business"
# STABLE_ENV - dummy for agreement with "Kenya has a stable political environment for prosperity of any business"
# MORE_OPS - dummy for agreement with "There is more business opportunities now compared to a year ago"
# COST_INCREASED - dummy for agreement with "The cost of running the business has increased compared to a year ago"
# TAX_TOO_HIGH - dummy for agreement with "Taxes and levies are too high and are a hindrance to the growth of business"
# KRA_AWARE - dummy for awareness of KRA
# KRA_INTERACT - dummy for interaction with KRA


cols_to_stdze <- c("a1a", "a1ci", "a2a", "a2bi", "a3i", "b14i", "b14ii", "b7iii_1", "b7iii_2", "b12", "b14i", "b14ii", "c19ii_1", "c19ii_2", "c19ii_3", "c19ii_4", "c19ii_5", "c19ii_6", "d7, d10")
new_col_names <- c("emp", "business_owner", "past_owner", "had_reg_past", "needed_reg_past", "needed_loan_past", "got_loan_past", "business_optimism", "business_too_big", "have_reg", "neded_loan", "got_loan", "favourable_business_condis", "govt_creating_ops", "stable_env", "more_ops", "cost_increased", "tax_too_high", "kra_aware", "kra_interact")

data <- convert_yes_no_many_cols(raw_data, cols_to_stdze, new_col_names)



# Renaming relevant columns
# EASE_OF_BUSINESS - rate ease of doing business from 1 (easy) to 3 (hard)
# TAX_IMPORTANT - agreement with "Tax is important for the country success and economy" from 1 (Agree) to 3 (Disagree)
# TAX_WRONG - agreement with "It is wrong if a taxpayer does not declare all of his income so as can pay less tax" from 1 (Agree) to 3 (Disagree)
# TAX_SERVICES - agreement with "Avoiding paying taxes if service provided by government is poor/delayed/not given at all is understandable" from 1 (Agree) to 3 (Disagree)
# TAX_ENFORCE - agreement with "You would happily pay taxes without any enforcement" from 1 (Agree) to 3 (Disagree)
# TAX_CHEAT - agreement with "You would cheat on tax if you have had a chance" from 1 (Agree) to 3 (Disagree)
# TAX_HEAVY - agreement with "Taxes are so heavy that tax evasion is an economic necessity" from 1 (Agree) to 3 (Disagree)
# TAX_DUTY - agreement with "Paying tax is civic duty and should be paid regardless of whether service provided by government is poor/delayed/not given from 1 (Agree) to 3 (Disagree)
# KRA_EXPERIENCE - rating of KRA on scale of 1 (Good), 2 (Neither), 3 (Bad), 4 (DK)
data$ease_of_business <- d4ata$c18i
data$tax_important <- data$d5_1
data$tax_wrong <- data$d5_2
data$tax_services <- data$d5_3
data$tax_enforce <- data$d5_4
data$tax_cheat <- data$d5_5
data$tax_heavy <- data$d5_6
data$tax_duty <- data$d5_7
data$kra_exp <- dta$d13i



# INCOME_SOURCE- main source of income
# 1 = remit, family; 2 = remit, other; 3 = remit, govt; 4 = pension; 5 = stock income; 6 = wage income; 7 = Selling agricultural goods; 8 = Other, 9 = irregular work
data <- convert_cols_to_single(data, c("a1cii_1", "a1cii_2", "a1cii_3", "a1cii_4", "a1cii_5", "a1cii_6", "a1cii_7", "a1cii_8"), "income_source", c(1, 2, 3, 4, 5, 6, 7, 8))
data <- convert_based_on_col(data, "a1cii_8oth", 9, "income_source", 9)

# NOT_OPERATE_BUSINESS - reason for not owning or operating business now
# 1 = respondent closed it; 2 = govt closed it; 3 = Respondent gave it away; 4 = Respondent moved to formal employment; 5 = Destroyed by disaster; 6 = Business demolished; 7 - COVID-related impacts; 8 = Other
data <- convert_cols_to_single(data, c("a4_1", "a4_2", "a4_3", "a4_4", "a4_5", "a4_6", "a4_7"), "income_source", c(1, 2, 3, 4, 5, 6, 7))
data <- convert_based_on_col(data, "a4_9oth", 1, "income_source", 8, reverse=TRUE)

# WHY_PAY_TAX - reasons for you or others paying tax
# 1 = it is compulsory; 2 = expect it will help service delivery; 3 = to ease economy; 4 = to upport government; 5 = religious reasons; 7 = for country loan repayment; 998 - Don't know; 999 = None
data <- convert_cols_to_single(data, c("d4_1", "d4_2"), "why_pay_tax", c(1, 2))
data <- convert_based_on_col(data, "d4_3oth", c(4, 5, 6, 7, 995, 998, 999), "why_pay_tax", c(3, 4, 5, 6, NA, 998, 999), many=TRUE)