#Primary Rental Market Rms

install.packages("cmhc")
install.packages("tidyr")
library(cmhc)
library(tidyr)

#Initializing static variables
survey_str <- "Rms"
series_list <- c("Vacancy Rates", "Average Rent", "Average Rent Change", "Median Rent", "Rental Universe")
dimension_str <- "Bedroom Type"
vac_dim_list <- c("Bedroom Type", "Structure Type", "Rent Ranges")
breakdown_str <- "Historical Time Periods"
muni_vec <- c("5921007" = "Nanaimo", "5917030" = "Oak Bay",
              "5917034" = "Victoria", "5915004" = "Surrey",
              "5915022" = "Vancouver", "5915025" = "Burnaby",
              "5915034" = "Coquitlam", "5915043" = "Port Moody",
              "5915055" = "West Vancouver", "5915075" = "Maple Ridge")
d_filter <- "Row/ Apartment"
ranges_filter_list <- c("Bachelor", "1 Bedroom", "2 Bedroom", "3 Bedroom +", "Total")
season_filter <- "October"
category_str <- "Primary Rental Market"
update_str <- Sys.Date()
update_str <- format(update_str, "%Y-%m-%d")
muni_index <- 0

#main Table
export_table_cols <- c("Classification", "Municipality", "Date_Range", "Bachelor", "Reliability_Code_Bach", "One_Bedroom", "Reliability_Code_1bed",
                       "Two_Bedroom", "Reliability_Code_2bed", "Three_Bedroom_plus", "Reliability_Code_3bed", "Total", "Reliability_Code_total",
                       "Category", "Row/Apartment", "Data_Source", "_lastupdate")
combined_R_CMHC <- data.frame(matrix(nrow = 0, ncol = length(export_table_cols)))
colnames(combined_R_CMHC) <- export_table_cols

#Range Table
range_table_cols <- c("Classification", "Municipality", "Date_Range", "Range_Less_Than_750", "Reliability_Code_750", "Range_750_999", "Reliability_Code_750_999",
                      "Range_1000_1249","Reliability_Code_1000_1249", "Range_1250_1499","Reliability_Code_1250_1499","Range_1500_plus", "Reliability_Code_1500",
                      "Non_Market_Unknown", "Reliability_Code_nonmarket", "Total", "Reliability_Code_total",
                      "Category", "Row/Apartment", "Data_Source", "_lastupdate")
range_R_CMHC <- data.frame(matrix(nrow = 0, ncol = length(range_table_cols)))
colnames(range_R_CMHC) <- range_table_cols

#Structure Size Table
structure_table_cols <- c("Classification", "Municipality", "Date_Range", "Units_3_5", "Reliability_Code_3_5", "Units_6_19", "Reliability_Code_6_19",
                          "Units_20_49", "Reliability_Code_20_49", "Units_50_199", "Reliability_Code_50_199", "Units_200_plus", "Reliability_Code_200",
                          "Total", "Reliability_Code_total", "Category", "Row/Apartment", "Data_Source", "_lastupdate")
structure_R_CMHC <- data.frame(matrix(nrow = 0, ncol = length(structure_table_cols)))
colnames(structure_R_CMHC) <- structure_table_cols

#Iterate Through Municipality
for(a in muni_vec)
{
  muni_index <- muni_index + 1
  muni_ID <- as.numeric(names(muni_vec)[muni_index])
  muni_name <- muni_vec[muni_index]
  
  #Iterate through Series
  for (series in series_list)
  {
    #different path for vacancy rates
    if(series == "Vacancy Rates")
    {
      for (vac_dim in vac_dim_list)
      {
        #more filters for rent ranges
        if (vac_dim == "Rent Ranges")
        {
          for (r_filter in ranges_filter_list)
          {
            #get CMHC
            current_table <- get_cmhc(survey_str, series, vac_dim, breakdown_str, "Default", muni_ID,
                                      filters = list("dwelling_type_desc_en" = d_filter, "season" = "October", "bedroom_count_type_desc_en" = r_filter))
            #Build Rent Ranges Table
            for (f in 1:nrow(current_table))
            {
              #get dateString year value
              date_str <- current_table$DateString[f]
              date_vec <- strsplit(date_str, split = ' ')
              date_num <- as.numeric(date_vec[[1]][2])
              date_month <- date_vec[[1]][1]
              
              if (date_num >= 2012 & date_num < 2023)
              {
                current_range <- as.character(current_table$`Rent Ranges`[f])
                current_val <- current_table$Value[f]
                current_rel <- current_table$Quality[f]
                
                #record value for each rent range
                if (current_range == "Less Than $750")
                {
                  seven_val <- current_val
                  seven_rel <- current_rel
                  
                } else if (current_range == "$750 - $999")
                {
                  s_to_n_val <- current_val
                  s_to_n_rel <- current_rel
                  
                } else if (current_range == "$1,000 - $1,249")
                {
                  th_to_tw_val <- current_val
                  th_to_tw_rel <- current_rel
                  
                } else if (current_range == "$1,250 - $1,499")
                {
                  tw_to_fi_val <- current_val
                  tw_to_fi_rel <- current_rel
                  
                } else if (current_range == "Total")
                {
                  total_val <- current_val
                  total_rel <- current_rel
                }
                #every time we get to Total create new row
                if (current_bed_type == "Total")
                {
                  new_row <- c(series, muni_name, date_num, seven_val, seven_rel, s_to_n_val, s_to_n_rel, 
                               th_to_tw_val, th_to_tw_rel, tw_to_fi_val, tw_to_fi_rel, total_val, total_rel,
                               "Primary Rental Market", d_filter, "CMHC", update_str)
                  
                  range_R_CMHC[nrow(range_R_CMHC) + 1] <- new_row
                  
                }
              }
            }
          }
        } else if (vac_dim == "Structure Size")
        {
          
          current_table <- get_cmhc(survey_str, series, vac_dim, breakdown_str, "Default", muni_ID,
                                    filters = list("dwelling_type_desc_en" = d_filter, "season" = "October"))
          #build Structure Size table
          for (g in 1:nrow(current_table))
          {
            #get dateString year value
            date_str <- current_table$DateString[g]
            date_vec <- strsplit(date_str, split = ' ')
            date_num <- as.numeric(date_vec[[1]][2])
            date_month <- date_vec[[1]][1]
            
            if (date_num >= 2012 & date_num < 2023)
            {
              current_range <- as.character(current_table$`Structure Size`[g])
              current_val <- current_table$Value[g]
              current_rel <- current_table$Quality[g]
            }
          }
        
        } else
        {
          
          current_table <- get_cmhc(survey_str, series, vac_dim, breakdown_str, "Default", muni_ID,
                                    filters = list("dwelling_type_desc_en" = d_filter, "season" = "October"))
          #Run table method
          build_table()
    
        }
      }
      
    } else
    {
      
      current_table <- get_cmhc(survey_str, series, dimension_str, breakdown_str, "Default", muni_ID,
                                filters = list("dwelling_type_desc_en" = d_filter, "season" = "October"))
      #Run Table method
      build_table()
      
    }
  }
}

#function to take input table from cmhc and put into final table for export
#only from 2012-2022
#might have to build something else for structure size and rent ranges because they have different fields
build_table <- function()
{
  for (e in 1:nrow(current_table))
  {
    #get dateString year value
    date_str <- current_table$DateString[e]
    date_vec <- strsplit(date_str, split = ' ')
    date_num <- as.numeric(date_vec[[1]][2])
    date_month <- date_vec[[1]][1]
    
    if (date_num >= 2012 & date_num < 2023)
    {
      current_bed_type <- as.character(current_table$`Bedroom Type`[e])
      current_val <- current_table$Value[e]
      current_rel <- as.character(current_table$Quality[e])
      #apply values to different Room types
      if(current_bed_type == "Bachelor")
      {
        bach_val <- current_val
        bach_rel <- current_rel
        
      } else if(current_bed_type == "1 Bedroom")
      {
        one_val <- current_val
        one_rel <- current_rel
      
      } else if(current_bed_type == "2 Bedroom")
      {
        two_val <- current_val
        one_rel <- current_rel
        
      } else if(current_bed_type == "3 Bedroom +")
      {
        three_val <- current_val
        three_rel <- current_rel
        
      } else if(current_bed_type == "Total")
      {
        tot_val <- current_val
        tot_rel <- current_rel
        
      }
      #every time we get to Total create new row
      if (current_bed_type == "Total")
      {
        new_row <- c(series, muni_name, date_num, bach_val, bach_rel, one_val, one_rel,
                     two_val, two_rel, three_val, three_rel, tot_val, tot_rel,
                     "Primary Rental Market", d_filter, "CMHC", update_str)
        
        combined_R_CMHC[nrow(combined_R_CMHC) + 1] <- new_row
        
      }
    }
  }
}





