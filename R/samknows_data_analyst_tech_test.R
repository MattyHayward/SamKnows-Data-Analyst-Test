## Matthew Hayward, 2022
## SamKnows Data Analyst Tech Test


## Inputs used:
#
# "../data/details_for_each_person.csv"
# "../data/download_speed_measurements.csv"
# "../data/upload_speed_measurements.csv"


## Outputs generated:
#  
#  "../output/Combined_table.csv"
#     - One line per person
#     - Only contains people in cities 'Samsville' and 'Databury'
#     - Removes measurements which have failed to fun
#     - Removes measurements which are otherwise missing (i.e. record "0")
#     - Only contains tests completed during January 2021
#     - Calculates a single average download and upload speed for each person,
#         including a 60th percentile daily download speed over the month.
#  
#  "../output/Samsville_Output_Table.html"
#  "../output/Databury_Output_Table.html"
#     - html summary tables generated for the two cities
#     - Shows the difference between download/upload speeds for the connection
#         types and the two ISPs.
#     - Simple statistics generated (mean, standard deviation, median, IQR)
#  
#  "../output/Download_Speeds.pdf"
#     - Violin plot of download speed distributions by ISPs, for the two
#         cities and for the three connection types.
#     - Arranged as matrix of subplots with variable axes.
#  
#  "../output/Download_Speeds_by_Hour.pdf"
#     - Line plot of download speeds across the hours of the day, split by
#         connection type.








#---------------- Code Begin

#------- Libraries (1)

library(tidyverse)       ## To load readr, dplyr, tidyr, ggplot2...
library(lubridate)       ## Date-time additional functionailty
library(reactable)       ## Enhanced html tables
library(htmlwidgets)     ## 
library(htmltools)       ## 



#------- Data reading and processing (2)

## Variables for date range of interest
start_date <- dmy_hms("1 Jan 2021 00:00:00")
end_date <- dmy_hms("31 Jan 2021 23:59:59")

## Read the person details, filtering for only two cities, Databury and
#  Samsville. (Removing Irrellevantsford)
person_details <- read_csv("../data/details_for_each_person.csv", show_col_types = FALSE) %>%
  filter(city == "Databury" | city == "Samsville")   

## This reads the download measurements, while filtering out tests that did not
#  complete successfully and filters out any tests performed outside of
#  January 2021:
dl_measurements_jan21 <- read_csv("../data/download_speed_measurements.csv", show_col_types = FALSE) %>%
  filter(did_test_complete_successfully == TRUE, time_of_measurement >= start_date & time_of_measurement <= end_date)

## The same here, but for upload measurements:
ul_measurements_jan21 <- read_csv("../data/upload_speed_measurements.csv", show_col_types = FALSE) %>%
  filter(did_test_complete_successfully == TRUE, time_of_measurement >= start_date & time_of_measurement <= end_date)

## Some cleaning is required. Some measurements have zeros recorded despite
#  flagging as successfully completed. These are removed here:
dl_measurements_jan21_nozeros <- dl_measurements_jan21 %>%
  filter(measured_download_speed_in_Mbps > 0.)
ul_measurements_jan21_nozeros <- ul_measurements_jan21 %>%
  filter(measured_upload_speed_in_Mbps > 0.)






#------- Statistical Operations (2)

## Group by IDs and calculate averages speeds and 60th percentiles
dl_avg <- dl_measurements_jan21_nozeros %>%
  group_by(person_id) %>%
  summarise(avg_download_speed = mean(measured_download_speed_in_Mbps))
ul_avg <- ul_measurements_jan21_nozeros %>%
  group_by(person_id) %>%
  summarise(avg_upload_speed = mean(measured_upload_speed_in_Mbps))

## For percentiles, need to group by both ID and day to be able to average the
#  download speed measure for each person each day, then group again by ID to
#  calculate the 60th percentile over the month for each person.
dl_60th_pct <- dl_measurements_jan21_nozeros %>%
  group_by(person_id, floor_date(time_of_measurement, "day")) %>%
  summarise(avg_download_daily = mean(measured_download_speed_in_Mbps), .groups = "drop") %>%
  group_by(person_id) %>%
  summarise(avg_download_60th_pct = quantile(avg_download_daily, 0.60), .groups = "drop")

## Joining columns to the main df.
combined_tbl <- person_details %>%
  inner_join(dl_avg, by = "person_id") %>%
  inner_join(ul_avg, by = "person_id") %>%
  inner_join(dl_60th_pct, by = "person_id")

## Further data cleaning. Some connection types appear to be mislabeled; this
#  can be rectified by logical comparisons using information we already know
#  about the connection types, i.e. by comparing expected average speeds and
#  changing if necessary.
## This solution, however, is quite naive as there are heavy assumptions made
#  about the data e.g. relatively low variance, no overlapping of speeds by
#  connection type, etc.
combined_tbl$type_of_broadband_connection <- ifelse((combined_tbl$avg_download_speed < 20.) & (combined_tbl$avg_upload_speed < 1.) & (combined_tbl$type_of_broadband_connection != "ADSL"), "ADSL", combined_tbl$type_of_broadband_connection)
combined_tbl$type_of_broadband_connection <- ifelse((combined_tbl$avg_download_speed > 100.) & (combined_tbl$type_of_broadband_connection != "Fibre"), "Fibre", combined_tbl$type_of_broadband_connection)
combined_tbl$type_of_broadband_connection <- ifelse((combined_tbl$avg_download_speed >= 20.) & (combined_tbl$avg_download_speed <= 100.) & (combined_tbl$type_of_broadband_connection != "VDSL"), "VDSL", combined_tbl$type_of_broadband_connection)

## Write new csv of this combined data frame:
write.csv(combined_tbl, "../output/Combined_table.csv", row.names = FALSE)




#------- Data Quality Issues (3)

## A number of quality issues exist in the original dataset, some of which
#  have been resolved earlier:
#     - Some people from different cities exist in the data
#     - There are plenty of data where, despite being labelled as completing
#        successfully, have zero results.
#     - A few instances of mislabelling of connection type
#
## Left uncleaned, the latter two could significantly affect any statistical
#  calculations performed on the data, for example any Fibre cases
#  mislabelled as ADSL could greatly increase the calculated mean.
#
## There are potentially other quality issues, as will be illustrated in:
#    "../output/Download_Speeds.pdf"
#  where there are a handful of mislabelled ISPs. They seem to match the
#  other ISP's speed distribution but are strong outliers in how they are
#  labelled currently. However, there are likely not many of these such to
#  make a significant effect.






#------- Summary table for connection types by ISP at Samsville and Databury (4a)

## Function for generating reactable with summary statistics for a
#  particular city

output_summary_table <- function(df, city_string) {

  ## Filter by city, group by ISP & connection type before calculating
  #  statistics for download & upload.
  summary_df <- df %>% 
    filter(city == city_string) %>%
    group_by(name_of_isp, type_of_broadband_connection) %>%
    summarise(across(avg_download_speed:avg_upload_speed, mean, .names = "mean_{.col}"),
              across(avg_download_speed:avg_upload_speed, sd, .names = "sd_{.col}"),
              across(avg_download_speed:avg_upload_speed, median, .names = "median_{.col}"),
              across(avg_download_speed:avg_upload_speed, IQR, .names = "iqr_{.col}"),
              .groups = "drop")
  
  ## and now generate reactable
  returned_df <- reactable(summary_df,
    ## Formatting interactive table
    columns = list(
      name_of_isp = colDef(name = "ISP"),
      type_of_broadband_connection = colDef(
        name = "Connection Type",
        style = list(borderRight = "2px solid #d4d4d4"),
        headerStyle = list(borderRight = "2px solid #d4d4d4")
        ),
      mean_avg_download_speed = colDef(
        name = "Mean",
        format = colFormat(digits = 2),
        style = list(borderLeft = "1px solid #d4d4d4"),
        headerStyle = list(borderLeft = "1px solid #d4d4d4")
        ),
      mean_avg_upload_speed = colDef(
        name = "Mean",
        format = colFormat(digits = 2),
        style = list(borderLeft = "1px solid #d4d4d4"),
        headerStyle = list(borderLeft = "1px solid #d4d4d4")
        ),
      sd_avg_download_speed = colDef(
        name = "SD",
        format = colFormat(digits = 2)
        ),
      sd_avg_upload_speed = colDef(
        name = "SD",
        format = colFormat(digits = 2)
        ),
      median_avg_download_speed = colDef(
        name = "Median",
        format = colFormat(digits = 2)
        ),
      median_avg_upload_speed = colDef(
        name = "Median",
        format = colFormat(digits = 2)
        ),
      iqr_avg_download_speed = colDef(
        name = "IQR",
        format = colFormat(digits = 2)
        ),
      iqr_avg_upload_speed = colDef(
        name = "IQR",
        format = colFormat(digits = 2)
        )
      ),
    columnGroups = list(
      colGroup(
        name = "",
        columns = "type_of_broadband_connection",
        headerStyle = list(borderRight = "1px solid #d4d4d4")
        ),
      colGroup(
        name = "Download Speed (Mbps)",
        columns = c("mean_avg_download_speed", "sd_avg_download_speed", "median_avg_download_speed", "iqr_avg_download_speed"),
        headerStyle = list(borderLeft = "1px solid #d4d4d4")
        ),
      colGroup(
        name = "Upload Speed (Mbps)",
        columns = c("mean_avg_upload_speed", "sd_avg_upload_speed",  "median_avg_upload_speed", "iqr_avg_upload_speed"),
        headerStyle = list(borderLeft = "1px solid #d4d4d4")
        )
      ),
    groupBy = "type_of_broadband_connection",
    highlight = TRUE,
    theme = reactableTheme(
      highlightColor = "#f0f5f9",
      style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
      )
    )
  
  return(returned_df)  ## Return reactable
}

## Calling function for both cities of interest, outputting both to .html files:
samsville_output_table <- output_summary_table(combined_tbl, "Samsville") %>%
  prependContent(h2(class = "title", "Samsville Internet Connection Speeds")) %>%
  saveWidget(
    file = "../output/Samsville_Output_Table.html",
    selfcontained = TRUE,
    title = "Samsville")
databury_output_table <- output_summary_table(combined_tbl, "Databury") %>%
  prependContent(h2(class = "title", "Databury Internet Connection Speeds")) %>%
  saveWidget(
    file = "../output/Databury_Output_Table.html",
    selfcontained = TRUE,
    title = "Databury")

## These tables are for 4a).
##  In summary, it is shown that for both cities Useus perform greater in terms
#   of down and up speeds than Fibrelicious with ADSL, however for the other
#   connection types (VDSL & Fibre) Fibrelicious perform better, with VDSL
#   having the smallest proportional difference between ISPs. 
##  Results are broadly similar between cities for ASDL. However, in Databury, 
#   Fibre connections are marginally slower with lower variance and VDSL
#   connections are significantly slower than in Samsville.




#------- Data visualisation (4b)

## New temp dataframe for this section - rearranging connection type order:
plotting_tbl <- combined_tbl
plotting_tbl$type_of_broadband_connection_sorted <- factor(
  plotting_tbl$type_of_broadband_connection,
  levels = c("Fibre", "VDSL", "ADSL"))

## Violin plot of connection speeds
p <- plotting_tbl %>%
  ggplot(
    aes(x = name_of_isp, y = avg_download_speed, fill = name_of_isp)) +
    geom_violin(position = position_dodge(1)) + 
    ggtitle("Download Speeds") + 
    xlab("ISP") +
    ylab("Download Speed (Mbps)") + 
    labs(fill = "ISP") +
    facet_grid(type_of_broadband_connection_sorted ~ city, scales = "free") + 
    theme(plot.title = element_text(hjust = 0.5))
  
## Save plot as (.pdf)
ggsave(plot = p,
       width = 5,
       height = 4,
       dpi = 300,
       filename = "../output/Download_Speeds.pdf")




## 4c) "If I am a consumer living in Databury and I have a Fibre connection,
#      am I going to get a better/worse speed from Fibrelicious or from Useus?
#      If so, how much better/worse?"

## A consumer in Databury will get a better download speed from Fibrelicious
#  than from Useus, by an average of 216 Mbps in comparison to 184 Mbps.
#  However upload speeds in the same situation may be marginally lower from
#  Fibrelicious with an average of 72.9 Mbps in comparison to 73.8 Mbps.
#  Unlike the download speeds, however this isn't as high confidence as the
#  range of average upload speeds overlap.






#------- Download speed differences by time of day (4d)

## Taking download speed data processed earlier, combining with person_id
#  for connection type, and processing for average speeds for every hour
#  of the day.
dl_by_hour <- dl_measurements_jan21_nozeros %>%
  inner_join(combined_tbl, by = "person_id") %>%
  mutate(hour_of_measurement = hour(time_of_measurement))

## Function to return a table of connection type filtered average speeds,
#  before joining them together for use in plot.
filter_by_hour <- function(df, connection_type) {
  returned_df <- df %>%
    filter(type_of_broadband_connection == connection_type) %>%
    group_by(hour_of_measurement) %>%
    summarise(avg_download_by_day = mean(measured_download_speed_in_Mbps), .groups = "drop")
  return(returned_df)
}

dl_by_hour_Fibre <- filter_by_hour(dl_by_hour, "Fibre") %>%
  rename(Fibre = avg_download_by_day)
dl_by_hour_VDSL <- filter_by_hour(dl_by_hour, "VDSL") %>%
  rename(VDSL = avg_download_by_day)
dl_by_hour_ADSL <- filter_by_hour(dl_by_hour, "ADSL") %>%
  rename(ADSL = avg_download_by_day)
dl_by_hour_all <- dl_by_hour_Fibre %>%
  inner_join(dl_by_hour_VDSL, by = "hour_of_measurement") %>%
  inner_join(dl_by_hour_ADSL, by = "hour_of_measurement")

## Reshaping for use in plot.
dl_by_hour_all <- pivot_longer(dl_by_hour_all, cols = 2:4, names_to="type", values_to="speeds")

## Rearranging for visual accessibility.
dl_by_hour_all$type <- factor(dl_by_hour_all$type, levels=c('Fibre','VDSL','ADSL'))

## Line plot of connection speeds over the day to find trends.
p <- dl_by_hour_all %>%
  ggplot(
    aes(x=hour_of_measurement, y=speeds, group=type)) +
    geom_line(aes(linetype=type, color=type)) +
    geom_point(aes(color=type)) +
    facet_grid(type ~ ., scales='free') +
    ggtitle("Download Speeds by Hour") + 
    xlab("Hour of Day") +
    ylab("Average Download Speed (Mbps)") + 
    labs(fill = "Connection Type") +
    theme(plot.title = element_text(hjust = 0.5))

## Save plot as (.pdf)
ggsave(plot = p,
       width = 5,
       height = 4,
       dpi = 300,
       filename = "../output/Download_Speeds_by_Hour.pdf")


## The generated figure shows that, for all connection types, there is
#  a signficant fall in download speeds between the hours 6pm-10pm.
#  This would likely represent evening peak speed restrictions imposed
#  by ISPs or potentially other peak time network congestion effects
#  over January 2021.

