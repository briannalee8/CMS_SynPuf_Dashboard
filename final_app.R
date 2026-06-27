#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# libraries
library(arrow)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(usmap)
library(usmapdata)
library(RColorBrewer)
library(scales)
library(DT)
library(waffle)
library(colorspace)
library(plotly)
library(networkD3)
library(htmlwidgets)
library(viridis)
library(bslib)
library(bsicons)
library(fresh)

##Setting a theme

my_theme <- create_theme(
  adminlte_color(
    light_blue = "#96B5F3",   # secondary
    green      = "#78c2ad",   # primary / minty
    teal       = "#56cc9d",   # success
    aqua       = "#6cc3d5",   # info
    yellow     = "#ffce67",   # warning
    red        = "#ff7851"    # danger
  ),
  adminlte_sidebar(
    dark_bg         = "#78c2ad",
    dark_color      = "#ffffff",
    dark_submenu_bg = "#67b09a"
  ),
  adminlte_global(
    content_bg  = "#f9f9f9",
    box_bg      = "#ffffff",
    info_box_bg = "#ffffff"
  )
)


file_location <- "/Users/briannalee/Documents/MS DS Program/DATA-502/Project Documents/Int DFs/R Files For Dashboard/"
#file_location <- "C:/Users/ianwa/OneDrive/Documents/Willamette/Classes/Data Visualization/Final Project/Data/"

# Create directory for logo (if it doesn't already exist)
dir.create("www")
file.copy("C:/Users/ianwa/OneDrive/Documents/Willamette/Classes/Data Visualization/Final Project/logo.png", "www/logo.png")

# Load Data
OutpatientCPT <- read_parquet(paste0(file_location,"OutpatientCPTValues.parquet"))
OutpatientEvent <- read_parquet(paste0(file_location,"OutpatientEventsFinal.parquet"))
MedEvents <- read_parquet(paste0(file_location, "SelectedMedEventsWithDemographics.parquet"))
Deliveries <- read_parquet(paste0(file_location, "Deliveries.parquet"))
Ectopic <- read_parquet(paste0(file_location, "EctopicEvents.parquet"))
Abortion <- read_parquet(paste0(file_location, "AbortionEvents.parquet"))
Inp_Cardiac <- read_parquet(paste0(file_location, "InpatientCardiacEvents.parquet"))
Inp_Per_Day <- read_parquet(paste0(file_location, "InpatientEventsPerDay.parquet"))
Out_Per_Day <- read_parquet(paste0(file_location, "OutpatientEventsPerDay.parquet"))

BeneAll <- readRDS(paste0(file_location,"BeneAll_1_10.rds"))
BeneDems <- readRDS(paste0(file_location,"BeneDems_1_10.rds"))

state_pop <- read_parquet(paste0(file_location,"state_pop.parquet"))


# Not Being Used
  # InpatientDiagnosis <- read_parquet(paste0(file_location,"InpatientDiagnosisICDFinal.parquet"))
  # MedEvents <- read_parquet(paste0(file_location,"SelectedMedEvents.parquet"))
  # InpatientEvent <- read_parquet(paste0(file_location,"InpatientEventsFinal.parquet"))
  # InpatientProcedure <- read_parquet(paste0(file_location,"InpatientProcedureICDFinal.parquet"))
  # OutpatientDiagnosis <- read_parquet(paste0(file_location,"OutpatientICDDiagnosisValues.parquet"))
  # CarrierClaims <- read_parquet(paste0(file_location,"SelectedCarrierClaims.parquet"))

# Adjust state_pop
state_pop <- state_pop %>% filter(Statefips!=72)

# Adjust Demographic Data
recode_states <- c("1" = "01", "2" = "02", "3" = "04", "4" = "05","5" = "06", "6" ="08", "7"="09", "8" ="10", "9" ="11",
                   "10"="12", "11"="13", "12"="15", "13"="16", "14"="17", "15"="18", "16"="19", "17"="20", "18"="21", "19"="22", 
                   "20"="23", "21"="24", "22"="25", "23"="26", "24"="27", "25"="28", "26"="29", "27"="30", "28"="31", "29"="32", 
                   "30"="33", "31"="34", "32"="35", "33"="36", "34"="37", "35"="38", "36"="39", "37"="40", "38"="41", "39"="42",
                   "41"="44", "42"="45", "43"="46", "44"="47", "45"="48", "46"="49", "47"="50", "49"="51", "50"="53", "51"="54",
                   "52"="55", "53" = "56")

bene_transformation <- function(df) {
  df %>% 
    mutate(
      SP_STATE_CODE = recode(SP_STATE_CODE, !!!recode_states),
      BENE_SEX_IDENT_CD = recode(BENE_SEX_IDENT_CD, "1" = "Male", "2" = "Female"),
      BENE_RACE_CD = recode(BENE_RACE_CD, "1" = "White", "2" = "Black", "3" = "Others", "5" = "Hispanic"),
      BD_YEAR = substr(BENE_BIRTH_DT, 1, 4),
      BD_MON = substr(BENE_BIRTH_DT, 5, 6),
      BD_DAY = substr(BENE_BIRTH_DT, 7, 8),
      BD_DATE = as.Date(paste(BD_YEAR, BD_MON, BD_DAY, sep = "-"))
    )
}

BeneAll <- bene_transformation(BeneAll)
BeneDems <- bene_transformation(BeneDems)
MedEvents <- bene_transformation(MedEvents)

BeneAll_Dep_wide <- BeneAll %>% select(DESYNPUF_ID, year, SP_DEPRESSN, BENE_SEX_IDENT_CD, BENE_RACE_CD) %>% 
  mutate(SP_DEPRESSN = recode(SP_DEPRESSN, "1"=1, "2"=0)) %>% 
  pivot_wider(names_from = year, values_from = SP_DEPRESSN, names_prefix = "depression_")%>% 
  filter(if_any(c(depression_2008, depression_2009, depression_2010), ~ . == 1))

BeneAll_Long_pre <- BeneAll %>% select(DESYNPUF_ID, year, SP_DEPRESSN, BENE_SEX_IDENT_CD, BENE_RACE_CD) %>% 
  mutate(SP_DEPRESSN = recode(SP_DEPRESSN, "1"=1, "2"=0)) %>% 
  filter(SP_DEPRESSN ==1) 


# Adjust per day data
INP_OUT_PERDAY <- Inp_Per_Day %>%
  full_join(Out_Per_Day, by = ("start_date")) %>%
  filter(!is.na(start_date)) %>%
  mutate(inpatient_events_n = replace_na(inpatient_events_n, 0),
         outpatient_events_n = replace_na(outpatient_events_n, 0),
         total_events = inpatient_events_n + outpatient_events_n) %>%
  pivot_longer(
    cols = c(inpatient_events_n, outpatient_events_n, total_events),
    names_to = "event_type",
    values_to = "n_events"
  )

# Adjust urgent/emergency data
UR_EM <- OutpatientEvent %>% 
  mutate(mon_yr = floor_date(start_date, "month"),
         dow = factor(weekdays(start_date), levels = c("Monday", "Tuesday", "Wednesday", 
                                                       "Thursday", "Friday", "Saturday", "Sunday")),
         urg_emer = case_when(
           calc_er_yn == TRUE ~ "Emergency Room Visit",
           TRUE ~ "Urgent Care Visit")) %>% 
  filter((calc_er_yn == TRUE)|(calc_urgentcare_yn == TRUE))%>% 
  left_join(BeneDems, by =("DESYNPUF_ID"))

UR_EM_CPT <- UR_EM %>%
  inner_join(OutpatientCPT, by = c("DESYNPUF_ID", "CLM_ID")) %>%
  mutate(cpt_lower = tolower(cpt_value))

# Adjust delivery data
DEL <- Deliveries %>% 
  mutate(mon_yr = floor_date(start_date, "month"),
         year = year(start_date),
         dow = weekdays(start_date),
        length_of_stay = floor(difftime(end_date,start_date, units="days")),
        ga_cat = case_when(ga_at_delivery <= 37 ~ "Pre-term",
                           ga_at_delivery > 37 & ga_at_delivery < 39 ~ "Early Term",
                          ga_at_delivery > 39 & ga_at_delivery < 41 ~ "Full Term",
                          ga_at_delivery >= 41 & ga_at_delivery < 42 ~ "Late Term",
                          ga_at_delivery >= 42 ~ "Post-term"),
        BENE_RACE_CD = recode(BENE_RACE_CD, "1" = "White", "2" = "Black", "3" = "Others", "5" = "Hispanic")) %>%
  left_join(BeneAll %>% select(DESYNPUF_ID, SP_DEPRESSN, SP_DIABETES, year), by = c ("DESYNPUF_ID", "year"))

# Adjust abortion data
NON_LIVE_AB <- Abortion %>%
  mutate(mon_yr = floor_date(start_date, "month"),
         year = year(start_date),
         BENE_RACE_CD = recode(BENE_RACE_CD, "1" = "White", "2" = "Black", "3" = "Others", "5" = "Hispanic"))  %>% 
  left_join(BeneAll %>% select(DESYNPUF_ID, SP_DEPRESSN, SP_DIABETES, year), by = c ("DESYNPUF_ID", "year"))
 
# Adjust ectopic data
NON_LIVE_EC <- Ectopic %>%
  mutate(mon_yr = floor_date(start_date, "month"),
         year = year(start_date),
         BENE_RACE_CD = recode(BENE_RACE_CD, "1" = "White", "2" = "Black", "3" = "Others", "5" = "Hispanic"))  %>% 
  left_join(BeneAll %>% select(DESYNPUF_ID, SP_DEPRESSN, SP_DIABETES, year), by = c ("DESYNPUF_ID", "year"))

# Adjust cardiac data
OTPT_CARD <- OutpatientEvent %>% filter(calc_cardiac_events_yn == TRUE) %>% 
  mutate(mon_yr = floor_date(start_date, "month"),
        year = year(start_date)) %>%
  left_join(BeneAll %>% select(DESYNPUF_ID, SP_COPD, SP_DIABETES,SP_ISCHMCHT, SP_CHF, BENE_HMO_CVRAGE_TOT_MONS, year), by = c ("DESYNPUF_ID", "year"))%>%
  left_join(BeneDems,  by = c("DESYNPUF_ID"))
  

INPT_CARD <- Inp_Cardiac %>% mutate(mon_yr = floor_date(start_date, "month"),
                                    year = year(start_date),
                                    length_of_stay = floor(difftime(end_date,start_date, units="days"))) %>% 
  left_join(BeneAll %>% select(DESYNPUF_ID, SP_COPD, SP_DIABETES,SP_ISCHMCHT, SP_CHF, BENE_HMO_CVRAGE_TOT_MONS, year), by = c ("DESYNPUF_ID", "year"))%>%
  left_join(BeneDems,  by = c("DESYNPUF_ID"))

# Adjust med data
MedEvents <- MedEvents %>%
  mutate(
    SRVC_YEAR = substr(SRVC_DT, 1, 4),
    SRVC_MON = substr(SRVC_DT, 5, 6),
    SRVC_DAY = substr(SRVC_DT, 7, 8),
    SRVC_DATE = as.Date(paste(SRVC_YEAR, SRVC_MON, SRVC_DAY, sep = "-"))
  )

# MH Visits with Depression Dgns
MHC <- OutpatientEvent %>% 
  mutate(mon_yr = floor_date(start_date, "month"),
         mon = format(start_date, "%b"))%>%
  mutate(year = year(start_date)) %>% 
  filter(calc_psych_yn == TRUE) %>% 
  left_join(BeneAll_Long_pre, by = c("DESYNPUF_ID", "year")) %>%
  mutate(across(SP_DEPRESSN, ~replace_na(., 0)))

# MCV per year per bene
MHC_per_year <- MHC %>% 
  group_by(DESYNPUF_ID, year) %>%
  summarise(mh_count = n())

# Bene with Depression in a given year and # outpt psych visits
BeneAll_Long <- BeneAll_Long_pre %>% left_join(MHC_per_year, by = c("DESYNPUF_ID", "year")) %>%
  mutate(across(mh_count, ~replace_na(., 0))) %>%
  mutate(mh_any = if_else(mh_count>=1,"Yes","No"))


#CSS Styling
css_code <- "
  .sidebar-filters {
    margin: 15px;
    padding: 15px;
    background: #FAFAFA;
    border-radius: 8px;
    border: 1px solid #EBEDF2;
  }
  
  .sidebar-filters .control-label {
    color: #2E2C2C;
  }
  
  .sidebar-filters .checkbox {
    color: #2E2C2C;
  }
  
  .sidebar-filters .filter-section-title {
    margin-top: 1px;
    margin-bottom: 10px;
    color: #000000;
    font-weight: 600;
    text-align: center;
  }
"


# Header
header <- dashboardHeader(
  title ="Evergreen Health Clinic",
  titleWidth = 300
)

# Sidebar
sidebar <- dashboardSidebar(
  width = 300,

  sidebarMenu(
    id = "pages",
    menuItem("Summary", tabName = "Summary"),
    menuItem("Urgent & Emergency Care", tabName = "Urgent"),
    menuItem("Medications", tabName = "Medications"),
    menuItem("Deliveries", tabName = "Deliveries"),
    menuItem("Cardiac Events", tabName = "Events"),
    menuItem("Mental Health Care", tabName = "Mental")
  ),
  
  div(class = "sidebar-filters", 
      tags$h4("FILTERS", class ="filter-section-title"),
      sliderInput("time_months",
                  label = "Months to View:",
                  min = as.Date("2007-11-01"),
                  max = as.Date("2010-12-31"),
                  value = c(as.Date("2007-11-01"),as.Date("2010-12-01")),
                  timeFormat = "%m-%Y"),
      checkboxGroupInput("sex",
                         label = "Select Sex of Patients to Include:",
                         choices = c("Male", "Female"),
                         selected = c("Male", "Female")),
      checkboxGroupInput("race",
                         label = "Select Race/Ethnicity of Patients to Include:",
                         choices = c("White", "Black", "Hispanic", "Others"),
                         selected = c("White", "Black", "Hispanic", "Others")),
      # actionButton("load_data", "Reload Data", width = "90%")
  )
)


# Body
body <- dashboardBody(
  tags$head(
    tags$style(
      HTML(css_code)
    )
  ),
  
  tabItems(
    tabItem(tabName = "Summary",
            fluidRow(
              box(title = "Dashboard Purpose", width = 6, p("The purpose of this dashboard is allow clinical teams interested in purchasing customized dashboard services to see the breadth and complexity of visualizations that could guide clinical staff in immediate decision making and future planning when the dashboard is customized to their specific data and clinical needs. 
                                                            For this reason, the product focuses on variation of visualizations, rather than using continuity as a tool, to demonstrate a wide range of possibilities. This is also why synthetic data was selected to prioritize large populations over plausibility of data to demostrate the tools ability to vizualize a realistic sized patient population.")),
              box(width = 6, img(src = "logo.png", width = "100%"))),
            fluidRow(
              box(title = "About the Data", width = 6, p("This dashboard is populated with Center for Medicaid & Medicare's SynPUF Synthetic Patient data. This data is a large, 
                                                         synthetic patient population showing 3 consecutive years of beneficiary data. Keeping the synthetic nature of this data in mind, analytic results, 
                                                         even when descriptive in nature, should not be considered reflective of plauisble estimates.")),
              box(width = 3, valueBoxOutput("total_inpatient_visits", width = "100%")),
              box(width = 3, valueBoxOutput("total_outpatient_visits", width = "100%"))
              ),
            fluidRow(
              box(title = "Glossary", width = 12,
                  tags$dl(
                    tags$dt("CPT Code"),
                    tags$dd("Current Procedural Terminology (CPT) codes are the system of codes used by medical professionals to classify procedures, and can be used to find similar procedures within or across patients.")
                    ),
                  tags$dl(
                    tags$dt("NDC"),
                    tags$dd("National Drug Codes (NDCs) are a system of codes assigned to every drug product in the US, and are maintained by the US Food and Drug Administration (FDA). Each NDC code is 10-11 digits and is unique for each combination of labeler, product, and package of a drug.")
                    )
                  )
            )
            ),
    tabItem(tabName = "Urgent",
            fluidRow(
              box(title = "Urgent and Emergency Care Visits by Month", width = 12, plotOutput("urgent_plot1")),
              box(title = "Urgent and Emergency Care Visits by Day of Week", width = 12, plotOutput("urgent_plot3")),
              box(title = "Urgent and Emergency Care Visits by State", width = 12, plotlyOutput("urgent_plot2")),
              box(title = "Search for CPT Code Occurrences in Emergency or Urgent Care Visits", width = 12,
                  textInput("search_cpt", label = "Search CPT Code:", placeholder = "85610"),
                  actionButton("search_cpt_button", "Search"),
                  br(),
                  DTOutput("urgent_table1"))
            )),
    tabItem(tabName = "Medications",
            fluidRow(
              box(title = "Total Days Supply of Medications Dispensed by Medication Type", width = 12, plotOutput("med_plot1")),
              box(title = "Number of Medications Dispense Events by State", plotlyOutput("med_plot2"), width = 12),
              box(title = "Top NDCs by Quantity Dispensed", width = 12,
                  fluidRow(column(width = 2, checkboxGroupInput("med_type",
                                                                label = "Select Medication Type(s):",
                                                                choices = c("Antibiotics", "GLP", "SSRI", "Stimulant"),
                                                                selected = c("Antibiotics", "GLP", "SSRI", "Stimulant"))),
                           column(width = 10, DTOutput("med_table1")))),
            )),
    tabItem(tabName = "Deliveries",
            fluidRow(box(title = "Types of Pregnancy Outcomes", width = 6, plotOutput("delivery_plot1")),
                     box(title = "Proportion of Live Births with Chronic Conditions", width = 6, plotOutput("delivery_plot3")),
                     box(title = "Maternal Age and Length of Stay by Gestaional Age at Delivery Category", width = 12,plotOutput("delivery_plot2")))),
    tabItem(tabName = "Events",
            fluidRow(box(title = "Length in Stay for Inpatient Cardiac Events by HMO Coverage Months", width = 6, plotOutput("cardiac_plot1")),
                     box(title = "Comorbiditiy Type and Count per Cardiac Event Claims", width = 6, plotlyOutput("cardiac_plot3")),
                     box(title = "Daily Count of Inpatient and Outpatient Cardiac Events", width = 12, plotOutput("cardiac_plot2")))),
    tabItem(tabName = "Mental",
            fluidRow(box(title = "Beneficiary Depression Diagnosis Status Year to Year", width = 12, 
                         helpText("Note: This plot does not respond to the Time Slider dashboard filter."),
                         sankeyNetworkOutput("mh_plot1")),
                     box(title = "Mental Health Visit Count by Year of Beneficiaries with Depression", width = 12, 
                         helpText("Note: This plot does not respond to the Time Slider dashboard filter."),
                         plotOutput("mh_plot2")),
                     box(title = "Average Mental Health Visits Per Month", width = 12, plotOutput("mh_plot3")))
            )
))


# Define UI for application that draws a histogram
ui <- dashboardPage(
  header,
  sidebar,
  body,
  use_theme(my_theme)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  summary_visits_data <- reactive({
    INP_OUT_PERDAY %>% 
      filter(start_date >= input$time_months[1],
             start_date <= input$time_months[2]
      ) %>%
      arrange(start_date)
  })
  
  summary_person_data <- reactive({
    BeneAll %>% 
      filter(start_date >= input$time_months[1],
             start_date <= input$time_months[2]
      ) %>%
      arrange(start_date)
  })
  
  urgent_data <- reactive({
    UR_EM %>% 
      filter(mon_yr >= input$time_months[1],
             mon_yr <= input$time_months[2],
             BENE_SEX_IDENT_CD %in% input$sex,
             BENE_RACE_CD %in% input$race
             ) %>%
      arrange(mon_yr)
  })
  
  med_data <- reactive({
    MedEvents %>% 
      filter(SRVC_DATE >= input$time_months[1],
             SRVC_DATE <= input$time_months[2],
             BENE_SEX_IDENT_CD %in% input$sex,
             BENE_RACE_CD %in% input$race
      ) %>%
      arrange(SRVC_DATE)
  })
  
  urgent_cpt_data <- reactive({
    UR_EM_CPT %>% 
      filter(mon_yr >= input$time_months[1],
             mon_yr <= input$time_months[2],
             BENE_SEX_IDENT_CD %in% input$sex,
             BENE_RACE_CD %in% input$race
      ) %>%
      arrange(mon_yr)
  })
  
  delivery_data <- reactive({
    DEL %>% 
      filter(mon_yr >= input$time_months[1],
             mon_yr <= input$time_months[2],
             BENE_SEX_IDENT_CD %in% input$sex,
             BENE_RACE_CD %in% input$race
      ) %>%
      arrange(mon_yr)
  })
  
  nld_ab_data <- reactive({
    NON_LIVE_AB %>% 
      filter(mon_yr >= input$time_months[1],
             mon_yr <= input$time_months[2],
             BENE_SEX_IDENT_CD %in% input$sex,
             BENE_RACE_CD %in% input$race
      ) %>%
      arrange(mon_yr)
  })
  
  nld_ec_data <- reactive({
    NON_LIVE_EC %>% 
      filter(mon_yr >= input$time_months[1],
             mon_yr <= input$time_months[2],
             BENE_SEX_IDENT_CD %in% input$sex,
             BENE_RACE_CD %in% input$race
      ) %>%
      arrange(mon_yr)
  })
  
  in_card_data <- reactive({
    INPT_CARD %>% 
      filter(mon_yr >= input$time_months[1],
             mon_yr <= input$time_months[2],
             BENE_SEX_IDENT_CD %in% input$sex,
             BENE_RACE_CD %in% input$race
      ) %>%
      arrange(mon_yr)
  })
  
    out_card_data <- reactive({
      OTPT_CARD %>% 
        filter(mon_yr >= input$time_months[1],
               mon_yr <= input$time_months[2],
               BENE_SEX_IDENT_CD %in% input$sex,
               BENE_RACE_CD %in% input$race
        ) %>%
        arrange(mon_yr)
    
  })
    
    dep_per_data <- reactive({
      BeneAll_Dep_wide %>% 
        filter(BENE_SEX_IDENT_CD %in% input$sex,
               BENE_RACE_CD %in% input$race
        ) 
      
    })
    
    dep_per_data2 <- reactive({
      BeneAll_Long %>% 
        filter(BENE_SEX_IDENT_CD %in% input$sex,
               BENE_RACE_CD %in% input$race
        ) 
      
    })
    
    dep_visit_data <- reactive({
      MHC %>% 
        filter(mon_yr >= input$time_months[1],
               mon_yr <= input$time_months[2],
               BENE_SEX_IDENT_CD %in% input$sex,
               BENE_RACE_CD %in% input$race
        ) 
      
    })
    
    output$total_inpatient_visits <- renderValueBox({
      
      total <- summary_visits_data() %>%
        filter(event_type == "inpatient_events_n") %>%
        pull(n_events) %>%
        sum(na.rm = TRUE)
      
      format_number <- function(x) {
        if (x >= 1000000) {
          paste0(round(x / 1000000, 1), "M")
        } else if (x >= 1000) {
          paste0(round(x / 1000, 1), "K")
        } else {
          as.character(x)
        }
      }
      
      valueBox(
        value    = format_number(total),  
        subtitle = "Total Inpatient Visits",
        icon     = icon("bed"),
        color    = "light-blue"
      )
    })
    
    output$total_outpatient_visits <- renderValueBox({
      
      total <- summary_visits_data() %>%
        filter(event_type == "outpatient_events_n") %>%
        pull(n_events) %>%
        sum(na.rm = TRUE)
      
      format_number <- function(x) {
        if (x >= 1000000) {
          paste0(round(x / 1000000, 1), "M")
        } else if (x >= 1000) {
          paste0(round(x / 1000, 1), "K")
        } else {
          as.character(x)
        }
      }
      
      valueBox(
        value    = format_number(total),  
        subtitle = "Total Outpatient Visits",
        icon     = icon("hospital"),
        color    = "navy"
      )
    })
    
  
    
  output$summary_plot1 <- renderPlot({
    if (nrow(summary_visits_data()) == 0) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void())
    }
    
    ggplot(summary_visits_data(), aes(start_date, n_events, color = event_type)) +
      geom_line() +
      labs(
        x = "Date",
        y = "Number of Visits",
        color = "Visit Type"
      ) +
      scale_color_manual(values = c("inpatient_events_n" = "royalblue2", 
                                    "outpatient_events_n" = "chocolate1", 
                                    "total_events" = "maroon4"), 
                         labels = c("inpatient_events_n" = "Inpatient", 
                                    "outpatient_events_n" = "Outpatient", 
                                    "total_events" = "Combined")) +
      theme_minimal()
  }) 
  
  output$summary_plot2 <- renderPlot({
    ggplot(BeneDems, aes(BENE_SEX_IDENT_CD, fill = BENE_SEX_IDENT_CD)) +
      geom_bar() +
      scale_y_continuous(labels = label_comma()) +
      scale_fill_manual(values = c("Female" = "darkorchid4", "Male" = "lightgoldenrod4")) +
      labs(x = "Sex", y = "Number of Beneficiaries", subtitle = "Number of Total Beneficiaries by Sex") +
      theme_minimal() +
      theme(legend.position = "none") 
  })
  
  output$summary_plot3 <- renderPlot({
    ggplot(BeneDems, aes(reorder(BENE_RACE_CD, BENE_RACE_CD, FUN = length), fill = BENE_RACE_CD)) +
      geom_bar() +
      scale_y_continuous(labels = label_comma()) +
      scale_fill_brewer(palette = "Dark2") +
      labs(x = "Race", y = "Number of Beneficiaries", subtitle = "Number of Total Beneficiaries by Race") +
      theme_minimal() +
      theme(legend.position = "none") 
  })
  
  table_data <- eventReactive(input$search_cpt_button, {
    urgent_cpt_data() %>% 
      filter(grepl(tolower(input$search_cpt), cpt_lower, fixed = TRUE)) %>%
      nrow()
  }, ignoreNULL = FALSE)
  
  
  ue_plot1_data <- reactive({
    urgent_data() %>% 
      group_by(urg_emer, start_date, mon_yr) %>%
      summarise(vist_per_day = n(), .groups = "drop_last") %>% 
      filter(!is.na(start_date)) %>%
      left_join(Out_Per_Day, by ="start_date") %>%
      mutate(prop_opt = vist_per_day / outpatient_events_n)
  })
  
  output$urgent_plot1 <- renderPlot({
    colors_urgent_plot1 <- c("Emergency Room Visit" = "darkred", "Urgent Care Visit" = "coral1")
    
    if (nrow(ue_plot1_data()) == 0) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void())
    }
    
    ggplot(ue_plot1_data(), aes(x=mon_yr, y=prop_opt, fill = urg_emer)) +
    geom_col(position = "dodge") +
    labs(fill ="") +
    xlab("Month of Visit") + 
    ylab("Percentage of Outpatient Visits") +
    scale_x_date(breaks = seq.Date(min(ue_plot1_data()$mon_yr), max(ue_plot1_data()$mon_yr), length.out = 10)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = colors_urgent_plot1)
  })
  
  ue_plot2_data <- reactive({
    urgent_data() %>% 
      mutate(fips = as.character(SP_STATE_CODE)) %>%
      group_by(fips)%>%
      summarise(count_urg_er = n())%>%
      left_join(state_pop, by = c("fips" = "Statefips"))%>%
      left_join(usmapdata::fips_data("state") %>% select(fips, full), by = "fips") %>%
      mutate(urg_er_rate = (count_urg_er / total_pop) * 100000,
             hover_text = paste("State:", full, "<br>ER/Urgent Care Claims per 100,000:", round(urg_er_rate, 1)))
  })
  
  output$urgent_plot2 <- renderPlotly({ 
    
    if (nrow(ue_plot2_data()) == 0) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void())
    }
    
    p <- plot_usmap(data = ue_plot2_data(), values = "urg_er_rate", color  = "white") +
      aes(text = hover_text) +
      scale_fill_distiller(palette = "Oranges", direction = 1, name="ER/Urgent Care Claims\n per 100,000 Patients") +
      theme(legend.position = "right")
    
    ggplotly(p, tooltip = "text")
  })
  
  ue_plot3_data <- reactive({
    urgent_data() %>% 
      group_by(start_date, dow, urg_emer) %>% 
      summarise(visits = n(), .groups = 'drop')%>%
      filter(!is.na(dow))
  })
  
  output$urgent_plot3 <- renderPlot({
    
    colors_urgent_plot3 <- c("Emergency Room Visit" = "darkred", "Urgent Care Visit" = "coral1")
    
    if (nrow(ue_plot3_data()) == 0) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void())
    }
    
    ggplot(ue_plot3_data(), aes(x = dow, y = visits, fill=urg_emer)) +
      geom_boxplot() +
      labs(fill = "") +
      xlab("Day of Week") + 
      ylab("Number of Visits") +
      ylim(0, (max(ue_plot3_data()$visits) * 1.05)) +
      scale_fill_manual(values = colors_urgent_plot3)
  })
  
  output$urgent_table1 <- renderDT({
    
    value <- if (table_data() == 0) {
      c("No", 0)
    } else {
      c("Yes", table_data())
    }
    
    table_df <- data.frame(
      Labels = c("Is this Code Found in any visits?", "Number of Codes Found"),
      Value = value,
      stringsAsFactors = FALSE
    )
    
    datatable(
      table_df,
      rownames = FALSE, 
      colnames = c("", ""),
      options = list(dom = 't'))
  })

  med_plot1_data <- reactive({
    med_data() %>%
      group_by(med_type) %>%
      summarise(total_days = sum(DAYS_SUPLY_NUM, na.rm = TRUE), .groups = "drop")
  })
  
  output$med_plot1 <- renderPlot({
    
    if (nrow(med_plot1_data()) == 0) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void())
    }
    
    ggplot(med_plot1_data(), aes(x=med_type, y=total_days, fill = med_type)) +
      geom_col() +
      labs(fill ="") +
      xlab("Medication Type") + 
      ylab("Total Days Supply Dispensed") +
      scale_fill_brewer(palette = "Set2", guide = "none") +
      scale_y_continuous(labels = label_comma())
  })
  
  med_plot2_data <- reactive({
    med_data() %>% 
      mutate(fips = as.character(SP_STATE_CODE)) %>%
      group_by(fips)%>%
      summarise(count_med_events = n())%>%
      left_join(state_pop, by = c("fips" = "Statefips"))%>%
      left_join(usmapdata::fips_data("state") %>% select(fips, full), by = "fips") %>%
      mutate(med_rate = (count_med_events / total_pop) * 100000,
             hover_text = paste("State:", full, "<br>Medication Dispense Events per 100,000:", round(med_rate, 1)))
  })
  
  output$med_plot2 <- renderPlotly({ 
    
    if (nrow(med_data()) == 0) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void())
    }
    
    p <- plot_usmap(data = med_plot2_data(), values = "med_rate", color  = "white") +
      aes(text = hover_text) +
      scale_fill_distiller(palette = "Greens", direction = 1, name="Medication Dispense Events \nper 100,000 patients") +
      theme(legend.position = "right")
    
    ggplotly(p, tooltip = "text")
  })
  
  med_table1_data <- reactive({
    med_data() %>%
      filter(med_type %in% input$med_type) %>%
      group_by(PRODUCTNDC, med_type) %>%
      summarise(quant_disp = sum(QTY_DSPNSD_NUM, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(quant_disp)) %>%
      rename(
        NDC = PRODUCTNDC,
        `Medication Type` = med_type,
        `Quantity Dispensed` = quant_disp
      )
  })
  
  output$med_table1 <- renderDataTable({
    
    if (nrow(med_table1_data()) == 0) {
      return(datatable(data.frame(Message = "No Data"), options = list(dom = "t"), rownames = FALSE))
    }
    
    datatable(med_table1_data(), options = list(pageLength = 10))
    
  })
  
  
  delivery_plot1_data <- reactive({
    del_counts_1 <- delivery_data() %>%
      summarise(count = n())%>%
      mutate(type = "Live Births") 
    
    del_counts_2 <- nld_ab_data() %>%
      summarise(count = n())%>%
      mutate(type = "Abortions") 
    
    del_counts_3 <- nld_ec_data() %>%
      summarise(count = n())%>%
      mutate(type = "Ectopic Pregnancies")
    
    rbind(del_counts_1, del_counts_2, del_counts_3)
  })
  
  output$delivery_plot1 <- renderPlot({
    
    if (sum(delivery_plot1_data()$count, na.rm = TRUE) == 0) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void())
    }
    
    colors_delivery_t1 <- c("Live Births" = "darkseagreen4", "Ectopic Pregnancies" = "cornflowerblue", "Abortions" = "deepskyblue2")
    
    ggplot(delivery_plot1_data(), aes(x = count, y = type,  fill = type)) +
      geom_bar(stat = "identity", width = 0.2) +
      xlab("Number of Events") +
      ylab("") +
      geom_text(aes(label = count), vjust = -0.9, hjust=0) +
      theme_minimal() +
      scale_fill_manual(values = colors_delivery_t1, guide = "none")
  })
  
  output$delivery_plot2 <- renderPlot({
    
    if (nrow(delivery_data()) == 0) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void())
    }
    
    delivery_plot2_data <- delivery_data() %>%
      mutate(ga_cat = factor(ga_cat, levels = c("Pre-term", "Early Term", "Full Term", "Late Term", "Post-term")))
    
    colors_delivery_t2 <- c("Pre-term" ="yellowgreen" , "Early Term" = "darkolivegreen4", "Full Term" = "darkolivegreen", "Late Term" = "aquamarine4", "Post-term" = "darkslategrey")
    shapes_delivery_t2 <- c("Pre-term" = 19 , "Early Term" = 1, "Full Term" = 17, "Late Term" = 2, "Post-term" = 15)
    
    ggplot(delivery_plot2_data, aes(x = age_at_event, y = length_of_stay,  color = ga_cat, shape = ga_cat)) +
      geom_jitter(size = 3) +
      xlab("Age of Mother at Delivery (Years)") +
      ylab("Maternal Length of Stay (Days)") +
      theme_minimal() +
      scale_color_manual(values = colors_delivery_t2, name = "Gestional Age Category") +
      scale_shape_manual(values = shapes_delivery_t2, name = "Gestional Age Category")
  })
  
  delivery_plot3_data <- reactive({
    if (nrow(delivery_data()) == 0) {
      return(data.frame(condition = character(), prop = numeric()))
    }
    
    data.frame(
      condition = c("Depression", "Diabetes"), 
      prop = c(
        (nrow(delivery_data() %>% filter(SP_DEPRESSN==1))/nrow(delivery_data())),
        (nrow(delivery_data() %>% filter(SP_DIABETES==1))/nrow(delivery_data()))
        )
      )
  })
  
  output$delivery_plot3 <- renderPlot({
    
    if (nrow(delivery_plot3_data()) == 0) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void())
    }
    
    ggplot(delivery_plot3_data(), aes(x=condition, y=prop)) +
      geom_segment( aes(y=0, yend=prop), color="yellowgreen") +
      geom_point( color="darkolivegreen", size=4, alpha=0.6) +
      xlab(" ") +
      ylab(" ") +
      ylim(0,1) +
      theme_minimal() +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
  
  
  cardiac_plot1_data <- reactive({
    in_card_data() %>% group_by(BENE_HMO_CVRAGE_TOT_MONS) %>%
      summarise(avg_los = mean(length_of_stay),
                count = n())%>%
      filter(!is.na(BENE_HMO_CVRAGE_TOT_MONS))
  })
  
  output$cardiac_plot1 <- renderPlot({
    
     if (nrow(cardiac_plot1_data()) == 0) {
        return(ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void())
      }
    
    ggplot(cardiac_plot1_data(), aes(x=BENE_HMO_CVRAGE_TOT_MONS, y=avg_los)) +
      geom_segment( aes(y=0, yend=avg_los), color="thistle3") +
      geom_point(color="darkmagenta", size=4, alpha=0.6) +
      xlab("Number of Months of HMO Coverage") +
      ylab("Average Length of Stay in Hospital (Days)") +
      ylim(0,10) +
      scale_x_continuous(breaks = 0:12, labels = as.character(0:12)) +
      theme_minimal() +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      ) 
  })
  
  cardiac_plot2_data <- reactive({
    c_plot2a_df <- in_card_data() %>% 
      group_by(start_date) %>%
      summarise(count = n()) %>%
      filter(!is.na(start_date)) %>%
      mutate(type = "Inpatient")
    
    c_plot2b_df <- out_card_data() %>% 
      group_by(start_date) %>%
      summarise(count = n()) %>%
      filter(!is.na(start_date)) %>%
      mutate(type = "Outpatient")
    
    rbind(c_plot2a_df, c_plot2b_df)
  })
  
  output$cardiac_plot2<- renderPlot({
    
    if (nrow(cardiac_plot2_data()) == 0) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void())
    }
    
    colors_card_t2 <- c("Inpatient" ="slateblue4" , "Outpatient" = "darkolivegreen")
    
    ggplot(cardiac_plot2_data(), aes(x = count, fill = type)) +
      geom_histogram(binwidth = 0.5, position = "stack", alpha = 0.8) +
      xlab("Number of Cardiac Event Visits Per Day") +
      ylab("Frequency") + 
      scale_fill_manual(values = colors_card_t2, name = NULL) +
      theme_minimal() 
  })
  
  cardiac_plot3_data <- reactive({
    c_plot3_int_df <- rbind( in_card_data() %>% select(DESYNPUF_ID, CLM_ID, SP_CHF, SP_COPD, SP_DIABETES, SP_ISCHMCHT), 
                             out_card_data() %>% select(DESYNPUF_ID, CLM_ID, SP_CHF, SP_COPD, SP_DIABETES, SP_ISCHMCHT)) %>%
      mutate(SP_CHF = recode(SP_CHF, "1" = 1, "2" = 0),
             SP_COPD = recode(SP_COPD, "1" = 1, "2" = 0),
             SP_DIABETES = recode(SP_DIABETES, "1" = 1, "2" = 0),
             SP_ISCHMCHT = recode(SP_ISCHMCHT, "1" = 1, "2" = 0))
    
    c_plot3_df <- c_plot3_int_df %>%
      group_by(SP_CHF, SP_COPD, SP_DIABETES, SP_ISCHMCHT)%>%
      summarise(count =n()) %>%
      filter(!is.na(SP_CHF)) %>%
      ungroup() %>%
      mutate(co_morb_cat = case_when(
        SP_CHF == "0" & SP_COPD == "0" & SP_DIABETES == "0" & SP_ISCHMCHT == "0" ~ "No Comorbidities",
        SP_CHF == "1" & SP_COPD == "0" & SP_DIABETES == "0" & SP_ISCHMCHT == "0" ~ "Chronic Heart Failure Only",
        SP_CHF == "0" & SP_COPD == "1" & SP_DIABETES == "0" & SP_ISCHMCHT == "0" ~ "COPD Only",
        SP_CHF == "0" & SP_COPD == "0" & SP_DIABETES == "1" & SP_ISCHMCHT == "0" ~ "Diabetes Only",
        SP_CHF == "0" & SP_COPD == "0" & SP_DIABETES == "0" & SP_ISCHMCHT == "1" ~ "Ischemic Heart Disease Only",
        SP_CHF == "1" & SP_COPD == "1" & SP_DIABETES == "0" & SP_ISCHMCHT == "0" ~ "CHF & COPD",
        SP_CHF == "1" & SP_COPD == "0" & SP_DIABETES == "1" & SP_ISCHMCHT == "0" ~ "CHF & Diabetes",
        SP_CHF == "1" & SP_COPD == "0" & SP_DIABETES == "0" & SP_ISCHMCHT == "1" ~ "CHF & Ischemic Heart Disease",
        SP_CHF == "0" & SP_COPD == "1" & SP_DIABETES == "1" & SP_ISCHMCHT == "0" ~ "COPD & Diabetes",
        SP_CHF == "0" & SP_COPD == "1" & SP_DIABETES == "0" & SP_ISCHMCHT == "1" ~ "COPD & Ischemic Heart Disease",
        SP_CHF == "0" & SP_COPD == "0" & SP_DIABETES == "1" & SP_ISCHMCHT == "1" ~ "Diabetes & Ischemic Heart Disease",
        SP_CHF == "1" & SP_COPD == "1" & SP_DIABETES == "1" & SP_ISCHMCHT == "0" ~ "CHF, COPD & Diabetes",
        SP_CHF == "0" & SP_COPD == "1" & SP_DIABETES == "1" & SP_ISCHMCHT == "1" ~ "COPD, Diabetes & Ischemic Heart Disease",
        SP_CHF == "1" & SP_COPD == "0" & SP_DIABETES == "1" & SP_ISCHMCHT == "1" ~ "CHF, Diabetes & Ischemic Heart Disease",
        SP_CHF == "1" & SP_COPD == "1" & SP_DIABETES == "0" & SP_ISCHMCHT == "1" ~ "CHF, COPD & Ischemic Heart Disease",
        SP_CHF == "1" & SP_COPD == "1" & SP_DIABETES == "1" & SP_ISCHMCHT == "1" ~ "All Comorbidities"))
    
    c_plot3_df <- c_plot3_df %>% 
      rowwise()%>%
      mutate(sum_com = sum(SP_CHF, SP_COPD, SP_DIABETES, SP_ISCHMCHT))
    
    c_plot3_df
  })
  
  output$cardiac_plot3<- renderPlotly({
    
     if (nrow(cardiac_plot3_data()) == 0) {
        return(ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void())
      }
         
    colors_card_t3 <- c("No Comorbidities" = hcl(h = 67, c = 29, l =90), "Chronic Heart Failure Only" = hcl(h = 298, c = 29, l =85),"COPD Only" = hcl(h = 71, c = 29, l =85),"Diabetes Only" = hcl(h = 135, c = 29, l =85),"Ischemic Heart Disease Only" = hcl(h = 250, c = 29, l =85),"CHF & COPD" = hcl(h = 298, c = 29, l =74),"CHF & Diabetes" = hcl(h = 135, c = 29, l =74),"CHF & Ischemic Heart Disease" = hcl(h = 20, c = 29, l =74),"COPD & Diabetes" = hcl(h = 105, c = 29, l =74),"COPD & Ischemic Heart Disease" = hcl(h = 161, c = 29, l =74),"Diabetes & Ischemic Heart Disease" = hcl(h = 315, c = 29, l =74),"CHF, COPD & Diabetes" = hcl(h = 315, c = 29, l =50),"COPD, Diabetes & Ischemic Heart Disease" = hcl(h = 256, c = 29, l =50),"CHF, Diabetes & Ischemic Heart Disease" = hcl(h = 119, c = 29, l =50),"CHF, COPD & Ischemic Heart Disease" = hcl(h = 23, c = 29, l =50),"All Comorbidities" = hcl(h = 307, c = 29, l =25))
    
    p <- ggplot(cardiac_plot3_data(), aes(y = count, x = sum_com, fill = co_morb_cat, text = paste("Cormorbidities:", co_morb_cat, "<br>Number of Comorbidities:", sum_com, "<br>Number of Claims:", count))) +
      geom_bar(stat = "identity", width = 0.2) +
      xlab("Number of Comorbidities") +
      ylab("Number of Claims") + 
      scale_fill_manual(values = colors_card_t3, name = NULL) +
      theme_minimal() 
    
    ggplotly(p, tooltip = "text")
  })
  
  
  output$mh_plot1<- renderSankeyNetwork({
    
    validate(need(nrow(dep_per_data()) > 0, "No data"))
    
    mh_plot1_df <- dep_per_data()
    
    links <- data.frame(
      source = c(0, 0, 1, 1, 2, 2, 3, 3),
      target = c(2, 3, 2, 3, 4, 5, 4, 5),
      value  = c(mh_plot1_df %>% filter(depression_2008 ==1 & depression_2009 ==1)%>%nrow() , mh_plot1_df %>% filter(depression_2008 ==1 & depression_2009 ==0)%>%nrow() ,
                 mh_plot1_df %>% filter(depression_2008 ==0 & depression_2009 ==1)%>%nrow() , mh_plot1_df %>% filter(depression_2008 ==0 & depression_2009 ==0)%>%nrow() ,
                 mh_plot1_df %>% filter(depression_2009 ==1 & depression_2010 ==1)%>%nrow() , mh_plot1_df %>% filter(depression_2009 ==1 & depression_2010 ==0)%>%nrow(), 
                 mh_plot1_df %>% filter(depression_2009 ==0 & depression_2010 ==1)%>%nrow(), mh_plot1_df %>% filter(depression_2009 ==0 & depression_2010 ==0)%>%nrow()),
      group  = c("dep_to_dep", "dep_to_no", "no_to_dep", "no_to_no",
                 "dep_to_dep", "dep_to_no", "no_to_dep", "no_to_no")
    )
    
    nodes <- data.frame(
      name = c(
        "Depression Diagnosis (2008)",      
        "No Depression Diagnosis (2008)",    
        "Depression Diagnosis (2009)",      
        "No Depression Diagnosis (2009)",    
        "Depression Diagnosis (2010)",      
        "No Depression Diagnosis (2010)"    
      ),
      group = c("dep", "no_dep", "dep", "no_dep", "dep", "no_dep")
    )
    
    
    
    my_color <- 'd3.scaleOrdinal()
  .domain(["Depression Diagnosis (2008)","No Depression Diagnosis (2008)", "Depression Diagnosis (2009)","No Depression Diagnosis (2009)","Depression Diagnosis (2010)", "No Depression Diagnosis (2010)"])
  .range(["#E8A0BF", "#A8D5BA", "#E8A0BF", "#A8D5BA", "#E8A0BF", "#A8D5BA"])'
    
    my_color2 <- 'd3.scaleOrdinal()
  .domain(["dep", "no_dep", "dep_to_dep", "dep_to_no", "no_to_dep", "no_to_no"])
  .range(["#C0392B", "#2980B9", "#E74C3C", "#E8A0BF", "#8ad5d4", "#2471A3"])'
    
    
    p <- sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  NodeGroup = "group",
                  LinkGroup = "group",
                  colourScale = my_color2,
                  fontSize = 14, nodeWidth = 30)
    
    
    onRender(p, '
    function(el) {
      
      d3.select(el).selectAll(".node text")
        .style("font-size", "12px")
        .style("font-family", "Arial")
        .style("font-weight", "bold")
        .style("fill", "#333333")
        
      d3.select(el).selectAll(".link")
        .style("opacity", 0.4)
        
      d3.select(el).selectAll(".node rect")
        .style("stroke", "none")
    }
  ')
    
    
  })
  
  mh_plot2_data <- reactive({
    if (nrow(dep_per_data2()) == 0) {
      return(data.frame(
        year = numeric(),
        mh_count = numeric(),
        count = numeric(),
        prop_mh_any = numeric()
      ))
    }
    
    plot_df_a <- dep_per_data2() %>% 
      filter(mh_any == "Yes") %>%
      group_by(year, mh_count) %>%
      summarise(count = n())
    
    plot_df_b <- dep_per_data2() %>%
      group_by(year, mh_any) %>%
      summarise(count_any = n()) %>%
      pivot_wider(names_from = "mh_any", values_from = "count_any")%>%
      rowwise()%>%
      mutate(tot = sum(No, Yes))%>%
      mutate(prop_mh_any = Yes / tot)%>%
      select(year,prop_mh_any)
    
    plot_df <- plot_df_a %>% inner_join(plot_df_b, by = "year")
    
    plot_df
  })
  
  output$mh_plot2 <- renderPlot({
    
      if (nrow(mh_plot2_data()) == 0) {
        return(ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void())
      }
    
    ggplot(mh_plot2_data(), aes(x = factor(year), y = mh_count)) +
      geom_point(aes(color=prop_mh_any, size=count)) +
      xlab("Beneficiary Year") +
      ylab("Number of Mental Health Encounters") +
      scale_color_viridis(option = "rocket", 
                          direction = -1,
                          begin = 0.15,
                          end = 0.95,
                          labels = scales::percent_format(accuracy = .1),
                          name = "% of patients with depression \n getting any mental health care") +
      scale_size_continuous(range = c(3, 6), name = "Number of patients") +
      theme_minimal() 
  })
  
  mh_plot3_data <- reactive({
    dep_visit_data() %>% group_by(mon_yr, mon) %>%
      summarise(count = n()) %>%
      group_by(mon) %>%
      summarise(avg = mean(count, na.rm = TRUE))%>%
      filter(!is.na(mon)) %>%
      mutate(mon = factor(mon,
                          levels = c("Jan", "Feb", "Mar", "Apr","May" ,"Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
  })
  
  output$mh_plot3 <- renderPlot({
    
      if (nrow(mh_plot3_data()) == 0) {
        return(ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void())
      }
    
    palette <- colorRampPalette(brewer.pal(9, "Blues")[3:9])(12)
    names(palette) <- levels(mh_plot3_data()$mon)
    
    ggplot(mh_plot3_data(), aes(x = mon, y = avg, fill = mon)) +
      geom_bar(stat="identity") +
      xlab("") +
      ylab("Average Number of Monthly Encounters") +
      scale_fill_manual(values = palette, guide = "none") +
      labs(fill ="") +
      theme_minimal() 
  })
  

}





# Run the application 
shinyApp(ui = ui, server = server)


