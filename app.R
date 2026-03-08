Sys.setenv(TZ = "Australia/Brisbane")

library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(shinymanager)

#----- Password Database Setup ----------------------------------------------------
if(isFALSE(file.exists("data/database.sqlite"))){
  credentials <- data.frame(
    user = c("rad", "admin"),
    password = c("rad", "admin"),
    admin = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  create_db(credentials_data = credentials,
            sqlite_path = "data/database.sqlite",
            passphrase = "")
}

#--------- UI ---------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  uiOutput("app_title"),
  sidebarPanel(width = 3,
               uiOutput("add_II_btn"),
               uiOutput("add_litho_btn"),
               uiOutput("add_oarm_btn"),
               uiOutput("dose_summary"),
               br(), br(),
               HTML("<i>For issues, questions, recommendations, contact the app's author - robert.dutoit@health.qld.gov.au</i>"),
               br(), br(),
               actionButton("log_out", "Log Out", class = "btn btn-primary", icon = icon("arrow-right-from-bracket"))
  ),
  mainPanel(
    tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
    width = 9,
    tabsetPanel(
      tabPanel("Case Log",
               br(),
               downloadButton("download_csv", "Download CSV"),
               actionButton("delete_case_row", "Delete Selected Row", icon = icon("eraser")),
               br(), br(),
               DTOutput("cases_table")
      ),
      tabPanel("Data Analysis",
               uiOutput("date_range"), 
               br(),
               splitLayout(uiOutput("xvar_1"), uiOutput("yvar_1"), uiOutput("colorvar_1")),
               plotlyOutput("plot1"),
               br(),
               splitLayout(uiOutput("xvar_2"), uiOutput("yvar_2"), uiOutput("colorvar_2")),
               plotlyOutput("plot2")
      ),
      # tabPanel("Manage Dropdowns",
      #          br(),
      #          actionButton("save", "Save and Reload", class = "btn btn-success", icon = icon("floppy-disk")),
      #          actionButton("add_row", "Add Row", icon = icon("plus")),
      #          actionButton("delete_row", "Delete Selected Row", icon = icon("eraser")),
      #          br(), br(),
      #          DTOutput("drop_down_table")
      # ),
      # tabPanel("Version History",
      #          HTML("<i>v0.1, 9 September 2025 - initial version for consultation by clinicians</i>"),
      #          br(), br(),
      #          HTML("<i>v0.2, 22 September 2025 - removed O-Arm Dose Selection and Ziehm data fields, added author contact note, added time filter slider in Data Analysis tab.</i>")
      # ),
      
      tabPanel("Admin Settings",
               uiOutput("admin_ui")
      ),
      
      tabPanel("*WIP* - Room Shielding",
               br(),
               numericInput("no_weeks", "Show Data From the Last X Weeks", value = 26),
               HTML("<span style='color:red'><i>Work in Progress - Accurate weekly DAP limits required for meaningful results.</i></span>"),
               br(),
               DTOutput("room_shielding_table")
      )
      
    )
  )
)

#--------- SERVER -----------------------------------------------------------------
server <- function(input, output, session) {
  
  #------ Password --------------------------------------------------------------
  res_auth <- secure_server(
    timeout = 15,
    check_credentials = check_credentials("data/database.sqlite", passphrase = "")
  )
  
  # Reactive output to detect admin
  output$is_admin <- reactive({
    res_auth$admin
  })
  outputOptions(output, "is_admin", suspendWhenHidden = FALSE)
  
  # Admin settings file
  admin_settings_file <- "data/admin_settings.csv"
  
  if (!file.exists(admin_settings_file)) {
    default_settings <- data.frame(
      app_title = "TUH Theatre Radiation Use Log",
      enable_II = TRUE,
      enable_litho = TRUE,
      enable_oarm = TRUE,
      stringsAsFactors = FALSE
    )
    write.csv(default_settings, admin_settings_file, row.names = FALSE)
  }
  
  # Reactive admin settings reader
  admin_settings <- reactiveFileReader(
    intervalMillis = 1000,
    session = session,
    filePath = admin_settings_file,
    readFunc = function(file) { read.csv(file, stringsAsFactors = FALSE) }
  )
  
  #--------- App Title ---------------------
  output$app_title <- renderUI({
    settings <- admin_settings()
    titlePanel(settings$app_title[1])
  })
  
  #--------- Sidebar Buttons --------------
  output$add_II_btn <- renderUI({
    if (admin_settings()$enable_II[1]) {
      tagList(
        actionButton("add_II_case", "Add II Case", class = "btn btn-primary",
                     icon = icon("plus"), style = "width:100%; font-size:18px; white-space:normal;"),
        br(), br()
      )
    }
  })
  
  output$add_litho_btn <- renderUI({
    if (admin_settings()$enable_litho[1]) {
      tagList(
        actionButton("add_litho_case", "Add Lithotripter Case", class = "btn btn-primary",
                     icon = icon("plus"), style = "width:100%; font-size:18px; white-space:normal;"),
        br(), br()
      )
    }
  })
  
  output$add_oarm_btn <- renderUI({
    if (admin_settings()$enable_oarm[1]) {
      tagList(
        actionButton("add_oarm_case", "Add O-Arm Case", class = "btn btn-primary",
                     icon = icon("plus"), style = "width:100%; font-size:18px; white-space:normal;"),
        br(), br()
      )
    }
  })
  
  # Log Out
  observeEvent(input$log_out, {
    session$reload()
  })
  
  #--------- Admin UI ----------------------
  output$admin_ui <- renderUI({
    if (res_auth$admin) {
      # Admin content
      settings <- admin_settings()
      tagList(
        br(),
        h4("App Display Settings"),
        textInput("admin_app_title", "Application Title", value = settings$app_title[1]),
        checkboxInput("enable_II", "Enable Add II Case Button", value = settings$enable_II[1]),
        checkboxInput("enable_litho", "Enable Add Lithotripter Case Button", value = settings$enable_litho[1]),
        checkboxInput("enable_oarm", "Enable Add O-Arm Case Button", value = settings$enable_oarm[1]),
        actionButton("save_admin_settings", "Save Above Settings", class = "btn btn-success", icon = icon("floppy-disk")),
        br(),
        br(),
        actionButton("save", "Save Table and Reload", class = "btn btn-success", icon = icon("floppy-disk")),
        actionButton("add_row", "Add Row", icon = icon("plus")),
        actionButton("delete_row", "Delete Selected Row", icon = icon("eraser")),
        br(), br(),
        DTOutput("drop_down_table")
      )
    } else {
      # Non-admin content
      tagList(
        br(),
        h4("Admin Access Required"),
        p("You do not have permission to view or modify these settings.")
      )
    }
  })
  
  # Save admin settings
  observeEvent(input$save_admin_settings, {
    new_settings <- data.frame(
      app_title = input$admin_app_title,
      enable_II = input$enable_II,
      enable_litho = input$enable_litho,
      enable_oarm = input$enable_oarm,
      stringsAsFactors = FALSE
    )
    write.csv(new_settings, admin_settings_file, row.names = FALSE)
    showNotification("Admin settings saved!", type = "message")
  })
  
  #------ Password --------------------------------------------------------------
  res_auth <- secure_server(
    timeout = 15,
    check_credentials = check_credentials(
      "data/database.sqlite",
      passphrase = ""))
  
  output$auth_output <- renderPrint({reactiveValuesToList(res_auth)})
  
  
  #------ Drop Down Table Manager ------------------------------------------------------------
  
  drop_down_csv_file <- "data/drop_down.csv"
  
  drop_down_data <- reactiveVal(read.csv(drop_down_csv_file, stringsAsFactors = FALSE))
  
  output$drop_down_table <- renderDT({
    datatable(drop_down_data(), rownames = FALSE, editable = TRUE, selection = "single", class = 'cell-border stripe', options = list(pageLength = -1, scrollX = TRUE, dom = 't'))
  }, server = FALSE)
  
  observeEvent(input$drop_down_table_cell_edit, {
    info <- input$drop_down_table_cell_edit
    df <- drop_down_data()
    
    i <- info$row
    j <- info$col + 1 # there's a plus one here because the hidden row indexes are a column apparently ??
    v <- info$value
    
    if (!is.null(i) && !is.null(j) && i > 0 && j > 0 &&
        i <= nrow(df) && j <= ncol(df)) {
      df[i, j] <- v
      drop_down_data(df)
    }
  })
  
  # Add row (start with blanks instead of NA)
  observeEvent(input$add_row, {
    df <- drop_down_data()
    new_row <- setNames(as.data.frame(as.list(rep("", ncol(df))), stringsAsFactors = FALSE),
                        colnames(df))
    df <- rbind(df, new_row)
    drop_down_data(df)
  })
  
  # Delete row (drop = FALSE ensures df stays a data frame)
  observeEvent(input$delete_row, {
    req(input$drop_down_table_rows_selected)
    df <- drop_down_data()
    df <- df[-input$drop_down_table_rows_selected, , drop = FALSE]
    drop_down_data(df)
  })
  
  # Save changes
  observeEvent(input$save, {
    write.csv(drop_down_data(), drop_down_csv_file, row.names = FALSE)
    showNotification("Changes saved!", type = "message")
    session$reload()
  })
  
  #------ Data Table Manager -----------------------------------------------------------
  
  cases_data <- reactiveFileReader(
    intervalMillis = 500,
    session = session,
    filePath = "data/cases.csv",
    readFunc = function(file) {
      df <- read.csv(file, stringsAsFactors = FALSE)
      colnames(df) <- c("Category", "Exam Date", "Accession No.", "Patient URN", "Patient DOB", "Rad Initials", "Theatre", "Equipment", "Examination Area", "Specific Examination", "Screening Time (s)", "Input Dose", "Input Dose Unit", "Dose (mGy)", "Input DAP", "Input DAP Unit", "DAP (Gy.cm2)","Time b/n 30 & 10-min call", "Time in Theatre", "O-Arm Number of Spins","O-Arm Spin kVp", "O-Arm Spin mAs", "O-Arm 3D Dose (mGy)", "O-Arm 3D DLP (mGy.cm)", "O-Arm Case Type", "Notes" )
      df <- df[nrow(df):1, ]   # reverse order so newest on top
      df$`Exam Date` <- as.Date(df$`Exam Date`)
      df
    }
  )
  
  rv <- reactiveValues(data = NULL)
  
  observe({
    rv$data <- cases_data()
  })
  
  observeEvent(input$delete_case_row, {
    if (is.null(input$cases_table_rows_selected)) {
      showNotification("Please select a row to delete first.", type = "error")
    } else {
      showModal(modalDialog(
        title = "Confirm Deletion",
        "Are you sure you want to delete the selected row?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete", "Delete", class = "btn-danger")
        )
      ))
    }
  })
  
  # Perform deletion if confirmed
  observeEvent(input$confirm_delete, {
    sel <- input$cases_table_rows_selected
    if (!is.null(sel)) {
      rv$data <- rv$data[-sel, ]
      # Save updated data back to CSV
      write.csv(rv$data[nrow(rv$data):1, ], "data/cases.csv", row.names = FALSE)
    }
    removeModal()
  })
  
  # Download CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("Theatre_Log_Export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$data[nrow(rv$data):1, ], file, row.names = FALSE)
    }
  )
  
  output$cases_table <- renderDT({
    req(cases_data())
    datatable(cases_data(), rownames = FALSE, selection = "single", options = list(pageLength = 10, scrollX = TRUE)) # Adding filter = 'top' breaks this ?????
  })
  
  #------ Sidebar Outputs -----------------------------------------------------------
  
  output$dose_summary <- renderUI({
    req(cases_data())
    
    n_last_7_days <- sum(cases_data()$`Exam Date` >= Sys.Date() - 7 & cases_data()$`Exam Date` <= Sys.Date(), na.rm = TRUE)
    n_last_30_days <- sum(cases_data()$`Exam Date` >= Sys.Date() - 30 & cases_data()$`Exam Date` <= Sys.Date(), na.rm = TRUE)
    n_last_365_days <- sum(cases_data()$`Exam Date` >= Sys.Date() - 365 & cases_data()$`Exam Date` <= Sys.Date(), na.rm = TRUE)
    oldest_case <- min(cases_data()$`Exam Date`, na.rm = TRUE)
    
    
    tagList(
      HTML("<b><u>Case Summary</u></b>"),
      br(),
      HTML(paste0("Cases completed past 7 days: <b>", n_last_7_days, "</b>")),
      br(),
      HTML(paste0("Cases completed past 30 days: <b>", n_last_30_days, "</b>")),
      br(),
      HTML(paste0("Cases completed past 365 days: <b>", n_last_365_days, "</b>")),
      br(),
      HTML(paste0("Oldest case: <b>", oldest_case, "</b>"))
    )
    
    
    
  })
  
  #------ Room Shielding Table -----------------------------------------------------
  dap_summary <- reactive({
    req(cases_data(), OT_DAP_limit_list(), input$no_weeks)
    
    limits <- OT_DAP_limit_list()
    
    df <- cases_data()[cases_data()$`Exam Date` >= Sys.Date() - input$no_weeks * 7 &
                         cases_data()$`Exam Date` <= Sys.Date(), ]
    summary_df <- df %>% 
      group_by(Theatre) %>%
      summarise(`Fluoro DAP (Gy.cm2)` = sum(`DAP (Gy.cm2)`, na.rm = TRUE),
                `Number of DAP entries` = sum(!is.na(`DAP (Gy.cm2)`)),
                #`Total Dose (mGy)` = sum(`Dose (mGy)`, na.rm = TRUE),
                `No. of O-Arm Spins` = sum(`O-Arm Number of Spins`, na.rm = TRUE),
                `O-Arm Spin DAP contribution (mGy.cm)` = sum(`O-Arm Number of Spins`, na.rm = TRUE) * 27,
                `Total Room DAP (Gy.cm2)` = (sum(`O-Arm Number of Spins`, na.rm = TRUE) * 27) + (sum(`DAP (Gy.cm2)`, na.rm = TRUE))
      )
    
    summary_df <- left_join(summary_df, limits, by = "Theatre")
    summary_df <- summary_df %>%
      mutate(Weekly_DAP_Limit_Gycm2 = Weekly_DAP_Limit_Gycm2 * input$no_weeks)
    summary_df <- summary_df %>%
      mutate(`% of DAP Limit Used` = ifelse(!is.na(Weekly_DAP_Limit_Gycm2) & Weekly_DAP_Limit_Gycm2 > 0,
                                            (`Total Room DAP (Gy.cm2)` / Weekly_DAP_Limit_Gycm2) * 100,
                                            NA)
      ) %>%
      arrange(desc(`% of DAP Limit Used`))
    summary_df
  })
  output$room_shielding_table <- renderDT({
    datatable(
      dap_summary(),
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = -1, scrollX = TRUE, dom = 't'),
      colnames = c("Theatre","Fluoro DAP (Gy.cm2)","Number of DAP entries","No. of O-Arm Spins","O-Arm Spin DAP contribution (mGy.cm)","Total Room DAP (Gy.cm2)","DAP Limit (Gy.cm2)","% of DAP Limit")
    ) |> DT::formatRound(
      c("Fluoro DAP (Gy.cm2)","O-Arm Spin DAP contribution (mGy.cm)","Total Room DAP (Gy.cm2)","% of DAP Limit Used"), 1)
  })
  
  #------ Reactive Lists (The 13 Watchers O.O) ---------------------------------------------------------
  
  drop_down_file_path = "data/drop_down.csv"
  
  
  rad_initials_list <- reactivePoll(999, session,
                                    checkFunc = function() {file.info(drop_down_file_path)$mtime},
                                    valueFunc = function() {c("","Other - Please Specify", read.csv(drop_down_csv_file, stringsAsFactors = FALSE)$Rad_Initials)})
  
  OT_list <- reactivePoll(999, session,
                          checkFunc = function() {file.info(drop_down_file_path)$mtime},
                          valueFunc = function() {c("","Other - Please Specify", read.csv(drop_down_csv_file, stringsAsFactors = FALSE)$Theatre)})
  
  OT_DAP_limit_list <- reactivePoll(999, session,
                                    checkFunc = function() {file.info(drop_down_file_path)$mtime},
                                    valueFunc = function() {df <- read.csv(drop_down_csv_file, stringsAsFactors = FALSE)
                                    df %>% select(Theatre, Weekly_DAP_Limit_Gycm2)})
  
  equipment_list <- reactivePoll(999, session,
                                 checkFunc = function() {file.info(drop_down_file_path)$mtime},
                                 valueFunc = function() {c("","Other - Please Specify", read.csv(drop_down_csv_file, stringsAsFactors = FALSE)$Equipment)})
  
  endoscopy_list <- reactivePoll(999, session,
                                 checkFunc = function() {file.info(drop_down_file_path)$mtime},
                                 valueFunc = function() {c("","Other - Please Specify", read.csv(drop_down_csv_file, stringsAsFactors = FALSE)$Endoscopy)})
  
  gen_surg_list <- reactivePoll(999, session,
                                checkFunc = function() {file.info(drop_down_file_path)$mtime},
                                valueFunc = function() {c("","Other - Please Specify", read.csv(drop_down_csv_file, stringsAsFactors = FALSE)$Gen_Surg)})
  
  max_fax_list <- reactivePoll(999, session,
                               checkFunc = function() {file.info(drop_down_file_path)$mtime},
                               valueFunc = function() {c("","Other - Please Specify", read.csv(drop_down_csv_file, stringsAsFactors = FALSE)$Max_Fax)})
  
  neuro_surg_list <- reactivePoll(999, session,
                                  checkFunc = function() {file.info(drop_down_file_path)$mtime},
                                  valueFunc = function() {c("","Other - Please Specify", read.csv(drop_down_csv_file, stringsAsFactors = FALSE)$Neuro_Surg)})
  
  orthopedics_list <- reactivePoll(999, session,
                                   checkFunc = function() {file.info(drop_down_file_path)$mtime},
                                   valueFunc = function() {c("","Other - Please Specify", read.csv(drop_down_csv_file, stringsAsFactors = FALSE)$Orthopedics)})
  
  paediatrics_list <- reactivePoll(999, session,
                                   checkFunc = function() {file.info(drop_down_file_path)$mtime},
                                   valueFunc = function() {c("","Other - Please Specify", read.csv(drop_down_csv_file, stringsAsFactors = FALSE)$Paediatrics)})
  
  pain_clinic_list <- reactivePoll(999, session,
                                   checkFunc = function() {file.info(drop_down_file_path)$mtime},
                                   valueFunc = function() {c("","Other - Please Specify", read.csv(drop_down_csv_file, stringsAsFactors = FALSE)$Pain_Clinic)})
  
  urology_list <- reactivePoll(999, session,
                               checkFunc = function() {file.info(drop_down_file_path)$mtime},
                               valueFunc = function() {c("","Other - Please Specify", read.csv(drop_down_csv_file, stringsAsFactors = FALSE)$Urology)})
  
  vascular_list <- reactivePoll(999, session,
                                checkFunc = function() {file.info(drop_down_file_path)$mtime},
                                valueFunc = function() {c("","Other - Please Specify", read.csv(drop_down_csv_file, stringsAsFactors = FALSE)$Vascular)})
  
  specific_exam_options <- reactive({ list(
    "Endoscopy" = endoscopy_list(),
    "Gen Surg" = gen_surg_list(),
    "Max Fax" = max_fax_list(),
    "Neuro Surg" = neuro_surg_list(),
    "Orthopedics" = orthopedics_list(),
    "Paediatrics" = paediatrics_list(),
    "Pain Clinic" = pain_clinic_list(),
    "Urology" = urology_list(),
    "Vascular" = vascular_list(),
    "Other - Please Specify" = c("Other - Please Specify")
  )
  })
  
  #------ Fixed Lists -----------------------------------------------------
  
  area_exam_list = c("", "Other - Please Specify", "Endoscopy","Gen Surg","Max Fax","Neuro Surg",
                     "Orthopedics","Paediatrics","Pain Clinic","Urology",
                     "Vascular")
  
  time_between_list = c("", "Nil - No 30 Min Call","Time Not Recorded",
                        "<5 Min","10 Min","15 Min","20 Min","25 Min",
                        "30 Min","35 Min","40 Min","45 Min","50 Min",
                        "60 Min","> 1 Hour")
  
  time_in_theatre_list = c("","Time Not Recorded","<5 Min","10 Min","15 Min",
                           "20 Min","25 Min","30 Min","35 Min","40 Min",
                           "45 Min","50 Min","60 Min","70 Min","80 Min",
                           "90 Min","100 Min","110 Min","120 Min",">120 Min")
  
  dose_units_list = c("", "uGy", "mGy", "dGy", "Gy")
  
  dap_units_list = c("", "mGy.cm2", "cGy.cm2", "uGy.m2", "dGy.cm2", "Gy.cm2", "mGy.m2", "Gy.m2")
  
  
  #------ Dose and DAP converter functions -----------------------------------------------------  
  dose_converter <- function(input_dose, input_dose_units) {
    if (input_dose_units == "uGy"){
      dose = input_dose / 1000}
    else if (input_dose_units == "mGy"){
      dose = input_dose}
    else if (input_dose_units == "dGy"){
      dose = input_dose * 100}
    else if (input_dose_units == "Gy"){
      dose = input_dose * 1000}
    return(dose)
  }
  
  dap_converter <- function(input_dap, input_dap_units) {
    if (input_dap_units == "mGy.cm2"){
      dap = input_dap / 1000}
    else if (input_dap_units == "cGy.cm2"){
      dap = input_dap / 100}
    else if (input_dap_units == "uGy.m2"){
      dap = input_dap / 100}
    else if (input_dap_units == "dGy.cm2"){
      dap = input_dap / 10}
    else if (input_dap_units == "Gy.cm2"){
      dap = input_dap}
    else if (input_dap_units == "mGy.m2"){
      dap = input_dap * 10}
    else if (input_dap_units == "Gy.m2"){
      dap = input_dap * 10000}
  }
  
  
  #------ Saving a New Case to File -----------------------------------------------------
  save_case <- function(new_row, file = "data/cases.csv") {
    if (!file.exists(file)) {
      write.csv(new_row, file, row.names = FALSE)
    } else {
      write.table(new_row, file, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
    }
  }
  
  #------ Area Dependent Drop down for Specific Exam -----------------------------------------------------
  observeEvent(input$area_exam, {
    selected_area <- input$area_exam
    
    # Get the options for the selected area, default to empty if none
    choices <- specific_exam_options()[[selected_area]]
    if (is.null(choices)) choices <- ""
    
    # Update the dropdown
    updateSelectInput(session, "specific_exam",
                      choices = choices,
                      selected = choices[1])
  })
  
  #------ Input Modal for II -----------------------------------------------------------------------------
  observeEvent(input$add_II_case, {
    showModal(modalDialog(title = "Add New II Case", size = "l",
                          fluidRow(
                            column(6,
                                   dateInput("exam_date", label = HTML("Exam Date<span style='color:red'>*</span>"), format = "dd/mm/yyyy", value = Sys.Date()),
                                   textInput("accession_number", label = HTML("Accession Number<span style='color:red'>*</span>")),
                                   numericInput("patient_urn", label = HTML("Patient URN (exclude -TTH)<span style='color:red'>*</span>"), value = NA),
                                   dateInput("patient_dob", label = HTML("Patient DOB<span style='color:red'>*</span>"), format = "dd/mm/yyyy", value = NA),
                                   selectInput("rad_initials", label = HTML("Rad Initials<span style='color:red'>*</span>"), choices = rad_initials_list()),
                                   conditionalPanel(condition = "input.rad_initials == 'Other - Please Specify'",textInput("rad_initials_other", label = HTML("Specify Other Rad<span style='color:red'>*</span>"))),
                                   selectInput("theatre", label = HTML("Theatre<span style='color:red'>*</span>"), choices = OT_list()),
                                   conditionalPanel(condition = "input.theatre == 'Other - Please Specify'",textInput("theatre_other", label = HTML("Specify Other Theatre<span style='color:red'>*</span>"))),
                                   selectInput("equipment", label = HTML("Equipment<span style='color:red'>*</span>"), choices = equipment_list()),
                                   conditionalPanel(condition = "input.equipment == 'Other - Please Specify'",textInput("equipment_other", label = HTML("Specify Other Equipment<span style='color:red'>*</span>")))
                            ),
                            column(6,
                                   selectInput("area_exam", label = HTML("Area of Examination<span style='color:red'>*</span>"), choices = area_exam_list),
                                   conditionalPanel(condition = "input.area_exam == 'Other - Please Specify'",
                                                    textInput("area_exam_other", label = HTML("Specify Other Area of Examination<span style='color:red'>*</span>"))),
                                   selectInput("specific_exam", label = HTML("Specific Examination<span style='color:red'>*</span>"), choices = ""),
                                   conditionalPanel(condition = "input.specific_exam == 'Other - Please Specify'",
                                                    textInput("specific_exam_other", label = HTML("Specify Other Specific Examination<span style='color:red'>*</span>"))),
                                   HTML("<b>Screening Time</b><span style='color:red'>*</span>  <i>(either or both time units)</i>"),
                                   fluidRow(
                                     column(6, numericInput("screening_min", "(min)", value = NA, min = 0)),
                                     column(6, numericInput("screening_sec", "(sec)", value = NA, min = 0))),
                                   fluidRow(column(6,numericInput("input_dose", label = HTML("Dose<span style='color:red'>*</span>"), value = NA)),
                                            column(6,selectInput("input_dose_units", label = HTML("Units<span style='color:red'>*</span>"), choices = dose_units_list, width = "100px"))),
                                   fluidRow(column(6, numericInput("input_dap", label = HTML("Dose Area Product<span style='color:red'>*</span>"), value = NA)),
                                            column(6, selectInput("input_dap_units", label = HTML("Units<span style='color:red'>*</span>"), choices = dap_units_list, width = "100px"))),
                                   selectInput("time_between", label = HTML("Time b/n 30min & 10min call<span style='color:red'>*</span>"), choices = time_between_list),
                                   selectInput("time_in_theatre", label = HTML("Time in Theatre (min)<span style='color:red'>*</span>"), choices = time_in_theatre_list),
                                   textInput("note", "Notes")
                            )
                          ),
                          footer = tagList(modalButton("Cancel"), actionButton("save_II_case_modal", "Save Case", class = "btn btn-success", icon = icon("floppy-disk")))
    ))
    
    shinyjs::disable("save_II_case_modal")
    observe({
      mandatory_ok <- all(
        !is.na(input$exam_date),                      
        nzchar(input$accession_number),               
        !is.na(input$patient_urn),  
        !is.na(input$patient_dob), 
        !is.null(input$rad_initials) && nzchar(input$rad_initials),
        !is.null(input$theatre) && nzchar(input$theatre),
        !is.null(input$equipment) && nzchar(input$equipment),
        !is.null(input$area_exam) && nzchar(input$area_exam),
        !is.null(input$specific_exam) && nzchar(input$specific_exam),
        !is.na(input$input_dose),
        !is.null(input$input_dose_units) && nzchar(input$input_dose_units),
        !is.na(input$input_dap),
        !is.null(input$input_dap_units) && nzchar(input$input_dap_units),
        !is.null(input$time_between) && nzchar(input$time_between),
        !is.null(input$time_in_theatre) && nzchar(input$time_in_theatre)
      )
      
      if (mandatory_ok) {
        shinyjs::enable("save_II_case_modal")
      } else {
        shinyjs::disable("save_II_case_modal")
      }
    })
  })
  
  #------ Save Button for II -----------------------------------------------------------------------------
  # store pending case
  new_case <- reactiveVal(NULL)
  
  # Save button logic
  observeEvent(input$save_II_case_modal, {
    
    # calculate screening time
    calculated_screening_time <- ifelse(
      is.na(input$screening_min),
      input$screening_sec,
      ifelse(is.na(input$screening_sec),
             input$screening_min * 60,
             input$screening_min * 60 + input$screening_sec)
    )
    
    row <- data.frame(
      case_type = "II",
      exam_date = input$exam_date,
      accession_number = input$accession_number,
      patient_urn = paste0(input$patient_urn, "-TTH"),
      patient_dob = input$patient_dob,
      rad_initials = ifelse(input$rad_initials == "Other - Please Specify", input$rad_initials_other, input$rad_initials),
      theatre = ifelse(input$theatre == "Other - Please Specify", input$theatre_other, input$theatre),
      equipment = ifelse(input$equipment == "Other - Please Specify", input$equipment_other, input$equipment),
      area_exam = ifelse(input$area_exam == "Other - Please Specify", input$area_exam_other, input$area_exam),
      specific_exam = ifelse(input$specific_exam == "Other - Please Specify", input$specific_exam_other, input$specific_exam),
      screening_time = calculated_screening_time,
      input_dose = input$input_dose,
      input_dose_units = input$input_dose_units,
      dose = dose_converter(input$input_dose, input$input_dose_units),
      input_dap = input$input_dap,
      input_dap_units = input$input_dap_units,
      dap = dap_converter(input$input_dap, input$input_dap_units),
      time_between = input$time_between,
      time_in_theatre = input$time_in_theatre,
      number_of_spins = NA,
      spin_kVp = NA,
      spin_mAs = NA,
      oarm_3D_dose = NA,
      oarm_3D_dlp = NA,
      case_type_detail = NA,
      note = input$note,
      stringsAsFactors = FALSE
    )
    
    new_case(row)
    
    # checks
    warnings <- c()
    
    if (!is.na(row$dap) && !is.na(row$dose) && row$dose > 0) {
      ratio <- row$dap / (row$dose / 1000)
      if (ratio < 25 || ratio > 1350) {
        warnings <- c(
          warnings,
          list(
            tags$li(
              style = "margin-bottom: 10px;",
              "Inputs imply an average Field Size of ",
              tags$b(paste0(round(ratio, 1), " cm2")),
              " (expected 25 – 1350 cm2). Check Dose and DAP values and units are correct."
            )
          )
        )
      }
    }
    
    if (!is.na(row$dose) && !is.na(row$screening_time) && row$screening_time > 0) {
      dose_rate <- (row$dose / row$screening_time) * 60
      if (dose_rate < 1 || dose_rate > 200) {
        warnings <- c(
          warnings,
          list(
            tags$li(
              style = "margin-bottom: 10px;",
              "Inputs imply an average Dose Rate of ",
              tags$b(paste0(round(dose_rate, 1), " mGy/min")),
              " (expected 1 – 200 mGy/min). Check Dose and Screening Time values and units are correct."
            )
          )
        )
      }
    }
    
    if (length(warnings) > 0) {
      # instead of replacing modal, inject an overlay inside the footer
      insertUI(
        selector = ".modal-footer",  # target footer of current input modal
        where = "beforeBegin",
        ui = div(
          id = "warning-overlay",
          style = "position:fixed; top:0; left:0; width:100%; height:100%;
                 background:rgba(0,0,0,0.6); z-index:9999; display:flex; 
                 align-items:center; justify-content:center;",
          div(style="background:white; padding:20px; border-radius:8px; max-width:500px;",
              h4("Warning: Unusual Values Detected"),
              tags$ul(warnings),
              div(style="text-align:right;",
                  actionButton("cancel_high_dose", "Go Back", icon = icon("arrow-left")),
                  actionButton("confirm_high_dose", "Continue Anyway", class="btn btn-danger", icon = icon("triangle-exclamation"))
              )
          )
        )
      )
    } else {
      save_case(row)
      removeModal()
      showNotification("II Case saved!", type = "message")
    }
  })
  
  # Go Back: just remove the overlay
  observeEvent(input$cancel_high_dose, {
    removeUI("#warning-overlay")
  })
  
  # Continue Anyway: save and close everything
  observeEvent(input$confirm_high_dose, {
    req(new_case())
    save_case(new_case())
    removeUI("#warning-overlay")  # remove warning overlay
    removeModal()                 # close input modal
    showNotification("II Case saved despite warnings!", type = "warning")
  })
  
  
  
  #------ Input Modal for Lithotripter -------------------------------------------------------------------
  observeEvent(input$add_litho_case, {
    showModal(modalDialog(title = "Add New Lithotripter Case", size = "l",
                          fluidRow(
                            column(6,
                                   dateInput("exam_date", label = HTML("Exam Date<span style='color:red'>*</span>"), format = "dd/mm/yyyy"),
                                   textInput("accession_number", label = HTML("Accession Number<span style='color:red'>*</span>")),
                                   numericInput("patient_urn", label = HTML("Patient URN (exclude -TTH)<span style='color:red'>*</span>"), value = NA),
                                   dateInput("patient_dob", label = HTML("Patient DOB<span style='color:red'>*</span>"), format = "dd/mm/yyyy", value = NA),
                                   selectInput("rad_initials", label = HTML("Rad Initials<span style='color:red'>*</span>"), choices = rad_initials_list()),
                                   conditionalPanel(condition = "input.rad_initials == 'Other - Please Specify'",textInput("rad_initials_other", label = HTML("Specify Other Rad<span style='color:red'>*</span>"))),
                                   selectInput("theatre", label = HTML("Theatre<span style='color:red'>*</span>"), choices = OT_list()),
                                   conditionalPanel(condition = "input.theatre == 'Other - Please Specify'",textInput("theatre_other", label = HTML("Specify Other Theatre<span style='color:red'>*</span>"))),
                            ),
                            column(6,
                                   selectInput("equipment", label = HTML("Equipment<span style='color:red'>*</span>"), choices = c("Lithotripter")),
                                   HTML("<b>Screening Time</b><span style='color:red'>*</span>  <i>(either or both time units)</i>"),
                                   fluidRow(
                                     column(6, numericInput("screening_min", "(min)", value = NA, min = 0)),
                                     column(6, numericInput("screening_sec", "(sec)", value = NA, min = 0))),
                                   fluidRow(column(6, numericInput("input_dap", label = HTML("Dose Area Product<span style='color:red'>*</span>"), value = NA)),
                                            column(6, selectInput("input_dap_units", label = HTML("Units<span style='color:red'>*</span>"), choices = c("mGy.cm2"), width = "100px"))),
                                   selectInput("time_between", label = HTML("Time b/n 30min & 10min call<span style='color:red'>*</span>"), choices = time_between_list),
                                   selectInput("time_in_theatre", label = HTML("Time in Theatre (min)<span style='color:red'>*</span>"), choices = time_in_theatre_list),
                                   textInput("note", "Notes"))
                          ),
                          footer = tagList(modalButton("Cancel"), actionButton("save_litho_case_modal", "Save Case", class = "btn btn-success", icon = icon("floppy-disk")))
    ))
    
    shinyjs::disable("save_litho_case_modal")
    observe({
      mandatory_ok <- all(
        !is.na(input$exam_date),                      
        nzchar(input$accession_number),               
        !is.na(input$patient_urn),  
        !is.na(input$patient_dob), 
        !is.null(input$rad_initials) && nzchar(input$rad_initials),
        !is.null(input$theatre) && nzchar(input$theatre),
        !is.null(input$equipment) && nzchar(input$equipment),
        !is.na(input$input_dap),
        !is.null(input$input_dap_units) && nzchar(input$input_dap_units),
        !is.null(input$time_between) && nzchar(input$time_between),
        !is.null(input$time_in_theatre) && nzchar(input$time_in_theatre)
      )
      
      if (mandatory_ok) {
        shinyjs::enable("save_litho_case_modal")
      } else {
        shinyjs::disable("save_litho_case_modal")
      }
    })
    
  })
  
  #------ Save Button for Lithotripter -------------------------------------------------------------------
  observeEvent(input$save_litho_case_modal, {
    
    if (is.na(input$screening_min)) {
      calculated_screening_time =  input$screening_sec
    } else if (is.na(input$screening_sec)) {
      calculated_screening_time = input$screening_min * 60
    } else {
      calculated_screening_time = input$screening_min * 60 + input$screening_sec
    }
    
    new_row <- data.frame(
      case_type = "Lithotripter",
      exam_date = input$exam_date,
      accession_number = input$accession_number,
      patient_urn = paste0(input$patient_urn, "-TTH"),
      patient_dob = input$patient_dob,
      rad_initials = ifelse(input$rad_initials == "Other - Please Specify", input$rad_initials_other, input$rad_initials),
      theatre = ifelse(input$theatre == "Other - Please Specify", input$theatre_other, input$theatre),
      equipment = input$equipment,
      area_exam = "Urology",
      specific_exam = "ESWL (Extracorporeal shock wave lithotripsy)",
      screening_time = calculated_screening_time,
      input_dose = NA,
      input_dose_units = NA,
      dose = NA,
      input_dap = input$input_dap,
      input_dap_units = input$input_dap_units,
      dap = dap_converter(input$input_dap, input$input_dap_units),
      time_between = input$time_between,
      time_in_theatre = input$time_in_theatre,
      number_of_spins = NA,
      spin_kVp = NA,
      spin_mAs = NA,
      oarm_3D_dose = NA,
      oarm_3D_dlp = NA,
      case_type_detail = NA,
      note = input$note,
      stringsAsFactors = FALSE
    )
    
    save_case(new_row)
    removeModal()
    showNotification("Lithotripter Case saved!", type = "message")
  })
  
  #------ Input Modal for O-Arm --------------------------------------------------------------------------
  observeEvent(input$add_oarm_case, {
    showModal(modalDialog(title = "Add New O-arm Case", size = "l",
                          fluidRow(
                            column(6,
                                   dateInput("exam_date", label = HTML("Exam Date<span style='color:red'>*</span>"), format = "dd/mm/yyyy"),
                                   textInput("accession_number", label = HTML("Accession Number<span style='color:red'>*</span>")),
                                   numericInput("patient_urn", label = HTML("Patient URN (exclude -TTH)<span style='color:red'>*</span>"), value = NA),
                                   dateInput("patient_dob", label = HTML("Patient DOB<span style='color:red'>*</span>"), format = "dd/mm/yyyy", value = NA),
                                   selectInput("rad_initials", label = HTML("Rad Initials<span style='color:red'>*</span>"), choices = rad_initials_list()),
                                   conditionalPanel(condition = "input.rad_initials == 'Other - Please Specify'",textInput("rad_initials_other", label = HTML("Specify Other Rad<span style='color:red'>*</span>"))),
                                   selectInput("theatre", label = HTML("Theatre<span style='color:red'>*</span>"), choices = OT_list()),
                                   conditionalPanel(condition = "input.theatre == 'Other - Please Specify'",textInput("theatre_other", label = HTML("Specify Other Theatre<span style='color:red'>*</span>"))),
                                   selectInput("equipment", label = HTML("Equipment<span style='color:red'>*</span>"), choices = c("O-Arm")),
                                   HTML("<b>Screening Time</b><span style='color:red'>*</span>  <i>(either or both time units)</i>"),
                                   fluidRow(
                                     column(6, numericInput("screening_min", "(min)", value = NA, min = 0)),
                                     column(6, numericInput("screening_sec", "(sec)", value = NA, min = 0)))
                            ),
                            column(6,
                                   fluidRow(column(6,numericInput("input_dose", label = HTML("Fluoroscopy Dose Total<span style='color:red'>*</span>"), value = NA)),
                                            column(6,selectInput("input_dose_units", label = HTML("Units<span style='color:red'>*</span>"), choices = c("mGy"), width = "100px"))),
                                   fluidRow(column(6, numericInput("input_dap", label = HTML("Fluoroscopy DAP Total<span style='color:red'>*</span>"), value = NA)),
                                            column(6, selectInput("input_dap_units", label = HTML("Units<span style='color:red'>*</span>"), choices = c("mGy.cm2"), width = "100px"))),
                                   numericInput("number_of_spins", label = HTML("Number of Spins<span style='color:red'>*</span>"), value = NA),
                                   conditionalPanel(condition = "input.number_of_spins > 0",numericInput("spin_kVp", "Spin kVp", value = NA)),
                                   conditionalPanel(condition = "input.number_of_spins > 0",textInput("spin_mAs", "Spin mAs")),
                                   conditionalPanel(condition = "input.number_of_spins > 0", numericInput("oarm_3D_dose", "3D Dose: CTDI Total (mGy)", value = NA)),
                                   conditionalPanel(condition = "input.number_of_spins > 0",numericInput("oarm_3D_dlp", "3D Dose: DLP Total (mGy.cm)", value = NA)),
                                   textInput("case_type", "Case Type"),
                                   selectInput("time_between", label = HTML("Time b/n 30min & 10min call<span style='color:red'>*</span>"), choices = time_between_list),
                                   selectInput("time_in_theatre", label = HTML("Time in Theatre (min)<span style='color:red'>*</span>"), choices = time_in_theatre_list),
                                   textInput("note", "Notes"))
                          ),
                          footer = tagList(modalButton("Cancel"), actionButton("save_oarm_case_modal", "Save Case", class = "btn btn-success", icon = icon("floppy-disk")))
    ))
    shinyjs::disable("save_oarm_case_modal")
    observe({
      mandatory_ok <- all(
        !is.na(input$exam_date),                      
        nzchar(input$accession_number),               
        !is.na(input$patient_urn),  
        !is.na(input$patient_dob), 
        !is.null(input$rad_initials) && nzchar(input$rad_initials),
        !is.null(input$theatre) && nzchar(input$theatre),
        !is.null(input$equipment) && nzchar(input$equipment),
        !is.na(input$input_dose),
        !is.null(input$input_dose_units) && nzchar(input$input_dose_units),
        !is.na(input$input_dap),
        !is.null(input$input_dap_units) && nzchar(input$input_dap_units),
        !is.na(input$number_of_spins),
        !is.null(input$time_between) && nzchar(input$time_between),
        !is.null(input$time_in_theatre) && nzchar(input$time_in_theatre)
      )
      if (mandatory_ok) {
        shinyjs::enable("save_oarm_case_modal")
      } else {
        shinyjs::disable("save_oarm_case_modal")
      }
    })
    
  })
  "mangae Dropdown"
  #------ Save Button for O-Arm --------------------------------------------------------------------------
  
  new_case <- reactiveVal(NULL)
  
  observeEvent(input$save_oarm_case_modal, {
    
    if (is.na(input$screening_min)) {
      calculated_screening_time =  input$screening_sec
    } else if (is.na(input$screening_sec)) {
      calculated_screening_time = input$screening_min * 60
    } else {
      calculated_screening_time = input$screening_min * 60 + input$screening_sec
    }
    
    row <- data.frame(
      case_type = "O-arm",
      exam_date = input$exam_date,
      accession_number = input$accession_number,
      patient_urn = paste0(input$patient_urn, "-TTH"),
      patient_dob = input$patient_dob,
      rad_initials = ifelse(input$rad_initials == "Other - Please Specify", input$rad_initials_other, input$rad_initials),
      theatre = ifelse(input$theatre == "Other - Please Specify", input$theatre_other, input$theatre),
      equipment = input$equipment,
      area_exam = NA,
      specific_exam = NA,
      screening_time = calculated_screening_time,
      input_dose = input$input_dose,
      input_dose_units = input$input_dose_units,
      dose = dose_converter(input$input_dose, input$input_dose_units),
      input_dap = input$input_dap,
      input_dap_units = input$input_dap_units,
      dap = dap_converter(input$input_dap, input$input_dap_units),
      time_between = input$time_between,
      time_in_theatre = input$time_in_theatre,
      number_of_spins = input$number_of_spins,
      spin_kVp = input$spin_kVp,
      spin_mAs = input$spin_mAs,
      oarm_3D_dose = input$oarm_3D_dose,
      oarm_3D_dlp = input$oarm_3D_dlp,
      case_type_detail = input$case_type,
      note = input$note,
      stringsAsFactors = FALSE
    )
    
    new_case(row)
    
    # checks
    warnings <- c()
    
    if (!is.na(row$dap) && !is.na(row$dose) && row$dose > 0) {
      ratio <- row$dap / (row$dose / 1000)
      if (ratio < 25 || ratio > 1350) {
        warnings <- c(
          warnings,
          list(
            tags$li(
              style = "margin-bottom: 10px;",
              "Inputs imply an average Field Size of ",
              tags$b(paste0(round(ratio, 1), " cm2")),
              " (expected 25 – 1350 cm2). Check Dose and DAP values and units are correct."
            )
          )
        )
      }
    }
    
    if (!is.na(row$dose) && !is.na(row$screening_time) && row$screening_time > 0) {
      dose_rate <- (row$dose / row$screening_time) * 60
      if (dose_rate < 1 || dose_rate > 200) {
        warnings <- c(
          warnings,
          list(
            tags$li(
              style = "margin-bottom: 10px;",
              "Inputs imply an average Dose Rate of ",
              tags$b(paste0(round(dose_rate, 1), " mGy/min")),
              " (expected 1 – 200 mGy/min). Check Dose and Screening Time values and units are correct."
            )
          )
        )
      }
    }
    
    if (length(warnings) > 0) {
      # instead of replacing modal, inject an overlay inside the footer
      insertUI(
        selector = ".modal-footer",  # target footer of current input modal
        where = "beforeBegin",
        ui = div(
          id = "warning-overlay",
          style = "position:fixed; top:0; left:0; width:100%; height:100%;
                 background:rgba(0,0,0,0.6); z-index:9999; display:flex; 
                 align-items:center; justify-content:center;",
          div(style="background:white; padding:20px; border-radius:8px; max-width:500px;",
              h4("Warning: Unusual Values Detected"),
              tags$ul(warnings),
              div(style="text-align:right;",
                  actionButton("cancel_OArm_high_dose", "Go Back", icon = icon("arrow-left")),
                  actionButton("confirm_OArm_high_dose", "Continue Anyway", class="btn btn-danger", icon = icon("triangle-exclamation"))
              )
          )
        )
      )
    } else {
      save_case(row)
      removeModal()
      showNotification("O-Arm Case saved!", type = "message")
    }
  })
  
  # Go Back: just remove the overlay
  observeEvent(input$cancel_OArm_high_dose, {
    removeUI("#warning-overlay")
  })
  
  # Continue Anyway: save and close everything
  observeEvent(input$confirm_OArm_high_dose, {
    req(new_case())
    save_case(new_case())
    removeUI("#warning-overlay")  # remove warning overlay
    removeModal()                 # close input modal
    showNotification("O-Arm Case saved despite warnings!", type = "warning")
  })
  #------ Data Plotting ----------------------------------------------------------------------------------
  
  output$date_range <- renderUI({
    req(cases_data())
    df <- cases_data()
    
    # Ensure the date column is a Date object
    df$`Exam Date` <- as.Date(df$`Exam Date`, format = "%Y-%m-%d")
    
    div(
      style = "width: 100%; text-align: center;",
      sliderInput(
        inputId = "date_range",
        label = "",
        min = min(df$`Exam Date`, na.rm = TRUE),
        max = max(df$`Exam Date`, na.rm = TRUE),
        value = c(min(df$`Exam Date`, na.rm = TRUE), max(df$`Exam Date`, na.rm = TRUE)),
        timeFormat = "%Y-%m-%d",
        width = "100%"
      )
    )
  })
  
  
  # Filtered data based on date range
  time_filtered_cases_data <- reactive({
    req(cases_data())
    df <- cases_data()
    
    if (!is.null(input$date_range)) {
      df <- df %>%
        filter(`Exam Date` >= input$date_range[1],
               `Exam Date` <= input$date_range[2])
    }
    df
  })
  
  output$xvar_1 <- renderUI({
    req(cases_data())
    selectInput("xvar_1", "X-axis Variable", choices = names(cases_data()), selected = "Exam Date")
  })
  
  output$yvar_1 <- renderUI({
    req(cases_data())
    selectInput("yvar_1", "Y-axis Variable", choices = names(cases_data()), selected = "Dose (mGy)")
  })
  
  output$colorvar_1 <- renderUI({
    req(cases_data())
    selectInput("colorvar_1", "Color by", choices = names(cases_data()), selected = "Category")
  })
  
  # UI for second plot
  output$xvar_2 <- renderUI({
    req(cases_data())
    selectInput("xvar_2", "X-axis Variable", choices = names(cases_data()), selected = "Equipment")
  })
  
  output$yvar_2 <- renderUI({
    req(cases_data())
    selectInput("yvar_2", "Y-axis Variable", choices = names(cases_data()), selected = "Screening Time (s)")
  })
  
  output$colorvar_2 <- renderUI({
    req(cases_data())
    selectInput("colorvar_2", "Color by", choices = names(cases_data()), selected = "Category")
  })
  
  # First plot
  output$plot1 <- renderPlotly({
    req(cases_data(), input$xvar_1, input$yvar_1, input$colorvar_1)
    plot_ly(time_filtered_cases_data(), x = ~get(input$xvar_1), y = ~get(input$yvar_1), color = ~get(input$colorvar_1), type = "scatter", mode = "markers") %>%
      layout(xaxis = list(title = input$xvar_1), yaxis = list(title = input$yvar_1))
  })
  
  # Second plot
  output$plot2 <- renderPlotly({
    req(cases_data(), input$xvar_2, input$yvar_2, input$colorvar_2)
    plot_ly(time_filtered_cases_data(), x = ~get(input$xvar_2), y = ~get(input$yvar_2), color = ~get(input$colorvar_2), type = "box") %>%
      layout(xaxis = list(title = input$xvar_2), yaxis = list(title = input$yvar_2), boxmode = "group")
  })
  
}
ui_secure <- secure_app(
  ui,
  enable_admin = TRUE,
  language = list(
    "please_authenticate" = "jlhgeluhrgv"
  )
)

shinyApp(ui_secure, server)
