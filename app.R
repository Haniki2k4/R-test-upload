# app.R
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("show", "shinyjs")
conflict_prefer("layout", "plotly")
library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(DT)
library(plotly)
library(tidyverse)


write_rds(read_xlsx("data/admission.xlsx"), "data/admission.rds")
write_rds(read_xlsx("data/diagnosis.xlsx"), "data/diagnosis.rds")
write_rds(read_xlsx("data/icd10.xlsx"), "data/icd10.rds")

admission_df <- reactiveVal(readRDS("data/admission.rds"))
diagnosis_df <- reactiveVal(readRDS("data/diagnosis.rds"))
icd10_df     <- reactiveVal(readRDS("data/icd10.rds"))



# -- 1. UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  useShinyjs(),
  
  tags$head(tags$style(HTML("
    body {
      background: #f4f6f9;
      margin: 0;
    }
    
    .header {
      background-color: #60afbc;
      color: #f1f2f8;
      padding: 15px 20px;
      font-size: 18px;
      font-weight: bold;
      position: fixed;
      top: 0;
      left: 0;
      right: 0;
      z-index: 1000;
      height: 60px;
      display: flex;
      align-items: center;
    }

    .sidebar {
      position: fixed;
      top: 60px;
      bottom: 0;
      left: 0;
      width: 250px;
      background-color: #78c2ad;
      padding: 15px;
      color: #f1f2f8;
      overflow-y: auto;
    }

    .sidebar a {
      color: #f1f2f8;
      display: block;
      padding: 8px;
      border-radius: 4px;
      text-decoration: none;
    }

    .sidebar a.active,
    .sidebar a:hover {
      background: #f3979b;
      color: f1f2f8;
    }

    .content {
      margin-left: 270px;
      margin-top: 80px;
      padding: 20px;
    }

    .content-box {
      background: #fff;
      padding: 15px;
      border-radius: 8px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      margin-bottom: 20px;
    }
    
    table.dataTable tbody tr:nth-child(even) {
      background-color: #e6f2ef; /* Nhạt màu minty */
    }

    @media screen and (max-width: 768px) {
      .sidebar {
        position: relative;
        width: 100%;
        margin-top: 10px;
      }
      .content {
        margin-left: 0;
      }
    }
  "))),
  
  # Header cố định
  div(class = "header", "🧾 HỆ THỐNG QUẢN LÝ MÃ HOÁ & THỐNG KÊ CÁC TRƯỜNG HỢP TAI NẠN GIAO THÔNG"),
  
  # Sidebar cố định
  div(class = "sidebar",
      strong("📊 Mục chính"),
      a("Dashboard tổng quan", href = "#!dashboard", id = "nav-dashboard", class = "nav-link"),
      strong("📁 Dữ liệu"),
      a("Xem dữ liệu", href = "#!data_viewer", id = "nav-data_viewer", class = "nav-link"),
      a("Tải dữ liệu", href = "#!upload", id = "nav-upload", class = "nav-link"),
      strong("📈 Phân tích & Đánh giá"),
      a("Số ca TNGT theo tuổi & giới", href = "#!freq", id = "nav-freq", class = "nav-link"),
      a("LOS theo nguồn nhập viện", href = "#!los_source", id = "nav-los_source", class = "nav-link"),
      a("Tử vong / Ra viện", href = "#!sep_mode", id = "nav-sep_mode", class = "nav-link"),
      a("Phân tích nhập viện", href = "#!adm_analysis", id = "nav-adm_analysis", class = "nav-link"),
      strong("ℹ️ Thông tin dữ liệu"),
      a("Thông tin số liệu", href = "#!info", id = "nav-info", class = "nav-link"),
      a("Đánh giá chất lượng số liệu", href = "#!quality", id = "nav-quality", class = "nav-link"),
      strong("❓ Hướng dẫn & Giới thiệu"),
      a("Hướng dẫn & Giới thiệu", href = "#!about", id = "nav-about", class = "nav-link")
  ),
  
  # Main content
  div(class = "content",
      uiOutput("pageContent")
  )
)



# -- 2. Server
server <- function(input, output, session) {
  
  # Nav highlighting
  observe({
    current <- session$clientData$url_hash
    navs <- c("dashboard","data_viewer","upload","freq","los_source","sep_mode","adm_analysis","info","about")
    for(nav in navs) {
      active <- paste0("#!", nav) == current
      toggleClass(selector = paste0("#nav-", nav), class="active", condition=active)
    }
  })
  
  # --- Computation shared ---
  computed <- reactive({
    adm <- admission_df()
    diag <- diagnosis_df()
    icd <- icd10_df()
    
    transport <- diag %>%
      filter(str_detect(diagnosis, "^V[0-9]{2}")) %>%
      inner_join(adm, by="admission_id") %>%
      filter(between(age_years,12,18))
    
    list(transport=transport, icd=icd)
  })
  
  analysis_data <- reactive({
    transport <- computed()$transport
    
    # Lọc giới tính nếu được chọn
    gender_val <- input$freq_gender_filter
    if (!is.null(gender_val) && gender_val != "" && gender_val != "Tất cả") {
      val <- ifelse(gender_val == "Nam", 1, 2)
      transport <- transport %>% filter(Gender == val)
    }
    
    # Thêm nhóm tuổi
    transport <- transport %>%
      mutate(age_group = cut(age_years, breaks = c(11, 14, 16, 18),
                             labels = c("12-14", "15-16", "17-18")))
    
    transport_age_group <- transport %>%
      select(admission_id, age_group) %>%
      distinct()
    
    gender_age_freq <- transport %>%
      count(Gender, age_group)
    
    diag <- diagnosis_df()
    ids <- unique(transport$admission_id)
    
    primary <- diag %>%
      filter(admission_id %in% ids, Other == "P", !str_detect(diagnosis, "^V[0-9]{2}")) %>%
      inner_join(transport_age_group, by = "admission_id") %>%
      mutate(injury_group = case_when(
        str_detect(diagnosis, "^S06") ~ "Chấn thương sọ não",
        str_detect(diagnosis, "^S42|^S52") ~ "Gãy xương chi trên",
        str_detect(diagnosis, "^S72|^S82") ~ "Gãy xương chi dưới",
        str_detect(diagnosis, "^S50|^S60|^S70") ~ "Tổn thương phần mềm",
        TRUE ~ "Khác"
      ))
    
    table_group <- table(primary$age_group, primary$injury_group)
    chi <- chisq.test(table_group)
    
    list(gender_age = gender_age_freq,
         table_grouped = as.data.frame.matrix(table_group),
         chi = chi)
  })
  
  
  # --- Dashboard filters ---
  filtered_data <- reactive({
    df <- computed()$transport
    if(!is.null(input$filter_gender) && input$filter_gender != "Tất cả") {
      val <- ifelse(input$filter_gender=="Nam",1,2)
      df <- df %>% filter(Gender==val)
    }
    if(!is.null(input$filter_age)) {
      df <- df %>% filter(between(age_years, input$filter_age[1], input$filter_age[2]))
    }
    df
  })
  
  #### Panels
  render_dashboard <- function() {
    tagList(
      h3("📊 Dashboard tổng quan"),
      div(class="content-box",
          fluidRow(
            column(6, selectInput("filter_gender","Giới tính:",choices=c("Tất cả","Nam","Nữ"),selected="Tất cả")),
            column(6, sliderInput("filter_age","Độ tuổi:",min=12,max=18,value=c(12,18)))
          )
      ),
      fluidRow(
        column(6, div(class="content-box", h4("🔝 10 trường hợp TNGT"), DTOutput("top10_table"))),
        column(6, div(class="content-box", h4("📑 Bảng chẩn thương thường gặp"), DTOutput("injuries_table")))
      ),
      fluidRow(
        column(6, div(class="content-box", h4("🔍 5 chẩn đoán kèm theo"), plotlyOutput("injuries_plot"))),
        column(6, div(class="content-box", h4("📊 Thống kê số ngày nằm viện (LOS)"), plotlyOutput("los_plot")))
      )
    )
  }
  
  render_data_viewer <- function(){
    tagList(
      h3("📁 Xem dữ liệu"),
      div(class="content-box",
          fluidRow(
            column(4, selectInput("selected_table","Chọn bảng:", 
                                  choices=c("Admission","Diagnosis","ICD10"))),
            column(4),
            column(4, downloadButton("download_current","Tải bảng hiện tại"))
          ),
          DTOutput("data_table")
      )
    )
  }
  
  render_upload <- function(){
    tagList(
      h3("📁 Cập nhật dữ liệu"),
      div(class="content-box",
          fileInput("upload_adm","Upload Admission (.xlsx/.csv)", accept=c(".xlsx",".csv")),
          fileInput("upload_diag","Upload Diagnosis (.xlsx/.csv)", accept=c(".xlsx",".csv")),
          fileInput("upload_icd","Upload ICD10 (.xlsx/.csv)", accept=c(".xlsx",".csv"))
      ),
      h3("📁 Tải dữ liệu"),
      div(class="content-box",
          downloadButton("download_all","Tải toàn bộ dữ liệu")
      )
    )
  }
  
  render_freq <- function(){
    tagList(
      h3("📈 Số ca TNGT"),
      div(class="content-box",
          fluidRow(
            column(6,
                   selectInput("freq_gender_filter", "Giới tính:",
                               choices = c("Tất cả", "Nam", "Nữ"),
                               selected = "Tất cả"))
          )
      ),
      div(class="content-box", plotlyOutput("gender_age_plot")),
      fluidRow(
        column(6, div(class="content-box", DTOutput("injury_tab"))),
        column(6, div(class="content-box", verbatimTextOutput("chi_result")))
      )
    )
  }
  
  
  render_los_source  <- function() tagList(h3("🚑 LOS theo nguồn nhập viện"), plotlyOutput("los_by_source_plot"))
  render_sep_mode    <- function() tagList(h3("💀 Tử vong / Ra viện"), plotlyOutput("sep_mode_plot"))
  render_adm_analysis<- function() tagList(h3("🏥 Phân tích loại nhập viện"), plotlyOutput("admtype_plot"))
  render_info <- function(){
    tagList(h3("ℹ️ Thông tin số liệu"))
  }
  
  render_quality <- function(){
    tagList(h3("🔍 Đánh giá chất lượng số liệu (chờ cập nhật)"))
  }
  render_about <- function(){
    tagList(h3("❓ Hướng dẫn & Giới thiệu"))
  }
  
  output$pageContent <- renderUI({
    hash <- sub("^#!","",session$clientData$url_hash)
    switch(hash,
           "dashboard" = render_dashboard(),
           "data_viewer" = render_data_viewer(),
           "upload" = render_upload(),
           "freq" = render_freq(),
           "los_source" = render_los_source(),
           "sep_mode" = render_sep_mode(),
           "adm_analysis" = render_adm_analysis(),
           "quality" = render_quality(),
           "info" = render_info(),
           "about" = render_about(),
           render_dashboard()
    )
  })
  
  ##### Render Dashboard components
  output$data_table <- renderDT({
    tbl <- switch(input$selected_table,
                  "Admission" = admission_df(),
                  "Diagnosis" = diagnosis_df(),
                  "ICD10"     = icd10_df())
    datatable(tbl, filter = "top", options = list(scrollX=TRUE))
  })
  
  
  output$top10_table <- renderDT({
    df <- filtered_data()
    df %>% count(diagnosis, sort=TRUE) %>% slice_head(n=10) %>%
      left_join(icd10_df(), by=c("diagnosis"="code")) %>%
      datatable(selection="single", options=list(scrollX=TRUE))
  }, server=TRUE)
  
  output$los_plot <- renderPlotly({
    df <- filtered_data()
    codes <- df %>% count(diagnosis) %>% slice_head(n=10) %>% pull(diagnosis)
    los <- df %>% filter(diagnosis %in% codes) %>% group_by(diagnosis) %>%
      summarise(los_mean=mean(los,na.rm=TRUE), los_median=median(los,na.rm=TRUE)) %>%
      left_join(icd10_df(), by=c("diagnosis"="code"))
    plot_ly(los, x=~diagnosis, y=~los_mean, type="bar", name="Mean", marker=list(color="#78c2ad")) %>%
      add_trace(y=~los_median, name="Median", marker=list(color="#5bc0de")) %>%
      layout(barmode="group", xaxis=list(title="ICD"), yaxis=list(title="LOS"))
  })
  
  output$injuries_plot <- renderPlotly({
    adm_ids <- filtered_data() %>% pull(admission_id) %>% unique()
    inj <- diagnosis_df() %>% filter(admission_id %in% adm_ids, Other=="P", !str_detect(diagnosis,"^V[0-9]{2}")) %>%
      count(diagnosis, sort=TRUE) %>% slice_head(n=5) %>%
      left_join(icd10_df(), by=c("diagnosis"="code"))
    plot_ly(inj, x=~n, y=~diagnosis, type="bar", orientation="h", marker=list(color="#78c2ad")) %>%
      layout(xaxis=list(title="Count"), yaxis=list(title="ICD"))
  })
  
  output$injuries_table <- renderDT({
    adm_ids <- filtered_data() %>% pull(admission_id) %>% unique()
    diagnosis_df() %>% filter(admission_id %in% adm_ids, Other=="P", !str_detect(diagnosis,"^V[0-9]{2}")) %>%
      count(diagnosis, sort=TRUE) %>% slice_head(n=5) %>%
      left_join(icd10_df(), by=c("diagnosis"="code")) %>%
      datatable(options=list(scrollX=TRUE))
  }, server=TRUE)
  
  output$injury_tab <- renderDT(
    analysis_data()$table_grouped,
    options = list(dom = 't'),
    server = TRUE
  )
  output$chi_result <- renderPrint(analysis_data()$chi)
  
  output$gender_age_plot <- renderPlotly({
    dat <- analysis_data()$gender_age
    dat$Gender <- factor(dat$Gender, levels = c(1,2), labels = c("Nam", "Nữ"))
    ggplotly(
      ggplot(dat, aes(age_group, n, fill = Gender)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("Nam"="#78c2ad", "Nữ"="#5bc0de")) +
        labs(title = "Số ca TNGT theo nhóm tuổi & giới", 
             x = "Nhóm tuổi", y = "Số ca", fill = "Giới tính") +
        theme_minimal()
    )
  })
  
  
  # Labels cho pie charts
  sep_mode_labels <- c(A="Xuất viện",B="Chuyển viện",D="Tử vong",H="Chăm sóc tại nhà",N="Không rõ",S="Chuyển chăm sóc",T="Khác")
  adm_source_labels <- c(A="Nhà riêng",B="Nơi khác",H="Bệnh viện khác",N="Dưỡng lão",S="Cơ sở y tế",T="Khác",Y="Sinh tại viện")
  
  output$los_by_source_plot <- renderPlotly({
    df <- computed()$transport %>% count(admsource) %>%
      mutate(label = paste0(adm_source_labels[admsource], " (", round(100 * n / sum(n), 1), "%)"))
    plot_ly(df, labels = ~label, values = ~n, type = "pie",
            marker = list(colors = RColorBrewer::brewer.pal(n = max(3, nrow(df)), name = "Set3")[1:nrow(df)])) %>%
      layout(title = "Phân bố nguồn nhập viện",
             xaxis = list(title = "Nguồn nhập viện"),
             yaxis = list(title = "Số ca"),
             margin = list(t = 50))
  })
  
  
  output$sep_mode_plot <- renderPlotly({
    df <- computed()$transport %>% count(sepmode) %>%
      mutate(label = paste0(sep_mode_labels[sepmode], " (", round(100 * n / sum(n), 1), "%)"))
    plot_ly(df, labels = ~label, values = ~n, type = "pie",
            marker = list(colors = RColorBrewer::brewer.pal(n = max(3, nrow(df)), name = "Set3")[1:nrow(df)])) %>%
      layout(title = "Tình trạng ra viện / tử vong",
             margin = list(t = 50))
  })
  
  
  output$admtype_plot <- renderPlotly({
    df <- computed()$transport %>%
      count(admtype) %>%
      arrange(n)
    plot_ly(df, x = ~n, y = ~reorder(admtype, n), type = "bar",
            orientation = 'h',
            marker = list(color = RColorBrewer::brewer.pal(n = nrow(df), name = "Set3"))) %>%
      layout(
        title = "LOS theo loại nhập viện", 
        yaxis = list(title = "Loại nhập viện"), 
        xaxis = list(title = "Số ca"),
        margin = list(t = 50)
      )
  })
  
  
  
  # Upload placeholder
  observeEvent(input$upload_adm, {
    file <- input$upload_adm
    req(file)
    ext <- tools::file_ext(file$datapath)
    newdata <- switch(ext,
                      xlsx = read_xlsx(file$datapath),
                      csv  = read.csv(file$datapath, stringsAsFactors = FALSE),
                      stop("Unsupported file type"))
    admission_df(newdata)
    showNotification("Đã cập nhật bảng Admission", type="message")
  })
  
  observeEvent(input$upload_diag, {
    file <- input$upload_diag
    req(file)
    ext <- tools::file_ext(file$datapath)
    newdata <- switch(ext,
                      xlsx = read_xlsx(file$datapath),
                      csv  = read.csv(file$datapath, stringsAsFactors = FALSE),
                      stop("Unsupported file type"))
    diagnosis_df(newdata)
    showNotification("Đã cập nhật bảng Diagnosis", type="message")
  })
  
  observeEvent(input$upload_icd, {
    file <- input$upload_icd
    req(file)
    ext <- tools::file_ext(file$datapath)
    newdata <- switch(ext,
                      xlsx = read_xlsx(file$datapath),
                      csv  = read.csv(file$datapath, stringsAsFactors = FALSE),
                      stop("Unsupported file type"))
    icd10_df(newdata)
    showNotification("Đã cập nhật bảng ICD10", type="message")
  })
  
  output$download_all <- downloadHandler(
    filename=function() paste0("all_data_", Sys.Date(), ".xlsx"), 
    content=function(f) write_xlsx(list(admission=admission_df(), 
                                        diagnosis=diagnosis_df(), 
                                        icd10=icd10_df()), f))
  
  output$download_current <- downloadHandler(
    filename = function() { paste0(input$selected_table, "_", Sys.Date(), ".xlsx") },
    content = function(file) {
      tbl <- switch(input$selected_table,
                    "Admission" = admission_df(),
                    "Diagnosis" = diagnosis_df(),
                    "ICD10"     = icd10_df())
      writexl::write_xlsx(tbl, file)
    }
  )
}

shinyApp(ui, server)