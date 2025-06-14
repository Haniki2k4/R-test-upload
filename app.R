# app.R
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
write_rds(read_xlsx("data/ICD10.xlsx"), "data/icd10.rds")

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
  div(class = "header", "🧾 THỐNG KÊ CÁC TRƯỜNG HỢP TAI NẠN GIAO THÔNG"),
  
  # Sidebar cố định
  div(class = "sidebar",
      a("Bảng thống kê", href = "#!dashboard", class = "nav-link", id = "nav-dashboard"),
      a("Thống kê phân tích", href = "#!analysis", class = "nav-link", id = "nav-analysis"),
      a("Xem dữ liệu", href = "#!data_viewer", class = "nav-link", id = "nav-data_viewer"),
      a("Đánh giá chất lượng", href = "#!quality", class = "nav-link", id = "nav-quality"),
      a("Thông tin số liệu", href = "#!info", class = "nav-link", id = "nav-info")
  ),
  
  # Main content
  div(class = "content",
      uiOutput("pageContent")
  )
)



# -- 2. Server
server <- function(input, output, session) {
  # Nav link highlighting
  observe({
    current <- session$clientData$url_hash
    navs <- c("dashboard","analysis","data_viewer","quality","info")
    lapply(navs, function(nav) {
      active <- paste0("#!", nav) == current
      toggleClass(selector = paste0("#nav-", nav), class = "active", condition = active)
    })
  })
  
  # Dashboard logic
  computed <- reactive({
    adm <- admission_df(); 
    diag <- diagnosis_df(); 
    icd <- icd10_df()
    transport <- diag %>% 
      filter(str_detect(diagnosis, "^V[0-9]{2}")) %>% 
      inner_join(adm, by="admission_id") %>% 
      filter(between(age_years,12,18))
    top10 <- transport %>% 
      count(diagnosis, sort=TRUE) %>% 
      slice_head(n = 10) %>% 
      left_join(icd, by=c("diagnosis"="code"))
    los <- transport %>% 
      filter(diagnosis %in% top10$diagnosis) %>%
      group_by(diagnosis) %>%
      summarise(n=n(), los_mean=mean(los,na.rm=TRUE), los_median=median(los,na.rm=TRUE), los_max=max(los,na.rm=TRUE)) %>%
      left_join(icd, by=c("diagnosis"="code"))
    adm_ids <- unique(transport$admission_id)
    injuries <- diag %>% filter(admission_id %in% adm_ids, Other=="P", !str_detect(diagnosis,"^V[0-9]{2}")) %>% count(diagnosis,sort=TRUE) %>% slice_head(n = 5) %>% left_join(icd, by=c("diagnosis"="code"))
    list(transport=transport, top10=top10, los=los, injuries=injuries)
  })
  
  # Analysis logic
  analysis_data <- reactive({
    transport <- computed()$transport %>% 
      mutate(age_group = cut(age_years, breaks=c(11,14,16,18), 
                             labels=c("12-14","15-16","17-18")))
    transport_age_group <- transport %>% 
      select(admission_id, age_group) %>% 
      distinct()
    gender_age_freq <- transport %>% count(Gender, age_group)
    diag <- diagnosis_df(); ids <- unique(transport$admission_id)
    primary <- diag %>% 
      filter(admission_id %in% ids, Other=="P", !str_detect(diagnosis,"^V[0-9]{2}")) %>%
      inner_join(transport_age_group, by="admission_id") %>%
      mutate(injury_group = case_when(
        str_detect(diagnosis,"^S06") ~ "Chấn thương sọ não", 
        str_detect(diagnosis,"^S42|^S52") ~ "Gãy xương chi trên",
        str_detect(diagnosis,"^S72|^S82") ~ "Gãy xương chi dưới", 
        str_detect(diagnosis,"^S50|^S60|^S70") ~ "Tổn thương phần mềm", 
        TRUE ~ "Khác"))
    table_group <- table(primary$age_group, primary$injury_group)
    chi <- chisq.test(table_group)
    list(gender_age=gender_age_freq, table_grouped=as.data.frame.matrix(table_group), chi=chi)
  })
  
  # Data render functions
  render_dashboard <- function() {
    tagList(
      h3("📊 Bảng thống kê"),
      
      fluidRow(
        column(7, div(class = "content-box",
                      h4("🔝 Top 10 chẩn đoán TNGT"),
                      style = "margin-bottom: 20px;",
                      DTOutput("top10_table"))),
        column(5, div(class = "content-box",
                      h4("🔍 Top 5 chẩn đoán kèm theo"),
                      style = "margin-bottom: 20px;",
                      plotlyOutput("injuries_plot")))
      ),
      
      fluidRow(
        column(5, div(class = "content-box",
                      h4("📑 Chẩn thương thường gặp"),
                      style = "margin-bottom: 20px;",
                      DTOutput("injuries_table"))),
        column(7, div(class = "content-box",
                      h4("📊 Biểu đồ số ngày nằm viện (LOS)"),
                      style = "margin-bottom: 20px;",
                      plotlyOutput("los_plot")))
      )
    )
  }
  
  render_analysis <- function() {
    tagList(
      h3("📈 Phân tích theo tuổi & giới"),
      
      div(class = "content-box",
          h4("👫 Biểu đồ tần suất theo nhóm tuổi & giới tính"),
          style = "margin-bottom: 20px;",
          plotlyOutput("gender_age_plot")),
      
      fluidRow(
        column(6, div(class = "content-box",
                      h4("📊 Bảng chéo: Nhóm tuổi × Chấn thương"),
                      DTOutput("injury_crosstab"))),
        column(6, div(class = "content-box",
                      h4("📈 Kiểm định Chi-squared"),
                      verbatimTextOutput("chi_result")))
      )
    )
  }
  
  
  render_data_viewer <- function() {
    tagList(
      h3("📁 Quản lý dữ liệu"),
      fluidRow(
        column(6, selectInput("upload_target", "Upload vào bảng:", choices=c("Admission","Diagnosis","ICD10"))),
        column(6, downloadButton("download_all", "Tải toàn bộ dữ liệu"))
      ),
      fluidRow(
        column(6, fileInput("upload_file", "Chọn file (.xlsx/.csv)", accept=c(".xlsx",".csv"))),
        column(6, selectInput("selected_table","Xem bảng:",choices=c("Admission","Diagnosis","ICD10")))
      ),
      div(class="content-box", DTOutput("data_table"))
    )
  }
  
  render_quality <- function() tagList(h3("🔍 Đánh giá chất lượng dữ liệu (đang cập nhật)"))
  render_info    <- function() tagList(h3("ℹ️ Thông tin số liệu (đang cập nhật)"))
  
  output$pageContent <- renderUI({
    switch(sub("^#!","", session$clientData$url_hash),
           dashboard = render_dashboard(),
           analysis = render_analysis(),
           data_viewer = render_data_viewer(),
           quality = render_quality(),
           info = render_info(),
           render_dashboard()
    )
  })
  
  # Render table & charts
  output$top10_table  <- renderDT(
    computed()$top10, 
    selection="single", 
    options=list(scrollX=TRUE)
  )
  
  output$injuries_table  <- renderDT(
    computed()$injuries, 
    options = list(scrollX=TRUE)
  )
  
  output$injuries_plot <- renderPlotly({
    inj <- computed()$injuries
    plot_ly(
      inj,
      x = ~n,
      y = ~diagnosis,
      type = "bar",
      orientation = "h",
      text = ~description,
      textposition = "none",
      hoverinfo = "text+x",
      marker = list(color = "#78c2ad")  # 💚 Minty Green
    ) %>%
      layout(
        # title = "Top 5 chẩn đoán kèm theo",
        xaxis = list(title = "Số ca"),
        yaxis = list(title = "Mã ICD")
      )
  })
  
  
  output$los_plot <- renderPlotly({
    los <- computed()$los
    sel <- input$top10_table_rows_selected
    pd <- if (length(sel)) {
      los %>% filter(diagnosis == computed()$top10$diagnosis[sel])
    } else los
    
    plot_ly(pd, x = ~diagnosis, y = ~los_mean, type = "bar",
            name = "Mean LOS",
            marker = list(color = '#78c2ad'),  # Minty Green
            text = ~description,
            textposition = "none", hoverinfo = "text+y") %>%
      add_trace(y = ~los_median, name = "Median LOS",
                type = "bar",
                marker = list(color = '#5bc0de'),  # Accent blue
                text = ~description,
                textposition = "none", hoverinfo = "text+y") %>%
      layout(
        barmode = "group",
        xaxis = list(title = "ICD Code"),
        yaxis = list(title = "Số ngày nằm viện"),
        legend = list(orientation = "h", x = 0.3, y = -0.2)
      )
  })
  
  output$gender_age_plot <- renderPlotly({
    ga <- analysis_data()$gender_age
    ggplotly(
      ggplot(ga, aes(age_group, n, fill = as.factor(Gender))) +
        geom_col(position = "dodge") +
        scale_fill_manual(
          name = "Giới tính",
          values = c("1" = "#78c2ad", "2" = "#5bc0de")  # 💚💙 Minty Colors
        ) +
        labs(
          title = "Tần suất TNGT theo nhóm tuổi & giới tính",
          x = "Nhóm tuổi",
          y = "Số ca"
        ) +
        theme_minimal()
    )
  })
  output$injury_crosstab <- renderDT(
    analysis_data()$table_grouped, options=list(dom='t'))
  
  output$chi_result <- renderPrint(analysis_data()$chi)
  
  # Data viewer actions
  output$data_table <- renderDT({
    df <- switch(input$selected_table, 
                 Admission=admission_df(), 
                 Diagnosis=diagnosis_df(), 
                 ICD10=icd10_df()); 
    datatable(df, filter="top", options=list(scrollX=TRUE))})
  
  observeEvent(input$upload_file,{req(input$upload_target); 
    file=input$upload_file; ext=tools::file_ext(file$datapath); 
    newdata = switch(ext, 
                     xlsx=read_xlsx(file$datapath), 
                     csv=read.csv(file$datapath,stringsAsFactors=FALSE), 
                     stop("Loại file không hợp lệ")); 
    switch(input$upload_target, 
           Admission=admission_df(newdata), 
           Diagnosis=diagnosis_df(newdata), 
           ICD10=icd10_df(newdata)); 
    showNotification(paste("Cập nhật bảng", input$upload_target), type="message")})
  
  output$download_all <- downloadHandler(
    filename=function() paste0("all_data_", Sys.Date(), ".xlsx"), 
    content=function(f) write_xlsx(list(admission=admission_df(), 
                                        diagnosis=diagnosis_df(), 
                                        icd10=icd10_df()), f))
}

shinyApp(ui, server)
