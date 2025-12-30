# =============================================================================
# تطبيق ARDL لاختيار النموذج الأمثل
# ARDL Optimal Model Selection App
# © 2025 MUHAMMAD ALKHALAF - جميع الحقوق محفوظة
# =============================================================================

# --- تحميل الحزم ---
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinyBS)
  library(DT)
  library(ARDL)
  library(urca)
  library(strucchange)
  library(lmtest)
  library(sandwich)
  library(ggplot2)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(writexl)
  library(officer)
  library(flextable)
})

# تحميل الوظائف المساعدة
source("R/scoring_system.R")
source("R/diagnostic_tests.R")
source("R/stationarity_tests.R")
source("R/export_functions.R")

# =============================================================================
# واجهة المستخدم (UI)
# =============================================================================

ui <- dashboardPage(
  skin = "blue",

  # --- Header ---
  dashboardHeader(
    title = "نظام ARDL لاختيار النموذج الأمثل",
    titleWidth = 350
  ),

  # --- Sidebar ---
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("📁 البيانات", tabName = "data_tab", icon = icon("database")),
      menuItem("⚙️ الإعدادات", tabName = "settings_tab", icon = icon("cog")),
      menuItem("📊 الاستقرارية", tabName = "stationarity_tab", icon = icon("chart-line")),
      menuItem("🏆 النماذج المرشحة", tabName = "models_tab", icon = icon("trophy")),
      menuItem("🔬 التشخيصات", tabName = "diagnostics_tab", icon = icon("microscope")),
      menuItem("📥 التصدير", tabName = "export_tab", icon = icon("download")),
      menuItem("❓ المساعدة", tabName = "help_tab", icon = icon("question-circle"))
    ),
    hr(),

    # إعدادات سريعة
    div(
      style = "padding: 10px;",

      # رفع الملف مع tooltip
      tags$div(
        title = "ارفع ملف البيانات بصيغة Excel (.xlsx) أو CSV. يجب أن يحتوي الصف الأول على أسماء المتغيرات.",
        fileInput("datafile", "📂 رفع ملف البيانات:",
          accept = c(".xlsx", ".xls", ".csv"),
          buttonLabel = "اختر ملف",
          placeholder = "لم يتم اختيار ملف"
        )
      ),

      # اختيار المتغيرات
      uiOutput("var_selectors"),
      hr(),

      # الفترات المتأخرة مع tooltip
      tags$div(
        title = "حدد الحد الأقصى لعدد الفترات المتأخرة (Lags). كلما زاد العدد، زاد عدد النماذج المختبرة.",
        sliderInput("max_lags", "🔢 الحد الأقصى للفترات المتأخرة:",
          min = 1, max = 8, value = 4, step = 1
        )
      ),

      # مستوى المعنوية مع tooltip
      tags$div(
        title = "مستوى المعنوية الإحصائية لرفض الفرضية الصفرية. القيمة الشائعة هي 5%.",
        selectInput("sig_level", "📊 مستوى المعنوية:",
          choices = c("1%" = 0.01, "5%" = 0.05, "10%" = 0.10),
          selected = 0.05
        )
      ),

      # الثابت والاتجاه مع tooltip
      tags$div(
        title = "حدد ما إذا كان النموذج يتضمن ثابتاً (Constant) و/أو اتجاهاً زمنياً (Trend).",
        checkboxGroupInput("deterministics", "📐 مكونات النموذج:",
          choices = c(
            "ثابت (Constant)" = "const",
            "اتجاه (Trend)" = "trend"
          ),
          selected = "const"
        )
      ),

      # المتغير الوهمي مع tooltip
      tags$div(
        title = "أضف متغيراً وهمياً (Dummy) إذا كان هناك انكسار هيكلي في البيانات.",
        checkboxInput("use_dummy", "📍 إضافة متغير وهمي للانكسار", value = FALSE)
      ),
      conditionalPanel(
        condition = "input.use_dummy == true",
        tags$div(
          title = "حدد سنة الانكسار الهيكلي. سيتم إنشاء متغير وهمي = 1 لجميع السنوات بعد هذه السنة.",
          numericInput("break_year", "📅 سنة الانكسار:", value = 2007, min = 1900, max = 2100)
        )
      ),
      hr(),

      # زر التشغيل
      tags$div(
        title = "اضغط لتشغيل التحليل. سيتم تقدير جميع توليفات ARDL الممكنة واختيار الأفضل.",
        actionButton("run_analysis", "🚀 تشغيل التحليل",
          class = "btn-primary btn-lg btn-block",
          style = "font-size: 18px; padding: 15px;"
        )
      ),

      # شريط حقوق النشر
      hr(),
      tags$div(
        style = "text-align: center; font-size: 11px; color: #888; padding: 10px;",
        HTML("© 2025 <strong>MUHAMMAD ALKHALAF</strong><br>جميع الحقوق محفوظة")
      )
    )
  ),

  # --- Body ---
  dashboardBody(
    # CSS مخصص للعربية
    tags$head(
      tags$style(HTML("
        body, .content-wrapper, .main-sidebar, .sidebar {
          font-family: 'Segoe UI', Tahoma, Arial, sans-serif;
        }
        .content-wrapper { direction: rtl; text-align: right; }
        .main-sidebar { direction: rtl; text-align: right; }
        .sidebar-menu { direction: rtl; }
        .box-title { font-size: 18px; font-weight: bold; }
        .info-box-number { font-size: 24px; }
        .score-badge {
          font-size: 28px; font-weight: bold;
          padding: 15px 25px; border-radius: 50%;
        }
        .score-high { background-color: #28a745; color: white; }
        .score-medium { background-color: #ffc107; color: black; }
        .score-low { background-color: #dc3545; color: white; }
        .dataTables_wrapper { direction: ltr; }
        .nav-tabs-custom > .tab-content { direction: rtl; }
        .help-tooltip { cursor: help; border-bottom: 1px dotted #666; }
        .copyright-footer {
          position: fixed; bottom: 0; width: 100%;
          background: #f4f4f4; padding: 10px; text-align: center;
          font-size: 12px; border-top: 1px solid #ddd;
        }
        .progress-message {
          font-size: 16px; font-weight: bold; color: #3c8dbc;
        }
      "))
    ),
    tabItems(
      # --- تبويب البيانات ---
      tabItem(
        tabName = "data_tab",
        fluidRow(
          box(
            title = "معاينة البيانات", width = 12, status = "primary",
            solidHeader = TRUE, collapsible = TRUE,
            tags$p(class = "text-muted", "عرض أول صفوف البيانات المرفوعة للتحقق من صحتها."),
            DTOutput("data_preview")
          )
        ),
        fluidRow(
          infoBoxOutput("obs_count", width = 3),
          infoBoxOutput("var_count", width = 3),
          infoBoxOutput("time_range", width = 3),
          infoBoxOutput("missing_count", width = 3)
        ),
        fluidRow(
          box(
            title = "الإحصاءات الوصفية", width = 12, status = "info",
            solidHeader = TRUE, collapsible = TRUE,
            tags$p(class = "text-muted", "ملخص إحصائي للمتغيرات: المتوسط، الانحراف المعياري، الحد الأدنى والأقصى."),
            DTOutput("desc_stats")
          )
        )
      ),

      # --- تبويب الإعدادات ---
      tabItem(
        tabName = "settings_tab",
        fluidRow(
          box(
            title = "أوزان نظام النقاط", width = 6, status = "warning",
            solidHeader = TRUE,
            tags$p(class = "text-muted", "حدد أهمية كل معيار في اختيار النموذج الأمثل. الوزن الأعلى يعني أهمية أكبر."),
            tags$div(
              title = "الأولوية القصوى: النموذج يجب أن يكون خالياً من الارتباط الذاتي في البواقي.",
              sliderInput("w_serial", "🔴 خلو من الارتباط الذاتي:", min = 0, max = 50, value = 30)
            ),
            tags$div(
              title = "معامل تصحيح الخطأ يجب أن يكون سالباً (بين -1 و 0) ومعنوياً إحصائياً.",
              sliderInput("w_ect", "🟢 ECT سالب ومعنوي:", min = 0, max = 50, value = 25)
            ),
            tags$div(
              title = "تباين البواقي يجب أن يكون متجانساً (Homoskedastic).",
              sliderInput("w_hetero", "🟡 خلو من عدم تجانس التباين:", min = 0, max = 30, value = 15)
            ),
            tags$div(
              title = "معاملات النموذج يجب أن تكون مستقرة عبر الزمن.",
              sliderInput("w_cusum", "🔵 استقرار النموذج (CUSUM):", min = 0, max = 30, value = 10)
            ),
            tags$div(
              title = "البواقي يجب أن تتبع التوزيع الطبيعي.",
              sliderInput("w_normality", "⚪ التوزيع الطبيعي:", min = 0, max = 20, value = 5)
            ),
            tags$div(
              title = "نسبة المتغيرات ذات المعنوية الإحصائية في النموذج.",
              sliderInput("w_signif", "🟣 المتغيرات المعنوية:", min = 0, max = 30, value = 15)
            )
          ),
          box(
            title = "ملخص الأوزان", width = 6, status = "success",
            solidHeader = TRUE,
            tags$p(class = "text-muted", "توزيع أوزان المعايير بشكل بياني."),
            plotlyOutput("weights_chart", height = "300px"),
            hr(),
            verbatimTextOutput("total_weight")
          )
        )
      ),

      # --- تبويب الاستقرارية ---
      tabItem(
        tabName = "stationarity_tab",
        fluidRow(
          box(
            title = "اختبارات جذر الوحدة", width = 12, status = "primary",
            solidHeader = TRUE,
            tags$p(class = "text-muted", "اختبارات ADF و PP و KPSS لتحديد رتبة التكامل لكل متغير. I(0) = مستقر، I(1) = يحتاج فرقاً أولاً."),
            DTOutput("unit_root_table")
          )
        ),
        fluidRow(
          box(
            title = "الرسوم البيانية للسلاسل الزمنية", width = 12, status = "info",
            solidHeader = TRUE, collapsible = TRUE,
            tags$p(class = "text-muted", "عرض بياني لتطور المتغيرات عبر الزمن."),
            plotlyOutput("ts_plots", height = "500px")
          )
        )
      ),

      # --- تبويب النماذج المرشحة ---
      tabItem(
        tabName = "models_tab",
        fluidRow(
          valueBoxOutput("best_model_box", width = 4),
          valueBoxOutput("best_score_box", width = 4),
          valueBoxOutput("models_tested_box", width = 4)
        ),
        fluidRow(
          box(
            title = "أفضل 10 نماذج مرشحة", width = 12, status = "success",
            solidHeader = TRUE,
            tags$p(class = "text-muted", "قائمة بأفضل النماذج مرتبة حسب النقاط. ✅ = اجتاز الاختبار، ❌ = فشل."),
            DTOutput("top_models_table")
          )
        ),
        fluidRow(
          box(
            title = "تفاصيل النموذج المختار", width = 6, status = "primary",
            solidHeader = TRUE,
            tags$p(class = "text-muted", "معلومات تفصيلية عن النموذج الأفضل."),
            uiOutput("selected_model_details")
          ),
          box(
            title = "معاملات الأجل الطويل والقصير", width = 6, status = "info",
            solidHeader = TRUE,
            tags$p(class = "text-muted", "تقديرات المعاملات مع الأخطاء المعيارية وقيم p."),
            DTOutput("coef_table")
          )
        )
      ),

      # --- تبويب التشخيصات ---
      tabItem(
        tabName = "diagnostics_tab",
        fluidRow(
          box(
            title = "نتائج الاختبارات التشخيصية", width = 12, status = "primary",
            solidHeader = TRUE,
            tags$p(class = "text-muted", "ملخص الاختبارات التشخيصية للنموذج المختار."),
            DTOutput("diagnostics_table")
          )
        ),
        fluidRow(
          box(
            title = "اختبار CUSUM", width = 6, status = "info",
            solidHeader = TRUE,
            tags$p(class = "text-muted", "اختبار استقرار المعاملات. الخط يجب أن يبقى داخل الحدود الحمراء."),
            plotOutput("cusum_plot", height = "350px")
          ),
          box(
            title = "اختبار CUSUM-SQ", width = 6, status = "info",
            solidHeader = TRUE,
            tags$p(class = "text-muted", "اختبار استقرار التباين. الخط يجب أن يبقى داخل الحدود الحمراء."),
            plotOutput("cusumsq_plot", height = "350px")
          )
        ),
        fluidRow(
          box(
            title = "Bounds Test", width = 12, status = "warning",
            solidHeader = TRUE,
            tags$p(class = "text-muted", "اختبار الحدود للتكامل المشترك. إذا كانت F أكبر من الحد الأعلى، يوجد تكامل مشترك."),
            verbatimTextOutput("bounds_test_result")
          )
        )
      ),

      # --- تبويب التصدير ---
      tabItem(
        tabName = "export_tab",
        fluidRow(
          box(
            title = "خيارات التصدير", width = 12, status = "success",
            solidHeader = TRUE,
            tags$p(class = "text-muted", "صدّر النتائج بالصيغة المناسبة لك."),
            fluidRow(
              column(
                3,
                h4("📊 جدول النتائج"),
                tags$p(class = "text-muted small", "تصدير جميع النماذج المختبرة"),
                downloadButton("download_excel", "تحميل Excel", class = "btn-success btn-block"),
                downloadButton("download_csv", "تحميل CSV", class = "btn-info btn-block")
              ),
              column(
                3,
                h4("📈 الرسوم البيانية"),
                tags$p(class = "text-muted small", "تصدير رسوم CUSUM والبواقي"),
                downloadButton("download_plots_png", "تحميل PNG", class = "btn-warning btn-block"),
                downloadButton("download_plots_pdf", "تحميل PDF", class = "btn-danger btn-block")
              ),
              column(
                3,
                h4("📄 التقرير"),
                tags$p(class = "text-muted small", "تقرير شامل بالنتائج"),
                downloadButton("download_report", "تحميل تقرير Word", class = "btn-primary btn-block")
              ),
              column(
                3,
                h4("💻 كود R"),
                tags$p(class = "text-muted small", "كود لإعادة إنتاج النتائج"),
                downloadButton("download_code", "تحميل كود R", class = "btn-secondary btn-block")
              )
            )
          )
        )
      ),

      # --- تبويب المساعدة ---
      tabItem(
        tabName = "help_tab",
        fluidRow(
          box(
            title = "دليل الاستخدام", width = 12, status = "info",
            solidHeader = TRUE,
            tags$h4("🎯 ما هو نموذج ARDL؟"),
            tags$p("نموذج الانحدار الذاتي للفجوات الموزعة (ARDL) هو أسلوب اقتصاد قياسي لدراسة العلاقات طويلة الأجل بين المتغيرات."),
            hr(),
            tags$h4("📋 خطوات الاستخدام:"),
            tags$ol(
              tags$li("ارفع ملف البيانات (Excel أو CSV)"),
              tags$li("اختر المتغير التابع والمتغيرات المستقلة"),
              tags$li("حدد الإعدادات (الفترات المتأخرة، مستوى المعنوية)"),
              tags$li("اضغط على 'تشغيل التحليل'"),
              tags$li("راجع النتائج في التبويبات المختلفة"),
              tags$li("صدّر النتائج بالصيغة المطلوبة")
            ),
            hr(),
            tags$h4("⭐ نظام النقاط:"),
            tags$p("يتم تقييم كل نموذج بناءً على:"),
            tags$ul(
              tags$li(tags$strong("الارتباط الذاتي (30 نقطة):"), " النموذج يجب أن يكون خالياً من الارتباط الذاتي"),
              tags$li(tags$strong("ECT (25 نقطة):"), " معامل تصحيح الخطأ يجب أن يكون سالباً ومعنوياً"),
              tags$li(tags$strong("التباين (15 نقطة):"), " التباين يجب أن يكون متجانساً"),
              tags$li(tags$strong("CUSUM (10 نقاط):"), " النموذج يجب أن يكون مستقراً"),
              tags$li(tags$strong("التوزيع (5 نقاط):"), " البواقي تتبع التوزيع الطبيعي"),
              tags$li(tags$strong("المعنوية (15 نقطة):"), " نسبة المتغيرات المعنوية")
            ),
            hr(),
            tags$div(
              style = "text-align: center; padding: 20px; background: #f5f5f5; border-radius: 10px;",
              tags$h4("© 2025 MUHAMMAD ALKHALAF"),
              tags$p("جميع الحقوق محفوظة"),
              tags$p(tags$a(href = "mailto:contact@example.com", "للتواصل والاستفسارات"))
            )
          )
        )
      )
    ),

    # Footer
    tags$div(
      class = "copyright-footer",
      HTML("© 2025 <strong>MUHAMMAD ALKHALAF</strong> | نظام ARDL لاختيار النموذج الأمثل | جميع الحقوق محفوظة")
    )
  )
)

# =============================================================================
# منطق الخادم (Server)
# =============================================================================

server <- function(input, output, session) {
  # --- التفاعلات ---
  rv <- reactiveValues(
    data = NULL,
    results = NULL,
    best_model = NULL,
    all_models = NULL
  )

  # --- قراءة البيانات ---
  observeEvent(input$datafile, {
    req(input$datafile)

    # التحقق من حجم الملف
    if (input$datafile$size > 10 * 1024 * 1024) {
      showNotification("⚠️ حجم الملف كبير جداً (الحد الأقصى 10 ميجابايت)", type = "error")
      return()
    }

    ext <- tools::file_ext(input$datafile$name)

    tryCatch(
      {
        if (ext %in% c("xlsx", "xls")) {
          rv$data <- read_excel(input$datafile$datapath)
        } else if (ext == "csv") {
          rv$data <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
        }

        # تحويل الأعمدة الرقمية
        rv$data <- rv$data %>%
          mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.))))

        showNotification("✅ تم تحميل البيانات بنجاح!", type = "message")
      },
      error = function(e) {
        showNotification(paste("❌ خطأ في قراءة الملف:", e$message), type = "error")
      }
    )
  })

  # --- اختيار المتغيرات ---
  output$var_selectors <- renderUI({
    req(rv$data)

    cols <- names(rv$data)

    tagList(
      tags$div(
        title = "اختر المتغير الذي تريد دراسة تأثير المتغيرات الأخرى عليه.",
        selectInput("dep_var", "🎯 المتغير التابع:",
          choices = cols, selected = cols[length(cols)]
        )
      ),
      tags$div(
        title = "اختر المتغيرات التي تعتقد أنها تؤثر على المتغير التابع.",
        checkboxGroupInput("indep_vars", "📌 المتغيرات المستقلة:",
          choices = cols, selected = cols[2:(length(cols) - 1)]
        )
      )
    )
  })

  # --- معاينة البيانات ---
  output$data_preview <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(pageLength = 10, scrollX = TRUE, language = list(
      search = "بحث:",
      lengthMenu = "عرض _MENU_ صفوف",
      info = "عرض _START_ إلى _END_ من _TOTAL_ صف"
    )))
  })

  # --- صناديق المعلومات ---
  output$obs_count <- renderInfoBox({
    req(rv$data)
    infoBox("المشاهدات", nrow(rv$data), icon = icon("list"), color = "blue")
  })

  output$var_count <- renderInfoBox({
    req(rv$data)
    infoBox("المتغيرات", ncol(rv$data), icon = icon("columns"), color = "green")
  })

  output$time_range <- renderInfoBox({
    req(rv$data)
    time_col <- rv$data[[1]]
    range_text <- paste(min(time_col, na.rm = TRUE), "-", max(time_col, na.rm = TRUE))
    infoBox("الفترة", range_text, icon = icon("calendar"), color = "yellow")
  })

  output$missing_count <- renderInfoBox({
    req(rv$data)
    missing <- sum(is.na(rv$data))
    infoBox("القيم المفقودة", missing,
      icon = icon("exclamation-triangle"),
      color = if (missing > 0) "red" else "green"
    )
  })

  # --- الإحصاءات الوصفية ---
  output$desc_stats <- renderDT({
    req(rv$data)

    stats <- rv$data %>%
      select(where(is.numeric)) %>%
      summarise(across(everything(), list(
        Mean = ~ mean(., na.rm = TRUE),
        SD = ~ sd(., na.rm = TRUE),
        Min = ~ min(., na.rm = TRUE),
        Max = ~ max(., na.rm = TRUE)
      ))) %>%
      pivot_longer(everything(), names_to = c("Variable", "Stat"), names_sep = "_") %>%
      pivot_wider(names_from = Stat, values_from = value)

    datatable(stats, options = list(pageLength = 20)) %>%
      formatRound(columns = 2:5, digits = 2)
  })

  # --- رسم الأوزان ---
  output$weights_chart <- renderPlotly({
    weights_df <- data.frame(
      Criterion = c("الارتباط الذاتي", "ECT", "التباين", "CUSUM", "التوزيع", "المعنوية"),
      Weight = c(
        input$w_serial, input$w_ect, input$w_hetero,
        input$w_cusum, input$w_normality, input$w_signif
      )
    )

    plot_ly(weights_df,
      x = ~Criterion, y = ~Weight, type = "bar",
      marker = list(color = c(
        "#dc3545", "#28a745", "#ffc107",
        "#17a2b8", "#6c757d", "#007bff"
      ))
    ) %>%
      layout(
        title = "توزيع أوزان المعايير",
        xaxis = list(title = ""),
        yaxis = list(title = "الوزن")
      )
  })

  output$total_weight <- renderText({
    total <- input$w_serial + input$w_ect + input$w_hetero +
      input$w_cusum + input$w_normality + input$w_signif
    paste("إجمالي الأوزان:", total, "نقطة")
  })

  # --- تشغيل التحليل مع شريط تقدم ---
  observeEvent(input$run_analysis, {
    req(rv$data, input$dep_var, input$indep_vars)

    # التحقق من صحة الإدخالات
    if (length(input$indep_vars) == 0) {
      showNotification("⚠️ يرجى اختيار متغير مستقل واحد على الأقل", type = "warning")
      return()
    }

    if (input$dep_var %in% input$indep_vars) {
      showNotification("⚠️ المتغير التابع لا يمكن أن يكون ضمن المتغيرات المستقلة", type = "warning")
      return()
    }

    withProgress(message = "جاري التحليل...", value = 0, {
      incProgress(0.1, detail = "تحضير البيانات...")

      tryCatch(
        {
          # تحضير البيانات
          analysis_data <- rv$data %>%
            select(all_of(c(input$dep_var, input$indep_vars))) %>%
            na.omit()

          # إضافة المتغير الوهمي
          if (input$use_dummy) {
            time_col <- rv$data[[1]]
            dummy_var <- ifelse(time_col >= input$break_year, 1, 0)
            analysis_data$DUMMY <- dummy_var[!is.na(rowSums(rv$data[, c(input$dep_var, input$indep_vars)]))]
          }

          incProgress(0.2, detail = "تقدير النماذج...")

          # تشغيل نظام تقييم النماذج
          weights <- c(
            serial = input$w_serial,
            ect = input$w_ect,
            hetero = input$w_hetero,
            cusum = input$w_cusum,
            normality = input$w_normality,
            signif = input$w_signif
          )

          rv$results <- evaluate_all_ardl_models(
            data = analysis_data,
            dep_var = input$dep_var,
            indep_vars = if (input$use_dummy) c(input$indep_vars, "DUMMY") else input$indep_vars,
            max_lags = input$max_lags,
            sig_level = as.numeric(input$sig_level),
            weights = weights,
            include_const = "const" %in% input$deterministics,
            include_trend = "trend" %in% input$deterministics
          )

          incProgress(0.9, detail = "إعداد النتائج...")

          rv$all_models <- rv$results$all_models
          rv$best_model <- rv$results$best_model

          incProgress(1, detail = "اكتمل!")

          showNotification(
            paste("✅ اكتمل التحليل! تم اختبار", rv$results$n_models, "نموذج"),
            type = "message", duration = 5
          )
          updateTabItems(session, "tabs", "models_tab")
        },
        error = function(e) {
          showNotification(paste("❌ خطأ:", e$message), type = "error", duration = 10)
        }
      )
    })
  })

  # --- جدول النماذج المرشحة ---
  output$top_models_table <- renderDT({
    req(rv$all_models)

    top_models <- rv$all_models %>%
      arrange(desc(total_score)) %>%
      head(10) %>%
      select(
        الترتيب = rank,
        النموذج = model_spec,
        النقاط = total_score,
        `الارتباط الذاتي` = serial_ok,
        ECT = ect_ok,
        التباين = hetero_ok,
        CUSUM = cusum_ok,
        AIC = aic,
        BIC = bic
      )

    datatable(top_models,
      options = list(pageLength = 10, scrollX = TRUE),
      selection = "single"
    ) %>%
      formatRound(columns = c("النقاط", "AIC", "BIC"), digits = 2) %>%
      formatStyle("النقاط",
        background = styleColorBar(range(top_models$النقاط), "#28a745"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # --- صناديق القيم ---
  output$best_model_box <- renderValueBox({
    req(rv$best_model)
    valueBox(
      rv$best_model$model_spec, "النموذج الأفضل",
      icon = icon("trophy"), color = "green"
    )
  })

  output$best_score_box <- renderValueBox({
    req(rv$best_model)
    valueBox(
      round(rv$best_model$total_score, 1), "النقاط",
      icon = icon("star"), color = "yellow"
    )
  })

  output$models_tested_box <- renderValueBox({
    req(rv$all_models)
    valueBox(
      nrow(rv$all_models), "النماذج المختبرة",
      icon = icon("flask"), color = "blue"
    )
  })

  # --- تفاصيل النموذج ---
  output$selected_model_details <- renderUI({
    req(rv$best_model)

    m <- rv$best_model

    tags$div(
      tags$h4("📋 ملخص النموذج"),
      tags$table(
        class = "table table-striped",
        tags$tr(tags$td("المواصفة:"), tags$td(tags$strong(m$model_spec))),
        tags$tr(tags$td("النقاط الإجمالية:"), tags$td(tags$strong(round(m$total_score, 2)))),
        tags$tr(tags$td("معامل ECT:"), tags$td(round(m$ect_coef, 4))),
        tags$tr(tags$td("p-value ECT:"), tags$td(format(m$ect_pvalue, scientific = TRUE, digits = 3))),
        tags$tr(tags$td("R²:"), tags$td(round(m$r_squared, 4))),
        tags$tr(tags$td("Adj R²:"), tags$td(round(m$adj_r_squared, 4)))
      )
    )
  })

  # --- جدول المعاملات ---
  output$coef_table <- renderDT({
    req(rv$best_model)

    coefs <- rv$best_model$coefficients

    datatable(coefs, options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatRound(columns = 2:4, digits = 4)
  })

  # --- اختبارات الاستقرارية ---
  output$unit_root_table <- renderDT({
    req(rv$data, input$dep_var, input$indep_vars)

    vars <- c(input$dep_var, input$indep_vars)
    results <- run_stationarity_tests(rv$data, vars)

    datatable(results, options = list(pageLength = 20, scrollX = TRUE)) %>%
      formatRound(columns = 3:4, digits = 4)
  })

  # --- الرسوم البيانية ---
  output$ts_plots <- renderPlotly({
    req(rv$data, input$dep_var, input$indep_vars)

    vars <- c(input$dep_var, input$indep_vars)
    time_col <- rv$data[[1]]

    plot_list <- lapply(vars, function(v) {
      plot_ly(
        x = time_col, y = rv$data[[v]], type = "scatter", mode = "lines",
        name = v
      ) %>%
        layout(xaxis = list(title = "الزمن"), yaxis = list(title = v))
    })

    subplot(plot_list, nrows = length(vars), shareX = TRUE)
  })

  # --- جدول التشخيصات ---
  output$diagnostics_table <- renderDT({
    req(rv$best_model)

    diag <- rv$best_model$diagnostics

    datatable(diag, options = list(pageLength = 10)) %>%
      formatRound(columns = c("Statistic", "P_Value"), digits = 4)
  })

  # --- رسم CUSUM ---
  output$cusum_plot <- renderPlot({
    req(rv$best_model)
    if (!is.null(rv$best_model$cusum_test)) {
      plot(rv$best_model$cusum_test, main = "CUSUM Test")
    }
  })

  output$cusumsq_plot <- renderPlot({
    req(rv$best_model)
    if (!is.null(rv$best_model$cusumsq_test)) {
      plot(rv$best_model$cusumsq_test, main = "CUSUM-SQ Test")
    }
  })

  # --- Bounds Test ---
  output$bounds_test_result <- renderPrint({
    req(rv$best_model)
    if (!is.null(rv$best_model$bounds_test)) {
      print(rv$best_model$bounds_test)
    } else {
      cat("غير متاح")
    }
  })

  # --- التصدير ---
  output$download_excel <- downloadHandler(
    filename = function() paste0("ARDL_Results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      export_to_excel(rv$results, file)
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() paste0("ARDL_Results_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(rv$all_models, file, row.names = FALSE)
    }
  )

  output$download_report <- downloadHandler(
    filename = function() paste0("ARDL_Report_", Sys.Date(), ".docx"),
    content = function(file) {
      export_word_report(rv$results, file)
    }
  )

  output$download_code <- downloadHandler(
    filename = function() paste0("ARDL_Code_", Sys.Date(), ".R"),
    content = function(file) {
      export_r_code(rv$results, input, file)
    }
  )
}

# =============================================================================
# تشغيل التطبيق
# =============================================================================
shinyApp(ui = ui, server = server)
