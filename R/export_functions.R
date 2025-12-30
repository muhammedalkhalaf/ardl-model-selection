# =============================================================================
# وظائف التصدير
# Export Functions Module
# =============================================================================

#' تصدير النتائج إلى Excel
#'
#' @param results نتائج التحليل
#' @param file مسار الملف
export_to_excel <- function(results, file) {
    sheets <- list(
        "النماذج المرشحة" = results$all_models,
        "معاملات النموذج الأفضل" = results$best_model$coefficients,
        "الاختبارات التشخيصية" = results$best_model$diagnostics
    )

    writexl::write_xlsx(sheets, file)
}

#' تصدير تقرير Word
#'
#' @param results نتائج التحليل
#' @param file مسار الملف
export_word_report <- function(results, file) {
    doc <- officer::read_docx()

    # العنوان
    doc <- doc %>%
        officer::body_add_par("تقرير تحليل ARDL", style = "heading 1") %>%
        officer::body_add_par(paste("تاريخ التقرير:", Sys.Date()), style = "Normal") %>%
        officer::body_add_par("", style = "Normal")

    # النموذج الأفضل
    doc <- doc %>%
        officer::body_add_par("النموذج الأفضل", style = "heading 2") %>%
        officer::body_add_par(paste("المواصفة:", results$best_model$model_spec), style = "Normal") %>%
        officer::body_add_par(paste("النقاط:", round(results$best_model$total_score, 2)), style = "Normal") %>%
        officer::body_add_par(paste("ECT:", round(results$best_model$ect_coef, 4)), style = "Normal") %>%
        officer::body_add_par(paste("R²:", round(results$best_model$r_squared, 4)), style = "Normal") %>%
        officer::body_add_par("", style = "Normal")

    # جدول المعاملات
    doc <- doc %>%
        officer::body_add_par("معاملات النموذج", style = "heading 2")

    coef_table <- flextable::flextable(results$best_model$coefficients)
    coef_table <- flextable::autofit(coef_table)
    doc <- flextable::body_add_flextable(doc, coef_table)

    doc <- doc %>%
        officer::body_add_par("", style = "Normal")

    # الاختبارات التشخيصية
    doc <- doc %>%
        officer::body_add_par("الاختبارات التشخيصية", style = "heading 2")

    diag_table <- flextable::flextable(results$best_model$diagnostics)
    diag_table <- flextable::autofit(diag_table)
    doc <- flextable::body_add_flextable(doc, diag_table)

    doc <- doc %>%
        officer::body_add_par("", style = "Normal")

    # أفضل 5 نماذج
    doc <- doc %>%
        officer::body_add_par("أفضل 5 نماذج مرشحة", style = "heading 2")

    top5 <- head(results$all_models, 5)[, c("rank", "model_spec", "total_score", "aic", "bic")]
    top5_table <- flextable::flextable(top5)
    top5_table <- flextable::autofit(top5_table)
    doc <- flextable::body_add_flextable(doc, top5_table)

    # حفظ
    print(doc, target = file)
}

#' تصدير كود R قابل لإعادة الإنتاج
#'
#' @param results نتائج التحليل
#' @param input إدخالات المستخدم
#' @param file مسار الملف
export_r_code <- function(results, input, file) {
    code <- paste0(
        "# =============================================================================\n",
        "# كود R لإعادة إنتاج نموذج ARDL\n",
        "# تم إنشاؤه بواسطة: تطبيق ARDL لاختيار النموذج الأمثل\n",
        "# التاريخ: ", Sys.Date(), "\n",
        "# =============================================================================\n\n",
        "# تحميل الحزم\n",
        "library(ARDL)\n",
        "library(lmtest)\n",
        "library(strucchange)\n\n",
        "# قراءة البيانات\n",
        "data <- read.csv('your_data.csv')  # غير المسار حسب ملفك\n\n",
        "# تقدير النموذج الأمثل\n",
        "# المواصفة: ", results$best_model$model_spec, "\n\n",
        "model <- ardl(\n",
        "  formula = ", input$dep_var, " ~ ", paste(input$indep_vars, collapse = " + "), ",\n",
        "  data = data,\n",
        "  order = c(", paste(results$best_model$lags, collapse = ", "), ")\n",
        ")\n\n",
        "# ملخص النموذج\n",
        "summary(model)\n\n",
        "# نموذج تصحيح الخطأ\n",
        "ecm <- uecm(model)\n",
        "summary(ecm)\n\n",
        "# Bounds Test\n",
        "bounds_f_test(model, case = 3)\n\n",
        "# الاختبارات التشخيصية\n",
        "# الارتباط الذاتي\n",
        "bgtest(model, order = 2)\n\n",
        "# عدم تجانس التباين\n",
        "bptest(model)\n\n",
        "# CUSUM\n",
        "cusum <- efp(model, type = 'OLS-CUSUM')\n",
        "plot(cusum)\n\n",
        "# CUSUM-SQ\n",
        "cusumsq <- efp(model, type = 'OLS-CUSUMSQ')\n",
        "plot(cusumsq)\n"
    )

    writeLines(code, file)
}

#' تصدير الرسوم البيانية
#'
#' @param results نتائج التحليل
#' @param file مسار الملف
#' @param format صيغة الملف (png/pdf)
export_plots <- function(results, file, format = "png") {
    if (format == "png") {
        png(file, width = 1200, height = 800, res = 150)
    } else {
        pdf(file, width = 12, height = 8)
    }

    par(mfrow = c(2, 2))

    # CUSUM
    if (!is.null(results$best_model$cusum_test)) {
        plot(results$best_model$cusum_test, main = "CUSUM Test")
    }

    # CUSUM-SQ
    if (!is.null(results$best_model$cusumsq_test)) {
        plot(results$best_model$cusumsq_test, main = "CUSUM-SQ Test")
    }

    # البواقي
    if (!is.null(results$best_model$model)) {
        resid <- residuals(results$best_model$model)

        # Histogram
        hist(resid,
            main = "Residuals Distribution",
            xlab = "Residuals", col = "lightblue", border = "white"
        )

        # ACF
        acf(resid, main = "ACF of Residuals")
    }

    dev.off()
}
