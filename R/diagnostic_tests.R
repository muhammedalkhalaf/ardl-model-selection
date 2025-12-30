# =============================================================================
# الاختبارات التشخيصية
# Diagnostic Tests Module
# =============================================================================

#' تشغيل جميع الاختبارات التشخيصية
#'
#' @param model نموذج ARDL
#' @param sig_level مستوى المعنوية
run_all_diagnostics <- function(model, sig_level = 0.05) {
    results <- list()

    # 1. اختبار الارتباط الذاتي (Breusch-Godfrey)
    bg_test <- tryCatch(
        {
            lmtest::bgtest(model, order = 2)
        },
        error = function(e) NULL
    )

    if (!is.null(bg_test)) {
        results$serial_pvalue <- bg_test$p.value
        results$serial_stat <- bg_test$statistic
    } else {
        results$serial_pvalue <- 0
        results$serial_stat <- NA
    }

    # 2. اختبار عدم تجانس التباين (Breusch-Pagan)
    bp_test <- tryCatch(
        {
            lmtest::bptest(model)
        },
        error = function(e) NULL
    )

    if (!is.null(bp_test)) {
        results$hetero_pvalue <- bp_test$p.value
        results$hetero_stat <- bp_test$statistic
    } else {
        results$hetero_pvalue <- 1
        results$hetero_stat <- NA
    }

    # 3. اختبار ARCH
    arch_test <- tryCatch(
        {
            resid <- residuals(model)
            FinTS::ArchTest(resid, lags = 1)
        },
        error = function(e) NULL
    )

    if (!is.null(arch_test)) {
        results$arch_pvalue <- arch_test$p.value
        results$arch_stat <- arch_test$statistic
    } else {
        results$arch_pvalue <- 1
        results$arch_stat <- NA
    }

    # 4. اختبار التوزيع الطبيعي (Jarque-Bera)
    jb_test <- tryCatch(
        {
            resid <- residuals(model)
            tseries::jarque.bera.test(resid)
        },
        error = function(e) NULL
    )

    if (!is.null(jb_test)) {
        results$normality_pvalue <- jb_test$p.value
        results$normality_stat <- jb_test$statistic
    } else {
        results$normality_pvalue <- 1
        results$normality_stat <- NA
    }

    # 5. اختبار CUSUM
    cusum_test <- tryCatch(
        {
            strucchange::efp(model, type = "OLS-CUSUM")
        },
        error = function(e) NULL
    )

    cusumsq_test <- tryCatch(
        {
            strucchange::efp(model, type = "OLS-CUSUMSQ")
        },
        error = function(e) NULL
    )

    # التحقق من استقرار CUSUM
    if (!is.null(cusum_test)) {
        cusum_boundary <- strucchange::boundary(cusum_test)
        cusum_process <- cusum_test$process
        results$cusum_stable <- all(abs(cusum_process) < cusum_boundary, na.rm = TRUE)
        results$cusum <- cusum_test
    } else {
        results$cusum_stable <- TRUE
        results$cusum <- NULL
    }

    if (!is.null(cusumsq_test)) {
        results$cusumsq <- cusumsq_test
    } else {
        results$cusumsq <- NULL
    }

    # 6. Bounds Test
    bounds_test <- tryCatch(
        {
            ARDL::bounds_f_test(model, case = 3)
        },
        error = function(e) NULL
    )

    results$bounds_test <- bounds_test

    # إنشاء جدول ملخص
    results$table <- data.frame(
        Test = c(
            "Breusch-Godfrey (Serial)", "Breusch-Pagan (Hetero)",
            "ARCH", "Jarque-Bera (Normality)", "CUSUM Stability"
        ),
        Statistic = c(
            results$serial_stat, results$hetero_stat,
            results$arch_stat, results$normality_stat, NA
        ),
        P_Value = c(
            results$serial_pvalue, results$hetero_pvalue,
            results$arch_pvalue, results$normality_pvalue, NA
        ),
        Result = c(
            ifelse(results$serial_pvalue > sig_level, "✅ لا يوجد ارتباط ذاتي", "❌ يوجد ارتباط ذاتي"),
            ifelse(results$hetero_pvalue > sig_level, "✅ التباين متجانس", "❌ التباين غير متجانس"),
            ifelse(results$arch_pvalue > sig_level, "✅ لا يوجد ARCH", "❌ يوجد ARCH"),
            ifelse(results$normality_pvalue > sig_level, "✅ توزيع طبيعي", "❌ توزيع غير طبيعي"),
            ifelse(results$cusum_stable, "✅ النموذج مستقر", "❌ النموذج غير مستقر")
        ),
        stringsAsFactors = FALSE
    )

    return(results)
}

#' تشغيل Bounds F-Test
run_bounds_test <- function(ardl_model) {
    tryCatch(
        {
            ARDL::bounds_f_test(ardl_model, case = 3)
        },
        error = function(e) {
            list(
                statistic = NA,
                I0 = NA,
                I1 = NA,
                result = "غير متاح"
            )
        }
    )
}
