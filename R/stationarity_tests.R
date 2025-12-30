# =============================================================================
# اختبارات الاستقرارية (جذر الوحدة)
# Stationarity Tests Module
# =============================================================================

#' تشغيل اختبارات الاستقرارية لجميع المتغيرات
#'
#' @param data البيانات
#' @param variables قائمة المتغيرات
run_stationarity_tests <- function(data, variables) {
    results_list <- list()

    for (var in variables) {
        y <- na.omit(data[[var]])

        if (length(y) < 10) next

        # ADF Test - مع ثابت
        adf_const <- tryCatch(
            {
                urca::ur.df(y, type = "drift", lags = 4, selectlags = "AIC")
            },
            error = function(e) NULL
        )

        # ADF Test - مع ثابت واتجاه
        adf_trend <- tryCatch(
            {
                urca::ur.df(y, type = "trend", lags = 4, selectlags = "AIC")
            },
            error = function(e) NULL
        )

        # ADF Test - بدون ثابت
        adf_none <- tryCatch(
            {
                urca::ur.df(y, type = "none", lags = 4, selectlags = "AIC")
            },
            error = function(e) NULL
        )

        # PP Test
        pp_test <- tryCatch(
            {
                urca::ur.pp(y, type = "Z-tau", model = "trend")
            },
            error = function(e) NULL
        )

        # KPSS Test
        kpss_test <- tryCatch(
            {
                urca::ur.kpss(y, type = "tau")
            },
            error = function(e) NULL
        )

        # استخراج النتائج
        # ADF with constant
        if (!is.null(adf_const)) {
            stat <- adf_const@teststat[1]
            cv5 <- adf_const@cval[1, 2]
            results_list[[length(results_list) + 1]] <- data.frame(
                Variable = var,
                Test = "ADF (Constant)",
                Statistic = round(stat, 4),
                Critical_5pct = round(cv5, 4),
                Result = ifelse(stat < cv5, "I(0) ✅", "I(1) ❌"),
                stringsAsFactors = FALSE
            )
        }

        # ADF with trend
        if (!is.null(adf_trend)) {
            stat <- adf_trend@teststat[1]
            cv5 <- adf_trend@cval[1, 2]
            results_list[[length(results_list) + 1]] <- data.frame(
                Variable = var,
                Test = "ADF (Trend)",
                Statistic = round(stat, 4),
                Critical_5pct = round(cv5, 4),
                Result = ifelse(stat < cv5, "I(0) ✅", "I(1) ❌"),
                stringsAsFactors = FALSE
            )
        }

        # ADF none
        if (!is.null(adf_none)) {
            stat <- adf_none@teststat[1]
            cv5 <- adf_none@cval[1, 2]
            results_list[[length(results_list) + 1]] <- data.frame(
                Variable = var,
                Test = "ADF (None)",
                Statistic = round(stat, 4),
                Critical_5pct = round(cv5, 4),
                Result = ifelse(stat < cv5, "I(0) ✅", "I(1) ❌"),
                stringsAsFactors = FALSE
            )
        }

        # PP Test
        if (!is.null(pp_test)) {
            stat <- pp_test@teststat[1]
            cv5 <- pp_test@cval[1, 2]
            results_list[[length(results_list) + 1]] <- data.frame(
                Variable = var,
                Test = "PP",
                Statistic = round(stat, 4),
                Critical_5pct = round(cv5, 4),
                Result = ifelse(stat < cv5, "I(0) ✅", "I(1) ❌"),
                stringsAsFactors = FALSE
            )
        }

        # KPSS Test (null = stationary)
        if (!is.null(kpss_test)) {
            stat <- kpss_test@teststat[1]
            cv5 <- kpss_test@cval[1, 2]
            results_list[[length(results_list) + 1]] <- data.frame(
                Variable = var,
                Test = "KPSS",
                Statistic = round(stat, 4),
                Critical_5pct = round(cv5, 4),
                Result = ifelse(stat < cv5, "I(0) ✅", "I(1) ❌"), # KPSS: reject = non-stationary
                stringsAsFactors = FALSE
            )
        }

        # First Difference Tests
        dy <- diff(y)

        adf_diff <- tryCatch(
            {
                urca::ur.df(dy, type = "drift", lags = 4, selectlags = "AIC")
            },
            error = function(e) NULL
        )

        if (!is.null(adf_diff)) {
            stat <- adf_diff@teststat[1]
            cv5 <- adf_diff@cval[1, 2]
            results_list[[length(results_list) + 1]] <- data.frame(
                Variable = paste0("Δ", var),
                Test = "ADF (Constant)",
                Statistic = round(stat, 4),
                Critical_5pct = round(cv5, 4),
                Result = ifelse(stat < cv5, "I(0) ✅", "I(1) ❌"),
                stringsAsFactors = FALSE
            )
        }
    }

    # دمج النتائج
    results_df <- do.call(rbind, results_list)

    return(results_df)
}

#' تحديد رتبة التكامل
determine_integration_order <- function(data, variable) {
    y <- na.omit(data[[variable]])

    # اختبار المستوى
    adf_level <- tryCatch(
        {
            urca::ur.df(y, type = "drift", lags = 4, selectlags = "AIC")
        },
        error = function(e) NULL
    )

    if (!is.null(adf_level)) {
        if (adf_level@teststat[1] < adf_level@cval[1, 2]) {
            return(0) # I(0)
        }
    }

    # اختبار الفرق الأول
    dy <- diff(y)
    adf_diff <- tryCatch(
        {
            urca::ur.df(dy, type = "drift", lags = 4, selectlags = "AIC")
        },
        error = function(e) NULL
    )

    if (!is.null(adf_diff)) {
        if (adf_diff@teststat[1] < adf_diff@cval[1, 2]) {
            return(1) # I(1)
        }
    }

    # اختبار الفرق الثاني
    d2y <- diff(dy)
    adf_diff2 <- tryCatch(
        {
            urca::ur.df(d2y, type = "drift", lags = 4, selectlags = "AIC")
        },
        error = function(e) NULL
    )

    if (!is.null(adf_diff2)) {
        if (adf_diff2@teststat[1] < adf_diff2@cval[1, 2]) {
            return(2) # I(2)
        }
    }

    return(NA)
}
