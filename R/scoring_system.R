# =============================================================================
# نظام النقاط لتقييم نماذج ARDL
# Scoring System for ARDL Model Evaluation
# =============================================================================

#' تقييم جميع توليفات ARDL
#'
#' @param data البيانات
#' @param dep_var المتغير التابع
#' @param indep_vars المتغيرات المستقلة
#' @param max_lags الحد الأقصى للفترات المتأخرة
#' @param sig_level مستوى المعنوية
#' @param weights أوزان المعايير
#' @param include_const تضمين الثابت
#' @param include_trend تضمين الاتجاه
evaluate_all_ardl_models <- function(data, dep_var, indep_vars, max_lags = 4,
                                     sig_level = 0.05, weights,
                                     include_const = TRUE, include_trend = FALSE) {
    # توليد جميع التوليفات الممكنة
    n_vars <- length(indep_vars) + 1 # +1 للمتغير التابع
    lag_combinations <- generate_lag_combinations(n_vars, max_lags)

    # دالة تقدير نموذج واحد (للمعالجة المتوازية)
    estimate_one <- function(i) {
        lags <- as.numeric(lag_combinations[i, ])
        tryCatch(
            {
                estimate_single_ardl(
                    data = data,
                    dep_var = dep_var,
                    indep_vars = indep_vars,
                    lags = lags,
                    include_const = include_const,
                    include_trend = include_trend,
                    sig_level = sig_level,
                    weights = weights
                )
            },
            error = function(e) NULL
        )
    }

    # تقدير جميع النماذج (مع إظهار التقدم)
    n_models <- nrow(lag_combinations)
    all_results <- vector("list", n_models)

    for (i in seq_len(n_models)) {
        all_results[[i]] <- estimate_one(i)
    }

    # إزالة النماذج الفاشلة
    all_results <- Filter(Negate(is.null), all_results)

    # تحويل إلى data.frame
    all_models_df <- do.call(rbind, lapply(all_results, function(x) {
        data.frame(
            model_spec = x$model_spec,
            total_score = x$total_score,
            serial_ok = x$diagnostics_summary$serial_ok,
            ect_ok = x$diagnostics_summary$ect_ok,
            hetero_ok = x$diagnostics_summary$hetero_ok,
            cusum_ok = x$diagnostics_summary$cusum_ok,
            normality_ok = x$diagnostics_summary$normality_ok,
            signif_count = x$diagnostics_summary$signif_count,
            ect_coef = x$ect_coef,
            ect_pvalue = x$ect_pvalue,
            r_squared = x$r_squared,
            adj_r_squared = x$adj_r_squared,
            aic = x$aic,
            bic = x$bic,
            stringsAsFactors = FALSE
        )
    }))

    # ترتيب حسب النقاط
    all_models_df <- all_models_df[order(-all_models_df$total_score), ]
    all_models_df$rank <- seq_len(nrow(all_models_df))

    # النموذج الأفضل
    best_idx <- which.max(sapply(all_results, function(x) x$total_score))
    best_model <- all_results[[best_idx]]

    return(list(
        all_models = all_models_df,
        best_model = best_model,
        n_models = length(all_results)
    ))
}

#' توليد جميع توليفات الفترات المتأخرة
generate_lag_combinations <- function(n_vars, max_lags) {
    lags_list <- rep(list(0:max_lags), n_vars)
    combinations <- expand.grid(lags_list)

    # إزالة الحالة التي تكون فيها جميع الفترات = 0
    combinations <- combinations[rowSums(combinations) > 0, ]

    return(combinations)
}

#' تقدير نموذج ARDL واحد
estimate_single_ardl <- function(data, dep_var, indep_vars, lags,
                                 include_const, include_trend, sig_level, weights) {
    # بناء صيغة النموذج
    model_spec <- paste0("ARDL(", paste(lags, collapse = ","), ")")

    # تجهيز البيانات
    y <- data[[dep_var]]
    X <- as.matrix(data[, indep_vars, drop = FALSE])

    # تقدير النموذج باستخدام حزمة ARDL
    order_vec <- lags
    names(order_vec) <- c(dep_var, indep_vars)

    # محاولة تقدير النموذج
    ardl_model <- tryCatch(
        {
            ARDL::ardl(
                formula = as.formula(paste(dep_var, "~", paste(indep_vars, collapse = " + "))),
                data = as.data.frame(data),
                order = order_vec
            )
        },
        error = function(e) NULL
    )

    if (is.null(ardl_model)) {
        return(NULL)
    }

    # استخراج المعلومات
    model_summary <- summary(ardl_model)

    # حساب ECM
    ecm_model <- tryCatch(
        {
            ARDL::uecm(ardl_model)
        },
        error = function(e) NULL
    )

    # استخراج معامل ECT
    if (!is.null(ecm_model)) {
        ecm_summary <- summary(ecm_model)
        coef_names <- rownames(ecm_summary$coefficients)

        # البحث عن معامل تصحيح الخطأ
        ect_idx <- grep("^L\\(", coef_names)[1]
        if (!is.na(ect_idx)) {
            ect_coef <- ecm_summary$coefficients[ect_idx, 1]
            ect_pvalue <- ecm_summary$coefficients[ect_idx, 4]
        } else {
            ect_coef <- NA
            ect_pvalue <- NA
        }
    } else {
        ect_coef <- NA
        ect_pvalue <- NA
    }

    # الاختبارات التشخيصية
    diagnostics <- run_all_diagnostics(ardl_model, sig_level)

    # حساب النقاط
    score_result <- calculate_model_score(
        diagnostics = diagnostics,
        ect_coef = ect_coef,
        ect_pvalue = ect_pvalue,
        model_summary = model_summary,
        sig_level = sig_level,
        weights = weights
    )

    # المعاملات
    coef_df <- data.frame(
        Variable = rownames(model_summary$coefficients),
        Estimate = model_summary$coefficients[, 1],
        Std_Error = model_summary$coefficients[, 2],
        t_value = model_summary$coefficients[, 3],
        p_value = model_summary$coefficients[, 4]
    )

    return(list(
        model_spec = model_spec,
        model = ardl_model,
        ecm_model = ecm_model,
        total_score = score_result$total_score,
        diagnostics_summary = score_result$summary,
        diagnostics = diagnostics$table,
        coefficients = coef_df,
        ect_coef = ect_coef,
        ect_pvalue = ect_pvalue,
        r_squared = model_summary$r.squared,
        adj_r_squared = model_summary$adj.r.squared,
        aic = AIC(ardl_model),
        bic = BIC(ardl_model),
        cusum_test = diagnostics$cusum,
        cusumsq_test = diagnostics$cusumsq,
        bounds_test = diagnostics$bounds_test
    ))
}

#' حساب نقاط النموذج
calculate_model_score <- function(diagnostics, ect_coef, ect_pvalue,
                                  model_summary, sig_level, weights) {
    score <- 0
    summary_list <- list()

    # 1. الارتباط الذاتي (الأولوية القصوى)
    serial_ok <- diagnostics$serial_pvalue > sig_level
    if (serial_ok) score <- score + weights["serial"]
    summary_list$serial_ok <- ifelse(serial_ok, "✅", "❌")

    # 2. ECT سالب ومعنوي
    ect_ok <- !is.na(ect_coef) && ect_coef < 0 && ect_coef > -1 &&
        !is.na(ect_pvalue) && ect_pvalue < sig_level
    if (ect_ok) score <- score + weights["ect"]
    summary_list$ect_ok <- ifelse(ect_ok, "✅", "❌")

    # 3. عدم تجانس التباين
    hetero_ok <- diagnostics$hetero_pvalue > sig_level
    if (hetero_ok) score <- score + weights["hetero"]
    summary_list$hetero_ok <- ifelse(hetero_ok, "✅", "❌")

    # 4. استقرار CUSUM
    cusum_ok <- diagnostics$cusum_stable
    if (cusum_ok) score <- score + weights["cusum"]
    summary_list$cusum_ok <- ifelse(cusum_ok, "✅", "❌")

    # 5. التوزيع الطبيعي
    normality_ok <- diagnostics$normality_pvalue > sig_level
    if (normality_ok) score <- score + weights["normality"]
    summary_list$normality_ok <- ifelse(normality_ok, "✅", "❌")

    # 6. المتغيرات المعنوية (نسبة من الوزن)
    coefs <- model_summary$coefficients
    n_signif <- sum(coefs[, 4] < sig_level, na.rm = TRUE)
    n_total <- nrow(coefs)
    signif_ratio <- n_signif / n_total
    score <- score + (signif_ratio * weights["signif"])
    summary_list$signif_count <- n_signif

    return(list(
        total_score = score,
        summary = summary_list
    ))
}
