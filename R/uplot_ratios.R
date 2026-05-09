#' Ratio Plot in Van Krevelen Space
#'
#' @title Molecular Formula Ratio Plot (Sample vs Control)
#' @name uplot_ratios
#' @family uplots
#'
#' @description
#' Computes the intensity ratio between a sample and a control group and visualizes
#' it in a Van Krevelen diagram. Optionally highlights unique molecular formulas
#' and plots the ratio distribution.
#'
#' @inheritParams main_docu
#'
#' @param df A data.table containing at least columns:
#'        `mf`, `oc`, `hc`, grouping variable `grp`, and intensity column `int_col`.
#' @param upper,lower Ratio filtering limits (default 90 / -90)
#' @param grp Column defining sample/control grouping
#' @param int_col Intensity column to use
#' @param control Character: control group name
#' @param sample Character: sample group name
#' @param uniques Logical: highlight uniquely present formulas
#' @param conservative Logical: stricter uniqueness definition
#' @param palname Color palette for projection
#' @param distrib Logical: include ratio distribution plot
#' @param main Optional main title
#'
#' @return A list with:
#'   - `ratio_table`
#'   - `plot_ratio_vk`
#'   - `plot_ratio_distr`
#'
# @examples
# out <- uplot_ratios(
#   df = mf_data_demo,
#   grp = "file",
#   control = "Nsea_a",
#   sample  = "Fjord 01a"
# )
#'
#' @import data.table
#' @import ggplot2
#' @importFrom plotly ggplotly
#'
#' @export
uplot_ratios <- function(df,
                         upper = 90,
                         lower = -90,
                         grp = "file_id",
                         int_col = "norm_int",
                         control,
                         sample,
                         uniques = FALSE,
                         conservative = FALSE,
                         palname = "ratios",
                         distrib = TRUE,
                         main = NA,
                         ...) {

  ratio <- int_sample <- int_control <- idx <- NULL
  n_sample <- n_control <- is_unique_sample <- is_unique_control <- NULL

  df <- as.data.table(df)

  #-----------------------------
  # 1. Input validation
  #-----------------------------
  required_cols <- c("mf", "oc", "hc", int_col, grp)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!control %in% df[[grp]])
    stop("Control group '", control, "' not found in ", grp)

  if (!sample %in% df[[grp]])
    stop("Sample group '", sample, "' not found in ", grp)

  #-----------------------------
  # 2. Pivot to sample/control table
  #-----------------------------
  df2 <- df[get(grp) %in% c(control, sample)]

  dfm <- dcast(
    df2,
    mf + oc + hc ~ get(grp),
    value.var = int_col,
    fun.aggregate = mean,
    fill = 0
  )

  # SAFELY rename only the last two columns
  setnames(dfm,
           old = names(dfm)[(ncol(dfm)-1):ncol(dfm)],
           new = c("int_control", "int_sample"))

  #-----------------------------
  # 3. Ratio calculation
  #-----------------------------
  dfm[, ratio := ((int_sample / (int_sample + int_control)) - 0.5) * 200]
  dfm <- dfm[is.finite(ratio)]
  dfm <- dfm[between(ratio, lower, upper)]

  if (is.na(main)) {
    main <- paste0("Molecular formula ratios:\nint_", sample,
                   " / (int_", sample, " + int_", control, ")")
  }

  #-----------------------------
  # 4. Unique MF detection
  #-----------------------------
  if (uniques) {

    df_count <- dcast(
      df,
      mf + oc + hc ~ get(grp),
      value.var = int_col,
      fun.aggregate = length,
      fill = 0
    )

    setnames(df_count,
             old = names(df_count)[(ncol(df_count)-1):ncol(df_count)],
             new = c("n_control", "n_sample"))

    uniques_sample  <- df_count[n_sample  > 0 & n_control == 0]
    uniques_control <- df_count[n_sample == 0 & n_control > 0]

    dfm[, is_unique_sample  := mf %in% uniques_sample$mf]
    dfm[, is_unique_control := mf %in% uniques_control$mf]
  }

  #-----------------------------
  # 5. Ratio distribution plot
  #-----------------------------
  plot_ratio_distr <- NULL
  if (distrib) {
    df_sorted <- dfm[order(ratio)]
    df_sorted[, idx := seq_len(.N)]

    plot_ratio_distr <- ggplot(df_sorted, aes(x = idx, y = ratio)) +
      geom_line() +
      labs(x = "Formula index", y = "Ratio", title = main) +
      theme_bw(base_size = 14)
  }


  #-----------------------------
  # 6. Van Krevelen projection
  #-----------------------------
  plot_ratio_vk <- uplot_vk(
    mfd   = dfm,
    z_var = "ratio",
    main  = main,
    ...
  )

  if (uniques) {
    plot_ratio_vk <- plot_ratio_vk +
      geom_point(
        data = dfm[is_unique_sample == TRUE],
        aes(oc, hc),
        shape = 21, size = 3,
        fill = "#1f77b4", colour = "black"
      ) +
      geom_point(
        data = dfm[is_unique_control == TRUE],
        aes(oc, hc),
        shape = 4, size = 4,
        colour = "red", stroke = 1.5
      )
  }


  #-----------------------------
  # 8. Return results
  #-----------------------------
  return(list(
    ratio_table      = dfm,
    plot_ratio_vk    = plot_ratio_vk,
    plot_ratio_distr = plot_ratio_distr
  ))
}
