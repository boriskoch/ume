
# Statistics functions for validation, evaluation, and visualization
# Outlier detection ####

#' Outlier detection using multiple statistical tests
#'
#' This function computes an `out_score` for each value in a selected column.
#' The score increases when a value is flagged as an outlier by one or more tests:
#' IQR test, quantile cutoffs, and Hampel filter.
#'
#' @inheritParams main_docu
#' @param dt A `data.table` or `data.frame`.
#' @param check_col A character string naming the column to test for outliers.
#' @param verbose Logical; print summary statistics when TRUE.
#'
#' @return A `data.table` containing new columns: `out_score`, `out_box`,
#'   `out_quantile`, and `out_hampel`.
#'
#' @keywords misc
#'
#' @import data.table
#' @import stats
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom grDevices boxplot.stats
#'
#' @examples
#' ustats_outlier(mf_data_demo, check_col = "ppm")
#'
#' @export

ustats_outlier <- function(dt,
                           check_col = "ppm",
                           verbose = FALSE,
                           ...) {

  out_score <- out_box <- out_quantile <- out_hampel <- NULL
  Outlier <- Value <- out_rosner <- NULL

# https://statsandr.com/blog/outliers-detection-in-r/
# TO DO make sure that only one sample is considered
  dt <- data.table::data.table(dt) # use data.table
  mav <- dt[, get(check_col)] # mav: mass accuracy values

# Outlier via interquartile range IQR via boxplot function
  out <- grDevices::boxplot.stats(mav)$out # this is based on interquartile range IQR)

  dt[, out_score:=0]
  dt[(get(check_col) %in% out), out_box:="1"]
  dt[(get(check_col) %in% out), out_score:=1]

# Outlier via Quantile 2.5% / 97.5%
  lower_bound <- quantile(mav, 0.075)
  upper_bound <- quantile(mav, 0.975)

  out2 <- dt[!(get(check_col) %between% c(lower_bound, upper_bound)), get(check_col)]
  dt[(get(check_col) %in% out2), out_quantile:="1"]
  dt[(get(check_col) %in% out2), out_score:=out_score+1]

# Outlier via HAMPEL filter (MEDIAN plus/minus 3 median absolute deviations)
  lower_bound <- median(mav) - 3 * stats::mad(mav, constant = 1)
  upper_bound <- median(mav) + 3 * stats::mad(mav, constant = 1)

  out3 <- dt[!(get(check_col) %between% c(lower_bound, upper_bound)), get(check_col)]
  dt[(get(check_col) %in% out3), out_hampel:="1"]
  dt[(get(check_col) %in% out3), out_score:=out_score+1]

# Outlier via Rosner test
  # does not identify additional outlier (in addition to the other three tests)
  #out_count <- dt[out_score>0, .N]
  #test <- EnvStats::rosnerTest(mav, k = out_count)

# to do: funzt nicht. detects no outliers
  # test <- data.table(test$all.stats)
  # out4 <- test[Outlier == T, Value]
  # dt[(get(check_col) %in% out4), out_rosner:="1"]
  # dt[(get(check_col) %in% out4), out_score:=out_score+1]

  ds <- dt[out_score !=0, .N, .(out_score, out_box, out_quantile, out_hampel
                                #, out_rosner
                                )]

  if (isTRUE(verbose)) {
    .msg("Outlier summary:")
    print(ds)
  }

  # fig <- ggplot2::ggplot(dt[out_score<3, .(file_id, ppm)]) +
  #   ggplot2::aes(x="", y = dt[out_score<3, get(check_col)]) +
  #   ggplot2::geom_boxplot() +
  #   ggplot2::theme_minimal()
  #
  # fig
  # plotly::ggplotly(fig)

  return(dt)
}
