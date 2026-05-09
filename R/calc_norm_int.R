#' @title Calculate Normalized Peak Intensities
#' @name calc_norm_int
#' @family calculations
#' @description
#' Computes normalized peak intensities for a molecular formula dataset and adds the results
#' as additional columns to the input `data.table` (`mfd`). It also calculates:
#'
#' * the number of molecular formula assignments per peak (`n_assignments`)
#' * the total occurrences of each formula across the dataset (`n_occurrence`)
#'
#' Normalized intensities are stored in a new column `norm_int`, and the reference
#' intensity used for normalization is stored in `int_ref`.
#'
#' Supported normalization methods:
#'
#' * `"none"` – no normalization; raw peak intensities are copied to `norm_int`
#' * `"bp"` – normalized to the base peak intensity per spectrum
#' * `"sum"` – normalized by the total sum of intensities per spectrum
#' * `"sum_ubiq"` – normalized by the sum of intensities of ubiquitous peaks across the dataset
#' * `"sum_rank"` – normalized by the sum of the top `n_rank` most intense peaks per spectrum
#' * `"euc"` – Euclidean normalization (optional, not implemented in current version)
#'
#' @inheritParams main_docu
#' @param ms_id Character; name of the column identifying individual spectra (default: `"file_id"`).
#' @param peak_id Character; name of the column identifying unique peaks (default: `"peak_id"`).
#' @param peak_magnitude Character; name of the column containing peak intensity values (default: `"i_magnitude"`).
#' @param normalization Character; normalization method to apply. One of `"bp"`, `"sum"`, `"sum_ubiq"`, `"sum_rank"`, `"none"`. Default is `"bp"`.
#' @param n_rank Integer; number of top-ranked peaks to use for `"sum_rank"` normalization (default: 200).
#' @param ... Additional arguments (currently unused).
#'
#' @return A `data.table` identical to `mfd` but with additional columns:
#' \describe{
#'   \item{norm_int}{Normalized peak intensity based on selected method.}
#'   \item{int_ref}{Reference intensity used for normalization (e.g., sum, base peak).}
#'   \item{n_assignments}{Number of formula assignments per peak (calculated internally).}
#'   \item{n_occurrence}{Number of occurrences of each formula across all spectra (calculated internally).}
#' }
#'
#' @examples
#' mfd_norm <- calc_norm_int(
#'   mfd = mf_data_demo,
#'   normalization = "sum_ubiq"
#' )
#' @export

calc_norm_int <- function(mfd,
                          ms_id = "file_id",
                          peak_id = "peak_id",
                          peak_magnitude = "i_magnitude",
                          normalization = c("bp", "sum", "sum_ubiq", "sum_rank", "none"),
                          n_rank=200,
                          verbose = FALSE,
                          ...)
{

# Evaluate choices of arguments
  normalization <- match.arg(normalization)

  norm_int <- int_ref <- LCMS <- NULL

  isotope_info <- get_isotope_info(mfd = mfd)

  if("ret_time_min" %in% names(mfd)){
    LCMS <- TRUE
    grp_cols <- c("ret_time_min", c(isotope_info[,label]))
  } else {
    LCMS <- FALSE
    grp_cols <- c(isotope_info[,label])
    }

  .msg("Normalizing dataset by '%s'.", normalization)

# Re-calculate the number of occurrences of a molecular formula in the dataset
  .msg("Recalculating the number of occurrences of a formula in the entire dataset.")
  mfd[, n_occurrence := .N, by = c(isotope_info[,label], grp_cols)]

# Remove existing normalized intensities
  rem_cols <- names(mfd)[names(mfd) %in% c("norm_int", "int_ref")]
  if(length(rem_cols)>0) mfd[, (rem_cols):=NULL]

  if(mfd[,.N]>0){

  # If i_magnitude is selected, magnitude values will be copied into norm_int
    if (normalization=="none"){
      mfd[, int_ref:=get(peak_magnitude)]
      mfd[, norm_int:=get(peak_magnitude)]
    }

  # Calculate relative intensity: base peak magnitude
    if (normalization=="bp"){
      mfd[, int_ref:=max(i_magnitude), ms_id]
      mfd[, norm_int:=i_magnitude*100/int_ref, ms_id]
    }

  # Calculate relative intensity: sum of total intensity
    if (normalization=="sum"){
      mfd[, int_ref:=sum(i_magnitude), ms_id]
      mfd[, norm_int:=i_magnitude*100/int_ref, ms_id]
    }

  # Calculate relative intensity: sum of intensity for ubiquitous peaks
    if (normalization=="sum_ubiq"){
    # Assign sum of magnitudes for ubiquitous peaks
      ref_int_table <- mfd[n_occurrence==max(n_occurrence), .(int_ref=sum(i_magnitude)), ms_id]
      mfd <- ref_int_table[mfd, on = ms_id]

    # Calculate rel. intensities for all peaks based on int_sum_ubiq
      mfd[, norm_int:=i_magnitude*100/int_ref, ms_id]
    }

  # Calculate relative intensity: sum of Top100 rank most intense peaks
  # assign ranks based on i_magnitude
    if (normalization=="sum_rank"){
      # mfd[, rank:=data.table::frank(-i_magnitude, ties.method = "min"), ms_id]
        mfd[order(get(ms_id), -i_magnitude), rank := seq_len(.N), ms_id]

    # select TOPx magnitudes and calculate their sum of i_magnitude
      ref_int <- mfd[rank<=n_rank, .(int_ref=sum(i_magnitude), .N), ms_id]
      mfd <- ref_int[mfd, on = ms_id]

    # Calculate rel. intensities for all peaks based on sum_int_rank
      mfd[, norm_int:=i_magnitude*100/int_ref, ms_id]
    }

  # Calculate rel. intensities for all peaks based on Euclidean normalization
  # mfd[, rel_intE:=f_norm_ipr_e(x = mfd$i_magnitude), ms_id]

  # Normalize data for statistics
  # quantile(mfd$ri, probs=c(0.25, 0.75), names=T)
    # Other ways: Univariate scaling / Pareto scaling
    # xi - mean / stddev
    # oder xi - mean / sqrt(stddev)
    # Hier angewendet: xi - median / IQR
    # IQR: Interquartile range

    #mfd[, norm_dat:=(i_magnitude-median(i_magnitude))/IQR(i_magnitude), ms_id]

  # t_norm <<- mfd[, .(bp=round(max(bp)/1000000,0), sum_int=round(max(sum_int)/1000000,0), sum_int_opt=round(max(sum_int_opt)/1000000,0)
    #                   #, rel_intE=round(max(rel_intE)/1000000,0)
    #                   , sum_int_rank=round(max(sum_int_rank)/1000000000,0)), ms_id]


  }
  # Renew column (ri) - basis for many plots and statistics
  #summary(mfd$ri)
  #ri<<-mfd$ri

  # Results overview
  # mfd[, .(counts=.N), by=.(n_assignments)]
  # mfd[, .(n_mf=length(mf_iso)), by = "file"]


  # to do: Data transformation ####


  # to do: Pareto scaling ####

  # source
  # https://www.intechopen.com/books/metabolomics-fundamentals-and-applications/processing-and-visualization-of-metabolomics-data-using-r
  #
  # f_paretoscale <- function(x){
  #   rowmean <- apply(z, 1, mean) # row means
  #   rowsd <-  apply(z, 1, sd) # row std dev
  #   rowsqrtsd <- sqrt(rowsd) # sqrt of sd
  #   rv <- sweep(z, 1, rowmean, "-") # mean center
  #   rv <- sweep(rv, 1, rowsqrtsd, "/") # divide by sqrtsd
  #   return(rv)
  # }

  # Euclidean Normalization ####
  #
  # f_norm_ipr_e <- function(x, lower_quantile=0.1, upper_quantile=0.9){
  #   q <- quantile(x,probs = c(lower_quantile, upper_quantile)) # Calculate percentile
  #   x_norm <- x/(q[[2]]-q[[1]])                        # Divide by inter percentile range 80 (IPR80)
  #   x_norm <- x_norm/norm(x_norm,type = "2") # divide by euclidean norm gain unit length
  #   return(x_norm)
  # }

# Re-calculate the number of formula assignments per peak
  .msg("Recalculating the number of formula assignments per mass peak.")
  mfd[, n_assignments:=calc_number_assignment(ms_id = get(ms_id), peak_id = peak_id, mf = mf)]

  return(mfd[])
}

