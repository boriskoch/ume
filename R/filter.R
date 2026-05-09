## Filter by mass spectrometric metadata ####

#' @title Filter molecular formula data by mass spectrometric metadata
#' @name filter_mf_data
#' @description This function filters molecular formulas by isotope numbers, element ratios, etc.
#' @family Formula subsetting
#' @inheritParams main_docu
#' @param c_iso_check (TRUE / FALSE); check if formulas are verified by the presence of the main daughter isotope
#' @param n_iso_check (TRUE / FALSE); check if formulas are verified by the presence of the main daughter isotope
#' @param s_iso_check (TRUE / FALSE); check if formulas are verified by the presence of the main daughter isotope
#' @param ma_dev Deviation range of mass accuracy in +/- ppm (default: 3 ppm)
#' @param dbe_max Maximum number for DBE
#' @param dbe_o_min Minimum number for DBE minus O atoms
#' @param dbe_o_max Maximum number for DBE minus O atoms
#' @param mz_min Minimum of mass to charge value
#' @param mz_max Maximum of mass to charge value
#' @param n_min Minimum number of nitrogen atoms
#' @param n_max Maximum number of nitrogen atoms
#' @param s_min Minimum number of nitrogen atoms
#' @param s_max Maximum number of nitrogen atoms
#' @param p_min Minimum number of nitrogen atoms
#' @param p_max Maximum number of nitrogen atoms
#' @param oc_min Minimum atomic ratio of oxygen / carbon
#' @param oc_max Maximum atomic ratio of oxygen / carbon
#' @param hc_min Minimum atomic ratio of hydrogen / carbon
#' @param hc_max Maximum atomic ratio of hydrogen / carbon
#' @param nc_min Minimum atomic ratio of nitrogen / carbon
#' @param nc_max Maximum atomic ratio of nitrogen / carbon
# @param c13_max Maximum number of 13C isotopes
# @param s34_max Maximum number of 34S isotopes
# @param n15_max Maximum number of 15N isotopes
# @param cl35_max Maximum number of 35Cl isotopes
# @param cl37_max Maximum number of 37Cl isotopes
#' @import data.table
#' @keywords misc
#' @author Boris P. Koch
#' @return data.table; subset of original molecular formula table

#' @examples
#' filter_mf_data(mfd = mf_data_demo, dbe_o_max = 10)
#' @export

filter_mf_data <- function(mfd,
                           c_iso_check=FALSE,
                           n_iso_check=FALSE,
                           s_iso_check=FALSE,
                           ma_dev=3,
                           dbe_max=999, dbe_o_min=-999, dbe_o_max=999,
                           mz_min=1, mz_max=9999,
                           n_min=0, n_max=999,
                           s_min=0, s_max=999,
                           p_min=0, p_max=999,
                           oc_min=0, oc_max=999,
                           hc_min=0, hc_max=999,
                           nc_min=0, nc_max=99,
                           #, c13_max=999, s34_max=999, n15_max=999
                           #, cl35_max=999, cl37_max=999
                           #, known_mf_display="all"
                           # "all" means that no filter is applied;
                           # other values must match column "category_label" in known_mf
                           # or begin with a capital "C" to search for a single or a list of formulas
                           verbose = FALSE,
                           ...)
{

# Evaluate choice of arguments
  #s_iso_check <- match.arg(s_iso_check)
  if(!is.logical(c_iso_check)) stop("'c_iso_check' must be TRUE or FALSE")
  if(!is.logical(n_iso_check)) stop("'n_iso_check' must be TRUE or FALSE")
  if(!is.logical(s_iso_check)) stop("'s_iso_check' must be TRUE or FALSE")

  # c_iso_check=F; n_iso_check=FALSE; s_iso_check=FALSE;
  # remove_blank_list = "Blank";
  # ma_dev=99;
  # dbe_max=999; dbe_o_min=-999; dbe_o_max=999; mz_min=1; mz_max=9999;
  # n_min=0; n_max=999; s_min=0; s_max=2; p_min=0; p_max=999;
  # oc_min=0; oc_max=999; hc_min=0; hc_max=999; nc_min=0; nc_max=99;
  # c13_max=999; s34_max=999; n15_max=999; cl35_max=999; cl37_max=999;
  # n_occurrence_=1; verbose=T
  # ... <- NULL

  .msg("Length of input formula list: %d formulas", nrow(mfd))

# Removing blanks
  # to do: this could be part of a general concept in which mf subsetting is performed or subtraction of peak magnitudes
  # mfd <- remove_blanks(mfd = mfd, ...)

# Subset categories of formula categories that are listed in known_mf
  # mfd <- subset_known_mf(mfd = mfd, ...)

# # Subsetting of file_id's
# # No subsetting:
#   if(is.null(select_file_ids) & verbose==TRUE) message("No subselection of files applied.\n")
#
# # Subsetting based on select_file_ids:
#   if(!is.null(select_file_ids)){
#     # check if all elements of select_file_ids are existing in mfd
#     if(verbose == TRUE & all(select_file_ids %in% unique(mfd$file_id))){
#         message("              Selected file_ids: ", paste0(sort(select_file_ids), collapse = ", "))
#       } else if (verbose == TRUE & !all(select_file_ids %in% unique(mfd$file_id))) {
#         message("Some file_id's are not present in table mfd (will be ignored for selection): ",
#                 paste(select_file_ids[!(select_file_ids %in% unique(mfd$file_id))]))
#       }
#     mfd <- mfd[file_id %in% unique(select_file_ids),] # subsetting
#     }

  # To do: Include search for a given formula  or a set of formulas
  # if (substr(known_mf_display[1], 1, 1) == "C") {
  #   mfd <- mfd[mf %in% known_mf_display,]
  #   cat("Selected subset of molecular formulas:\n", known_mf_display)
  #   return(mfd)
  #   stop()} else {cat("No subselection of specific formulas applied.\n")}

  # to do: use Ying's final peaklist and verify why c_iso_check differs from ume version!!!

  if(is.null(c_iso_check) | c_iso_check==FALSE){c_iso_check <- 0} else {
    c_iso_check <- 1}

# Apply filter:
  mfd <- mfd[int13c>=c_iso_check
             & abs(ppm) <= ma_dev
             & dbe<=dbe_max
             & dbe_o %between% c(dbe_o_min, dbe_o_max)
             & mz %between% c(mz_min, mz_max)
             & `14N` %between% c(n_min, n_max)
             & `32S` %between% c(s_min, s_max)
             & `31P` %between% c(p_min, p_max)
             & oc %between% c(oc_min, oc_max)
             & hc %between% c(hc_min, hc_max)
             & hc %between% c(nc_min, nc_max)
            #& snp_check!="sn"
            #& nsp_type!="N2 S1 P0"
            #& s_pah_check !="del"
            ,]

  if(s_iso_check==TRUE){mfd <- mfd[(`32S`>0 & int34s>0) | `32S`==0]}
  if(n_iso_check==TRUE){mfd <- mfd[(`14N`>0 & int15n>0) | `14N`==0]}

  .msg("\nNumber of formulas after molecular formula filter: ", mfd[, .N])
  return(mfd)
}

## Subsetting categories from known_mf ####

#' @title Subsetting known molecular formula categories
#' @name subset_known_mf
#' @family Formula subsetting
#' @description Subset all molecular formulas that are present in one or more
#'  categories of ume::known_mf. Based on presence / absence.

#' @inheritParams main_docu
#' @param select_category List of category names that should be selected
#' @param exclude_category List of category names that should be ignored
#' @import data.table
#' @keywords misc
#' @return data.table; subset of original molecular formula data.table (mfd)
#' @examples subset_known_mf(category_list = c("marine_dom"), mfd = mf_data_demo, verbose = TRUE)
#' @export

subset_known_mf <- function(mfd,
                            select_category = NULL,
                            exclude_category = NULL,
                            verbose = FALSE,
                            ...) {

# check if all elements of category_list are existing in known_mf

if(!is.null(select_category) || !is.null(exclude_category)){
  all_categ <- unique(ume::known_mf[, category])
  selected_categ <- c(select_category, exclude_category)
  valid_sel_categ <- all_categ[all_categ %in% selected_categ]
  incl_categ <- valid_sel_categ[valid_sel_categ %in% select_category]
  excl_categ <- valid_sel_categ[valid_sel_categ %in% exclude_category]

  invalid_categ <- selected_categ[!(selected_categ %in% valid_sel_categ)]

  if(length(invalid_categ) > 0){
    warning("These categories are not present in table ume::known_mf (will be ignored for subsetting):\n",
            paste0(invalid_categ, "' '"))
  }
} else {
  .msg("No categories selected. Returning 'mfd' unchanged.")
  return(mfd)
}

  .msg("Length of input formula list: %d formulas", nrow(mfd))

# Subsetting based on select_category:
  # Select categories
  if(length(select_category)>0){
    .msg("Selected categories:\n", paste0(sort(incl_categ), " "))
    mfd <- mfd[mf %in% unique(ume::known_mf[category %in% incl_categ, mf])]
    .msg("   Length of formula list after category selection: ", nrow(mfd))
  }
  # Exclude categories
  if(length(exclude_category)>0){
    .msg("Excluded categories:\n", paste0(sort(excl_categ), " "))
    mfd <- mfd[!(mf %in% unique(ume::known_mf[category %in% excl_categ, mf]))]
    .msg("   Length of formula list after category exclusion: ", nrow(mfd))
  }

  return(mfd)
}

## Filter by (relative) peak magnitude ####

#' @title Filter by (relative) peak magnitude
#' @name filter_int
#' @family Formula subsetting
#' @description This function filters molecular formulas by (relative) peak abundances.

#' @inheritParams main_docu
#' @inheritDotParams calc_norm_int
#'
#' @param norm_int_min Lower threshold (>=) of (normalized) peak magnitude
#' @param norm_int_max Upper threshold (<=) of (normalized) peak magnitude
#'
#' @import data.table
#' @keywords misc
#' @return data.table; subset of original molecular formula table
#' @examples
#' filter_int(mfd = calc_norm_int(mfd = mf_data_demo,
#' normalization = "sum_rank", n_rank = 100), norm_int_min = 1)
#' @export

filter_int <- function(mfd, norm_int_min = NULL, norm_int_max = NULL,
                       verbose = FALSE, ...) {

  # norm_int <- NULL

  .msg("Length of input formula list: %d formulas", nrow(mfd))

  if(mfd[,.N]>0 & (!is.null(norm_int_min) | !is.null(norm_int_max))) {
    .msg("norm_int_min: %f; norm_int_max: %f", norm_int_min, norm_int_max)

 # Filter by intensity thresholds:
    if(!is.null(norm_int_min)){mfd <- mfd[norm_int >= norm_int_min]}
    if(!is.null(norm_int_max)){mfd <- mfd[norm_int <= norm_int_max]}

    .msg("Number of formulas after intensity filter: %d", nrow(mfd))
  } else {
    .msg("No filter intensity filters provided. Returning 'mfd' unchanged.")
  }
  return(mfd[])
}


## Automated filter for mass accuracy ####

#' @title Automated filter for mass accuracy
#' @name filter_mass_accuracy
#' @family Formula subsetting
#' @description This function automatically sets a filter for mass accuracy for each individual spectrum.

#' @inheritParams main_docu
#' @param ma_col Name of the column that contains mass accuracy values in ppm (string)
#' @param file_col Name of the column that contains file name

#' @import data.table
#' @keywords misc
#' @return data.table; subset of original molecular formula table
#' @export

filter_mass_accuracy <- function(mfd, ma_col = "ppm", file_col = "file_id", msg = FALSE, ...) {

  .msg("Length of input formula list: %d formulas", nrow(mfd))

  check_ppm <- mfd[`14N` == 0 &
                     `32S` == 0 &
                     `31P` == 0 &
                     dbe_o <= 10,
                   .(ppm_filt = round(max(abs(quantile(ppm, .975)), abs(quantile(ppm, .025))), 2)),
                   by = .(file_id)]

  .msg("Automated mass accuracy filter applied.")

  mfd <- check_ppm[mfd, on = "file_id"]

}
