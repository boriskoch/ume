
#' @title Retrieve NIST element and isotope data
#' @name get_isotope_info
#' @description
#' Checks if element/isotope columns are present in `mfd`
#' and lookup of NIST isotope information (based on [masses]).
#' Can be applied to a formula library and any table having molecular formula data.
#' If only an element name is identified, the symbol and data of the lightest isotope
#' of the element will be returned.
#' For example, the column name "C" will return "12C" isotope data.
#' @inheritParams main_docu
#' @keywords misc
#'
#' @references
#' Hill E.A. (1900). On a system of indexing chemical literature; adopted
#' by the classification division of the U. S. patent office.
#' *Journal of the American Chemical Society*, **22**, 478-494.
#' \doi{10.1021/ja02046a005}
#'
#' @return A data.table containing information on all isotopes identified in mfd
#' and a column "orig_name" having the original names of the
#' isotope / element columns in `mfd`. Results are ordered according to Hill system.
#' @examples get_isotope_info(mfd = mf_data_demo, verbose = TRUE)
#' @import data.table
#' @export

get_isotope_info <- function(mfd,
                             masses = ume::masses,
                             verbose = FALSE,
                             ...) {

  orig_name <- new_name <- link_name <- NULL

# Save the original column headers of mfd
  dt_names <- data.table(orig_name = names(mfd))
  dt_names[, link_name:=tolower(orig_name)]

# Remove column names that can be ambiguous
# such as sn for signal to noise or sc for S/C ratio
  bad_cols <- c("sc", "sn")

  if(any(bad_cols %in% dt_names$orig_name)){
    dt_names <- dt_names[!(orig_name %in% bad_cols)]
  }

# Find those names, which match directly with masses (column "label")
  res1 <- masses[tolower(label) %in% dt_names$link_name]
  res1[, link_name:=tolower(label)]
  res1 <- dt_names[, .(orig_name, link_name)][res1, on = "link_name"]

# Find those names, which match with column "symbol" in masses
# and select the lightest isotope
  res2 <- masses[tolower(symbol) %in% dt_names$link_name, .SD[which.min(nm)], by = symbol]
  res2[, link_name:=tolower(symbol)]
  res2 <- dt_names[res2, on = "link_name"]

# Combine the two results
  res <- rbind(res1, res2, fill=T)
  res[, link_name:=NULL]
  setkey(res, hill_order)

# Warning if no columns were identified
  if(nrow(res[!is.na(label)])==0){
    warning("No element / isotope columns identified in 'mfd'.")
  }

  if(verbose==T){message("Isotopes identified: '", paste0(res$label, collapse = "', '"), "'\n")}
  return(res)
  }
