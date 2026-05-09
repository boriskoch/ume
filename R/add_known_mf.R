# Add metainformation derived from known_mf ####

#' @title Add metainformation derived from ume::known_mf
#' @name add_known_mf
#' @family Formula assignment
#' @description Annotate molecular formulas categories using `ume::known_mf`.
#' Join molecular formula data and metadata about known formulas
#' (e.g. annotate carboxylic-rich alicyclic molecules; CRAM).
#' The name of the molecular formula column will be set to "mf".
#'
#' This function works with:
#' * a **vector** of molecular formulas: returns a 2-column `data.table(mf, categories)`
#' * a **data.table** with a formula column: returns the table with an added `categories` column
#'
#' @inheritParams main_docu
#' @param mfd Either (1) a character vector of molecular formulas, or
#'   (2) a data.frame / data.table containing such a column.
#' @param mf_col Name of the molecular formula column if `mfd` is a table (default: "mf").
#' Formulas have upper case element symbols and elements in the formula are ordered according to the Hill system.
#' @param wide Logical. If TRUE, return one column per category (CRAM, surfactant, ...).
#'   If FALSE (default), return only a single `categories` column.
#' @keywords misc
#' @import data.table
#' @author Boris P. Koch
#' @references
#' **CRAM**
#' Hertkorn N., Benner R., Frommberger M., Schmitt-Kopplin P., Witt M.,
#' Kaiser K., Kettrup A., Hedges J.I. (2006). Characterization of a major
#' refractory component of marine dissolved organic matter.
#' *Geochimica et Cosmochimica Acta*, **70**, 2990-3010.
#' \doi{10.1016/j.gca.2006.03.021}
#' **Surfactants**
#' Lechtenfeld O.J., Koch B.P., Gasparovic B., Frka S., Witt M.,
#' Kattner G. (2013). The influence of salinity on the molecular and
#' optical properties of surface microlayers in a karstic estuary.
#' Marine Chemistry, 150, 25-38.
#' \doi{10.1016/j.marchem.2013.01.006}
#'
#' **Ideg**
#' Flerus R., Lechtenfeld O.J., Koch B.P., McCallister S.L., Schmitt-Kopplin P.,
#' Benner R., Kaiser K., Kattner G. (2012). A molecular perspective on
#' the ageing of marine dissolved organic matter. *Biogeosciences*, **9**,
#' 1935-1955.
#' \doi{10.5194/bg-9-1935-2012}
#'
#' **iTerr**
#' Medeiros P.M., Seidel M., Niggemann J., Spencer R.G.M., Hernes P.J.,
#' Yager P.L., Miller W.L., Dittmar T., Hansell D.A. (2016).
#' A novel molecular approach for tracing terrigenous dissolved organic matter
#' into the deep ocean. *Global Biogeochemical Cycles*, **30**, 689-699.
#' \doi{10.1002/2015gb005320}
#'
#' @return A data.table containing additional columns having information on formula categories
#' @examples add_known_mf(mfd = mf_data_demo)
#' @export

add_known_mf <- function(
    mfd,
    mf_col = "mf",
    known_mf = ume::known_mf,
    wide = FALSE,
    ...
) {

  categories <- NULL

  # Case 1: character vector of MFs
  if (is.character(mfd)) {
    dt <- data.table::data.table(mf = mfd)
  } else {
    # Case 2: molecular formula table
    if (!mf_col %in% names(mfd)) {
      stop("Column '", mf_col, "' not found in input table.")
    }
    dt <- data.table::as.data.table(mfd)
    data.table::setnames(dt, mf_col, "mf")
  }

  # ---- Identify relevant categories ----------------------------------------
  categ <- ume::tab_ume_labels[use_in_ume == 1, unique(label)]
  unique_mfs <- unique(dt$mf)

  # Subset known formulas to reduce work
  kmf <- known_mf[mf %in% unique_mfs & category %in% categ]

  # ---- Create "categories" column (comma separated) -------------------------

  category_map <- kmf[, .(categories = paste(unique(category), collapse = ", ")), by = mf]

  # Merge into dt
  dt <- category_map[dt, on = "mf"]

  # ---- Optional: wide format (CRAM, surfactant, etc.) -----------------------

  if (wide) {
    kmf_wide <- data.table::dcast(
      kmf,
      mf ~ category,
      fun.aggregate = length,
      fill = 0,
      value.var = "mf_kf_id"
    )
    dt <- kmf_wide[dt, on = "mf"]
  }

  # Default for surfactant filter
  # to do: Can be removed once categories column is used for subsetting by filter function
  if ("surfactant" %in% colnames(dt)) {
    dt[is.na(surfactant), surfactant := 0]
  }

  return(dt[])
}
