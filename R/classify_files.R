#' @title Classify FTMS files into categories based on filename patterns
#' @name classify_files
#' @family helper
#'
#' @description
#' Classifies entries into categories (blank, standard, pool, sample, …)
#' based on pattern rules applied to a specific search column.
#' The identifiers returned in each category are also configurable.
#'
#' @details
#' Default behavior:
#' - `"blank"`: `blank_check == "blank"` or pattern `"blk"`
#' - `"standard"`: pattern `"srfa"`
#' - `"pool"`: pattern `"pool"`
#' - `"sample"`: everything unmatched
#'
#' Pattern matching is case-insensitive.
#'
#' @param fi `data.table`. Must contain the columns specified in
#'   `search_col` and `id_col`.
#' @param search_col Character. Name of the column used for pattern matching.
#'   Defaults to `"link_rawdata"`.
#' @param id_col Character. Name of the column whose values are returned for
#'   each category. Defaults to `"file_id"`.
#' @param patterns Named list of character vectors.
#'   Each list entry is a category name, and its value is a vector of patterns.
#' @param include_blank_check Logical; if TRUE and `blank_check` exists, it is
#'   used to assign `"blank"`.
#' @param return Either `"list"` (default) or `"table"`.
#'   - `"list"` → named list of ID vectors
#'   - `"table"` → `fi` with added column `category_analysis`
#'
#' @return Named list or a classified `data.table`.
#'
#' @examples
#' # Minimal demo data
#' fi <- data.table::data.table(
#'   file_id       = 1:6,
#'   filename      = c("NS_blk_01.raw", "SRFA_20.raw", "Pool_A.raw",
#'                     "Sample_01.raw", "Sample_02.raw", "MQ_blank.raw"),
#'   blank_check   = c("blank", NA, NA, NA, NA, "blank"),  # optional column
#'   link_rawdata  = c("NS_blk_01.raw", "SRFA_20.raw", "Pool_A.raw",
#'                     "Sample_01.raw", "Sample_02.raw", "MQ_blank.raw")
#' )
#'
#' # 1) Default behavior: return named list of file_ids by category
#' classify_files(fi)
#'
#' # 2) Use a different column for pattern matching
#' classify_files(fi, search_col = "filename")
#'
#' # 3) Return another ID field (here: file_id → stays the same for demo)
#' classify_files(fi, id_col = "file_id")
#'
#' # 4) Return the full table with new category column
#' classify_files(fi, return = "table")
#' @export

classify_files <- function(
    fi,
    search_col = "link_rawdata",
    id_col = "file_id",
    patterns = list(
      blank     = c("blk", "blank", "mq"),
      standard  = c("srfa", "standard"),
      pool      = c("pool")
    ),
    include_blank_check = TRUE,
    return = c("list", "table")
) {
  return <- match.arg(return)

  search_lower <- category_analysis <- blank_check <- NULL

  # --- validate ---------------------------------------------------------------
  if (!data.table::is.data.table(fi)) {
    stop("fi must be a data.table.")
  }
  if (!(search_col %in% names(fi))) {
    stop(sprintf("Column '%s' not found in fi.", search_col))
  }
  if (!(id_col %in% names(fi))) {
    stop(sprintf("ID column '%s' not found in fi.", id_col))
  }

  # --- prepare search column --------------------------------------------------
  fi[, search_lower := tolower(get(search_col))]

  # initialize category_analysis
  fi[, category_analysis := NA_character_]

  # --- (1) blank_check --------------------------------------------------------
  if (include_blank_check && "blank_check" %in% names(fi)) {
    fi[blank_check == "blank", category_analysis := "blank"]
  }

  # --- (2) apply pattern rules ------------------------------------------------
  for (cat in names(patterns)) {
    pats <- patterns[[cat]]
    regex <- paste0(pats, collapse = "|")

    fi[
      is.na(category_analysis) & grepl(regex, search_lower, ignore.case = TRUE),
      category_analysis := cat
    ]
  }

  # --- (3) remaining → "sample" ----------------------------------------------
  fi[is.na(category_analysis), category_analysis := "sample"]

  # --- cleanup ----------------------------------------------------------------
  fi[, search_lower := NULL]

  # --- output -----------------------------------------------------------------
  if (return == "table") {
    return(fi[])
  }

  # return list of identifiers by category_analysis
  out <- fi[, unique(get(id_col)), by = category_analysis]
  split(out$V1, out$category_analysis)
}

