#' @title Create a Data Summary Table for Element Ratios and Parameters
#' @name calc_data_summary
#' @description Generates a data summary table that provides intensity-weighted averages for element ratios,
#'   mass accuracy, and additional parameters. Results can be grouped based on the specified grouping columns.
#' @family calculations
#' @inheritParams main_docu
#' @import data.table
#'
#' @details This function computes a variety of weighted averages and summary statistics for mass spectrometry data
#'   using the provided peak list (`mfd`). Calculated values include weighted averages for elemental counts
#'   (e.g., Carbon, Hydrogen), elemental ratios (e.g., O/C, H/C), and additional parameters such as the base peak intensity
#'   and summed intensities. It also calculates the aromaticity index (`wa(AI)`) based on the elemental composition.
#'   If grouping columns are provided, the summary statistics are calculated for each group.
#'
#'   The function also joins additional indices (`ideg`, `iterr`) from related functions `calc_ideg()` and `calc_iterr()`
#'   to the final summary table.
#'
#' @return A `data.table` containing the summarized results, with columns including:
#'   \describe{
#'     \item{n(mf)}{Number of molecular formulas per group.}
#'     \item{accuracy (median)}{Median accuracy in parts-per-million (ppm) for the identified peaks.}
#'     \item{accuracy (3 sigma cut-off)}{Maximum ppm accuracy within a three-sigma range.}
#'     \item{wa(mz)}{Weighted average m/z value.}
#'     \item{wa(DBE)}{Weighted average Double Bond Equivalent (DBE).}
#'     \item{wa(element)}{Weighted averages for elements (C, H, N, O, P, S) and ratios (O/C, H/C, N/C, S/C).}
#'     \item{wa(NOSC)}{Weighted average nominal oxidation state of carbon.}
#'     \item{wa(delG0_Cox)}{Weighted average Gibbs free energy (Cox) in kJ/mol.}
#'     \item{wa(AI)}{Weighted average aromaticity index.}
#'     \item{wa(C/N) and wa(C/S)}{Ratios derived from N/C and S/C.}
#'     \item{ideg, ideg_n}{Indices for degree of identification, as calculated by `calc_ideg()`.}
#'     \item{iterr, iterr_n, iterr2, iterr2_n}{Iteration error indices from `calc_iterr()`.}
#'     \item{median(i_magnitude)}{Median intensity value.}
#'     \item{int(basepeak)}{Intensity of the base peak.}
#'     \item{int(summed)}{Summed intensity of all peaks.}
#'   }
#'
#' @examples
#' # Example using demo data, grouping by file ID
#' calc_data_summary(mfd = mf_data_demo, grp = c("file_id"))
#' @export

calc_data_summary <- function(mfd, grp = "file_id", ...){

  `wa(N/C)` <- `wa(S/C)` <- `wa(N/C)` <- NULL
  `wa(C)` <- `wa(H)` <- `wa(N)` <- `wa(O)` <- `wa(P)` <- `wa(S)` <- NULL

# Calculation of intensity-weighted averages
  # message(paste0("Data summary grouped by: ", grp))
  # mfd$file_id <- as.character(mfd$file_id)
  # df_orig$file_id <- as.character(df_orig$file_id)

  mfd[, i_magnitude:=as.numeric(i_magnitude)]

  ds <- mfd[,.("n(mf)"=length(mf)
              , "accuracy (median)"=round(stats::median(ppm),2)
              , "accuracy (3 sigma cut-off)"=round(max(ppm_filt),2)
              , "wa(mz)"=round(stats::weighted.mean(mz, i_magnitude),0)
              , "wa(DBE)"=round(stats::weighted.mean(dbe, i_magnitude),1)
              , "wa(C)"=round(stats::weighted.mean(`12C`, i_magnitude),1)
              , "wa(H)"=round(stats::weighted.mean(`1H`, i_magnitude),1)
              , "wa(N)"=round(stats::weighted.mean(`14N`, i_magnitude),2)
              , "wa(O)"=round(stats::weighted.mean(`16O`, i_magnitude),1)
              , "wa(P)"=round(stats::weighted.mean(`31P`, i_magnitude),2)
              , "wa(S)"=round(stats::weighted.mean(`32S`, i_magnitude),2)
              , "wa(O/C)"=round(stats::weighted.mean(oc, i_magnitude),3)
              , "wa(H/C)"=round(stats::weighted.mean(hc, i_magnitude),3)
              , "wa(N/C)"=round(stats::weighted.mean(nc, i_magnitude),4)
              #,"wa(P/C)"=round(weighted.mean(pc, ri),1)
              , "wa(S/C)"=round(stats::weighted.mean(sc, i_magnitude),4)
              , "wa(NOSC)"=round(stats::weighted.mean(nosc, i_magnitude),3)
              , "wa(delG0_Cox)"=round(stats::weighted.mean(delg0_cox, i_magnitude),2)
              , "median(i_magnitude)"=round(stats::median(i_magnitude),2)
              , "int(basepeak)"=max(i_magnitude)
              , "int(summed)"=sum(i_magnitude)
  )
  , by=grp]

  # Calculate aromaticity index (AI) according to Koch $ Dittmar 2006, 2016
  # This calculation differs from the calculation for individual formulas
  # because the weighted average c-o-s-n-p is always >0 for DOM. By that AI is always defined.

  ds[ , "wa(AI)":=round((1 + `wa(C)`-`wa(O)`-`wa(S)`-(0.5*(`wa(H)`+`wa(N)`+`wa(P)`)))/(`wa(C)`-`wa(O)`-`wa(S)`-`wa(N)`-`wa(P)`),2)]

  ds[, c("wa(C/N)", "wa(C/S)"):=.(round(1/`wa(N/C)`,0), round(1/`wa(S/C)`,0))]

  ds <- calc_ideg(mfd, grp = grp)[ds, on = grp]
  ds <- calc_iterr(mfd, grp = grp)[ds, on = grp]

  cols <- c(grp, "n(mf)", "wa(mz)", "wa(DBE)", "wa(AI)"
            , "wa(C)", "wa(H)", "wa(N)", "wa(O)", "wa(P)", "wa(S)"
            , "wa(O/C)", "wa(H/C)", "wa(N/C)", "wa(S/C)"
            , "wa(C/N)", "wa(C/S)"
            , "wa(NOSC)", "wa(delG0_Cox)"
            , "ideg", "ideg_n"
            , "iterr2", "iterr2_n"
            , "iterr", "iterr_n"
            , "median(i_magnitude)"
            , "int(basepeak)", "int(summed)"
  )

  setcolorder(ds, c(cols, colnames(ds)[!(colnames(ds) %in% c(cols))]))

  return(ds)
}

