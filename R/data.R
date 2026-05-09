#' @title Demo peak list
#' @name peaklist_demo
#' @description Contains parts of the peaklist (200 - 300 m/z) from mass spectra
#' to use as demonstration and validation dataset.
#' The sample mass spectra contain one blank, three replicates
#' of North Sea water, and three Arctic fjord samples as triplicates.
#' @family ume data
#' @format A data.table having 31,091 rows and 7 variables:
#' \describe{
#'   \item{file_id}{A unique identifier for a mass spectrum (integer)}
#'   \item{file}{A unique label for a mass spectrum or sample (character)}
#'   \item{peak_id}{A unique identifier for a peak in the entire peak list (integer)}
#'   \item{mz}{Mass to charge ratio of the singly charged molecular ion (numeric)}
#'   \item{i_magnitude}{Peak magnitude of the molecular ion (numeric)}
#'   \item{s_n}{Signal to noise ratio of the molecular ion (numeric)}
#'   \item{res}{Mass resolution of the peak / ion (numeric)}
#' }
#' @source {taken from www.awi.de}
#' @examples data(peaklist_demo)
"peaklist_demo"



#' @title Demo formula library (200 - 300 Da, neutral mass)
#' @name lib_demo
#' @description Contains a small molecular formula library for demonstration and
#' validation purposes. Complete formula libraries are available
#' in the 'ume.formulas' data package.
#' @family ume data
#' @format A data.table having ~115,111 rows and 12 variables:
#' \describe{
#' \item{vkey}{First two digits represent the formula library version;
#' last digits are unique identifiers for each formula}
#' \item{mf}{Neutral molecular formula (no differentiation of isotopes)}
#' \item{mass}{Calculated exact neutral mass of a formula (based on ume::masses)}
#' }
#' @examples data(peaklist_demo)
"lib_demo"



#' @title Masses: Elements and isotopes
#' @name masses
#' @description Contains masses, valences, isotopes and isotope ratios of elements
#' based on data by NIST Physical Measurement Laboratory (https://www.nist.gov/pml).
#' @family ume data
#'
#' @references
#' Hill E.A. (1900). On a system of indexing chemical literature; adopted
#' by the classification division of the U. S. patent office.
#' *Journal of the American Chemical Society*, **22**, 478-494.
#' \doi{10.1021/ja02046a005}
#'
#' @format A data.table having 288 rows and 23 variables:
#' \describe{
#' \item{element}{Element symbol in lower case}
#' \item{symbol}{Element symbol in upper case}
#' \item{isotope}{Isotope symbol in lower case}
#' \item{label}{Isotope symbol in upper case}
#' \item{nm}{Nominal mass of the isotope}
#' \item{exact_mass}{Exact mass of the isotope}
#' \item{mole_fraction}{Mole fraction compared to all isotopes of an element}
#' \item{relative_abundance}{Relative abundance compared to the main (most abundant) isotope}
#' \item{valence}{Valence at standard conditions}
#' \item{valence2}{Alternative valence at standard conditions}
#' \item{hill_order}{Rank in Hill Order for molecular formulas (cf. https://en.wikipedia.org/wiki/Chemical_formula)}
#' }
#' @source {https://www.nist.gov/pml/atomic-weights-and-isotopic-compositions-relative-atomic-masses}
#' @examples data(masses)
"masses"



#' @title mf_data_demo
#' @name mf_data_demo
#' @description Contains molecular formula data and metainformation on formulas.
#' The metainformation
#' @family ume data
#' @format A data.table with ~9245 rows (formulas) and 65 variables:
#' \describe{
#' \item{file_id}{Unique ID (integer) for each analysis}
#' \item{peak_id}{Unique ID (integer) for each mass peak in the peak list 'pl'}
#' \item{mz}{Mass to charge ratio of the singly charged molecular ion (numeric)}
#' \item{i_magnitude}{Measured mass peak magnitude of the singly charged molecular ion (numeric)}
#' \item{norm_int}{Normalized intensity as calculated by calc_norm_int()}
#' \item{m}{Neutral measured mass of the molecular ion}
#' \item{m_cal}{Neutral calculated mass of the assigned formula}
#' \item{ppm}{Realtive mass accuracy of measured mass compared to m_cal (in ppm)}
#' \item{nm}{Nominal mass of the neutral molecule}
#' \item{mf}{molecular formula (no differentiation of isotopes)}
#' \item{dbe}{Double bond equivalent}
#' \item{`12C`}{Number of carbon atoms (12C)}
#' \item{`1H`}{Number of hydrogen atoms}
#' \item{hc}{hydrogen / carbon ratio in a molecular formula}
#' \item{oc}{oxygen / carbon ratio in a molecular formula}
#' \item{nc}{nitrogen / carbon ratio in a molecular formula}
#' \item{sc}{sulfur / carbon ratio in a molecular formula}
#' \item{ai}{Aromaticity index according to Koch and Dittmar (2008, 2016)}
#' \item{z}{z score according to Stenson et al. (2003)}
#' \item{kmd}{Kendrick mass defect (based on CH2-units) according to Kendrick (1963)}
#' \item{ppm_filt}{Calculated threshold value for relative mass accuracy (in ppm) that can be used for formular filtering}
#' \item{mf_id}{Identifier for each unique molecular formula identified in the unfiltered dataset}
#' \item{CRAM}{Molecular formula that was identified (CRAM == 1) as carboxylic rich alicyclic molecule according to Hertkorn et al. (2006). See ume::known_mf for details.}
#' \item{int13c}{Measured relative peak magnitude of the 13C1 isotope compared to the parent ion (0 if isotope was not existing)}
#' \item{int15n}{Measured relative peak magnitude of the 15N1 isotope compared to the parent ion (0 if isotope was not existing)}
#' \item{int34s}{Measured relative peak magnitude of the 34S1 isotope compared to the parent ion (0 if isotope was not existing)}
#' \item{dev_n_c}{Deviation of the 12C/13C isotope ratio represented in carbon numbers according to Koch et al. (2007)}
#' \item{dbe_o}{DBE minus O}
#' \item{nosc}{Nominal oxidation state of carbon according to LaRowe & Van Cappellen (2011)}
#' \item{delg0_cox}{Standard molal Gibbs energies of the oxidation half reactions of organic compounds according to LaRowe & Van Cappellen (2011)}
#' \item{co_tot}{Total number of carbon and oxygen atoms in a molecular formula}
#' \item{nsp_tot}{Total number of nitrogen, sulfur, and phosphorus atoms in a molecular formula}
#' \item{n_occurrence_orig}{Number of occurrences of a molecular formula in the entire unfiltered set of formulas}
#' \item{n_assignments_orig}{Number of molecular formula assignments per molecular mass in the unfiltered set of formulas}
#' \item{n_assignments}{Number of molecular formula assignments per molecular mass after filter process}
#' \item{int_bp}{Magnitude of the base peak in a mass spectrum}
#' \item{int_bp}{Total magnitude of the reference that was used for normalization (cf. calc_norm_int())}
#' }
#' @source {taken from www.awi.de}
#' @examples data(mf_data_demo)
"mf_data_demo"



#' @title Collection of known formulas, for which additional information is available.
#' @name known_mf
#' @description Known formulas; contains formulas for which additional knowledge is available.
#' This can be also calibration lists.
#' Due to size reasons the table is restricted to what is covered by standard UME formula library
#' (mz<=700, elements CHONSP considered).
#' The original version is part of the UME database and transferred to UME using UTF-8 encoding.
#' CRAM molecular formulas are taken from the supplementary material that is provided by Hertkorn et al. (2006).
#' @family ume data
#' @format A data.table with ~300,000 rows and 14 variables:
#' \describe{
#' \item{mz}{Mass to charge ratio (numeric)}
#' \item{mf}{molecular formula}
#' }
#' @source {taken from www.awi.de}
#' @examples data(known_mf)
"known_mf"



#' @title Labels of UME columns.
#' @name tab_ume_labels
#' @format A data.table that is derived from the MarChem database:
#' \describe{
#' \item{label}{Identifier for each label}
#' \item{nice_label}{Label that can be used e.g. in figures}
#' \item{use_in_ume}{Shows if label is used in the UME shiny app}
#' }
#' @family ume data
#' @source {taken from www.awi.de}
#' @examples data(tab_ume_labels)
"tab_ume_labels"


#' @title nice_labels_dt
#' @name nice_labels_dt
#' @family ume data
#' @format A data.table with labels that can be used for plots
#' \describe{
#' \item{name_substitute}{Name that will be displayed instead of the standard column name}
#' \item{name_pattern}{Name of the standard column in ume tables}
#' }
#' @source {taken from www.awi.de}
#' @examples data(nice_labels_dt)
"nice_labels_dt"


#' @title Internal raster of the UME logo
#' @name ume_logo_raster
#' @docType data
#' @description This object contains the preloaded raster version of the UME logo,
#' generated once during package development from
#' `inst/figures/ume_package_icon.png`.
#'
#' It is used internally by `uplots_add_ume_logo()` to avoid runtime
#' dependencies on the **png** package and to ensure the logo can be added
#' without file I/O.
#'
#' @format A 3D numeric array representing an RGBA raster image.
#'
#' @usage NULL
#' @keywords internal
NULL
