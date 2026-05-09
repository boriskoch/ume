![UltraMassExplorer](inst/figures/ume_package_icon.png)

### Version 1.6.2 (2026-xx-xx)


### Version 1.6.1 (2026-05-09)

Submitted to CRAN.

#### New features
- `calc_isotope_pattern()` returns exact mass, nominal mass, probability,
  relative abundance, molecular formula (`mf`), and isotope-specific molecular
  formula (`mf_iso`) for each isotope peak.
- `inchi_to_mf()`: Extracts molecular formula string from an InChI code.
- `check_neutral_mf()`: Is a given string for a neutral molecular formula correct?
  - Added optional pre-validation of molecular formulas via check_neutral_mf() in convert_molecular_formula_to_data_table(). 
  Invalid, charged, or non-formula inputs now produce informative error messages.
  - Introduced robust parsing of molecular formulas based on masses, 
  supporting a wide range of elements and isotope labels without hard-coded symbol lists.
 - `calc_nm()` and related workflows now correctly preserve input order and
  duplicate formulas when character vectors are supplied.
  
#### Stability and bug fixes
- `convert_molecular_formula_to_data_table()`
  - Added new argument `isotope_default`. Unspecific element notation such as `Mo` can now be resolved either to the
  most abundant isotope (`"most_abundant"`, default) or to the lightest isotope (`"lightest"`).
  - Fixed parsing errors for formulas containing multi-letter element symbols (such as `Mg) that were previously misinterpreted.
  - Fixed incorrect splitting of multi-letter element symbols without counts (e.g. "Mo" was parsed as "M" + "o")
  - Resolved internal inconsistencies related to isotope handling and duplicate detection.
  - Fixed inconsistency where masses were calculated using the correct isotope but column names still reflected the lightest isotope
  - Original input order and duplicate molecular formulas are now preserved.
  - Improved handling of repeated element or isotope tokens such as `C2H5OH` or `Cl3[37Cl2][37Cl4]`.
- `create_isotope_expanded_table()` now also accepts character vectors of
  molecular formulas as input.
- `convert_data_table_to_molecular_formulas()` was rewritten to preserve
  original row order and duplicate entries using internal row identifiers.  
- `check_neutral_mf()`
  - Fixed length mismatch in data.table assignment when handling non-empty entries
  - Improved robustness for inputs containing NA values
  
#### Improvements
- `uplot_ms()`
  - Replaced .SD[order()]-based data reduction with a faster data.table::frank()-based ranking approach
  - Significantly improved performance for large peak lists (10⁵–10⁶+ peaks), especially when using data_reduction < 1
  - Reduced memory overhead by avoiding repeated sorting and subsetting operations

### Version 1.6.0 (2026-03-31)

#### Stability and bug fixes
- Fixed handling of grouping (grp) in calc_ideg() and calc_iterr() for LC–MS data with multiple spectra per analysis.
- Fixed inconsistent colour scale behaviour in uplot_cvm() when z_var was omitted.
- Fixed empty or malformed colour labels (e.g. "Number of C+O ()").

#### New features
- `validate_isotope_presence()`: Validates parent molecular formulas based on the presence or absence of
corresponding isotope daughter signals within the same mass spectrum.
- `create_isotope_expanded_table()`:
  - Creates a new molecular formula table containing the original parent formulas
and their corresponding single-isotope daughter formulas.
  - the function is now applied in `create_custom_formula_library()` as well.

- A complete overhaul of all UME plotting functions was implemented to ensure consistent styling, simplified function code, and flexible user control:
  - Simplified plot concept:
    - All uplot_*() functions now consist only of data preparation + base ggplot construction.
    - Colour scales, titles, themes, labels, logo, and layout are fully managed by the wrapper.
  - New unified plot wrapper (uplot_wrapper())
    - Centralizes theme application, palette handling, label mapping, dot-size control, branding, and optional Plotly output.
    - All uplot_*() functions now delegate styling to this wrapper.
  - New theme_uplots()
    - Clean UME visual style with unified typography, axis styling, and optional dot-size override.
  - New branding helpers
    - uplots_add_ume_logo() — device-independent logo placement using NPC coordinates.
    - uplots_add_ume_label() — vertical “UltraMassExplorer” label outside the plot panel.
    - Positioning parameters for logo and label
  - Automatic axis/legend label mapping (uplots_map_labels())
    - Replaces internal variable names (e.g. oc, hc, norm_int) with human-readable labels.
  - New user-facing controls across all plots
    - size_dots, palname, col_bar
    - title_show, title_size
     -ume_logo, ume_label
  - `interactive` = TRUE for interactive plotly output

#### Improvements
- `add_known_mf()`:
  - now accepts either a vector of molecular formulas or a data.table. 
  A vector input gives a 2-column lookup table (mf, categories). A table input 
  returns the original table with added `categories` column.
  - Optional wide = TRUE mode to create one column per category (CRAM, surfactant, etc.).
  - Simplified and more robust internal logic; no unintended column removal.
  - function tests added to package
  
- `uplot_dbe_vs_ma()` replaces `uplot_dbe_vs_ppm()`

- known_mf: AbioS formulas added (after Gomez-Saez et al., 2021)
- `calc_iterr()`: documentation updated and reference added

### Version 1.5.2 (2025-12-07)

#### Uplot layout

- Introduced `theme_uplots()` to unify the layout of plot functions.

#### Documentation improvements

- Removed all \dontrun{} examples and replaced them with CRAN-compliant examples (using tempdir() where writing is required).
- Added or improved @return documentation for many functions.
- Expanded and standardized parameter descriptions for int_col, grp, z_var, palname, and others.
- Added full roxygen documentation for all plotting functions and internal helpers.

#### CRAN compliance: file writing and network access

- All functions now avoid writing to the user's home directory unless explicitly requested.
- All examples and tests write only to tempdir().
- download_library() was rewritten for full CRAN compliance:
  - no automatic downloads at install/load time,
  - no downloads in non-interactive sessions (e.g. CRAN checks),
  - explicit user confirmation required before downloading libraries,
  - SHA256 checksums verified for all downloads,
  - safe local caching under ~/.ume/.

#### CRAN compliance: graphical parameters and working directory

- Removed all global changes to par()

#### Plotting system overhaul

- Updated legacy base-graphics plots to ggplot2, or ensured safe usage without altering global par().
- Added optional Plotly output to multiple plotting functions.
- Standardized UME logo placement; uplot_layout() is now internal (non-exported).
- Improved colour handling for:
  - uplot_vk()
  - uplot_kmd()
  - uplot_dbe_vs_c()
  - isotope precision plots.
- Added optional data-reduction binning to speed up rendering of large scatterplots.
- Added additional references for uplots

#### Stability and bug fixes

- Rewrote remove_empty_columns() to avoid data.table warnings related to .. scoping.
- Updated calc_neutral_mass() error messages to satisfy unit tests; removed unnecessary options() calls.
- Improved uplot_ratios():
  - corrected intensity-ratio computation,
  - fixed conservative and non-conservative unique-MF logic,
  - enhanced VK colour mapping and Plotly conversion.

- Strengthened robustness of uplot_vk() when z_var is missing or not numeric.
- Removed all hidden side effects from exported functions (options, par(), working directory, filesystem writes).

#### Internal quality improvements

- Simplified internal helpers:
  - .msg()
  - .prepare_peaklist_columns()
  - .normalize_column_aliases()

- Removed deprecated internal utilities (e.g. wcsv()).
- Strengthened validation logic in peaklist and data-cleaning functions.
- Harmonized column checking and naming behaviour across the entire package.

### Version 1.5.1 (2025-12-01)

#### Modifications
- `download_library()` now follows CRAN rules for downloading external data. 
- As `calc_norm_int()` always recalculates `n_occurrence` and `n_assignments`, 
the argument `ms_id` is passed to `calc_number_assignments()`.

#### Bug fixes
- `calc_norm_int()`: Fixed a corrupt message when using `verbose = TRUE`.

### Version 1.5.0 (2025-11-23)
This version is the first submission to CRAN.

#### Depricated and changed arguments:
- `filter_mf_data`: The `select_file_ids` argument is deprecated.
- `msg` is depricated and replaced `verbose`.
- `add_known_mf` now only adds a single column (`categories`) to `mfd`
- `check_peaklist` was renamed as `as_peaklist` and now allows the import of 
external peaklists from e.g. csv-files. 

#### External Formula Libraries via Zenodo
UME's large molecular formula libraries (15–125 MB) are now hosted on Zenodo
(https://doi.org/10.5281/zenodo.17606457) for open and persistent access.

- `lib_02.rds`: medium-sized balanced library
- `lib_05.rds`: extended high-coverage library (default)

These library objects are now S3 class objects.

#### New functions
- `download_library()`, allows users to download and load molecular formula libraries from Zenodo:
  - Downloads missing libraries automatically
  - Verifies file integrity via SHA256 checksums
  - Caches libraries in memory to avoid repeated loading
  - Avoids repeated downloads unless `overwrite = TRUE`
  - Loads the library directly as a `data.table`
- Lookup function `.f_label()` looks for pretty labels in the table `ume::nice_labels_dt`.

#### Improvements
- Added detailed documentation and examples for working with external libraries.
- Some functions were declared as internal and not exported.
- `uplot_cluster()` now returns a list object with cluster and mds results and figures.
- `uplot_pca()` now returns a list object that includes a plotly PCA figure.
- several documentation elements transferred to `main_doc.R`
- References added to function descriptions.

### Version 1.4.2 (2025-11-06)

#### New features
* Added centralized validation for core ume data.table types: peaklist, formula_table, 
  and formula_library to ensure consistent structure, types, and column names.
* Introduced schema definitions and generic check_table_schema() helper
  to ensure consistent column names and types.
* Legacy check_*() functions (check_peaklist(), check_mfd(), check_formula_library())
  kept as type-specific validators, now routed through a unified system.
* Package functions (ume_assign_formulas(), ume_filter_formulas(), etc.)
  now automatically verify input table structures to prevent runtime errors.
* Provides clearer error messages and allows easy extension 
  for new table types in the future.
* New internal function `.msg()` for handling messages (verbose).
* `ume_vignette.pdf` added to repository and can be accessed via gitlab `readme`.
* New helper function `classify_files()` to automatically group files into
  categories (e.g., blanks, standards, pools, samples) based on pattern rules.
  The function is fully flexible: both the search column and the returned ID
  column can be specified by the user.

#### Bug fix
* `calc_norm_int()`: Normalization via "sum_rank" fixed so that the sum 
  is always based on the exact number of argument `n_rank`.

### Version 1.4.1 (2025-10-30)
#### Enhancements
* Updates of documentation.
* Added a new internal helper `normalize_verbose()` to standardize message 
  control across functions.
  Both `verbose` (preferred) and the legacy `msg` argument are supported.
  If both are provided, `verbose` takes precedence and a warning is issued.

#### Deprecated
* The argument `msg` is now deprecated in favor of `verbose`.
  Existing code using `msg` will continue to work but may trigger a warning.

### Version 1.4.0 (2025-10-20)
This version introduces a new nomenclature. All columns carrying information on
isotopes are now named according to the official IUPAC nomenclature to avoid ambiguities.

For example, the column 'c' that contains the number of atoms of 12C is now called '12C' (capital "C"!).

This had implications for the entire `ume` data pipeline.
Functions such as `check_mfd()`, `check_formula_library()`, and `check_peaklist()` 
can now help to enforce the new nomenclature.

#### Function updates
  - `calc_recalibrate_ms.R()` now expects a filename (`file`). The new argument 
  `insufficient_calibrants` (valid argument values: "extrapolate", "remove_spectrum") 
  handles spectra, in which no calibrant masses were identified. The argument value "extrapolate"
  takes the median of calibration slope and intercept for all spectra that could be calibrated 
  with at least two masses and uses these values to calibrate the spectra that showed no 
  calibration masses. The argument value "remove_spectrum" deletes all peaks of those spectra for 
  which no calibrant masses were identified. 
  - `get_isotope_info()` is now a fundamental function that identifies element / isotope 
  information in any table. It returns the original names of the isotope columns of the table
  and related IUPAC information on the isotope. 
  - `identify_isotope_columns()` is depricated and merged into `get_isotope_info()`.
  - `add_known_mf()` now provides a column that contains all category labels. 
  In future versions separate columns for each category (such as "CRAM") will be depricated.
  - `assign_formulas()`: pl (peaklist) can now also be a numeric mass vector 
  or a single mass. 
  For numeric input, a minimal peaklist is constructed internally. The result
  data.table is now returned visibly (before: `return(invisible(mfd))`). The consistency
  of the numeric peaklist is now checked by the function `check- _peaklist()`.
  - `check_peaklist`() now allows manual assignment of the column names containing the 
  mass spectrum filename, file identifier (numeric column), the m/z values, and peak magnitude.
  The columns will be renamed according to the internal naming of these column in `ume`. 
  - `check_mfd()` and `check_formula_library()` now enforce the new isotope nomenclature.
  - `calc_data_summary()`, `calc_eval_params()`, `calc_norm_int()`, `calc_recalibrate()`,
  `convert_molecular_formula_to_data_table()`, `create_custom_formula_library()`, `eval_isotopes()`,
  `eval_isotopes()`, `order_columns()`, filter functions, and plotting functions were all modified 
  to match the new isotope nomenclature.
  - `calc_data_summary()`: hard-coded conversion of `i_magnitude` column to data type `numeric`.
  - `calc_dbe()` now stops if the valence of an element in the formula is not provide. 
  Function modified to match new isotope nomenclature.
  - `convert_data_table_to_molecular_formulas` has a new argument `keep_element_sums`
  that provides columns for the count of atoms for each element 
  (sum of isotope counts such as 'C_tot'). Function modified to match new isotope nomenclature.
  
#### Other
  - Documentations and Vignette updated.
  - All functions that perform calculations are now summarized in the function 
    family 'calculations'.
  - Unit tests added.

### Version 1.3.1 (2025-09-03)
#### Bug fixes
  - Fixed a bug in `assign_formulas()` that was introduced in version 1.2.1 
    (April 7 2025).

#### Function updates
  - `convert_molecular_formula_to_data_table`: nominal mass (nm) is now also returned.
  - `calc_exact_mass`: Now returns a single numeric vector. If `mfd` is a 
    character value, it is interpreted as a molecular formula and evaluated:
    - `calc_exact_mass("C2H4")` returns 28.031300129.
  - `calc_nm`: Now always returns a single numeric vector. If `mfd` is a 
  character value, it is interpreted as a molecular formula and evaluated:
    - `calc_nm("C2H4")` returns 28.
  - `uplot_ms()`: the column specified by the argument `label` is now internally
  converted by as.factor().
  
#### Data update
  - `ume::peaklist_demo`: integer column `file_id` added
    (to be consistent with changes in version 0.2.4. 
    Column `file` now contains the names of the MS spectra.
    Columns `m_min`, `m_max`, and `m` were removed because they can be 
    calculated using `calc_neutral_mass()` and `calc_ma_abs()`.

- Other:
  - Vignette updated
  - Documentation of package data updated
  
### Version 1.3.0
#### Function updates
  - `assign_formulas()`: The arguments `memory_efficient` (FALSE / TRUE) and 
    `chunk_n` (number of peaks in each chunk) allow processing in chunks to be 
    more memory efficient.  
  - `remove_blanks()`: if a column for retention time is detected or provided
    (via the `ret_time_col` argument), blanks will be removed only for a given 
    retention time and not for the entire spectrum. 
    The argument `LCMS` is deprecated.
  - `main_docu.R` updated.

### Version 1.2.2
- internal table known_mf updated (corrected one false formula from Hertkorn paper)
- calc_dbe now also accepts molecular formula strings or character vectors as input

### Version 1.2.1 
#### Function updates:
    - `calc_recalibrate_ms()`: argument `formula_library` was removed.
      The calibration is now only based on lists of molecular formulas provided 
      either by `calibr_list` or by `custom_calibr_list`.
    - `assign_formulas()` is now much faster for small libraries (n<=10 entries), 
      because the peaklist is pre-filtered before matching with the library.
    - `identify_isotope_columns()`: column names "sn" and "sc" are explicitly 
      excluded to avoid confusions with element names.
- Unit tests added.

### Version 1.2
#### New Functions
- `identify_isotope_columns()`: Apply this function to a data.table 
  to identify columns that have element or isotope information.
- `convert_data_table_to_molecular_formulas()`: Create molecular formula strings
  for a table that has element or isotope information.
- `create_ume_formula_library()` completely renovated. 
  The function now excepts any element and isotope by providing two molecular 
  formulas for the upper and lower limit of each isotope in the final library.

- Functions `calc_dbe()`, `calc_nm()`, `calc_exact_mass()` now consider all 
  element and isotopes and a flexible usage of spelling.
- Major update in package documentation: The internal helper 
  function `main_docu()` documents arguments that occur in 
  many `ume` functions (@inheritParams main_docu).


### Version 1.1.2
#### Function update
- `convert_molecular_formula_to_data_table()` is now fundamentally faster, 
  recognizes isotopes in a formula (square brackets), and also returns 
  the exact mass of a formula. The function can now be used to build 
  small custom formula libraries.
#### Minor changes
- `assign_formulas()` now checks if all required function arguments are available.

### Version 1.1.1
- Documentation improved.
- All internal function moved to R folder and declared as internal.
- `uplot_cluster()` now supports custom column names.

### Version 1.1.0
#### New and updated plot functions
    - `uplot_cluster()`: Cluster + NMDS function added: `uplot_cluster()`
    - `uplot_pca()`: 
    - `uplot_ms()`: Revised and a `data_reduction` argument was added 
       to accelerate plotting. 
    - `uplot_ratios()`: For comparing peak intensities of molecular formulas 
      between two spectra.
    - `uplot_cvm()`: new                          
    - `uplot_freq()`: new
    - `uplot_freq_ma()`: new
    - `uplot_freq_vs_ppm()`: new
    - `uplot_hc_vs_m()`: new
    - `uplot_heteroatoms()`: new
    - `uplot_isotope_precision()`: update
    - `uplot_kmd()`: new
    - `uplot_layout()`: new
    - `uplot_lcms()`: update
    - `uplot_ma_vs_mz()`: new
    - `uplot_n_mf_per_sample()`: new
    - `uplot_pca()`: new
    - `uplot_ratios()`: new
    - `uplot_reproducibility()`: new
    - `uplot_ri_vs_sample()`: new
    - `uplot_vk()`: update
    - `uplot_ppm_average()`: new
    - `uplot_dbe_vs_o()`: new
    - `uplot_dbe_vs_c()`: new
    - `uplot_dbe_vs_ppm()`: new
    - `uplot_dbe_minus_o()`: new
    - `ustats_outlier()`: moved from `stats.R`
     
- Plot functions are now in separate R file in the repository

### Version 1.0.6
- package `xml2` has been removed as a dependency. It is only required for 
  the function `read_xml_peaklist()`, which now checks for the xml2 package 
  installation specifically.
- `known_mf` updated to UTF-8 encoding.
- Results from functions `assign_formulas()` and `check_formula_library()`
  are now returned invisibly. 

### Version 1.0.5
- Bug fixes
    - `calc_neutral_mass()`: now takes a vector as function argument.
    - `remove_blanks()`: There was an error for LC data.

### Version 1.0.4
- `assign_formulas()` adapted for formula libraries containing only one formula 
  (e.g. when assigning the post-column standard in LCMS)
- `calc_recalibrate_ms()` adapted for formula libraries containing only 
  one formula (e.g. when assigning the post-column standard in LCMS)
- Check added for `ume:::extract_metadata_from_ufz_files()`
- Bug fix solved in `check_formula_library()`
- `calc_recalibrate_ms()` udated because of changes in `calc_neutral_mass()`

### Version 1.0.3
- New plot added for LCMS data (as provided by Dr. Xianyu Kong)
- Post-column standard (Naproxen) added to ume::known_mf.
- Error handling in `add_known_mf()` improved if a molecular formula column 
  is not existing in the source table `mfd`.
- Documentations updated:
    - `add_missing_element_columns()`
    - `assign_formulas()`

### Version 1.0.2
- Vignette updated
- Improvement of internal function `read_xml_files()`. Default `folder_path` 
  now is `NULL`, which opens a dialogue box for folder selection.
- Improvement of internal function `extract_metadata_from_UFZ_files()`. 
  Default `folder_path` now is `NULL`, which opens a dialogue box 
  for folder selection.
- `calc_db()` error handling updated and argument `element_names` added, 
  which handles the style of element / isotope symbols 
  ("lower case" (default) / "upper case")

### Version 1.0.1
- Internal function added to retrieve metadata from UFZ filenames: `extract_metadata_from_UFZ_files()`

#### Notes
- New structure of News.md

### Version 1.0.0
#### BREAKING CHANGES
- First draft version for upload to CRAN
- Unnecessary dependencies removed
- Unit tests added

#### Notes
- Improved documentation for many functions

### Version 0.3.2
- Internal functions separated from package.

### Version 0.3.1
#### Main changes
This version now includes unit tests. 
Following versions will allow that functions can be applied to single values 
and vectors in tables.
-   Updates for `calc_ma()` and `calc_ma_abs()`: 
    -   checks added in functions
    -   unit tests added
    -   application of functions now
-   Dependencies `DT` and `pander` removed from package.

### Version 0.2.20

#### New functions
-   Create a custom molecular formula library from a list of 
    molecular formulas (`create_custom_formula_library()`)
-   Internal function (database access required): 
    Search for molecular formula targets in database (`search_for_mf_target()`)

#### Bugfix 
- `eval_isotopes()`: Provide warning, if there is no isotope information available in molecular formula data.

### Version 0.2.19

-   First implementation of function by Shuxian Gao (UFZ Leipzig) 
    for the elimination of molecular formula multiple assignments.
-   Function added for converting a vector of molecular formula strings 
    into a data.table: `convert_molecular_formulas()`
-   pdf manual added to git repository (<https://gitlab.awi.de/bkoch/ume>)
-   Help documentation for data objects expanded.

### Version 0.2.18

-   Function added for reading a set of xml peaklists and store them into 
    a single data.table: `read_xml_peaklist()`
-   Function added for searching for molecular formulas and molecular masses 
    in MarChem Database: `search_for_mf_target()`

### Version 0.2.17

-   List of metabolome target formulas (courtesy to F. Bussmann) added to ume::known_mf
-   List of formulas added to `ume::known_mf` that indicates photo- 
    and biodegradation (Seibt, 2017; PhD thesis)

### Version 0.2.16

-   Calibration function `calc_recalibrate_ms()` now allows for 
    customized formula lists as reference for calibration.

### Version 0.2.15

-   Diversity calculations added (`calc_shannon_index()`, `calc_simpson_index()`, `calc_pielou_eveness()`).
-   Function for automated recalibration improved.
-   Documentation updated.

### Version 0.2.14

-   Formula assignment procedure updated. The library search now 
    uses `data.table::foverlap()`, which doubles the speed of formula assignment.

### Version 0.2.13

-   First release of function `process_orbi_data()` that reads scans 
    from a list of mzML files and assigns formulas to each scan.

### Version 0.2.12

-   function `create_ume_formula_library()` updated
-   Vignette updated (installation procedure)

### Version 0.2.11

-   Wrapper function for Orbitrap added for internal use

### Version 0.2.10

-   Changes in calibration procedure (`calc_recalibrate_ms.R`):\
    calibr_list was extended by "E_coli_metabolome" for the calibration of metabolome samples

-   Mass accuracy plot (`uplot.freq_ma` in `plot.R`):\
    bug fix: calculation of median and quantile mass accuracies now ignores missing values.

-   Updates in documentation

### Version 0.2.9

-   Calculation of aromaticity modified:\
    `calc_eval_params.R`: AI will only be calculated for molecular formulas 
    in which C \> O + N + P `calc_data_summary.R`: wa(AI) is now calculated 
    from intensity weighted average element numbers.

### Version 0.2.8

-   Automated re-calibration:\
    `calc_recalibrate_ms.R`: At least 5 calibrants must now be detected 
    in an analyses for recalibration. Otherwise the respective analysis (file_id)
    will be removed from the recalibration peaklist.
-   Documentation
-   Minor bug fixes

### Version 0.2.7

-   Added a `NEWS.md` file to track changes to the package.
-   Added a test version of `assign_formulas_new.R` that is more memory 
  efficient and faster (now based on data.table::foverlap())
-   Applied pkgdown to generate a website for the ume package

### Version 0.2.6

-   Major changes in function documentations. Using @inheritDotParams, 
  wrapper functions now have argument descriptions of the sub-functions available.

### Version 0.2.4

-   `file_id` now ALWAYS has to be numeric.
