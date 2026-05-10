
![Package Icon](../inst/figures/ume_package_icon.png)

# An R package for UltraMassExplorer (UME)

------------------------------------------------------------------------

👉 [The complete UME manual (Vignette)](/vignettes/vignette_ume.pdf)

https://github.com/boriskoch/ume repository is a public release mirror of UME.  
Active development takes place in AWI GitLab.

## Usage - quick tour

#### Download predefined UME formula library (Zenodo)

The official UME molecular formula libraries (lib_02.rds, lib_05.rds)  
are available at:

👉 https://doi.org/10.5281/zenodo.17606457

You can download them within `ume`:
`lib <- download_library(library = "lib_02.rds")`

#### Molecular formula assignment

`data(peaklist_demo)`

`mfd <- ume_assign_formulas(pl = peaklist_demo, formula_library = lib, pol = "neg", ma_dev = 0.5, verbose = TRUE)`

#### Formula filter process

`mfd_filt <- ume_filter_formulas(mfd = mfd, normalization = "none", c_iso_check = T, dbe_o_max = 10, p_max = 0, s_max = 1, n_max = 2, verbose = TRUE)`

#### Data summary

`ds <- calc_data_summary(mfd_filt)`

#### Visualization examples

Plot mass spectrum

`uplot_ms(pl = peaklist_demo, label = "file")`

Plot van Krevelen diagram

`uplot_vk(mfd_filt)`

Plot evaluation of carbon istope abundance:

`uplot_isotope_precision(mfd_filt)`

------------------------------------------------------------------------

## Installation 

\*\*\*

`install.packages("ume")`


## Create your custom molecular formula library

*Be aware that this step requires a lot of memory and processing time!*

`lib <- create_ume_formula_library(max_formula = "C1000[13C1]H3000[15N1]N6O1000P3S3[34S1]", min_formula = "C1H1", max_mass = 250, heu_filter = TRUE, verbose = T)`
