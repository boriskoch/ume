
utils::globalVariables(c('masses','mz','isotope','valence','label','dbe','mf','mf_id',
                         '12C', '1H', '14N', '16O', '31P', '32S'
                         , 'bl'
                         , 'ume_logo_raster'
                         ,'file_id','peak_id','mass','m','m_min','m_max','category','info1','info2','info3'
                         ,'cas','mw','import_date','ppm_filt','i_magnitude','s_n','res','vkey'
                         ,'m_cal','nm','del','ppm'
                         ,'oc','hc','nc','sc','dbe_o','ai','wf','z','kmd','nosc','delg0_cox'
                         ,'relint13c_calc','int13c_calc','relint34s_calc','int34s_calc','snp_check','nsp_type'
                         ,'co_tot','nsp_tot','n_occurrence_orig','n_assignments_orig','element','symbol'
                         ,'exact_mass','mole_fraction','relative_abundance','valence2','hill_order'
                         ,'last_update','label_id','nice_label','use_in_ume', 'int13c', 'int34s', 'int15n', 'dev_n_c', 'tab_ume_labels', 'surfactant'
                         , '.', 'N', 'print_and_capture'
                         , 'bp', 'rel_int', 'sum_int', 'rel_sum_int', 'n_occurrence', 'sum_int_opt'
                         , 'rel_sum_int_opt', 'sum_int_rank', 'rel_sum_int_rank', 'norm_dat', 'n_assignments'
                         , "grDevices", "col2rgb", "colorRampPalette", "gray"
                         , "rainbow", "rgb", "terrain.colors"
                         , "graphics", "lines", "mtext", "par"
                         , 'viridis', 'inferno', 'nice_labels_dt', 'name_pattern', 'name_substitute'
                         , 'NOP', 'NOPS', 'NOS', 'OPS', 'PSN'
                         , 'create_ume_formula_library'
                         , 'max_oc', 'max_hc', 'max_nc', 'max_pc', 'max_sc', 'min_s', 'max_s', 'min_p'
                         , 'max_p', 'min_n', 'max_n', 'min_h', 'max_h', 'max_c13', 'max_n15', 'max_s34'
                         , 'max_na', "ma_dev", "pol"
                         , 'norm_int_min', 'norm_int_max'
                         , "ret_time_min", "mfd_filt"
                         , "excl_cols", "colnames", "..colnames", "..excl_cols"
                         , 'z_var'
                         , 'n_calibrants'
                         , 'sample_tag', 'intensity'
                         , 'ideg', 'i_magnitude_neg', 'i_magnitude_pos'
                         , 'iterr', 'iterr2'
                         , 'i_magnitude_neg', 'i_magnitude_pos'
                         , 'i_magnitude_sum_neg', 'i_magnitude_sum_pos'
                         , 'i_magnitude_sum_neg2', 'i_magnitude_sum_pos2'
                         , 'i_magnitude_length_neg2', 'i_magnitude_length_pos2'
                         , 'i_magnitude_length_neg', 'i_magnitude_length_pos'
                         , '..cols', 'NEG'
                         , '..col_file_id'
                         , '..label'
                         , 'norm_int'
                         , 'basePeakMZ', 'basePeakIntensity', 'retentionTime', 'polarity'
                         , '.dropEmptyCols','.getNumFromSpectrumId'
                         , '..idvars', 'count', 'count_element'
                         )
)

# utils::globalVariables(c(names(known_mf)))
# utils::globalVariables(names(data("lib_demo")))
# utils::globalVariables(names(data("masses")))
# utils::globalVariables(names(data("mf_data_demo")))
# utils::globalVariables(names(data("tab_ume_labels")))

