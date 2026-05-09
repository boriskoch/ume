# Spreadsheet 10: "Ratios" ========================================
# _____________________________________________________________
# Differential mass spectrum ####
# _____________________________________________________________

uplot_diff_ms<-function(dfm=dfm, sample, control, z_var="NOSC", subset="non-uniques")
{
  # if(subset=="non-uniques"){dfm<-dfm[get(sample)!=0 & get(control)!=0,]}
  # if(subset=="uniques"){dfm<-dfm[get(sample)==0 | get(control)==0,]}
  # dfm[,zz:=get(z_var)]
  # setorder(dfm, zz)
  # dfm[,diff:=get(sample)-get(control)]
  # dfm[,urls:=paste0("https://pubchem.ncbi.nlm.nih.gov/search/#collection=compounds&query_type=mf&query=", MF, "&mw_gte=",floor(M_cal), "&mw_lt=", ceiling(M_cal))]
  # tooltips <- paste("MF: <strong>", dfm$MF,"</strong><br />")
  # scatterD3(data = dfm,
  #           x=M_cal,
  #           y=diff,
  #           xlab = "Calculated mass (Da)",
  #           ylab = "Intensity (Sample-Control)",
  #           col_var = zz,
  #           col_lab = z_var,
  #           url_var = urls,
  #           lasso=T,
  #           tooltip_text = tooltips,
  #           transitions = T,
  #           menu = F,
  #           # axes_font_size = "120%",
  #           # legend_font_size = "14px",
  #           point_opacity = 0.5,
  #           hover_opacity = 1
  # )
}
