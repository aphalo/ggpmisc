sd_section("Package overview", "",
           c("ggpmisc-package")
)

sd_section("Geoms",
  "Geoms, short for geometric objects, describe the type of plot you will produce.",
  c(
    "geom_null",
    "geom_debug"
  )
)

sd_section("Statistics",
  "It's often useful to transform your data before plotting, and that's what statistical transformations do.",
  c(
    "stat_debug_group",
    "stat_debug_panel",
    "stat_dens2d_filter",
    "stat_dens2d_labels",
    "stat_fit_augment",
    "stat_fit_glance",
    "stat_fit_deviations",
    "stat_fit_residuals",
    "stat_poly_eq",
    "stat_peaks"
  )
)

sd_section("Fortify or Try",
  "Convert R objects containing observations in other formats into 'tibble' objects suitable for plotting.",
  c(
    "try_data_frame"
  )
)
