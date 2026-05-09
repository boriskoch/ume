#' Add the UME package logo to a ggplot
#'
#' @description
#' Inserts the UME package logo (stored in `inst/figures/ume_package_icon.png`)
#' into a ggplot at the bottom-right inside the plotting panel.
#'
#' The logo is drawn in **absolute device units (mm)** which guarantees:
#'
#' * consistent physical size across output devices,
#' * correct (non-distorted) aspect ratio,
#' * no expansion of axis limits, and
#' * predictable placement within the plot.
#'
#' The logo is added via a `rasterGrob()` positioned using **panel-relative NPC
#' coordinates** for reliable placement independent of data scale.
#'
#' @param p A ggplot object to which the UME logo should be added.
#' @param width_mm Numeric. Logo width in millimetres.
#'   The height is computed automatically using the PNG's native aspect ratio.
#' @param x_npc_logo Numeric in `[0, 1]`. Horizontal position of the logo inside
#'   the plot panel, expressed as NPC (normalized parent coordinates).
#'   `1` = right edge, `0` = left edge.
#' @param y_npc_logo Numeric in `[0, 1]`. Vertical position of the logo inside
#'   the plot panel.
#'   `0` = bottom (x-axis), `1` = top.
#' @import grid
#' @return A modified ggplot object containing the UME logo as an annotation.
#' @details
#' Unlike `annotation_raster()` or `annotation_custom()` with data units, this
#' implementation uses a custom `rasterGrob()` inside a viewport that specifies
#' the logo's size in **mm**, not in data space.
#' This ensures that:
#'
#' * the logo's **aspect ratio is preserved**,
#' * the logo **never distorts** when resizing the plot window,
#' * the logo stays correctly positioned relative to the plotting region, and
#' * axis limits **remain unchanged**.
#'
#' This approach is robust across PNG/PDF output, facets, different aspect
#' ratios, and multi-panel arrangements.
#' @keywords internal

uplots_add_ume_logo <- function(
    p,
    width_mm = 12,
    x_npc_logo = 1.03,
    y_npc_logo = 0.03,
    ...
) {

  logo_img <- ume_logo_raster   # loaded from data/

  # aspect ratio preserved
  ar <- dim(logo_img)[1] / dim(logo_img)[2]
  height_mm <- width_mm * ar

  logo_grob <- grid::rasterGrob(
    logo_img,
    x      = grid::unit(x_npc_logo, "npc"),
    y      = grid::unit(y_npc_logo, "npc"),
    width  = grid::unit(width_mm, "mm"),
    height = grid::unit(height_mm, "mm"),
    just   = c("right", "bottom"),
    interpolate = TRUE
  )

  p +
    ggplot2::annotation_custom(
      grob = logo_grob,
      xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = Inf
    ) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 10, r = 55, b = 20, l = 10)
    )
}

#' Add vertical UME branding label to a ggplot (internal helper)
#'
#' @description
#' Adds the vertical, italic **UltraMassExplorer** branding label to the
#' right-hand side of a ggplot.
#'
#' The label is positioned **outside the plot panel** using NPC coordinates
#' (normalized parent coordinates) and is therefore independent of data scale.
#' It does **not** affect axis limits.
#'
#' @param p A ggplot object to annotate.
# @param label Character string. Text to draw vertically.
#   Default: `"UltraMassExplorer"`.
#' @param colour Hex colour string for the label.
#'   Default: UME brand colour `"#00ACE9"`.
#' @param text_size Numeric font size (in points).
#' @param x_npc_label Horizontal offset outside the panel (NPC units).
#'   `0` = at panel border, positive values move right.
#' @param y_npc_label Vertical offset (NPC units).
#'   Default = `0` (aligned to bottom of panel); positive moves label upward.
#'
#' @import grid
#' @return A ggplot object with the branding label added.
#'
#' @keywords internal

uplots_add_ume_label <- function(
    p,
    label = "UltraMassExplorer",
    colour = "#00ACE9",
    text_size = 8,
    x_npc_label = 0.03,
    y_npc_label = 0.05
) {
  brand_grob <- grid::textGrob(
    label,
    x = grid::unit(1 + x_npc_label, "npc"),
    y = grid::unit(y_npc_label, "npc"),
    just = "left",
    hjust = 0,
    vjust = 0,
    rot = 90,
    gp = grid::gpar(
      fontsize = text_size,
      fontface = "italic",
      col = colour
    )
  )

  p +
    ggplot2::annotation_custom(
      grob = brand_grob,
      xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = Inf
    ) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 10, r = 40, b = 10, l = 10))
}



#' Map internal UME variable names to human-readable labels
#'
#' @description
#' Internal helper that replaces variable names used inside UME (e.g. `"oc"`,
#' `"rel_int"`, `"n_assignments"`, `"wa(O/C)"`, etc.) with corresponding
#' human-readable labels based on the lookup table `nice_labels_dt`.
#'
#' Intended for internal use by UME plotting functions. Not exported.
#'
#' @param p A ggplot object.
#' @param x Optional string. Name of the x variable to relabel.
#' @param y Optional string. Name of the y variable to relabel.
#' @param colour Optional string. Name used for legend color scaling.
#' @param fill Optional string. Name used for fill aesthetics.
#' @param size Optional string. Name used for size legend.
#' @param label_table Data table containing `name_pattern` (regex) and
#'   `name_substitute` (replacement).
#'   Defaults to `ume::nice_labels_dt`.
#'
#' @return A ggplot object with updated labels where matched.
#'
#' @keywords internal

uplots_map_labels <- function(
    p,
    x = NULL, y = NULL, colour = NULL, fill = NULL, size = NULL,
    label_table = ume::nice_labels_dt
) {
  map_one <- function(var) {
    if (is.null(var)) return(NULL)
    hit <- label_table[name_pattern == var, name_substitute]
    if (length(hit) == 0) var else hit[1]
  }

  p + ggplot2::labs(
    x      = map_one(x),
    y      = map_one(y),
    colour = map_one(colour),
    fill   = map_one(fill),
    size   = map_one(size)
  )
}


#' Internal wrapper for consistent UME plot styling
#'
#' @description
#' This internal helper applies the unified UME visual appearance to a ggplot
#' object, including:
#'
#' * the `theme_uplots()` theme,
#' * consistent colour palette replacement for the `colour` aesthetic,
#' * optional axis/legend label mapping via `uplots_map_labels()`,
#' * optional title handling,
#' * optional UME branding (vertical label + logo),
#' * consistent dot-size handling,
#' * optional conversion to an interactive Plotly object.
#'
#' It is used by all `uplot_*()` functions and is **not exported**.
#'
#' @inheritParams main_docu
#' @inheritParams uplots_add_ume_logo
#' @inheritParams uplots_add_ume_label
#' @param p A ggplot object created by one of the `uplot_*()` functions.
#' @param title Optional character string. Plot title. Set `title_show = FALSE`
#'   to suppress the title entirely.
#' @param title_show Logical. Display the plot title? Default: `TRUE`.
#' @param title_size Numeric. Font size of the title (points).
#' @param ume_logo Logical. Add the UME package logo? Default: `TRUE`.
#' @param ume_label Logical. Add the vertical UME branding label?
#'   Default: `TRUE`.
#' @param map_labels A list specifying which variables should get mapped to
#'   human-readable labels using `uplots_map_labels()`. Expected elements:
#'   `x`, `y`, `colour`, `fill`, `size`. May be `NULL` to suppress mapping.
#'
#' @param p A ggplot object created by a uplot_* function.
#' @param palname Colour palette name passed to f_colorz().
#' @param col_bar Logical. Show colour bar?
#' @param colour_scale Character. Controls how the colour aesthetic is handled.
#'   One of:
#'   \itemize{
#'     \item `"auto"` (default): Automatically chooses a continuous scale for numeric
#'       variables and a discrete scale for categorical variables.
#'     \item `"continuous"`: Forces a continuous colour scale.
#'     \item `"discrete"`: Forces a discrete colour scale.
#'     \item `"none"`: Do not modify the colour scale.
#'   }
# @param x_integer Logical. If `TRUE`, the x-axis is forced to use integer
#   break values based on the data range. This is particularly useful for
#   variables that are inherently discrete but stored as numeric values
#   (e.g. number of carbon atoms).
#
# @param y_integer Logical. If `TRUE`, the y-axis is forced to use integer
#   break values based on the data range. This is useful for discrete-derived
#   quantities such as DBE or frequency counts.
#' @param x_npc_logo,y_npc_logo NPC coordinates for logo placement.
#' @param x_npc_label,y_npc_label NPC coordinates for label placement.
# @param plotly Logical. Return plotly object?
#' @param interactive Logical. Return plotly object?
#' @param ... Additional arguments:
#'   - base_size, base_family (theme)
#'   - size_dots
#'
#' @keywords internal
#' Internal wrapper for consistent UME plot styling
#'
#' @keywords internal
uplot_wrapper <- function(
    p,
    title       = NULL,
    title_show  = TRUE,
    title_size  = 14,
    text_size   = 8,
    size_dots   = 1,
    # x_integer = FALSE,
    # y_integer = FALSE,
    palname     = "viridis",
    fun_label   = NULL,        # semantic label, e.g. "median"
    col_bar     = TRUE,
    colour_scale = c("auto", "continuous", "discrete", "none"),
    ume_logo    = TRUE,
    ume_label   = FALSE,
    x_npc_logo  = 1.11,
    y_npc_logo  = 0.03,
    x_npc_label = 0.03,
    y_npc_label = 0.08,
    map_labels  = NULL,
    plotly      = FALSE,
    interactive = FALSE,
    ...
) {

  dots <- list(...)

  colour_scale <- match.arg(colour_scale)

  # does the plot actually use a colour aesthetic?
  has_colour <- !is.null(p$mapping$colour) &&
    !is.null(map_labels) &&
    !is.null(map_labels[["colour"]])

  # actual plotted colour variable (e.g. "zz" or "file_id")
  colour_var_plot <- NULL
  is_continuous_colour <- FALSE

  if (has_colour) {
    colour_var_plot <- all.vars(p$mapping$colour)[1]

    if (!is.null(colour_var_plot) &&
        colour_var_plot %in% names(p$data)) {
      is_continuous_colour <- is.numeric(p$data[[colour_var_plot]])
    }
  }

  # resolve colour scale automatically if requested
  if (colour_scale == "auto") {
    colour_scale_use <- if (is_continuous_colour) "continuous" else "discrete"
  } else {
    colour_scale_use <- colour_scale
  }

  # ============================================================================
  # 0. COLOUR SCALE LABEL (base label + aggregation suffix)
  # ============================================================================

  if (has_colour) {

    # base label from nice_labels_dt, based on the semantic variable name
    colour_base_label <- uplots_map_labels(
      ggplot2::ggplot(),
      colour = map_labels$colour
    )$labels$colour

    if (length(colour_base_label) == 0 || !nzchar(colour_base_label)) {
      colour_base_label <- NULL
    }

    # aggregation suffix (already semantic)
    colour_label_suffix <- if (!is.null(fun_label) && nzchar(fun_label)) {
      paste0("\n(", fun_label, ")")
    } else {
      ""
    }

    scale_label <- if (!is.null(colour_base_label)) {
      paste0(colour_base_label, colour_label_suffix)
    } else {
      ggplot2::waiver()
    }

  } else {
    scale_label <- ggplot2::waiver()
  }


  # ============================================================================
  # 1. THEME
  # ============================================================================
  theme_args <- c("base_size", "base_family")
  theme_dots <- dots[names(dots) %in% theme_args]

  p <- p + do.call(theme_uplots, theme_dots)

  # ============================================================================
  # 2. TITLE
  # ============================================================================
  if (isTRUE(title_show) && !is.null(title) && nzchar(title)) {
    p <- p +
      ggplot2::ggtitle(title) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = title_size)
      )
  }

  # ============================================================================
  # 3. COLOUR SCALE (single, authoritative definition)
  # ============================================================================
  if (has_colour) {

    # remove existing colour scales to avoid warnings
    p$scales$scales <- Filter(
      function(s) !("colour" %in% s$aesthetics),
      p$scales$scales
    )

    if (colour_scale_use == "continuous") {
      p <- p + ggplot2::scale_color_gradientn(
        colours = f_colorz(
          seq(0, 1, length.out = 256),
          palname = palname,
          return_palette = TRUE
        ),
        name  = scale_label,
        guide = if (isTRUE(col_bar)) "colourbar" else "none"
      )
    }

    if (colour_scale_use == "discrete") {
      p <- p + ggplot2::scale_color_viridis_d(
        name  = scale_label,
        guide = if (isTRUE(col_bar)) "legend" else "none"
      )
    }

    if (colour_scale_use == "none") {
      # do nothing
    }
  }

  # ============================================================================
  # 4. AXIS / LEGEND LABEL MAPPING
  # ============================================================================
  if (!is.null(map_labels)) {
    p <- uplots_map_labels(
      p,
      x      = map_labels$x,
      y      = map_labels$y,
      colour = map_labels$colour,
      fill   = map_labels$fill,
      size   = map_labels$size
    )
  }

  # ============================================================================
  # 4b. INTEGER-LIKE AXES (auto)
  # ============================================================================
  auto_integer_axis <- function(values) {
    values <- values[is.finite(values)]
    if (length(values) == 0) return(FALSE)

    all(abs(values - round(values)) < .Machine$double.eps^0.5)
  }

  integer_breaks <- function(values, n_target = 6) {
    values <- values[is.finite(values)]
    if (length(values) == 0) return(NULL)

    vmin <- floor(min(values))
    vmax <- ceiling(max(values))
    span <- vmax - vmin

    if (span <= 0) return(vmin)

    # gewünschte Schrittweite
    raw_step <- span / n_target

    # schöne ganzzahlige Schrittweiten
    candidates <- c(1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000)
    step <- candidates[which.min(abs(candidates - raw_step))]

    seq(vmin, vmax, by = step)
  }

  # ---- X axis ----
  x_var <- all.vars(p$mapping$x)[1]
  if (!is.null(x_var) && x_var %in% names(p$data)) {

    xvals <- p$data[[x_var]]

    if (is.numeric(xvals) && auto_integer_axis(xvals)) {
      p <- p + ggplot2::scale_x_continuous(
        breaks = integer_breaks(xvals)
      )
    }
  }

  # ---- Y axis ----
  y_var <- all.vars(p$mapping$y)[1]
  if (!is.null(y_var) && y_var %in% names(p$data)) {

    yvals <- p$data[[y_var]]

    if (is.numeric(yvals) && auto_integer_axis(yvals)) {

      if (!is.null(map_labels) && identical(map_labels$y, "N")) {

        breaks_y <- integer_breaks(yvals)

        # 👉 sicherstellen, dass 0 enthalten ist
        breaks_y <- sort(unique(c(0, breaks_y)))

        p <- p + ggplot2::scale_y_continuous(
          breaks = breaks_y,
          limits = c(0, NA),
          expand = ggplot2::expansion(mult = c(0, 0.05))
        )

      } else {
        p <- p + ggplot2::scale_y_continuous(
          breaks = integer_breaks(yvals)
        )
      }
    }
  }

  # ============================================================================
  # 5. POINT SIZE HANDLING
  # ============================================================================
  for (i in seq_along(p$layers)) {
    layer <- p$layers[[i]]
    if (inherits(layer$geom, "GeomPoint") &&
        is.null(layer$aes_params$size)) {
      layer$aes_params$size <- size_dots
    }
  }

  # ============================================================================
  # 6. BRANDING LABEL
  # ============================================================================
  if (isTRUE(ume_label)) {
    p <- uplots_add_ume_label(
      p,
      text_size   = text_size,
      x_npc_label = x_npc_label,
      y_npc_label = y_npc_label
    )
  }

  # ============================================================================
  # 7. BRANDING LOGO
  # ============================================================================
  if (isTRUE(ume_logo)) {
    p <- uplots_add_ume_logo(
      p,
      x_npc_logo = x_npc_logo,
      y_npc_logo = y_npc_logo
    )
  }

  # ============================================================================
  # 8. ALLOW OUTSIDE PANEL DRAWING
  # ============================================================================
  p <- p + ggplot2::coord_cartesian(clip = "off")

  # ============================================================================
  # 9. PLOTLY OUTPUT + HOVER TEXT
  # ============================================================================
  if (isTRUE(interactive)) {

    df <- p$data

    hover_vars <- character(0)
    if (!is.null(map_labels)) {
      hover_vars <- unlist(map_labels, use.names = FALSE)
    }

    # always include aggregation count if present
    hover_vars <- unique(c(hover_vars, "N"))
    hover_vars <- intersect(hover_vars, names(df))

    if (length(hover_vars) > 0) {

      get_nice_label <- function(x) {
        hit <- nice_labels_dt[name_pattern == x, name_substitute]
        if (length(hit) == 0) x else hit[1]
      }

      hover_text <- apply(
        df[, hover_vars, with = FALSE],
        1,
        function(row) {
          paste(
            mapply(
              function(nm, val)
                sprintf("%s: %s", get_nice_label(nm), val),
              names(row), row,
              SIMPLIFY = TRUE
            ),
            collapse = "<br>"
          )
        }
      )

      p <- p + ggplot2::aes(text = hover_text)
    }

    # remove annotations before ggplotly
    p$layers <- Filter(
      function(l) !inherits(l$geom, "GeomCustomAnn"),
      p$layers
    )

    return(plotly::ggplotly(p, tooltip = "text"))
  }

  p
}




.uplots_nice_label <- function(var, label_table = ume::nice_labels_dt) {
  if (is.null(var)) return(NULL)
  hit <- label_table[name_pattern == var, name_substitute]
  if (length(hit) == 0) var else hit[1]
}

.safe_hover_data <- function(dt, cols) {
  cols <- intersect(cols, names(dt))
  if (length(cols) == 0) return(NULL)
  dt[, ..cols]
}

# internal helper
.uplots_logo_base64 <- function() {
  img_path <- system.file("figures", "ume_package_icon.png", package = "ume")
  if (!nzchar(img_path)) return(NULL)

  bin <- readBin(img_path, "raw", file.info(img_path)$size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(bin))
}

# Make sure that mfd in plots is the correct object
.input_require_mfd <- function(mfd, fun_name) {
  if (!inherits(mfd, "mfd")) {
    stop(
      sprintf(
        "%s expects an object of class 'mfd'. Use as_mfd() to convert your data.",
        fun_name
      ),
      call. = FALSE
    )
  }
}

# Set bin width for histogram plots
.uplots_binwidth_fd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)

  iqr <- stats::IQR(x)
  if (iqr == 0) return(NA_real_)

  2 * iqr / length(x)^(1/3)
}


