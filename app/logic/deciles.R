#' @export
load_data <- function() {
  box::use(
    arrow[read_parquet],
    here[here]
  )
  read_parquet(here("app", "www", "data", "deciles.parquet"))
}

#' @export
prep_data <- function(data, ord_var, nm) {
  box::use(
    visitalaneysluverds[vnv_convert],
    glue[glue],
    scales[breaks_pretty, percent, number],
    dplyr[mutate, filter],
    stringr[str_c]
  )

  plot_dat <- data |>
    filter(
      order_var == ord_var,
      name %in% nm
    ) |>
    mutate(
      value_perc = value / sum(value),
      .by = c(year, name)
    ) |>
    mutate(
      decile = factor(decile),
      data_id = paste0(year, "_", decile),
      value = value * 1e6,
      value = vnv_convert(value, year),
      value_per_n = value / n,
      tooltip = glue(
        str_c(
          "<b>{year}</b><br>",
          "Tíundarhluti: {decile}<br>",
          "Hlutfall: {percent(value_perc, accuracy = 1, decimal.mark = ',', big.mark = '.')}<br>",
          "Magn: {number(value, scale = 1e-9, accuracy = 10, decimal.mark = ',', big.mark = '.', suffix = ' ma.kr')}<br>",
          "Magn á mann: {number(value_per_n, scale = 1e-6, accuracy = 0.1, decimal.mark = ',', big.mark = '.', suffix = ' m.kr')}"
        )
      )
    )

  return(plot_dat)
}

#' @export
create_decile_plot <- function(plot_dat, nm, ord_var) {
  box::use(
    ggplot2[
      ggplot,
      aes,
      scale_x_continuous,
      scale_y_continuous,
      scale_fill_brewer,
      scale_color_brewer,
      theme,
      labs,
      coord_cartesian
    ],
    ggiraph[
      geom_col_interactive,
      geom_line_interactive,
      geom_point_interactive,
      girafe,
      opts_hover_inv,
      opts_hover
    ],
    scales[breaks_pretty, label_percent, label_number],
    patchwork[wrap_plots, plot_annotation],
    glue[glue]
  )

  p1 <- plot_dat |>
    ggplot(aes(year, value_perc)) +
    geom_col_interactive(
      aes(
        fill = decile,
        tooltip = tooltip,
        data_id = data_id
      ),
      width = 0.95
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = breaks_pretty()
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = label_percent(accuracy = 1)
    ) +
    scale_fill_brewer(
      palette = "RdBu"
    ) +
    theme(
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Skipting"
    )

  p2 <- plot_dat |>
    ggplot(aes(year, value)) +
    geom_col_interactive(
      aes(
        fill = decile,
        tooltip = tooltip,
        data_id = data_id
      ),
      width = 0.95
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = breaks_pretty()
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = label_number(
        scale = 1e-9,
        accuracy = 1,
        decimal.mark = ",",
        big.mark = ".",
        suffix = "ma.kr"
      )
    ) +
    scale_fill_brewer(
      palette = "RdBu"
    ) +
    theme(
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Magn"
    )

  p3 <- plot_dat |>
    ggplot(aes(year, value_per_n)) +
    geom_line_interactive(
      aes(
        group = decile,
        tooltip = tooltip,
        data_id = data_id,
        col = decile
      ),
      linewidth = 1
    ) +
    geom_point_interactive(
      aes(
        tooltip = tooltip,
        data_id = data_id,
        col = decile
      ),
      size = 2
    ) +
    scale_x_continuous(
      expand = c(0.01, 0.01),
      breaks = breaks_pretty()
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = label_number(
        scale = 1e-6,
        accuracy = 1,
        decimal.mark = ",",
        big.mark = ".",
        suffix = "m.kr"
      )
    ) +
    scale_color_brewer(
      palette = "RdBu"
    ) +
    coord_cartesian(
      clip = "off"
    ) +
    theme(
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Á mann"
    )

  design <- "
  111111222222
  333333333333
  "

  p <- wrap_plots(
    p1, p2, p3,
    design = design,
    heights = c(1, 0.9)
  ) +
    plot_annotation(
      title = nm,
      subtitle = glue("Breyta notuð til að skipta í tíundarhluta: {ord_var}"),
      caption = "Sýnt á verðlagi 2024"
    )


  asp <- 0.6
  size <- 12
  girafe(
    ggobj = p,
    width_svg = size,
    height_svg = size * asp,
    options = list(
      opts_hover_inv(css = "opacity:0.1;"),
      opts_hover(css = "stroke-width:2;")
    )
  )
}
