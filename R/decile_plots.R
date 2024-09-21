library(tidyverse)
library(metill)
library(ggiraph)
library(ggh4x)
library(glue)
library(patchwork)
library(visitalaneysluverds)

theme_set(theme_metill())

d <- read_parquet("data/deciles.parquet")

ord_var <- "Ráðstöfunartekjur"
nm <- "Ráðstöfunartekjur (Tekjur - Skattar)"

plot_dat <- d |>
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
        "Hlutfall: {scales::percent(value_perc, accuracy = 1)}<br>",
        "Magn: {metill::isk(value, scale = 1e-9)}<br>",
        "Magn á mann: {metill::isk(value_per_n, scale = 1e-6)}"
      )
    )
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
    labels = scales::label_percent(accuracy = 1)
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
    labels = metill::label_isk(scale = 1e-9)
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
    labels = metill::label_isk(scale = 1e-6)
  ) +
  scale_color_brewer(
    palette = "RdBu"
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
#3333333333#
"

p <- p1 + p2 + p3 +
  plot_layout(
    design = design,
    heights = c(1, 0.9)
  ) +
  plot_annotation(
    title = nm,
    subtitle = glue("Breyta notuð til að skipta í tíundarhluta: {ord_var}"),
    caption = "Sýnt á verðlagi 2024"
  )

girafe(
  ggobj = p,
  width_svg = 10,
  height_svg = 10,
  options = list(
    opts_hover_inv(css = "opacity:0.1;"),
    opts_hover(css = "stroke-width:2;")
  )
)
