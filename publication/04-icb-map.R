## Script to create map for breakdown of england into regions,
# ICBs and Trusts for winter 2025/24 paper

library(sf)
library(patchwork)


box::use(
  box / redshift,
  box / themes,
  prj / projection_plots
)

ggplot2::theme_set(
  projection_plots$theme_pancasts() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12),
      text = ggplot2::element_text(size = 10)
    )
)

plot_output_dir <- fs::dir_create(here::here("publication/plots"))
plot_output_dir_tiff <- fs::dir_create(here::here(plot_output_dir, "tiff"))
plot_supplement_dir_tiff <- fs::dir_create(here::here(plot_output_dir_tiff, "supplement"))

rs <- redshift$data_model(c("REDACTED", "REDACTED"))

trusts <- rs$REDACTED |>
  # bring in each trusts population
  dplyr::left_join(rs$REDACTED, by = c("code" = "trust_code")) |>
  # associate a postcode to a coordinate
  # subset to only trusts we care about
  dplyr::mutate(type_hosp = type, type = "trust") |>
  dplyr::filter(is_active, type_hosp == "acute")

icb_pop <- openxlsx::read.xlsx(
  "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/clinicalcommissioninggroupmidyearpopulationestimates/mid2011tomid2022integratedcareboards2023geography/sapeicb202320112022.xlsx",
  sheet = 16
) |>
  tibble::tibble() |>
  dplyr::slice(-c(1:2)) |>
  janitor::row_to_names(1) |>
  janitor::clean_names() |>
  dplyr::select(
    "icb23cd" = icb_2023_code,
    "population" = total
  ) |>
  dplyr::mutate(population = as.numeric(population))

country <- rs$REDACTED |>
  dplyr::filter(ctry24nm %in% c("Scotland", "Wales")) |>
  dplyr::mutate(type = "country") |>
  dplyr::collect() |>
  sf::st_sf(crs = "epsg:4326")

# load ICB polygons
icbs <- rs$REDACTED |>
  dplyr::collect() |>
  sf::st_as_sf() |>
  dplyr::mutate(type = "ICB") |>
  sf::st_set_crs(sf::st_crs(country)) |>
  # add in our population for plotting
  dplyr::left_join(icb_pop, by = dplyr::join_by(icb23cd == icb23cd))

# load regions polygons
regions <- rs$REDACTED |>
  dplyr::collect() |>
  sf::st_as_sf() |>
  dplyr::mutate(type = "region") |>
  sf::st_set_crs(sf::st_crs(country))

# define our label sizes for population legend
labels_legend <- c("0 - 100k", "100k - 200k", "200k - 300k", "300k - 400k")

combined <- dplyr::bind_rows(
  country,
  icbs,
  regions
) |>
  # downscale to per 100k
  dplyr::mutate(
    # bin up to look nicer on map
    bins = dplyr::case_when(
      population < 500000 ~ "0 - 500K",
      population < 1000000 ~ "500K - 1M",
      population < 1500000 ~ "1M - 1.5M",
      population < 2000000 ~ "1.5M - 2M",
      population >= 2000000 ~ "2M+",

      .default = NA
    ),

    bins = ordered(bins, levels = c("0 - 500K", "500K - 1M", "1M - 1.5M", "1.5M - 2M", "2M+")),

    population = population / 100000
  )


labels_legend <- unique(sort(combined$bins))
nation_plot <- country |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = combined |> dplyr::filter(type == "country"),
    ggplot2::aes(geometry = geometry),
    color = "grey10",
    fill = "grey95"
  ) +
  ggplot2::geom_sf(
    data = combined |> dplyr::filter(type == "ICB"),
    ggplot2::aes(geometry = geometry, fill = bins, linewidth = "Integrated Care\nBoard (ICB)"),
    color = "grey10"
  ) +
  ggplot2::geom_sf(
    data = combined |> dplyr::filter(type == "region"),
    ggplot2::aes(geometry = geometry, linewidth = "NHS Region"),
    color = "grey10",
    fill = NA
  ) +
  ggplot2::scale_shape_manual(
    name = NULL,
    values = c("NHS Trust" = 21)
  ) +
  ggplot2::scale_linewidth_manual(
    name = NULL,
    values = c("NHS Region" = 0.8, "Integrated Care\nBoard (ICB)" = 0.1),
    guide = ggplot2::guide_legend(override.aes = list(linewidth = c(0.8, 0.2)))
  ) +
  ggplot2::coord_sf(crs = "epsg:27700", ylim = c(0, 700000), xlim = c(100000, 700000)) +
  ggplot2::scale_fill_brewer(name = "ICB catchment\npopulation", palette = "Greens") +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    legend.box = "vertical",
    panel.grid = ggplot2::element_blank()
  ) +
  ggplot2::ggtitle(
    "NHS Regions and ICBs within England",
    "Scotland and Wales are not within the catchment area"
  ) +
  ggplot2::guides(
    linewidth = ggplot2::guide_legend(order = 2, title = "Spatial granularity"),
    shape = ggplot2::guide_legend(order = 1)
  )

nation_plot


london_plot <- combined |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = combined |>
      dplyr::filter(type == "ICB", stringr::str_detect(icb23nm, "(?i)(london)")),
    ggplot2::aes(geometry = geometry, fill = bins, linewidth = "Integrated Care\nBoard (ICB)"),
    color = "grey10"
  ) +
  ggplot2::scale_shape_manual(name = NULL, values = c("NHS Trust" = 21)) +
  ggplot2::scale_linewidth_manual(name = NULL, values = c("NHS Region" = 0.9, "Integrated Care\nBoard (ICB)" = 0.5)) +
  ggplot2::coord_sf(crs = "epsg:27700", ylim = c(155000, 205000), xlim = c(493000, 565000)) +
  ggplot2::scale_fill_brewer(name = "ICB catchment\npopulation", palette = "Greens") +
  ggplot2::guides(linewidth = "none", shape = "none", fill = "none") +
  ggplot2::ggtitle("London") +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    legend.box = "vertical",
    plot.title = ggplot2::element_text(size = 10),
    panel.grid = ggplot2::element_blank()
  )

london_plot

whole_plot <- nation_plot + patchwork::inset_element(london_plot, 0.6, 0.7, 1.05, 0.92)

whole_plot

ggplot2::ggsave(whole_plot, filename = here::here("publication/plots/nhs_map.png"), width = 9, height = 9)

ggplot2::ggsave(
  filename = here::here(plot_supplement_dir_tiff, "fig_f.tiff"),
  plot = whole_plot,
  width = 16,
  height = 16,
  dpi = 300,
  units = "cm"
)
