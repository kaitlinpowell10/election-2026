build_county_map <- function(election, height = "700px") {
  county_results <- election |>
    dplyr::arrange(FIPS, dplyr::desc(votes)) |>
    dplyr::group_by(FIPS, county) |>
    dplyr::summarise(
      total_votes = sum(votes),
      tied = dplyr::n() > 1L && votes[[1]] == votes[[2]],
      winner_candidate = if (tied) "Tie" else candidate[[1]],
      winner_party = if (tied) "Unresolved" else party[[1]],
      winner_votes = if (tied) NA_real_ else votes[[1]],
      winner_pct = if (tied) NA_real_ else vote_percentage[[1]],
      runner_up = if (dplyr::n() > 1L) candidate[[2]] else NA_character_,
      margin_votes = if (tied || dplyr::n() < 2L) NA_real_ else
        votes[[1]] - votes[[2]],
      margin_pp = if (tied || dplyr::n() < 2L) NA_real_ else
        vote_percentage[[1]] - vote_percentage[[2]],
      reporting_timestamp = dplyr::first(reporting_timestamp),
      candidate_details = paste0(
        paste0(
          "<div class='popup-result-row'><span>",
          htmltools::htmlEscape(candidate),
          "</span><strong>",
          format(votes, big.mark = ",", scientific = FALSE),
          " · ",
          sprintf("%.1f%%", vote_percentage),
          "</strong></div>"
        ),
        collapse = ""
      ),
      .groups = "drop"
    )

  county_boundaries <- local({
    boundary_dir <- tempfile("ca-county-boundaries-")
    dir.create(boundary_dir)
    on.exit(unlink(boundary_dir, recursive = TRUE))
    utils::unzip(
      "data/raw/boundaries/cb_2025_us_county_20m.zip",
      exdir = boundary_dir
    )
    sf::st_read(
      file.path(boundary_dir, "cb_2025_us_county_20m.shp"),
      quiet = TRUE
    ) |>
      dplyr::filter(STATEFP == "06") |>
      dplyr::transmute(FIPS = GEOID, boundary_county = NAME)
  })

  if (nrow(county_boundaries) != 58L ||
      nrow(county_results) != 58L ||
      anyDuplicated(county_boundaries$FIPS) ||
      anyDuplicated(county_results$FIPS) ||
      !setequal(county_boundaries$FIPS, county_results$FIPS) ||
      any(sf::st_is_empty(sf::st_geometry(county_boundaries)))) {
    stop("County geometry join failed: expected 58 unique California FIPS matches.")
  }

  county_map <- county_boundaries |>
    dplyr::left_join(county_results, by = "FIPS") |>
    sf::st_transform(4326)

  if (anyNA(county_map$county) ||
      any(county_map$county != county_map$boundary_county)) {
    stop("County name and FIPS cross-check failed for the boundary join.")
  }

  blue_scale <- grDevices::colorRampPalette(c("#DCEAF0", "#24566F"))(100)
  red_scale <- grDevices::colorRampPalette(c("#F2E0E0", "#8D3742"))(100)
  max_margin <- max(county_map$margin_pp, na.rm = TRUE)
  county_map$fill_color <- vapply(seq_len(nrow(county_map)), \(i) {
    if (is.na(county_map$winner_party[[i]])) {
      return("#D8DCDE")
    }

    if (county_map$winner_party[[i]] == "Democratic") {
      scale <- blue_scale
    } else if (county_map$winner_party[[i]] == "Republican") {
      scale <- red_scale
    } else if (county_map$winner_party[[i]] == "Unresolved") {
      return("#D8DCDE")
    } else {
      return("#8C7654")
    }

    intensity <- if (max_margin > 0) {
      county_map$margin_pp[[i]] / max_margin
    } else {
      0
    }
    scale[max(1L, min(100L, 1L + round(99 * intensity)))]
  }, character(1))

  county_map$hover_label <- ifelse(
    is.na(county_map$winner_votes),
    paste0(county_map$county, " County · no resolved lead"),
    paste0(
      county_map$county, " County · ", county_map$winner_candidate,
      " (", county_map$winner_party, ") · ",
      sprintf("%.1f", county_map$margin_pp), " pp"
    )
  )

  county_map$popup_html <- vapply(seq_len(nrow(county_map)), \(i) {
    county <- htmltools::htmlEscape(county_map$county[[i]])
    if (is.na(county_map$winner_votes[[i]])) {
      lead <- "Tie or unresolved result"
      margin <- "No resolved margin"
    } else {
      lead <- paste0(
        htmltools::htmlEscape(county_map$winner_candidate[[i]]), " · ",
        htmltools::htmlEscape(county_map$winner_party[[i]])
      )
      margin <- paste0(
        format(county_map$margin_votes[[i]], big.mark = ","),
        " votes · ", sprintf("%.2f percentage points", county_map$margin_pp[[i]])
      )
    }

    paste0(
      "<div class='county-popup'><p class='popup-kicker'>",
      county, " County</p><p class='popup-lead'>", lead,
      "</p><p class='popup-margin'>Victory margin: ", margin,
      "</p><div class='popup-candidates'>",
      county_map$candidate_details[[i]],
      "</div><p class='popup-time'>County report: ",
      county_map$reporting_timestamp[[i]], "</p></div>"
    )
  }, character(1))

  bounds <- sf::st_bbox(county_map)
  legend_html <- paste0(
    "<div class='county-map-legend'><strong>County lead</strong>",
    "<div class='legend-row'><span>Democratic</span><span class='legend-gradient legend-blue'></span><span>Narrow → wide</span></div>",
    "<div class='legend-row'><span>Republican</span><span class='legend-gradient legend-red'></span><span>Narrow → wide</span></div>",
    "<div class='legend-row'><span class='legend-chip legend-other'></span><span>Other-party lead</span></div>",
    "<div class='legend-row'><span class='legend-chip legend-neutral'></span><span>Tie / no result</span></div>",
    "</div>"
  )

  leaflet::leaflet(
    county_map,
    height = height,
    options = leaflet::leafletOptions(
      zoomControl = TRUE,
      minZoom = 4,
      maxZoom = 10,
      scrollWheelZoom = FALSE
    )
  ) |>
    leaflet::addProviderTiles("CartoDB.PositronNoLabels") |>
    leaflet::addPolygons(
      layerId = ~FIPS,
      fillColor = ~fill_color,
      fillOpacity = 0.92,
      color = "#FFFFFF",
      weight = 1,
      opacity = 1,
      label = ~hover_label,
      labelOptions = leaflet::labelOptions(
        direction = "auto",
        textsize = "13px",
        style = list(
          "font-weight" = "600",
          "color" = "#172D3D",
          "padding" = "5px 8px"
        )
      ),
      popup = ~popup_html,
      popupOptions = leaflet::popupOptions(maxWidth = 340, minWidth = 250),
      highlightOptions = leaflet::highlightOptions(
        color = "#172D3D",
        weight = 2,
        bringToFront = TRUE,
        fillOpacity = 1
      )
    ) |>
    leaflet::addControl(
      html = htmltools::HTML(legend_html),
      position = "bottomright"
    ) |>
    leaflet::addControl(
      html = htmltools::HTML(
        "<div class='map-attribution'>Results: California Secretary of State · Boundaries: U.S. Census Bureau, 2025</div>"
      ),
      position = "bottomleft"
    ) |>
    leaflet::fitBounds(
      bounds[["xmin"]], bounds[["ymin"]],
      bounds[["xmax"]], bounds[["ymax"]]
    )
}
