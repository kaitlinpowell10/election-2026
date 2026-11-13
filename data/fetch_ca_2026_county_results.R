library(jsonlite)
library(dplyr)
library(readr)
library(tidyr)

api_root <- "https://api.sos.ca.gov/returns/governor"
election_day <- as.Date("2026-11-03")
retrieved_at <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

counties <- c(
  "Alameda", "Alpine", "Amador", "Butte", "Calaveras", "Colusa",
  "Contra Costa", "Del Norte", "El Dorado", "Fresno", "Glenn", "Humboldt",
  "Imperial", "Inyo", "Kern", "Kings", "Lake", "Lassen", "Los Angeles",
  "Madera", "Marin", "Mariposa", "Mendocino", "Merced", "Modoc", "Mono",
  "Monterey", "Napa", "Nevada", "Orange", "Placer", "Plumas", "Riverside",
  "Sacramento", "San Benito", "San Bernardino", "San Diego", "San Francisco",
  "San Joaquin", "San Luis Obispo", "San Mateo", "Santa Barbara",
  "Santa Clara", "Santa Cruz", "Shasta", "Sierra", "Siskiyou", "Solano",
  "Sonoma", "Stanislaus", "Sutter", "Tehama", "Trinity", "Tulare",
  "Tuolumne", "Ventura", "Yolo", "Yuba"
)

county_fips <- c(
  "Alameda" = "06001", "Alpine" = "06003", "Amador" = "06005",
  "Butte" = "06007", "Calaveras" = "06009", "Colusa" = "06011",
  "Contra Costa" = "06013", "Del Norte" = "06015", "El Dorado" = "06017",
  "Fresno" = "06019", "Glenn" = "06021", "Humboldt" = "06023",
  "Imperial" = "06025", "Inyo" = "06027", "Kern" = "06029",
  "Kings" = "06031", "Lake" = "06033", "Lassen" = "06035",
  "Los Angeles" = "06037", "Madera" = "06039", "Marin" = "06041",
  "Mariposa" = "06043", "Mendocino" = "06045", "Merced" = "06047",
  "Modoc" = "06049", "Mono" = "06051", "Monterey" = "06053",
  "Napa" = "06055", "Nevada" = "06057", "Orange" = "06059",
  "Placer" = "06061", "Plumas" = "06063", "Riverside" = "06065",
  "Sacramento" = "06067", "San Benito" = "06069",
  "San Bernardino" = "06071", "San Diego" = "06073",
  "San Francisco" = "06075", "San Joaquin" = "06077",
  "San Luis Obispo" = "06079", "San Mateo" = "06081",
  "Santa Barbara" = "06083", "Santa Clara" = "06085",
  "Santa Cruz" = "06087", "Shasta" = "06089", "Sierra" = "06091",
  "Siskiyou" = "06093", "Solano" = "06095", "Sonoma" = "06097",
  "Stanislaus" = "06099", "Sutter" = "06101", "Tehama" = "06103",
  "Trinity" = "06105", "Tulare" = "06107", "Tuolumne" = "06109",
  "Ventura" = "06111", "Yolo" = "06113", "Yuba" = "06115"
)

retrieval_id <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
raw_dir <- file.path("data", "raw", retrieval_id)
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)

fetch_json <- function(url, path) {
  status <- suppressWarnings(
    utils::download.file(url, path, mode = "wb", quiet = TRUE, method = "libcurl")
  )
  if (status != 0L || !file.exists(path) || file.info(path)$size == 0) {
    stop("Could not download a non-empty API response from ", url)
  }

  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  tryCatch(
    jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE),
    error = \(error) stop("Invalid JSON from ", url, ": ", conditionMessage(error))
  )
}

parse_reporting_timestamp <- function(value, url) {
  value <- sub(" a\\.m\\.$", "AM", value)
  value <- sub(" p\\.m\\.$", "PM", value)
  parsed <- as.POSIXct(
    value,
    format = "%B %d, %Y, %I:%M %p",
    tz = "America/Los_Angeles"
  )

  if (is.na(parsed)) {
    stop("Unrecognized reporting timestamp from ", url, ": ", value)
  }

  if (as.Date(parsed, tz = "America/Los_Angeles") < election_day ||
      format(parsed, "%Y", tz = "America/Los_Angeles") != "2026") {
    stop("API response is not dated for the November 3, 2026 election: ", url)
  }

  sub(
    "([+-][0-9]{2})([0-9]{2})$",
    "\\1:\\2",
    format(parsed, "%Y-%m-%dT%H:%M:%S%z", tz = "America/Los_Angeles")
  )
}

clean_party <- function(value) {
  value <- trimws(value)
  dplyr::case_when(
    tolower(value) %in% c("dem", "democratic") ~ "Democratic",
    tolower(value) %in% c("rep", "republican") ~ "Republican",
    TRUE ~ value
  )
}

slugify_county <- function(county) {
  tolower(gsub(" ", "-", county, fixed = TRUE))
}

state_url <- api_root
state_path <- file.path(raw_dir, "governor-statewide.json")
state_retrieved_at <- retrieved_at()
state_response <- fetch_json(state_url, state_path)

if (!identical(state_response$raceTitle, "Governor - Statewide Results")) {
  stop("The statewide API response is not the expected Governor results.")
}

state_timestamp <- parse_reporting_timestamp(state_response$ReportingTime, state_url)
state_candidates <- tibble::tibble(
  candidate = vapply(state_response$candidates, \(x) trimws(x$Name), character(1)),
  party = vapply(state_response$candidates, \(x) clean_party(x$Party), character(1)),
  votes = readr::parse_number(
    vapply(state_response$candidates, \(x) x$Votes, character(1))
  )
)

if (nrow(state_candidates) == 0L || anyNA(state_candidates)) {
  stop("The statewide Governor response has missing candidate data.")
}

county_records <- lapply(counties, \(county) {
  url <- paste0(api_root, "/county/", slugify_county(county))
  path <- file.path(raw_dir, paste0("governor-", slugify_county(county), ".json"))
  response_retrieved_at <- retrieved_at()
  response <- fetch_json(url, path)
  expected_title <- paste0("Governor - ", county, " County Results")
  county_races <- Filter(\(race) identical(race$raceTitle, expected_title), response)

  if (length(county_races) != 1L) {
    stop("Expected one county Governor result for ", county, " at ", url)
  }

  race <- county_races[[1]]
  if (length(race$candidates) == 0L) {
    stop("No Governor candidates returned for ", county, " at ", url)
  }

  list(
    county = county,
    source_url = url,
    retrieved_at_utc = response_retrieved_at,
    reporting = race$Reporting,
    reporting_timestamp = parse_reporting_timestamp(race$ReportingTime, url),
    candidates = race$candidates
  )
})

candidate_rows <- dplyr::bind_rows(lapply(county_records, \(record) {
  tibble::tibble(
    county = record$county,
    reporting_timestamp = record$reporting_timestamp,
    candidate_record = list(record$candidates)
  ) |>
    tidyr::unnest_longer(candidate_record) |>
    tidyr::unnest_wider(candidate_record)
})) |>
  dplyr::transmute(
    county,
    reporting_timestamp,
    candidate = gsub("[[:space:]]+", " ", trimws(Name)),
    party = clean_party(Party),
    votes = readr::parse_number(as.character(Votes)),
    api_vote_percentage = readr::parse_number(as.character(Percent))
  ) |>
  dplyr::mutate(FIPS = unname(county_fips[county])) |>
  dplyr::relocate(FIPS, .after = county)

if (nrow(candidate_rows) == 0L ||
    anyNA(candidate_rows) ||
    any(candidate_rows$county == "") ||
    anyNA(candidate_rows$FIPS) ||
    any(candidate_rows$candidate == "") ||
    any(candidate_rows$party == "") ||
    any(candidate_rows$votes < 0) ||
    any(candidate_rows$votes %% 1 != 0)) {
  stop("County candidate rows contain missing, invalid, or non-integer vote data.")
}

if (length(unique(candidate_rows$county)) != 58L ||
    !setequal(unique(candidate_rows$county), counties) ||
    length(unique(candidate_rows$FIPS)) != 58L) {
  stop("County coverage is not exactly the 58 California counties.")
}

if (anyDuplicated(candidate_rows[c("county", "candidate")])) {
  stop("Duplicate county-candidate rows were returned by the API.")
}

state_candidate_names <- sort(state_candidates$candidate)
county_candidate_sets <- split(candidate_rows$candidate, candidate_rows$county)
if (any(!vapply(
  county_candidate_sets,
  \(x) identical(sort(x), state_candidate_names),
  logical(1)
))) {
  stop("County candidate lists do not match the statewide Governor candidate list.")
}

candidate_results <- candidate_rows |>
  dplyr::group_by(county) |>
  dplyr::mutate(
    county_total = sum(votes),
    vote_percentage = 100 * votes / county_total
  ) |>
  dplyr::ungroup()

if (any(candidate_results$county_total <= 0) ||
    any(abs(candidate_results$vote_percentage -
              candidate_results$api_vote_percentage) > 0.051)) {
  stop("County vote totals or calculated percentages do not match API percentages.")
}

percentage_totals <- candidate_results |>
  dplyr::group_by(county) |>
  dplyr::summarise(total = sum(vote_percentage), .groups = "drop")
if (any(abs(percentage_totals$total - 100) > 1e-8)) {
  stop("Candidate vote percentages do not sum to 100% in every county.")
}

county_vote_totals <- candidate_results |>
  dplyr::group_by(candidate) |>
  dplyr::summarise(county_votes = sum(votes), .groups = "drop") |>
  dplyr::full_join(
    state_candidates |>
      dplyr::select(candidate, statewide_votes = votes),
    by = "candidate"
  )
if (anyNA(county_vote_totals) ||
    any(county_vote_totals$county_votes != county_vote_totals$statewide_votes)) {
  stop("Summed county votes do not match the statewide API candidate totals.")
}

clean_results <- candidate_results |>
  dplyr::select(
    county, FIPS, candidate, party, votes, vote_percentage,
    reporting_timestamp
  ) |>
  dplyr::arrange(county, dplyr::desc(votes), candidate)

if (anyNA(clean_results) ||
    anyDuplicated(clean_results[c("county", "candidate")])) {
  stop("Final cleaned data contains missing values or duplicate rows.")
}

manifest <- dplyr::bind_rows(
  tibble::tibble(
    scope = "statewide",
    county = "Statewide",
    source_url = state_url,
    retrieved_at_utc = state_retrieved_at,
    reporting_timestamp = state_timestamp,
    reporting = state_response$Reporting
  ),
  dplyr::bind_rows(lapply(county_records, \(record) {
    tibble::tibble(
      scope = "county",
      county = record$county,
      source_url = record$source_url,
      retrieved_at_utc = record$retrieved_at_utc,
      reporting_timestamp = record$reporting_timestamp,
      reporting = record$reporting
    )
  }))
)

readr::write_csv(clean_results, "data/clean/ca_2026_county_results.csv")
readr::write_csv(clean_results, "data/ca_2026_county_results.csv")
readr::write_csv(manifest, file.path(raw_dir, "manifest.csv"))

readr::write_lines(
  c(
    "# California 2026 Governor results data",
    "",
    paste0("- Retrieved: ", retrieved_at(), " (UTC)."),
    "- Status: unofficial, as of the API reporting timestamps; the Secretary of State says county final results are due December 3, 2026 and state certification is scheduled for December 11, 2026.",
    "- Event: November 3, 2026 General Election. The live Governor API response does not include an election ID; its Governor race title and November 2026 reporting timestamps were checked against the current Secretary of State election page.",
    "- County coverage: all 58 California counties; county FIPS are Census five-digit identifiers; one row per county and candidate.",
    "- Vote percentages are recalculated as candidate votes divided by all returned candidate votes in that county. The API's rounded percentages are checked against the recalculated values.",
    "- Canonical cleaned data: `clean/ca_2026_county_results.csv` (a root-level compatibility copy is also written).",
    paste0(
      "- Raw responses and a source/retrieval manifest are stored in `raw/",
      retrieval_id,
      "/`."
    ),
    "",
    "## Sources",
    "",
    "- API documentation: https://cms.cdn.sos.ca.gov/media/3-api-documentation-v3.pdf",
    "- Statewide Governor results: https://api.sos.ca.gov/returns/governor",
    "- County Governor endpoint pattern: https://api.sos.ca.gov/returns/governor/county/{county-slug}",
    "- Secretary of State 2026 General Election page: https://www.sos.ca.gov/elections/upcoming-elections/general-election-november-3-2026",
    "- County boundaries: https://www2.census.gov/geo/tiger/GENZ2025/shp/cb_2025_us_county_20m.zip",
    "",
    "Run `Rscript data/fetch_ca_2026_county_results.R` to retrieve a new timestamped raw snapshot, validate it, and refresh the cleaned CSV."
  ),
  "data/README.md"
)

message(
  "Validated ", nrow(clean_results), " county-candidate rows across ",
  length(unique(clean_results$county)), " counties. Raw snapshot: ", raw_dir
)
