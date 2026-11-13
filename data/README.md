# California 2026 Governor results data

- Retrieved: 2026-11-13T16:31:16Z (UTC).
- Status: unofficial, as of the API reporting timestamps; the Secretary of State says county final results are due December 3, 2026 and state certification is scheduled for December 11, 2026.
- Event: November 3, 2026 General Election. The live Governor API response does not include an election ID; its Governor race title and November 2026 reporting timestamps were checked against the current Secretary of State election page.
- County coverage: all 58 California counties; county FIPS are Census five-digit identifiers; one row per county and candidate.
- Vote percentages are recalculated as candidate votes divided by all returned candidate votes in that county. The API's rounded percentages are checked against the recalculated values.
- Canonical cleaned data: `clean/ca_2026_county_results.csv` (a root-level compatibility copy is also written).
- Raw responses and a source/retrieval manifest are stored in `raw/20261113T163035Z/`.

## Sources

- API documentation: https://cms.cdn.sos.ca.gov/media/3-api-documentation-v3.pdf
- Statewide Governor results: https://api.sos.ca.gov/returns/governor
- County Governor endpoint pattern: https://api.sos.ca.gov/returns/governor/county/{county-slug}
- Secretary of State 2026 General Election page: https://www.sos.ca.gov/elections/upcoming-elections/general-election-november-3-2026
- County boundaries: https://www2.census.gov/geo/tiger/GENZ2025/shp/cb_2025_us_county_20m.zip

Run `Rscript data/fetch_ca_2026_county_results.R` to retrieve a new timestamped raw snapshot, validate it, and refresh the cleaned CSV.
