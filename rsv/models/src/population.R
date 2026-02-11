# Script to create lookup for trust populations by age
# relies on mrf_structures already having been run.

# preferably this would have the mergers sorted,
# or perhaps takes place in redshift?
# difficult to do in redshift as we need to define `age_breakdowns`.

box::use(
  box / redshift
)

# access lookups
rs <- redshift$data_model()

if (!exists("age_breakdowns")) {
  message("Pulling in the age groupings from mrf_structures.R")
  source("./rsv/models/src/mrf_structures.R", local = TRUE)
}

# NOTE THIS DOES NOT HAVE THE TRUST MERGER CORRECTIONS
age_props <- rs$REDACTED |>
  # Only for "useful" records, and only since 2020
  dplyr::filter(
    month_start_date >= "2020-01-01",
    !dplyr::if_any(c(trust_code, lsoa11cd, age), is.na)
  ) |>
  # We only care about regular NHS trusts i.e. those in our ODS-based lookup
  dplyr::semi_join(rs$REDACTED, dplyr::join_by(trust_code == code)) |>
  # age_breakdowns defined in mrf_structures.R:
  dplyr::mutate(age_group = cut(age, age_breakdowns, right = FALSE)) |>
  # Aggregate over all time, at LSOA-ageband level
  dplyr::group_by(lsoa11cd, trust_code, age_group) |>
  # need to do as.numeric as the SQL database operations will treat admissions
  # only as integers:
  dplyr::summarise(admissions = as.numeric(sum(admissions, na.rm = TRUE))) |>
  dplyr::ungroup() |>
  dplyr::group_by(lsoa11cd, age_group) |>
  dplyr::mutate(
    "p_lsoa_admissions_admitted_to_trust" = admissions /
      sum(
        admissions,
        na.rm = TRUE
      )
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(trust_code, age_group) |>
  dplyr::mutate(
    "p_trust_admissions_admitted_from_lsoa" = admissions /
      sum(
        admissions,
        na.rm = TRUE
      )
  ) |>
  dplyr::ungroup() |>
  # add in region for ease downstream
  dplyr::left_join(
    rs$REDACTED |>
      dplyr::select(code, nhser24nm),
    by = c("trust_code" = "code")
  ) |>
  dplyr::rename(nhs_region_name = nhser24nm) |>
  dplyr::collect()


pops <- rs$REDACTED |>
  # collect early as SQL doesn't like our string manipulation functions
  dplyr::collect() |>
  dplyr::filter(stringr::str_starts(lsoa11cd, "E")) |>
  dplyr::group_by(lsoa11cd) |>
  dplyr::summarise(dplyr::across(dplyr::starts_with("age_"), ~ sum(.))) |>
  dplyr::ungroup() |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("age_"),
    names_to = "age",
    values_to = "count"
  ) |>
  # treat top age level with "_plus" in it as the same as the penultimate
  dplyr::mutate(age = as.integer(stringr::str_extract(age, "\\d+"))) |>
  # age_breakdowns defined in mrf_structures.R:
  dplyr::mutate(age_group = cut(age, age_breakdowns, right = FALSE)) |>
  dplyr::group_by(age_group, lsoa11cd) |>
  dplyr::summarise(count = sum(count)) |>
  dplyr::ungroup()

# bring it all together to produce one row per trust per age
trust_age_population <- age_props |>
  dplyr::left_join(pops, by = c("lsoa11cd", "age_group")) |>
  dplyr::filter(!is.na(count)) |>
  dplyr::group_by(trust_code, age_group, nhs_region_name) |>
  dplyr::summarise(
    population = sum(p_lsoa_admissions_admitted_to_trust * count)
  ) |>
  dplyr::ungroup()

rm(pops, age_props)
