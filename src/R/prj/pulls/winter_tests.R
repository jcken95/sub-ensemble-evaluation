#' @name winter_tests
#' @section Version: 0.0.1
#'
#' @title SGSS data pull of influenza, norovirus and RSV winter tests from SGSS
#'
#' @seealso
#' * [winter_tests$query_winter_tests()]
#'
".__module__."

box::use(
  box / deps_,
  box / help_,
  . / helpers
)

.on_load <- function(ns) {
  deps_$need(
    "dplyr"
  )
}



#' Information about organisms
#'
#' This is the subset of organisms which we are currently interested in, in
#' terms of winter test results.
organism_info <- function() {
  tibble::tribble(
    ~organism_code, ~disease, ~organism_name, ~result,
    "318X.0000", "norovirus", "Norovirus", "positive",
    "3322.0000", "influenza", "Influenza-A", "positive",
    "3322.XXXX", "influenza", "Influenza-A", "negative",
    "3326.0000", "influenza", "Influenza-B", "positive",
    "0000.0004", "influenza", "Influenza-B", "negative",
    "3327.0000", "influenza", "Influenza-C", "positive",
    "0000.0005", "influenza", "Influenza-C", "negative",
    "3321.0005", "influenza", "Influenza", "ungrouped",
    "0000.0006", "influenza", "Influenza", "negative",
    "0000.0007", "influenza", "Influenza", "void",
    "0000.0008", "influenza", "Influenza", "indeterminate",
    "3370.0000", "rsv", "RSV", "positive",
    "3370.XXXX", "rsv", "RSV", "negative",
    "0000.0032", "rsv", "RSV", "void",
    "0000.0033", "rsv", "RSV", "indeterminate"
  )
}



#' Construct winter tests query
#'
#' Translated from a legacy handwritten SQL query.
#'
#' @param from_date,to_date Dates between which to fetch records (inclusive). By
#'   default, records from the last 6 months are retrieved.
#'
#' @returns A dbplyr lazy query.
#'
#' @examples
#' box::use(prj / pulls)
#'
#' pulls$query_winter_tests() |>
#'   pulls$autopull(pulls$make_s3_path("winter_tests"), date_column = "specimen_date")
#'
#' @export
query_winter_tests <- function(from_date = Sys.Date() - 180, to_date = Sys.Date()) {

  con <- helpers$connect_sgssdw()


  query <- dplyr::tbl(con, "FACT_OPIE_AND_SPECIMEN_REQUEST") |>

    # Date filter - but avoid unnecessary (and expensive) ->char->date conversion within the database
    # e.g. from_date="2024-04-01" is translated to 20240401, matching the format in the database column
    dplyr::filter(
      Specimen_Date_SK >= !!(from_date |> as.Date() |> gsub("-", "", x = _) |> as.integer()),
      Specimen_Date_SK <= !!(to_date |> as.Date() |> gsub("-", "", x = _) |> as.integer())
    ) |>


    # Attach various tables; the comments here are the aliases used in the legacy SQL query

    # o
    # N.B. this is the only table we will do a "filtering join", i.e. inner-join rather than left-join
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_ORGANISM") |>
        dplyr::select(
          Organism_Species_Code,
          Organism_Species_Name,
          Organism_SK,
          # NB. we've been told there's *some* good serotype data for *flu*, but
          # not necessarily for anything else
          Serotype
        ) |>
        dplyr::filter(Organism_Species_Code %in% !!(
          organism_info() |>
            dplyr::filter(result == "positive") |>
            dplyr::pull(organism_code)
        )),
      dplyr::join_by(OPIE_Organism_SK == Organism_SK)
    ) |>


    # dem
    dplyr::left_join(
      dplyr::tbl(con, "DIMENSION_DEMOGRAPHIC") |>
        dplyr::select(
          Age_in_Years,
          Demographic_SK
        ),
      dplyr::join_by(OPIE_Demographic_SK == Demographic_SK)
    ) |>


    # test
    dplyr::left_join(
      dplyr::tbl(con, "DIMENSION_TEST_METHOD") |>
        dplyr::select(
          Test_Method_Description,
          Test_Method_SK
        ),
      dplyr::join_by(Test_Method_SK)
    ) |>


    # ro
    dplyr::left_join(
      dplyr::tbl(con, "DIMENSION_REQUESTING_ORGANISATION") |>
        dplyr::select(
          Requesting_Organisation_Type_Code,
          Requesting_Organisation_Type_Description,
          Medical_Requestor_SK
        ),
      dplyr::join_by(OPIE_Requesting_Organisation_SK == Medical_Requestor_SK)
    ) |>


    # soa
    dplyr::left_join(
      dplyr::tbl(con, "DIMENSION_SUPER_OUTPUT_AREA") |>
        dplyr::select(
          Local_Authority_Code,
          Local_Authority_Name,
          PHE_Region_Code,
          PHE_Region_Name,
          Lower_Super_Output_Area_SK
        ),
      dplyr::join_by(OPIE_Super_Output_Area_SK == Lower_Super_Output_Area_SK)
    ) |>


    # lg
    dplyr::left_join(
      dplyr::tbl(con, "DIMENSION_LAB_GEOGRAPHY") |>
        dplyr::select(
          POSTCODE,
          Lab_Geography_SK
        ),
      dplyr::join_by(OPIE_Reporting_Lab_Geography_SK == Lab_Geography_SK)
    ) |>


    # di
    dplyr::left_join(
      dplyr::tbl(con, "DIMENSION_INDICATORS") |>
        dplyr::select(
          PostCode_Source,
          Indicators_SK
        ),
      dplyr::join_by(OPIE_Indicators_SK == Indicators_SK)
    ) |>


    # dsg
    dplyr::left_join(
      dplyr::tbl(con, "DIMENSION_SITE_GEOGRAPHY") |>
        dplyr::select(
          Medical_Facility_Type_Name,
          NHS_Trust_Code,
          NHS_Trust_Name,
          NHS_England_Region_Code,
          NHS_England_Region_Name,
          Site_Geography_SK
        ),
      dplyr::join_by(OPIE_Site_Geography_SK == Site_Geography_SK)
    ) |>


    # dsb
    dplyr::left_join(
      dplyr::tbl(con, "DIMENSION_SPECIMEN_BACTERAEMIA") |>
        dplyr::select(
          Specimen_Group_Description,
          Specimen_Bacteraemia_SK
        ),
      dplyr::join_by(OPIE_Specimen_Bacteraemia_SK == Specimen_Bacteraemia_SK)
    ) |>


    # Now we _do_ need to convert dates to actual date format, for output
    dplyr::mutate(
      dplyr::across(
        c(Specimen_Date_SK, Lab_Report_Date_SK, Symptom_Onset_Date_SK, SGSS_RECEIVED_DATE_SKEY),
        \(.) as.Date(as.character(.))
      )
    ) |>

    dplyr::select(
      "person_episode_id" = CDR_OPIE_ID, # formerly known as "uuid"
      "specimen_request_id" = CDR_Specimen_Request_SK, # formerly known as "psuedo_id" (sic)
      "organism_species_code" = Organism_Species_Code,
      "organism_species_name" = Organism_Species_Name,
      "organism_subtype" = Serotype,
      "test_method_code" = Test_Method_SK,
      "test_method_name" = Test_Method_Description,
      "specimen_date" = Specimen_Date_SK,
      "lab_report_date" = Lab_Report_Date_SK,
      "symptom_onset_date" = Symptom_Onset_Date_SK,
      "sgss_received_date" = SGSS_RECEIVED_DATE_SKEY,
      "patient_age" = Age_in_Years,
      "requesting_organisation_type_code" = Requesting_Organisation_Type_Code,
      "requesting_organisation_type_name" = Requesting_Organisation_Type_Description,
      "medical_facility_type" = Medical_Facility_Type_Name,
      "trust_code" = NHS_Trust_Code,
      "trust_name" = NHS_Trust_Name,
      "nhs_region_code" = NHS_England_Region_Code,
      "nhs_region_name" = NHS_England_Region_Name,
      "patient_ltla_code" = Local_Authority_Code,
      "patient_ltla_name" = Local_Authority_Name,
      "ltla_source" = PostCode_Source,
      "phe_region_code" = PHE_Region_Code,
      "phe_region_name" = PHE_Region_Name,
      "lab_postcode" = POSTCODE,
      "specimen_group" = Specimen_Group_Description
    )


  attr(query, "date_column") <- "specimen_date"
  query
}
