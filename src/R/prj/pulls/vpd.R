#' @name vpd
#' @section Version: 0.0.1
#'
#' @title Vaccine preventable disease data pull
#'
#' @seealso
#' * [vpd$show_organisms()]
#' * [vpd$query_vpd()]
#'
".__module__."

box::use(
  box / deps_,
  box / help_,
  . / helpers
)

.on_load <- function(ns) {
  deps_$need(
    "cli",
    "dplyr",
    "lubridate",
    "stringr",
    "tibble"
  )
}


#' Construct tibble of allowed organisms and species/subspecies status
#'
#' @returns A tibble.
#'
#' @export
show_organisms <- function() {
  tibble::tibble(
    organism_name = c(
      "ROTAVIRUS", "BORDETELLA PARAPERTUSSIS", "BORDETELLA PERTUSSIS", "ENTEROVIRUS",
      "CLOSTRIDIUM TETANI", "CORYNEBACTERIUM DIPHTHERIAE", "COXSACKIE A", "COXSACKIE B",
      "ECHOVIRUS", "HAEMOPHILUS INFLUENZAE", "MEASLES VIRUS", "NEISSERIA MENINGITIDIS",
      "PARVOVIRUS B19", "POLIOVIRUS", "RUBELLA VIRUS", "STREPTOCOCCUS PNEUMONIAE", "TETANUS",
      "MUMPS VIRUS", "ENTEROVIRUS UNTYPED", "HEPATITIS A", "STREPTOCOCCUS GROUP B",
      "STREPTOCOCCUS AGALACTIAE"
    )
  ) |>
    dplyr::mutate(
      is_subspecies = organism_name %in% c("STREPTOCOCCUS GROUP B", "STREPTOCOCCUS AGALACTIAE")
    )
}

#' Construct parameterised VPD query
#'
#' @param required_organism_names organism names for query. Defaults to `NA` which pulls all organisms.
#'   See [vpd$show_organisms()] for all possible organisms.
#' @param specimen_date_range Character vector. Inclusive range of specimen dates for query.
#'   Defaults to `NA` which pulls all dates.
#'   Dates must be specified in one of the following formats: "YYYYMMDD", "YYYY-MM-DD", "YYYY/MM/DD".
#'   If specifying dates, you must specify two dates (earliest and latest).
#'
#' @returns A dbplyr lazy query.
#'
#' @examples
#' box::use(prj / pulls)
#'
#' pulls$query_vpd() |>
#'   pulls$autopull(pulls$make_s3_path("vpd"))
#'
#' @export
query_vpd <- function(
    required_organism_names = NA,
    specimen_date_range = NA
    ) {
  if (!anyNA(required_organism_names)) {
    check_organisms(required_organism_names)
  }

  if (!any(is.na(specimen_date_range))) {
    check_date_length(specimen_date_range)
    specimen_date_range <- format_dates(specimen_date_range)
  }

  initialise_query() |>
    join_geography() |>
    join_date() |>
    join_outrigger() |>
    join_organisms(required_organism_names = required_organism_names) |>
    join_demographics() |>
    join_indicators() |>
    join_vaccinations() |>
    join_specimen_bacteramia() |>
    join_soa() |>
    join_ethnicity() |>
    join_specimen_date(specimen_date_range = specimen_date_range) |>
    join_lab_report_date() |>
    join_site_geog() |>
    join_gp_geog() |>
    join_symptom_onset_date() |>
    join_source_lab_geog() |>
    join_reporting_lab_geography() |>
    join_test() |>
    join_request_org() |>
    clean_query()
}


#' Construct SQL pull to find all relevant CDR_OPIE_IDs
#'
#' @param con Database connection specified via [DBI::dbConnect()].
#'
#' @returns A dbplyr lazy query.
initialise_query <- function(con = helpers$connect_sgssdw()) {
  dplyr::tbl(con, "FACT_OPIE_AND_SPECIMEN_REQUEST") |>
    #  datedim
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_DATE"),
      dplyr::join_by(SGSS_RECEIVED_DATE_SKEY == Date_SK)
    ) |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_ORGANISM"),
      dplyr::join_by(OPIE_Organism_SK == Organism_SK)
    ) |>
    dplyr::filter(
      Organism_Species_Name %in% c(
        "ROTAVIRUS", "BORDETELLA PARAPERTUSSIS", "BORDETELLA PERTUSSIS", "ENTEROVIRUS",
        "CLOSTRIDIUM TETANI", "CORYNEBACTERIUM DIPHTHERIAE", "COXSACKIE A", "COXSACKIE B",
        "ECHOVIRUS", "HAEMOPHILUS INFLUENZAE", "MEASLES VIRUS", "NEISSERIA MENINGITIDIS",
        "PARVOVIRUS B19", "POLIOVIRUS", "RUBELLA VIRUS", "STREPTOCOCCUS PNEUMONIAE",
        "TETANUS", "MUMPS VIRUS", "ENTEROVIRUS UNTYPED", "HEPATITIS A"
      ) |
        Organism_Subspecies_Name %in% c("STREPTOCOCCUS GROUP B", "STREPTOCOCCUS AGALACTIAE")
    ) |>
    dplyr::select(CDR_OPIE_ID) |>
    dplyr::distinct()
}

# Join GP and hospital consultant geographies to query
join_geography <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::left_join(
      dplyr::tbl(con, "FACT_OPIE_AND_SPECIMEN_REQUEST") |>
        dplyr::select(!c(
          OPIE_GP_Geography_SK,
          OPIE_Hospital_Consultant_Geography_SK
        )),
      dplyr::join_by(CDR_OPIE_ID)
    )
}

# Join dates to query
join_date <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_DATE") |>
        dplyr::select(
          !c(
            Date_Name,
            Date_Time_Name,
            dplyr::starts_with("Calendar_Quarter"),
            Calendar_Year_Key,
            dplyr::starts_with("Day_Of_The_"),
            dplyr::starts_with("Financial_"),
            dplyr::contains("Month"),
            dplyr::starts_with("Start_Of_Week"),
            dplyr::contains("Week")
          )
        ),
      dplyr::join_by(OPIE_EARLIEST_RECEIVED_DATE_SKEY == Date_SK)
    )
}

# Join outrigger information to query
join_outrigger <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "OUTRIGGER_SPECIMEN_REQUEST") |>
        dplyr::select(
          PATIENT_DEATH_DATE, Reference_Lab_ID,
          Reference_Lab_Name, ETHNICITY_SK, Specimen_Request_Outrigger_SK
        ),
      dplyr::join_by(Opie_Outrigger_SK == Specimen_Request_Outrigger_SK)
    )
}


# Join organism information to query
join_organisms <- function(
    query,
    con = dbplyr::remote_con(query),
    required_organism_names = NA
    ) {
  query <- query |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_ORGANISM") |>
        dplyr::select(
          Serotype, Phagetype, Molecular_Type, Toxin_Type, Other_Type,
          Organism_Species_Code, Organism_Subspecies_Code,
          Organism_Species_Name, Organism_Subspecies_Name, Organism_SK
        ) |>
        dplyr::mutate(
          organism_code = dplyr::coalesce(Organism_Subspecies_Code, Organism_Species_Code),
          organism_name = dplyr::coalesce(Organism_Subspecies_Name, Organism_Species_Name),
          .keep = "unused"
        ),
      dplyr::join_by(OPIE_Organism_SK == Organism_SK)
    )

  if (all(!is.na(required_organism_names))) {
    query <- dplyr::filter(query, organism_name %in% required_organism_names)
  }


  attr(query, "date_column") <- "date"
  query
}

# Join patient demographics to query
join_demographics <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_DEMOGRAPHIC") |>
        dplyr::select(
          Patient_Sex, Patient_Immunocompromised_Indicator, Age_in_Years,
          Age_in_Months, Patient_Death_Indicator, Demographic_SK
        ),
      dplyr::join_by(OPIE_Demographic_SK == Demographic_SK)
    )
}

# Join indicators to query
join_indicators <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_INDICATORS") |>
        dplyr::select(Outbreak_Indicator, Hospital_Acquired_Indicator, Asymptomatic_Indicator, Indicators_SK),
      dplyr::join_by(OPIE_Indicators_SK == Indicators_SK)
    )
}

# Join vaccination status to query
join_vaccinations <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_VACCINATION_STATUS") |>
        dplyr::select(Vaccination_Status_Description, Vaccination_Status_SK),
      dplyr::join_by(OPIE_VACCINATION_STATUS_SKEY == Vaccination_Status_SK)
    )
}

# Join specimen bacteramia to query
join_specimen_bacteramia <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_SPECIMEN_BACTERAEMIA"),
      dplyr::join_by(OPIE_Specimen_Bacteraemia_SK == Specimen_Bacteraemia_SK)
    )
}

# Join super output area to query
join_soa <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_SUPER_OUTPUT_AREA") |>
        dplyr::select(
          Country_Description, Local_Authority_Name, Local_Authority_Code,
          UK_Region_Code, UK_Region_Description, Health_Protection_Team_Name,
          Health_Protection_Team_Code, Lower_Super_Output_Area_SK
        ),
      dplyr::join_by(OPIE_Super_Output_Area_SK == Lower_Super_Output_Area_SK)
    )
}

# Join ethinicity to query
join_ethnicity <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "REF_ETHNICITY") |>
        dplyr::select(Ethnicity_Description, Ethnicity_SK),
      dplyr::join_by(ETHNICITY_SK == Ethnicity_SK)
    )
}


# Join specimen date to query
join_specimen_date <- function(
    query,
    con = dbplyr::remote_con(query),
    specimen_date_range = NA
    ) {

  joining_table <- dplyr::tbl(con, "DIMENSION_DATE") |>
    dplyr::select(Earliest_Specimen_Date = Date, Date_SK)

  if (all(!is.na(specimen_date_range))) {
    # defining dates outside of filter because SQL is SQL
    min_date <- min(specimen_date_range)
    max_date <- max(specimen_date_range)
    joining_table <- dplyr::filter(
      joining_table,
      Date_SK >= min_date,
      Date_SK <= max_date
    )
  }

  query |>
    dplyr::inner_join(
      joining_table,
      dplyr::join_by(OPIE_Specimen_Date_SK == Date_SK)
    )
}

# Join lab report date to query
join_lab_report_date <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_DATE") |>
        dplyr::select(
          Date_SK,
          sgss_received = Date
        ),
      dplyr::join_by(OPIE_Lab_Report_Date_SK == Date_SK)
    )
}


# Join site geography to query
join_site_geog <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_SITE_GEOGRAPHY") |>
        dplyr::select(
          Site_Geography_SK,
          trust_code = NHS_Trust_Code,
          trust_name = NHS_Trust_Name
        ),
      dplyr::join_by(OPIE_Site_Geography_SK == Site_Geography_SK)
    )
}


# Join GP practice geographs to query
join_gp_geog <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "GP_Practice_Geography") |>
        dplyr::select(
          !c(
            Telephone_Number,
            dplyr::starts_with("Address_Line_"),
            dplyr::starts_with("CCG_")
          )
        ),
      dplyr::join_by(OPIE_Patient_GP_Practice_Geography_SK == GP_Practice_Geography_SK)
    )
}


# Join symptom onset date to query
join_symptom_onset_date <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_DATE") |>
        dplyr::select(Date_SK),
      dplyr::join_by(Symptom_Onset_Date_SK == Date_SK)
    )
}

# Join source lab geography to query
join_source_lab_geog <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_LAB_GEOGRAPHY") |>
        dplyr::select(
          Source_Lab_Code = Lab_Geography_Code,
          Source_Lab_Name = Lab_Geography_Name_Current,
          Source_Lab_Region_Code = UK_Region_Code,
          Source_Lab_Region_Name = UK_Region_Description,
          Lab_Geography_SK
        ),
      dplyr::join_by(OPIE_Source_Lab_Geography_SK == Lab_Geography_SK)
    )
}


# Join reporting lab geography to query
join_reporting_lab_geography <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_LAB_GEOGRAPHY") |>
        dplyr::select(Reporting_Lab = Lab_Geography_Code, Lab_Geography_SK),
      dplyr::join_by(Reporting_Lab_Geography_SK == Lab_Geography_SK)
    )
}


# Join testing information to query
join_test <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_TEST_METHOD") |>
        dplyr::select(
          test_method_code = Test_Method_SK,
          test_method_name = Test_Method_Description
        ),
      dplyr::join_by(Test_Method_SK == test_method_code)
    )
}

# Join requesting organisation to query
join_request_org <- function(query, con = dbplyr::remote_con(query)) {
  query |>
    dplyr::inner_join(
      dplyr::tbl(con, "DIMENSION_REQUESTING_ORGANISATION") |>
        dplyr::select(
          Medical_Requestor_SK,
          requesting_organisation_type_name = Requesting_Organisation_Type_Description,
          requesting_organisation_type_code = Requesting_Organisation_Type_Code
        ),
      dplyr::join_by(OPIE_Requesting_Organisation_SK == Medical_Requestor_SK)
    )
}

# Remove junk from query and clean up dates
clean_query <- function(query) {
  query |>
    dplyr::select(
      # just used for joining
      !c(Opie_Outrigger_SK, Site_Geography_SK, Super_Output_Area_SK),
      # duplicate columns; used in joins
      !c(
        OPIE_Specimen_Bacteraemia_SK,
        OPIE_Organism_SK,
        OPIE_VACCINATION_STATUS_SKEY
      )
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      specimen_date = as.Date(as.character(OPIE_Specimen_Date_SK)),
      sgss_received_date = as.Date(as.character(SGSS_RECEIVED_DATE_SKEY)),
      lab_report_date = as.Date(as.character(Lab_Report_Date_SK)),
      symptom_onset_date = as.Date(as.character(Symptom_Onset_Date_SK)),
      .keep = "unused"
    ) |>
    ## there are two dates columns loosely called sgss_received
    ## removing the one we don't want for clarity
    dplyr::select(!sgss_received) |>

    # Tidy up names a bit
    dplyr::rename_with(tolower)
}

#' Small helper to format & check human readable dates to SQL-friendly format
#'
#' @param input_dates Dates specified as a string in format "YYYYMMDD",
#'   "YYYY-MM-DD" or "YYYY/MM/DD"
#'
#' @returns A numeric vector.
format_dates <- function(input_dates) {
  formatted_dates <- input_dates |>
    # remove `-` or `/` - note that this might look weird with firacode font
    stringr::str_remove_all("-|/") |>
    as.numeric()

  today <- lubridate::today() |>
    stringr::str_remove_all("-") |>
    as.numeric()

  # check some basic date formats

  # want year > 1900
  if (any(floor(formatted_dates / 10^4) < 1900)) {
    cli::cli_abort(c(
      "Date is out of scope.",
      "i" = "Year must be after 1900.",
      "x" = "Your earliest year is {min(floor(formatted_dates / 10^4))}.",
      "i" = "An earliest year of less than 1000 indicates an incorrect date format. You may have used lubridate::today()." # nolint: line_length_linter
    ))
  }

  # want date <= today
  if (any(formatted_dates > today)) {
    cli::cli_abort(c(
      "Date is out of scope.",
      "i" = "Date must be no later than {lubridate::today()}.",
      "x" = "Your latest date {lubridate::ymd(max(formatted_dates))}."
    ))
  }

  # don't have >= 32 days a year
  if (any(formatted_dates %% 100 >= 32)) {
    cli::cli_abort(c(
      "Date is nonsensical.",
      "i" = "Day must be in range [01, 31].",
      "x" = "You've entered a day of {max(formatted_dates %% 100)}."
    ))
  }

  if (any(formatted_dates %% 100 == 0)) {
    cli::cli_abort(c(
      "Date is nonsensical.",
      "i" = "Day must be in range [01, 31].",
      "x" = "You've entered a day of {0}."
    ))
  }

  formatted_dates
}

#' Terminates function if organism name is not recognised
#'
#' @param organism_names Organism names to check.
check_organisms <- function(organism_names) {
  is_allowed_org <- organism_names %in% show_organisms()$organism_name

  if (any(!is_allowed_org)) {
    unknown_organisms <- organism_names[!is_allowed_org]

    cli::cli_abort(
      c(
        "You must specify an allowed organism name.",
        "x" = "You have specifed {length(specimen_date_range)} date(s).",
        "i" = "See `show_organisms()` for allowed organism names."
      )
    )
  }
}


#' Terminates function if dates are not of correct format
#'
#' @param input_dates Dates to check.
check_date_length <- function(input_dates) {
  if (length(input_dates) != 2) {
    cli::cli_abort(
      c(
        "If specifying dates, you must specify exactly 2 dates.",
        "x" = "You have specifed {length(specimen_date_range)} date(s)."
      )
    )
  }
}
