# RSV models

## Contents

We have two operational models - the admissions and occupancy models.

The occupancy model depends on the admissions model being first run.

The tests model is exploratory and not intended for production outputs.

## Data

To produce the data required for the admissions and occupancy models you will need to run the

`~/pipelines/hospitals/run_sources.R`

using the

`run_rsv_hospital.yaml` configuration file

## Models

### Admissions

#### `hospital_cases`

This model uses SGSS tests, for inpatients (e.g. only admitted patients in a trust).

It is a hierarchical GAM forecasting forward growth rates.

The model is fit at a NHS Region level, with stratifications for age group.

Day of week effects are accounted for.

### Occupancy

Occupancy models are fit to UEC bed occupancy rate data, only for paediatric beds.
The models fit with a beta distribution as the outcome is a proportion.

#### `glmm`

This model uses the output of the `hospital_cases` forecast to convert to admissions.

The length of stay of RSV patients is very short, so a length of stay approach.

Random effects are used for the regional converstion, as well as an intercept.

Day of week effects are accounted for.

#### `gam`

This model forecasts the occupancy rate directly as a growth rate extension model.

The model smooths temporal trends spatially using a tensor smooth.

Day of week effects are accounted for.
