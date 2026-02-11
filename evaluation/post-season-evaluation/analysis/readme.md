## Post season retrospective analysis

Scripts are split up to keep them small and focused. Assumed that each is run in turn:

  - `00-depends.R` specify and load dependencies for analysis
  - `01-load_data.R` read in data / connect to redshift tables
  - `02-scoring_wis.R` scoring and descriptive analysis of (mostly) admissions forecasts
  - `03-scoring_trend.R` scoring and descriptive analysis of trend probabilities
  - `04-lomo.R` leave-one-model-out analysis to understand model importance
