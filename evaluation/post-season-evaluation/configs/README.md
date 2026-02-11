# Config files for post-season evaluation baseline models

Each config file looks just like the standard config files for out within-season runs, however:

   * The number of lookbacks is very long, to allow us to fit over the entire season with one run of a {targets} script.
   * The only model run is the `gam_dow` model; the baseline.

Results are stored on s3:

## covid

 - PATH REDACTED
 - PATH REDACTED

## influenza

 - PATH REDACTED
 - PATH REDACTED

## rsv

 - PATH REDACTED
 - PATH REDACTED
