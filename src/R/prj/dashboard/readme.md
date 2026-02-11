# About

box module for creating the winter dashboard

## usage

The module contains a template and a function to create the regional winter dashboards.

To create the dashboards, run:

``` r
box::use(prj / dashboard)

dashboard$generate_dashboards(
  "regional_dashboards",
  paste0(
    "PATH REDACTED"
  )
)
```

the dashboards will be written to `regional_dashboards`
