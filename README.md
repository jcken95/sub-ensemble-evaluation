# Sub ensemble evaluation

Code to facilitate evaluation of UKHSA forecasts over winter 2024-25 and to accompany the paper _Evaluation of short-term multi-target respiratory forecasts over winter 2024-25 in England using sub-ensemble contribution analyses_.

Paper link: tbc

Authors: [@jcken95](https://www.github.com/jcken95), [@DrAuxetic](https://www.github.com/DrAuxetic), [@owenjonesuob](https://www.github.com/owenjonesuob), [Steven Riley](https://www.gov.uk/government/people/steven-riley), [Thomas Ward](https://www.researchgate.net/scientific-contributions/Thomas-Ward-2205092081), [@maria-tang](https://www.github.com/maria-tang),  [@jonathonmellor](https://www.github.com/jonathonmellor)

## Code content

This is a one-way push of the relevant contents of the operational repository used to deliver forecasts within UKHSA in winter 2024/25.

The repository represents the state of code at the end of the 2024/25 season as well as scripts to support the publication analysis.

The Winter 24/25 season was the first time we used [{targets}](https://books.ropensci.org/targets/) for our modelling pipelines. This allowed for many operational enhancements including checkpointing of code, enhanced parallelisation and a simpler-to-run interface over standard R scripts.

### Key notes

The code is not presented with accompanying data due to the large scale of data and sharing restrictions placed upon the targets and indicators.
This code is not actively being developed, and is intended as a archive of past work, rather than being representative of current production code.
To protect data & cyber security data infrastructure artifacts have been removed.
Non-modelling scripts (e.g. exploratory analyses) have been removed from the repository.
We have open sourced this code to support the transparency of the project with the epidemic forecasting community.

### Structure

 * Each disease has it's own directory, with each directory there is a modelling directory for each metric.
 * Evaluation has a directory split up as follows:
   * `evaluation/post-season-evaluation` exploratory post-season evaluation work
   * `evaluation/within-season-evaluation` a quarto document used for within-season monitoring of forecast performance
 * Code used to generate analysis, figures and table for the paper are in `publication`   
 * Common functions across models are accessed via a box module for this repository, as well as common functions used across the team (e.g. for data access).

### Changes between 2024/25 & 2025/26

Over summer 2025 there were minor changes to the code base. The primary changes were:

 * Development of Norovirus into a regional model (previously only a national model)
 * Addition of new models to our ensemble suite
