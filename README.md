# EU Loneliness Explorer

Shiny app over the LONELY-EU WP3 database of European surveys that measure
loneliness (deliverable D3.1). Live at
https://sppqgn-arturo-bertero.shinyapps.io/Lon_Shiny/, embedded at
https://lonelinessineurope.eu/eu-loneliness-explorer. Method and variable
definitions: the technical report on OSF, https://osf.io/3t9kg/.

## Files

| File | What it is |
|---|---|
| `ui.R`, `server.R` | The app |
| `DATABASE.xlsx` | The data the app reads: one row per survey, 22 columns, sheet 1 |
| `www/logo.png` | Header logo |
| `renv.lock` | Package versions (R 4.4.2) |
| `rsconnect/` | Deployment record for shinyapps.io (no credentials) |

## Run locally

```r
renv::restore()   # once, installs the versions in renv.lock
shiny::runApp()
```

## Deploy

With the shinyapps.io account configured in your own R session (never in a
committed file): `rsconnect::deployApp(appName = "Lon_Shiny")`. The record
in `rsconnect/` keeps the existing URL.

## Updating the data

Replace `DATABASE.xlsx` with the new release, keeping the column names, and
redeploy. The `Availability` column is read by the Analytics tab.

Author: Arturo Bertero (University of Milan). Project: LONELY-EU, Horizon
Europe, Grant Agreement No. 101177225.
