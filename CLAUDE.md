# EU Loneliness Explorer (Lon_Shiny)

This folder is a git clone of https://github.com/arturobertero/Lon_Shiny,
the source of record for the Explorer (LONELY-EU WP3, deliverable D3.1).
A copy may also sit inside a OneDrive folder that colleagues sync; the
GitHub repo is the one that counts.

## Every change, in this order

1. `git pull` before touching anything. ABSL may have pushed.
2. Edit, then run the app locally: `shiny::runApp()`.
3. Commit and `git push`.
4. Redeploy from this folder: `rsconnect::deployApp(appName = "Lon_Shiny")`.
   The record in `rsconnect/` keeps the live URL.

A change that is pushed but not deployed, or deployed but not pushed, is
the drift this order exists to prevent.

## Never

- Commit `.Rhistory`, `.RData`, `.Rproj.user/` or any token. The
  shinyapps.io credentials live in RStudio's Publishing settings, never in
  a file or a script.
- Replace `DATABASE.xlsx` with a file that lacks the `Availability`
  column: the Analytics tab reads it.
- Add a package without `renv::snapshot()` updating `renv.lock`.
