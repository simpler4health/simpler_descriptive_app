# Shiny automatically sources every file in R/ before app.R.
# This file (its name is what matters) disables that behaviour, so the
# files are only loaded by the explicit source() calls in app.R.