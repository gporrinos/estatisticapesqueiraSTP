





# =========================
# LOAD PACKAGES
# =========================
for(package in c("shiny",
                 "callr",
                 "httr",
                 "readxl",
                 "writexl",
                 "dplyr",
                 "jsonlite",
                 "DBI",
                 "RSQLite",
                 "plotly",
                 "shinyFiles",
                 "bs4Dash"))
  load_package(package)







if (needs_update("fdishinyr", "fdiwg/fdishinyr")) {
  remotes::install_github("fdiwg/fdishinyr", dependencies = TRUE, upgrade = "never")
}




if (needs_update("artfishr", "fdiwg/artfishr")) {
  remotes::install_github("fdiwg/artfishr", dependencies = TRUE, upgrade = "never")
}


