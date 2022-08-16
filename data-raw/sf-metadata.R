geog_short <- c("NAT",
          "LAD",
          "GOR",
          "UTLA",
          "OA",
          "LSOA",
          "MSOA",
          "WM",
          "EU",
          "WAC",
          "WAR",
          "NUTS1",
          "NUTS2",
          "NUTS3")
geog <- c("Countries",
          "Local_Authority_Districts",
          "Regions",
          "Counties_and_Unitary_Authorities",
          "\Output_Area",
          "Lower_Super_Output_Areas",
          "Middle_Super_Output_Areas",
          "Westminster_Parliamentary_Constituencies",
          "European_Electoral_Regions",
          "National_Assembly_for_Wales_Constituencies",
          "National_Assembly_for_Wales_Electoral_Regions",
          "NUTS_Level_1",
          "NUTS_Level_2",
          "NUTS_Level_3")
tag <- c("UK", "UK", "EN", "UK", rep("", 3), "UK", "UK", "WA", "WA", rep("", 3))
type = c(rep("BGC|BFC|BFE|BUC", 4), rep("", 3), rep("BGC|BFC|BFE|BUC", 4), rep("", 3))
month = c(rep("DEC|December", 11), rep("January", 3))
boundary_type = c(rep("Administrative_Boundaries", 4), rep("Census_Boundaries", 3),
                  rep("Electoral_Boundaries", 4), rep("Eurostat_Boundaries", 3))

metadata <- data.frame(geog_short, geog, boundary_type, type, tag, month)

usethis::use_data(metadata, overwrite = TRUE)
