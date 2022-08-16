# Changes to Local Authority boundaries
  # Local Authority District (LAD)
  # Local Education Agency (LEA)
  # Upper Tier Local Authority (UTLA)

# 2012 ----

  # St Albans
  # Welwyn Hatfield

  lookup_1112 <- data.frame(LAD11CD = c("E07000100", "E07000104"),
                            LAD11NM = c("St Albans", "Welwyn Hatfield"),
                            LAD12CD = c("E07000240", "E07000241"),
                            LAD12NM = c("St Albans", "Welwyn Hatfield"),
                            stringsAsFactors = FALSE)

  usethis::use_data(lookup_1112, overwrite = TRUE)

# 2013 ----

  # Northumberland
  # East Hertfordshire
  # Stevenage
  # Gateshead

  lookup_1213 <- data.frame(LAD12CD = c("E06000048", "E07000097", "E07000101", "E08000020"),
                            LAD12NM = c("Northumberland", "East Hertfordshire", "Stevenage", "Gateshead"),

                            UTLA12CD = c("E06000048", "E10000015", "E10000015", "E08000020"),
                            UTLA12NM = c("Northumberland", "Hertfordshire", "Hertfordshire", "Gateshead"),

                            LAD13CD = c("E06000057", "E07000242", "E07000243", "E08000037"),
                            LAD13NM = c("Northumberland", "East Hertfordshire", "Stevenage", "Gateshead"),

                            UTLA13CD = c("E06000057", "E10000015", "E10000015", "E08000037"),
                            UTLA13NM = c("Northumberland", "Hertfordshire", "Hertfordshire", "Gateshead"),

                            stringsAsFactors = FALSE)

  usethis::use_data(lookup_1213, overwrite = TRUE)

# 2018 ----

  # Shepway -> Folkestone and Hythe

  lookup_1718 <- data.frame(LAD17CD = "E07000112",
                            LAD17NM = "Shepway",
                            LAD18CD = "E07000112",
                            LAD18NM = "Folkestone and Hythe",
                            stringsAsFactors = FALSE)

  usethis::use_data(lookup_1718, overwrite = TRUE)

# 2019 ----

  # Suffolk

  suff1819 <- data.frame(LAD18CD = c("E07000201", "E07000204", "E07000205", "E07000206"),
                         LAD18NM = c("Forest Heath", "St Edmundsbury", "Suffolk Coastal", "Waveney"),

                         UTLA18CD = c(rep("E10000029", 4)),
                         UTLA18NM = c(rep("Suffolk", 4)),

                         LEA18CD = c(rep(935, 4)),
                         LEA18NM = c(rep("Suffolk", 4)),

                         LAD19CD = c(rep("E07000245", 2), rep("E07000244", 2)),
                         LAD19NM = c(rep("West Suffolk", 2), rep("East Suffolk", 2)),

                         UTLA19CD = c(rep("E10000029", 4)),
                         UTLA19NM = c(rep("Suffolk", 4)),

                         LEA19CD = c(rep(935, 4)),
                         LEA19NM = c(rep("Suffolk", 4)),

                         stringsAsFactors = FALSE)
  # Dorset

  dors1819 <- data.frame(LAD18CD = c("E06000028", "E06000029", "E07000048", "E07000049", "E07000050", "E07000051", "E07000052", "E07000053"),
                         LAD18NM = c("Bournemouth", "Poole", "Christchurch", "East Dorset", "North Dorset", "Purbeck", "West Dorset", "Weymouth and Portland"),

                         UTLA18CD = c("E06000028", "E06000029", rep("E10000009", 6)),
                         UTLA18NM = c("Bournemouth", "Poole", rep("Dorset", 6)),

                         LEA18CD = c(837, 836, rep(835, 6)),
                         LEA18NM = c("Bournemouth", "Poole", rep("Dorset", 6)),

                         LAD19CD = c(rep("E06000058", 3), rep("E06000059", 5)),
                         LAD19NM = c(rep("Bournemouth, Christchurch and Poole", 3), rep("Dorset", 5)),

                         UTLA19CD = c(rep("E06000058", 3), rep("E06000059", 5)),
                         UTLA19NM = c(rep("Bournemouth, Christchurch and Poole", 3), rep("Dorset", 5)),

                         LEA19CD = c(rep(839, 3), rep(838, 5)),
                         LEA19NM = c(rep("Bournemouth, Christchurch and Poole", 3), rep("Dorset", 5)),

                         stringsAsFactors = FALSE)

  # Full

  lookup_1819 <- rbind(suff1819, dors1819)

  usethis::use_data(lookup_1819, overwrite = TRUE)

# 2020 ----

  # Buckinghamshire

  lookup_1920 <- data.frame(LAD19CD = c("E07000004", "E07000005", "E07000006", "E07000007"),
                            LAD19NM = c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe"),

                            UTLA19CD = c(rep("E10000002", 4)),
                            UTLA19NM = c(rep("Buckinghamshire", 4)),

                            LAD20CD = c(rep("E06000060", 4)),
                            LAD20NM = c(rep("Buckinghamshire", 4)),

                            UTLA20CD = c(rep("E06000060", 4)),
                            UTLA20NM = c(rep("Buckinghamshire", 4)),

                            stringsAsFactors = FALSE)

  usethis::use_data(lookup_1920, overwrite = TRUE)

# 2021 ----

  # Northamptonshire

  lookup_2021 <- data.frame(LAD20CD = c("E07000150", "E07000152", "E07000153", "E07000156", "E07000151", "E07000154", "E07000155"),
                            LAD20NM = c("Corby", "East Northamptonshire", "Kettering", "Wellingborough",
                                        "Daventry", "Northampton", "South Northamptonshire"),

                            UTLA20CD = c(rep("E10000021", 7)),
                            UTLA20NM = c(rep("Northamptonshire", 7)),

                            LEA20CD = c(rep(928, 7)),
                            LEA20NM = c(rep("Northamptonshire", 7)),

                            LAD21CD = c(rep("E06000061", 4), rep("E06000062", 3)),
                            LAD21NM = c(rep("North Northamptonshire", 4), rep("West Northamptonshire", 3)),

                            UTLA21CD = c(rep("E06000061", 4), rep("E06000062", 3)),
                            UTLA21NM = c(rep("North Northamptonshire", 4), rep("West Northamptonshire", 3)),

                            LEA21CD = c(rep(940, 4), rep(941, 3)),
                            LEA21NM = c(rep("North Northamptonshire", 4), rep("West Northamptonshire", 3)),

                            stringsAsFactors = FALSE)

  usethis::use_data(lookup_2021, overwrite = TRUE)
