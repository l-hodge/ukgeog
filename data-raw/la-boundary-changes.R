# Changes to LA Boundaries

# 2012 ----
# - St Albans
# - Welwyn Hatfield
LAD_1112 <- data.frame(LAD11CD = c("E07000100", "E07000104"),
                       LAD11NM = c("St Albans", "Welwyn Hatfield"),
                       LAD12CD = c("E07000240", "E07000241"),
                       LAD12NM = c("St Albans", "Welwyn Hatfield"),
                       stringsAsFactors = FALSE)

usethis::use_data(LAD_1112, overwrite = TRUE)

# 2013 ----
LAD_1213 <- data.frame(LAD12CD = c("E06000048", "E07000097", "E07000101", "E08000020"),
                       LAD12NM = c("Northumberland", "East Hertfordshire", "Stevenage", "Gateshead"),
                       LAD13CD = c("E06000057", "E07000242", "E07000243", "E08000037"),
                       LAD13NM = c("Northumberland", "East Hertfordshire", "Stevenage", "Gateshead"),
                       stringsAsFactors = FALSE)

usethis::use_data(LAD_1213, overwrite = TRUE)

# 2017 ----
LAD_1718 <- data.frame(LAD17CD = "E07000112",
                       LAD17NM = "Shepway",
                       LAD18CD = "E07000112",
                       LAD18NM = "Folkestone and Hythe",
                       stringsAsFactors = FALSE)

usethis::use_data(LAD_1718, overwrite = TRUE)

# 2018 ----
# Suffolk
suff1819 <- data.frame(LAD18CD = c("E07000201", "E07000204", "E07000205", "E07000206"),
                       LAD18NM = c("Forest Heath", "St Edmundsbury", "Suffolk Coastal", "Waveney"),
                       LAD19CD = c(rep("E07000245", 2), rep("E07000244", 2)),
                       LAD19NM = c(rep("West Suffolk", 2), rep("East Suffolk", 2)),
                       stringsAsFactors = FALSE)
# Dorset
dors1819 <- data.frame(LAD18CD = c("E06000028", "E06000029", "E07000048", "E07000049", "E07000050", "E07000051", "E07000052", "E07000053"),
                       LAD18NM = c("Bournemouth", "Poole", "Christchurch", "East Dorset", "North Dorset", "Purbeck", "West Dorset", "Weymouth and Portland"),
                       LAD19CD = c(rep("E06000058", 3), rep("E06000059", 5)),
                       LAD19NM = c(rep("Bournemouth, Christchurch and Poole", 3), rep("Dorset", 5)),
                       stringsAsFactors = FALSE)
# Full
LAD_1819 <- rbind(suff1819, dors1819)

usethis::use_data(LAD_1819, overwrite = TRUE)

# 2019 ----
# Buckinghamshire
LAD_1920 <- data.frame(LAD19CD = c("E07000004", "E07000005", "E07000006", "E07000007"),
                       LAD19NM = c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe"),
                       LAD20CD = c(rep("E06000060", 4)),
                       LAD20NM = c(rep("Buckinghamshire", 4)),
                       stringsAsFactors = FALSE)

usethis::use_data(LAD_1920, overwrite = TRUE)

# Buckinghamshire
UTLA_1920 <- data.frame(UTLA19CD = c("E10000002"),
                        UTLA19NM = c("Buckinghamshire"),
                        UTLA20CD = c("E06000060"),
                        UTLA20NM = c("Buckinghamshire"),
                        stringsAsFactors = FALSE)

usethis::use_data(UTLA_1920, overwrite = TRUE)

# Dorset, Poole and Bournemouth
# CTYUA_1819 <- data.frame(CTYUA19CD = c(""),
#                          CTYUA19NM = c("Bournemouth", "Poole","Dorset"),
#                          CTYUA20CD = c("E06000059"),
#                          CTYUA20NM = c(rep("Bournemouth, Christchurch and Poole", 3)"))





