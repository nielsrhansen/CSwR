angle <- read.csv("phipsi.csv")
angle[, c("phi", "psi")] <- pi * angle[, c("phi", "psi")] / 180

save(angle, file = "../CSwR_package/data/angle.RData", compress = "xz")
