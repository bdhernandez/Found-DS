str(refine)

#separated product code into 2 columns using tidyr::separate
refine2 <- separate(refine, productcode, c("prodcode", "prodnum"), sep = "-" )
refine2

#add product categories in category column, linked to values in prodcode column
refine2$prodcat[refine2$prodcode == "p"] <- "Smartphone"
refine2$prodcat[refine2$prodcode == "v"] <- "TV"
refine2$prodcat[refine2$prodcode == "x"] <- "Laptop"
refine2$prodcat[refine2$prodcode == "q"] <- "Tablet"
refine2

#add full address for geocoding; used tidyr::unite to merge 3 columns
refine2 <- unite(refine2, full_address, address, city, country, sep = ",")

#create dummy variables for company; used ifelse function
refine2$akzo_company <- ifelse(refine2$company == "akzo", 1, 0)
refine2$phillips_company <- ifelse(refine2$company == "phillips", 1, 0)
refine2$vanhouten_company <- ifelse(refine2$company == "van houten", 1, 0)
refine2$unilever_company <- ifelse(refine2$company == "unilever", 1, 0)
refine2

#create dummy variables for product category; used ifelse function
refine2$product_smartphone <- ifelse(refine2$prodcat == "Smartphone", 1, 0)
refine2$product_TV <- ifelse(refine2$prodcat == "TV", 1, 0)
refine2$product_laptop <- ifelse(refine2$prodcat == "Laptop", 1, 0)
refine2$product_tablet <- ifelse(refine2$prodcat == "Tablet", 1, 0)
refine2

write.csv(refine2, file = "refine_clean.csv")
