## data processing for the light beer data

beer <- read.csv("lightbeer-raw.csv")

## some simple re-naming
beer$household <- as.integer(beer$household_id)
beer$upc_description <- as.character(beer$upc_description)
beer$household_id <- NULL

## prices, coupons, packaging
beer$beer_spend <- beer$price_paid_deal + beer$price_paid_non_deal - beer$coupon_value
beer$beer_floz <- beer$quantity*beer$multi*beer$size_floz
beer$price_floz = beer$beer_spend/beer$beer_floz
# drop the rest
beer$price_paid_deal <- beer$price_paid_non_deal <- beer$coupon_value <- beer$total_spent <- NULL
beer$multi <- beer$size_floz <- NULL

## promos
beer$promotion <- TRUE
beer$promotion[is.na(beer$promotion_type)] <- FALSE
beer$promotion_type <- NULL

## rename brand
beer$beer_brand <- beer$brand_descr
beer$brand_descr <- NULL

## Geography
sm <- c("BOSTON", "CHICAGO", "HOUSTON", "INDIANAPOLIS",
        "JACKSONVILLE", "KANSAS CITY", "LOS ANGELES", "SURBURBAN NY",
        "URBAN NY", "EXURBAN NY", "ORLANDO", "SAN FRANCISCO", "SEATTLE",
        "ATLANTA", "CINCINNATI", "CLEVELAND", "DALLAS", "DENVER", "DETROIT",
        "MIAMI", "MILWAUKEE", "MINNEAPOLIS", "NASHVILLE", "PHILADELPHIA",
        "PITTSBURGH", "PORTLAND, OR", "SAN DIEGO", "ST. LOUIS", "TAMPA",
        "BALTIMORE", "BIRMINGHAM", "BUFFALO-ROCHESTER",
        "HARTFORD-NEW HAVEN", "LITTLE ROCK", "MEMPHIS",
        "NEW ORLEANS-MOBILE", "OKLAHOMA CITY-TULSA", "PHOENIX",
        "RALEIGH-DURHAM", "SALT LAKE CITY", "COLUMBUS", "WASHINGTON DC",
        "ALBANY", "CHARLOTTE", "DES MOINES", "GRAND RAPIDS", "LOUISVILLE",
        "OMAHA", "RICHMOND", "SACRAMENTO", "SAN ANTONIO", "SYRACUSE")

statename <- c("ALABAMA", "ARIZONA", "ARKANSAS", "CALIFORNIA",
               "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA",
               "FLORIDA", "GEORGIA", "IDAHO", "ILLINOIS", "INDIANA",
               "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND",
               "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI",
               "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY",
               "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO",
               "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND",
               "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH",
               "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING") #no HI or AK

beer$region <- factor(beer$region, levels=1:4, labels=c("EAST","CENTRAL","SOUTH","WEST")) 
beer$state <- statename[as.numeric(factor(beer$fips_state_code))]

beer$market <- sm[beer$scantrack_market_identifier]
nosm <- is.na(beer$market)
beer$market[nosm] <- paste("RURAL", beer$state[nosm])

beer$market <- factor(beer$market)
beer$state <- factor(beer$state)
beer$scantrack_market_identifier <- NULL 
beer$fips_state_code <- NULL 

## Demographics
beer$buyertype <- 
  factor(beer$household_composition, levels=c(1,3,7,8,2,5,6))
levels(beer$buyertype) <- c("married",rep("male",3),rep("female",3))
beer$marital_status <- beer$household_composition <- NULL

inc <- beer$household_income
beer$household_income[inc < 12] <- "under20k"
beer$household_income[inc > 11 & inc < 22] <- "20-60k"
beer$household_income[inc > 21 & inc < 27] <- "60-100k"
beer$household_income[inc > 26 & inc < 30] <- "100-200k"
beer$household_income[inc == 30] <- "200k+"
beer$income <- factor(beer$household_income, levels=c("under20k","20-60k","60-100k","100-200k","200k+"))
beer$household_income <- NULL


beer$childrenUnder6 <- beer$age_and_presence_of_children%in%c(1,4,5,7)
beer$children6to17 <- beer$age_and_presence_of_children%in%c(2:7)
beer$age_and_presence_of_children <- NULL

beer$age <- factor( apply( cbind(beer$age_of_female_head, beer$age_of_male_head), 1, max), levels=0:9 )
levels(beer$age) <- c(NA, "<30","<30","30-39","30-39","40-49","40-49","50+","50+","50+")
beer$age_of_female_head <- beer$age_of_male_head <- NULL


beer$employment <- rep("none",nrow(beer))
beer$employment[ beer$female_head_employment%in%c(1,2) | beer$male_head_employment%in%c(1,2) ] <- "part"
beer$employment[ beer$female_head_employment==3 | beer$male_head_employment==3 ] <- "full"
beer$employment <- factor(beer$employment, levels=c("none","part","full"))
beer$female_head_employment <- beer$male_head_employment <- NULL


edu <- apply(cbind(beer$female_head_education, beer$male_head_education), 1, max)
beer$degree <- rep("none", nrow(beer))
beer$degree[ edu %in% 3:4 ] <- "HS"
beer$degree[ edu == 5 ] <- "College"
beer$degree[ edu == 6 ] <- "Grad"
beer$degree <- factor(beer$degree, levels=c("none","HS","College","Grad"))
beer$female_head_education <- beer$male_head_education <- NULL


beer$occupation <- rep("none/retired",nrow(beer))
beer$occupation[ beer$female_head_occupation==11 | beer$male_head_occupation==11 ] <- "unskilled labor"
beer$occupation[ beer$female_head_occupation==6 | beer$male_head_occupation==6 ] <- "semi-skilled labor"
beer$occupation[ beer$female_head_occupation%in%c(3,4,8) | beer$male_head_occupation%in%c(3,4,8) ] <- "clerical/sales/service"
beer$occupation[ beer$female_head_occupation==5 | beer$male_head_occupation==5 ] <- "skilled labor"
beer$occupation[ beer$female_head_occupation==9 | beer$male_head_occupation==9 ] <- "farm"
beer$occupation[ beer$female_head_occupation==7 | beer$male_head_occupation==7 ] <- "military"
beer$occupation[ beer$female_head_occupation==10 | beer$male_head_occupation==10 ] <- "student"
beer$occupation[ beer$female_head_occupation%in%c(1,2) | beer$male_head_occupation%in%c(1,2) ] <- "prof"
beer$occupation <- factor(beer$occupation, levels=c("none/retired","student", "farm","military","semi-skilled labor",
                                             "skilled labor", "unskilled labor", "clerical/sales/service", "prof") )
levels(beer$occupation) <- c(rep("none/retired/student",2), rep("labor/craft/military/farm",5), "clerical/sales/service", "prof")
beer$female_head_occupation <- beer$male_head_occupation <- NULL

beer$ethnic <- factor(beer$race, levels=1:5, labels=c("white","black","asian","other","hispanic"))
beer$ethnic[beer$hispanic_origin==1] <- "hispanic"
beer$race <- beer$hispanic_origin <- NULL

beer$microwave <- beer$kitchen_appliances%in%c(1,4,5,7)
beer$dishwasher <- beer$kitchen_appliances%in%c(2,4,7)
beer$kitchen_appliances <- NULL

beer$tv <- rep("none", nrow(beer))
beer$tv[beer$tv_items==1] <- "network"
beer$tv[beer$tv_items==2] <- "basic"
beer$tv[beer$tv_items==3] <- "premium"
beer$tv <- factor(beer$tv, levels=c("none","network", "basic", "premium"))
beer$tv_items<- NULL
beer$tvcable <- factor(beer$tv, levels=c("none","network","basic","premium"))
levels(beer$tvcable) <- c("none","none","basic","premium")
beer$tv <- NULL


beer$singlefamilyhome <- beer$type_of_residence%in%1:2
beer$type_of_residence <- NULL 

# beer$internet <- beer$household_internet_connection==2
beer$household_internet_connection <- NULL 

beer$npeople <- beer$household_size
beer$npeople[beer$npeople>4] <- 5
beer$npeople <- factor(beer$npeople,levels=1:5,labels=c(1:4,'5plus'))
beer$household_size <- NULL

## some other drops for the midterm
beer$state <- beer$region <- NULL

what <- c("household","upc_description","quantity",
  "beer_brand","beer_spend","beer_floz","price_floz")
beer <- beer[,c(what,names(beer)[!names(beer)%in%what])]
beer <- beer[beer$beer_spend>0,] # give-aways

cat("data.frame `beer' has variables:\n")
print(sapply(beer,levels))

write.table(beer, file="LightBeer.csv", sep=",", row.names=FALSE)









