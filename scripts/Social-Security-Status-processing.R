library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Social Security Status
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

# Get geography object for CT and subcounty divisions
ctGeos <- getCTGeos("town")
# years for which we will process this dataset
#yearList = c(2015)
yearList = c(2010:2018)

tn = "B19055"
acsdata <- getACSData(ctGeos, yearList = yearList, table = tn)

dataset <- data.table()
for(data in acsdata) {
    year <- data@endyear
    print(paste("Processing: ", year))
    year <- paste(year-4, year, sep = "-")

    # Total households counted
    # this total also used as denominator
    total <- data[, 1]
    acs.colnames(total) <- "Number:Total"

    with.ssi <- data[, 2]
    percent.with.ssi <- divide.acs(with.ssi, total, method = "proportion")

    without.ssi <- data[, 3]
    percent.without.ssi <- divide.acs(without.ssi, total, method = "proportion")

    # merge in fips
    datafips <- data.table(fips = getACSFips(data))

    # Cast to separate data frames
    numberEstimates <- data.table(
        datafips$fips,
        estimate(total),
        estimate(with.ssi),
        estimate(without.ssi),
        year,
        "Number",
        "Social Security Status"
    )
    numberMOES <- data.table(
        datafips$fips,
        standard.error(total) * 1.645,
        standard.error(with.ssi) * 1.645,
        standard.error(without.ssi) * 1.645,
        year,
        "Number",
        "Margins of Error"
    )
    numberNames <- c(
        "FIPS",
        "Number:Total",
        "Number:With SSI",
        "Number:Without SSI",
        "Year",
        "Measure Type",
        "Variable"
     )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)

    numbersData.melt <- melt(
        rbind(numberEstimates, numberMOES),
        id.vars = c("FIPS", "Year", "Measure Type", "Variable"),
        variable.name = "Social Security Status",
        variable.factor = F,
        value.name = "Value",
        value.factor = F
     )

    percentEstimates <- data.table(
        datafips$fips,
        estimate(percent.with.ssi),
        estimate(percent.without.ssi),
        year,
        "Percent",
        "Social Security Status"
    )
    percentMOES <- data.table(
        datafips$fips,
        standard.error(percent.with.ssi) * 1.645,
        standard.error(percent.without.ssi) * 1.645,
        year,
        "Percent",
        "Margins of Error"
    )
    percentNames <- c(
        "FIPS",
        "Percent:With SSI",
        "Percent:Without SSI",
        "Year",
        "Measure Type",
        "Variable"
     )
    setnames(percentEstimates, percentNames)
    setnames(percentMOES, percentNames)

    percentsData.melt <- melt(
        rbind(percentEstimates, percentMOES),
        id.vars = c("FIPS", "Year", "Measure Type", "Variable"),
        variable.name = "Social Security Status",
        variable.factor = F,
        value.name = "Value",
        value.factor = F
     )

    dataset <- rbind(dataset, numbersData.melt, percentsData.melt)
}

#Final Additions, processing
# Split Measure type and SSI Status out of "Social Security" column, then drop that column
dataset[,c("Measure Type", "Social Security Status"):=do.call(Map, c(f = c, strsplit(`Social Security Status`, ":", fixed = T)))]
#dataset[,`Social Security` := NULL]

# Round Values according to type/variable
dataset[`Measure Type` == "Number", Value := round(Value, 0)]
dataset[`Measure Type` != "Number", Value := round(Value*100, 1)]

# Join town names by FIPS code
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

dataset <- merge(fips, dataset, by = "FIPS", all.x=T)

#set final column order
dataset <- dataset %>% 
  select(Town, FIPS, Year, `Social Security Status`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Social Security Status`, `Measure Type`, Variable) 

# Write to File
write.table(
    dataset,
    file.path(getwd(), "data", "social-security-status-2018.csv"),
    sep = ",",
    row.names = F,
    na = "-9999"
)
