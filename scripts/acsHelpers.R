library(acs)
library(data.table)

# split fips into County and County Subdivision Components
getCTGeos <- function(level = "town") {
    level <- tolower(level)
    if (!(level %in% c("town", "county", "both"))) {
            stop(
                "Unknown geographic level specified.
                 Currently supported values for \"level\" parameter are \"town\", \"county\", or \"both\"!"
                )
    }
    # Set flags - these will be used to determine which geos to include as the process continues
    returnTowns <- level %in% c("town", "both")
    returnCounties <- level %in% c("county", "both")

    # Get our FIPS from file
    town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
    town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
    fips <- (town_fips_dp$data[[1]])
    fips <- as.data.table(fips)
    #fips <- read.csv(file.path(getOption("common_path"), "Geography", "town_fips.csv"))

    subdiv_fips <- fips[c(2:170),]
    subdiv_fips$county <- sapply(subdiv_fips$FIPS, function(x) {
        as.numeric(substr(x,3,4))
    })
    countyfips <- unique(subdiv_fips$county)

    if (returnTowns) { # If we're return towns (or both), process subdivision fips into separate column
        subdiv_fips$subdiv <- sapply(subdiv_fips$FIPS, function(x) {
            as.numeric(substr(x,5,9))
        })
    }

    # Generate ACS structured Geo Object
    # no matter what, start with geo object that contains one entry for the state level
    geosObj <- geo.make(state=09)
    # loop through by county, regardless of desired level(s).
    for(i in 1:length(countyfips)) {
        county <- countyfips[i]
        if (returnTowns) { # If we're return towns (or both)
            subdivs <- subdiv_fips[subdiv_fips$county==county,4]
            geos <- geo.make(state=09, county=county, county.subdivision=subdivs$subdiv)
            geosObj <- geosObj + geos
        }
        if (returnCounties) { # If we're return counties (or both)
            geos <- geo.make(state = 09, county = county)
            geosObj <- geosObj + geos
        }
    }
    return(geosObj)
}

## Helper Function for Easy Summing of ACS.R S4 Object Columns (e.g. table disaggregations)
## Returns a formal acs object with all class methods preserved (e.g. estimate, standard.error)
acsSum <- function(acsobj, range, newColumnName) {
    if (length(range) > 1) {
        returnObj <- acsobj[,range[1]]
        for (i in 2:length(range)) {
            returnObj <- returnObj + acsobj[,range[i]]
        }
    } else {
        return(returnObj <- acsobj[,range])
    }
    acs.colnames(returnObj) <- newColumnName
    return(returnObj)
}

divide.acs <- function(num, den, method = "ratio", verbose = T) {
    q <- acs::divide.acs(num, den, method, verbose = verbose)

    if (method == "proportion" & any(is.nan(standard.error(q)))) {
        print("Fixing NaN")
        q <- acs::divide.acs(num, den, method = "ratio", verbose = verbose)
    }

    return(q)
}

## Wrapper for getting ACS
getACSData <- function(geoObject, yearList, table) {
    data.list <- list()
    for (y in yearList) {
        print(paste("Processing ACS year", y, sep=":"))
        # check if file exists for given year/table
        filename <- paste("ACS", (as.numeric(y)-2000), "5YR", table, "with", "ann.csv", sep = "_")
        rawFile <- file.path(getwd(), "raw", filename)
        if (file.exists(rawFile)) {
            print("Found raw file")
            data <- read.acs(rawFile, endyear = y, geocols = 1:3)
        } else {
            print("No raw file - Using API")
            # if not, fetch from API
            data <- acs.fetch(geography=geoObject, endyear=y, table.number=table, col.names="pretty", 
                              key="ed0e58d2538fb239f51e01643745e83f380582d7")
        }
        print(paste("Fetched", data@endyear, sep=":"))
        data.list <- c(data.list, data)
    }
    return(data.list)
}

# Abstracted function to pull, format, and return a vector of FIPS that correlates with given acs object
getACSFips <- function(acsObject) {
    if (setequal(names(geography(acsObject)), c("Id", "Id2", "Geography"))) {
        # this is read from flat file with read.acs
        datafips <- as.data.table(geography(acsObject))
        datafips[, fips := strsplit(Id, "US", fixed = T)[[1]][2], by = Id]
    } else if (length(names(geography(acsObject))) == 4) {
        # this must be from API and at town level
        datafips <- data.table(geography(acsObject)[3], geography(acsObject)[4])
        datafips$fips <- paste(
            "09",
            ifelse(is.na(datafips$county), "", sprintf("%03i", datafips$county)),
            ifelse(is.na(datafips$countysubdivision), "", datafips$countysubdivision),
            sep=""
        )
    } else {
        # this must be from API and at county level
        datafips <- data.table(geography(data)[3])
        datafips$fips <- paste(
            "09",
            ifelse(is.na(datafips$county), "", sprintf("%03i", as.integer(datafips$county))),
            sep = ""
        )
    }
    return(datafips$fips)
}

append.table <- function(data, file, ...) {
    if(!is.data.table(data) & !is.data.frame(data) & is.character(file)) {
        message("Assuming new dataset provided as file path . . .")
        if (!file.exists(data)) {
            stop("New dataset file does not exist")
        }

        data <- fread(data, colClasses = "character")
    }

    if (nrow(data) == 0) {
        stop("Can't append empty table!")
    }

    if (!file.exists(file)) {
        stop("output file does not exist! Writing table as new file. . .")
    }

    old <- fread(file, colClasses = "character")
    if (!setequal(names(data), names(old))) {
        extra <- setdiff(names(data), names(old))
        missing <- setdiff(names(old), names(name))
        message <- character(0)

        if (length(extra) > 0) {
            message <- c(message, "\n", "Columns in new data not present in existing data: ", paste0(extra, collapse = ", "))
        }
        if (length(missing) > 0) {
            message <- c(message, "\n", "Columns in existing data not present in new data: ", paste0(missing, collapse = ", "))
        }

        message <-  c("Column names don't match!", message)
        stop(message)
    } else {
        # check if columns are in the same order - warn if not
        if (!all.equal(names(data), names(old))) {
            warning("Column order does not match! Using established order in old data. . .")
        }

        # backup for posterity - this algorithm for picking a new suffix number is really naive
        # for instance, if you have __.1.bak and __.3.bak, it will create __.2.bak
        # but that would be misleading looking at it later, so be very careful.
        backup.file <- paste(file, "bak", sep = ".")
        backup.index <- 0
        while (file.exists(backup.file)) {
            backup.index <- backup.index + 1
            backup.file <- paste(file, backup.index, "bak", sep = ".")
        }
        message("Backing up old datset to new file: ", backup.file)
        file.copy(file, backup.file)

        # Write to File
        write.table(
            rbind(old, data),
            file,
            ...
        )
    }
}
