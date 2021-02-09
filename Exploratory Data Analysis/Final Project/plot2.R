plot2 <- function(){
        # 2. Have total emissions from PM2.5 decreased in the Baltimore City, 
        # Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system 
        # to make a plot answering this question.
        
        #checking or installing libraries
        list.of.packages <- c("ggplot2", "dplyr")
        new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
        if(length(new.packages)) install.packages(new.packages)
        
        #download data from web
        if (!file.exists("NEIdata.zip")){
                dir.create("./NEIdata")
                urlzip <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
                download.file(urlzip, destfile = "./NEIdata.zip")
                unzip("./NEIdata.zip", exdir = "./NEIdata" )
        }
        
        #load data
        if (!exists("scc")){
                scc <- readRDS("./NEIdata/Source_Classification_Code.rds")
                sumscc <- readRDS("./NEIdata/summarySCC_PM25.rds") 
        }
        
        
        # creating table with sumarise year and emissions
        total.Emissions <- sumscc %>% 
                filter(fips == "24510") %>%
                group_by(year) %>% 
                summarise(Emissions = sum(Emissions))
        
        # create png
        png(filename = 'NEIdata/plot2.png')
        with(total.Emissions, plot(year, Emissions, type = 'o', main = expression('Total '~PM[2.5]~' Baltimore City, Maryland Emissions by Year'), ylab = expression(~PM[2.5]~' Emissions'), xlab = 'Year'))
        dev.off()
}