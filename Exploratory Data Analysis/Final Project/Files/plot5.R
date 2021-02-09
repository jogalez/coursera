plot5 <- function(){
        # 5. How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
        
        #checking or installing libraries
        list.of.packages <- c("ggplot2", "dplyr")
        new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
        if(length(new.packages)) install.packages(new.packages)
        library(ggplot2)
        
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
        
        
        # creating table with motor vehicle emissions
        motor.Emissions <- sumscc %>% 
                filter(fips == "24510" & type == 'ON-ROAD') %>%
                group_by(type, year) %>% 
                summarise(Emissions = sum(Emissions))
        
        # create png
        png(filename = 'NEIdata/plot5.png')
        plotpng <- ggplot(motor.Emissions, aes(year, Emissions)) + 
                geom_point() + 
                geom_line() + 
                ggtitle(expression('Total '~PM[2.5]~' Motor Vehicle Emissions in Baltimore City by Year')) + 
                ylab(expression(~PM[2.5]~' Emissions')) 
        print(plotpng)
        dev.off()
}