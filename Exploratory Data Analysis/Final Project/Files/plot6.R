plot6 <- function(){
        # 6. Compare emissions from motor vehicle sources in Baltimore City with emissions 
        # from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
        # Which city has seen greater changes over time in motor vehicle emissions?
        
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
                filter(fips %in% c("24510","06037") & type == 'ON-ROAD') %>%
                group_by(fips, year) %>% 
                summarise(Emissions = sum(Emissions))
        
        # create png
        png(filename = 'NEIdata/plot6.png')
        plotpng <- ggplot(motor.Emissions, aes(year, Emissions, col = fips)) + 
                geom_point() + 
                geom_line() + 
                ggtitle(expression('Total '~PM[2.5]~' Motor Vehicle Emissions in Baltimore City and Los Angeles by Year')) + 
                ylab(expression(~PM[2.5]~' Emissions')) +
                scale_colour_discrete(name = 'City', labels = c('Los Angeles', 'Baltimore'))
        print(plotpng)
        dev.off()
}