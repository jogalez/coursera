plot3 <- function(){
        # 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
        # variable, which of these four sources have seen decreases in emissions from 1999-2008 
        # for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 
        # plotting system to make a plot answer this question. library(ggplot2)
        
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
        
        
        # creating table with sumarise year and emissions
        total.Emissions <- sumscc %>% 
                filter(fips == "24510") %>%
                group_by(type, year) %>% 
                summarise(Emissions = sum(Emissions))
        
        # create png
        png(filename = 'NEIdata/plot3.png')
        plot3png <- ggplot(total.Emissions, aes(year, Emissions, col = type)) + 
                geom_point() + 
                geom_line() + 
                ggtitle(expression('Total '~PM[2.5]~' Baltimore City, Maryland Emissions by Type and Year')) + 
                ylab(expression(~PM[2.5]~' Emissions')) + 
                xlab('Year') + scale_color_discrete(name = 'Type of Sources')
        print(plot3png)
        dev.off()
}