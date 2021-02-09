plot4 <- function(){
        # 4. Across the United States, how have emissions from coal combustion-related 
        # sources changed from 1999-2008?
        
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
        
        
        # creating table with coal emissions
        coal.Emissions <- scc[grepl("coal", scc$Short.Name, ignore.case = TRUE),]
        coal.Summary.Emissions <- sumscc[sumscc$SCC %in% coal.Emissions$SCC,]
        total.Coal.Emissions <- coal.Summary.Emissions %>% 
                group_by(type, year) %>% 
                summarise(Emissions = sum(Emissions))
        
        # create png
        png(filename = 'NEIdata/plot4.png')
        plotpng <- ggplot(total.Coal.Emissions, aes(year, Emissions, col = type)) + 
                geom_point() + 
                geom_line() + 
                ggtitle(expression('Total '~PM[2.5]~' Coal Emissions by Type and Year')) + 
                ylab(expression(~PM[2.5]~' Emissions')) + 
                xlab('Year') + scale_color_discrete(name = 'Type of Sources')
        print(plotpng)
        dev.off()
}