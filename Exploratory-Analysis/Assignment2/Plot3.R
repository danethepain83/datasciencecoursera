setwd("C:/Git/datasciencecoursera/Exploratory-Analysis/Assignment2")

usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
totalPM25ByYear <- tapply(NEI$Emissions, NEI$year, sum)

## Calculate total PM2.5 emissions for each type by year in Baltimore
Baltimore <- NEI[NEI$fips == "24510", ]
emissionsByTypeAndYear <- aggregate(Emissions ~ type * year, data = Baltimore, 
                                    FUN = sum)

usePackage("ggplot2")
usePackage("grid")

## Setup ggplot with data frame
q <- qplot(y = Emissions, x = year, data = emissionsByTypeAndYear, color = type, 
           facets = . ~ type)

## Add layers
q + scale_x_continuous(breaks = seq(1999, 2008, 3)) + theme_bw() + geom_point(size = 3) + 
  geom_line() + labs(y = expression("Total " * PM[2.5] * " Emissions (in tons)")) + 
  labs(x = "Year") + labs(title = expression("Total " * PM[2.5] * " Emissions in Baltimore (1999 - 2008)")) + 
  theme(axis.text = element_text(size = 8), axis.title = element_text(size = 14), 
        panel.margin = unit(1, "lines"), plot.title = element_text(vjust = 2), 
        legend.title = element_text(size = 11)) + scale_colour_discrete(name = "Type")

dev.copy(png, filename = "Plot3.png", width=800, height=700)
dev.off()