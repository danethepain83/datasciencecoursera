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

## Find all the coal combustion related sources from 'SCC'
CoalCombustionSources <- SCC[SCC$EI.Sector == "Fuel Comb - Comm/Institutional - Coal", 
                             ]["SCC"]

## Subset emissions due to coal combustion sources from 'NEI'
emissionFromCoal <- NEI[NEI$SCC %in% CoalCombustionSources$SCC, ]

## Calculate the emissions due to coal each year across United States
totalCoalEmissionsByYear <- tapply(emissionFromCoal$Emissions, emissionFromCoal$year, 
                                   sum)

## Create the plot
plot(totalCoalEmissionsByYear, x = rownames(totalCoalEmissionsByYear), type = "n", 
     axes = FALSE, ylab = expression("Coal Related PM"[2.5] * " Emission (in tons)"), 
     xlab = "Year", main = expression("Coal Related PM"[2.5] * " Emission across United States (1999 - 2008)"))
points(totalCoalEmissionsByYear, x = rownames(totalCoalEmissionsByYear), pch = 16, 
       col = "red")
lines(totalCoalEmissionsByYear, x = rownames(totalCoalEmissionsByYear), col = "blue")
axis(2)
axis(side = 1, at = seq(1999, 2008, by = 3))
box()

dev.copy(png, filename = "Plot4.png", width=800, height=700)
dev.off()