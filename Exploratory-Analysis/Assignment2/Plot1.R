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

plot(totalPM25ByYear, x = rownames(totalPM25ByYear), type = "n", axes = FALSE, 
     ylab = expression("Total PM"[2.5] * " Emission (in tons)"), xlab = "Year", 
     main = expression("Total PM"[2.5] * " Emission (1999 - 2008)"))
points(totalPM25ByYear, x = rownames(totalPM25ByYear), pch = 16, col = "red")
lines(totalPM25ByYear, x = rownames(totalPM25ByYear), col = "blue")
axis(2)
axis(side = 1, at = seq(1999, 2008, by = 3))
box()

dev.copy(png, filename = "Plot1.png", width=480, height=480)
dev.off()