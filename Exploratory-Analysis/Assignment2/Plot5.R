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

## Find all the motor vehicle sources from 'SCC'
motorVehicleSourceDesc <- unique(grep("Vehicles", SCC$EI.Sector, ignore.case = TRUE, 
                                      value = TRUE))
motorVehicleSourceCodes <- SCC[SCC$EI.Sector %in% motorVehicleSourceDesc, ]["SCC"]

## Subset emissions due to motor vehicle sources in from 'NEI' for Baltimore
emissionFromMotorVehiclesInBaltimore <- NEI[NEI$SCC %in% motorVehicleSourceCodes$SCC & 
                                              NEI$fips == "24510", ]

## Calculate the emissions due to motor vehicles in Baltimore for every year
totalMotorVehicleEmissionsByYear <- tapply(emissionFromMotorVehiclesInBaltimore$Emissions, 
                                           emissionFromMotorVehiclesInBaltimore$year, sum)

plot(totalMotorVehicleEmissionsByYear, x = rownames(totalMotorVehicleEmissionsByYear), 
     type = "n", axes = FALSE, ylab = expression("Motor Vehicle Related PM"[2.5] * 
                                                   " Emission (in tons)"), xlab = "Year", main = expression("Motor Vehicle Related PM"[2.5] * 
                                                                                                              " Emission in Baltimore (1999 - 2008)"))
points(totalMotorVehicleEmissionsByYear, x = rownames(totalMotorVehicleEmissionsByYear), 
       pch = 16, col = "red")
lines(totalMotorVehicleEmissionsByYear, x = rownames(totalMotorVehicleEmissionsByYear), 
      col = "blue")
axis(2)
axis(side = 1, at = seq(1999, 2008, by = 3))
box()

dev.copy(png, filename = "Plot5.png", width=800, height=700)
dev.off()