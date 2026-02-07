library(tidyverse)
library(terra)
library(tidyterra)
library(RUtils)

source(codeFile("usefulextents.R"))
source(codeFile("coastsNcities.R"))

degrees=1
incidentProfilesRaw <- read_csv(inputFile(paste0("SharkIncidents-SST-SHL-",degrees,"Deg.csv")))

incidentProfiles <- incidentProfilesRaw |>
  filter(!is.na(CHL_mean))
|>
  # mutate(CHL_min = log(CHL_min),
  #        CHL_mean = log(CHL_mean),
  #        CHL_max = log(CHL_max),
  #        CHLAttack_min = log(CHLAttack_min),
  #        CHLAttack_mean = log(CHLAttack_mean),
  #        CHLAttack_max = log(CHLAttack_max),
  #       ) |>
  slice_tail()
dataDegrees <- slice_head(incidentProfiles)$DataDegrees
rangeDegrees <- slice_head(incidentProfiles)$RangeDegrees

chart <- ggplot(incidentProfiles, aes(x=CHLAttack_mean,y=SSTAttack_mean)) +
  geom_point() +
  customTheme()

chart

chart <- ggplot(incidentProfiles, aes(xmin=CHLAttack_min,xmax=CHLAttack_max,ymin=SSTAttack_min,ymax=SSTAttack_max)) +
  geom_rect(alpha=0.05) +
  geom_point(mapping = aes(x=CHLAttack_mean,y=SSTAttack_mean),color="darkorange") +
  geom_point(mapping = aes(x=CHL_mean,y=SST_mean),color="darkgreen") +
  geom_segment(mapping = aes(x=CHLAttack_mean,y=SSTAttack_mean, xend=CHL_mean, yend=SST_mean),linewidth=0.1) +
  #geom_text(mapping = aes(x=CHLAttack_mean,y=SSTAttack_mean, label = str_to_title(Location)),size=2, nudge_x = 0.1,hjust = 0) +
  xlab("mean CHL mg/m^3") + ylab("mean SST C") +
  scale_x_continuous(expand = expansion(add=0),trans = "log",breaks = c(0.015,0.03,0.06,0.125,0.25,0.5,1,2,4,8,16)) +

  ggtitle("Temperature, CHL concentrations for 81 NSW White Shark Attacks since 2004",
          paste("Green In region +/-", round(rangeDegrees,1),"degree around attack",
          "\nOrange In region +/-", round(dataDegrees,1),"degrees around attack")) +
  customTheme() +
  theme( axis.title.x = element_text()
         )
chart
saveSquareChart("SST-CHL-Near Attacks",chart)

# plot all attacks
incidentVect <- vect(incidentProfiles,geom=c("Lon","Lat"), crs = GDA2020_CRS)

chart <- ggplot() +
  #geom_sf() +
  geom_spatvector(data = NSW_Coast,linewidth = 0.01,color = "grey10") +
  geom_spatvector(data = NSW_Cities,color="grey40") +
  geom_spatvector_text(data = NSW_Cities,color="grey10",
                       mapping = aes(label=values(NSW_Cities)$name),
                       size=2.5,
                       hjust="right",
                       nudge_x = -0.05,
  ) +
  geom_spatvector(data = incidentVect,color="orange") +
  coord_sf() +
  scale_x_continuous(expand = expansion(add=0),limits = c(149.5, 156)) +
  scale_y_continuous(expand = expansion(add=0),limits =c(-37.893, -27.961)) +
  xlab("") + ylab("") +
  customTheme()

chart
saveSquareChart("SST-CHL-Near Attacks",chart)



tracesProfilesRaw <- read_csv(inputFile(paste0("SharkIncidents-SST-SHL-Traces",degrees,"Deg.csv"))) |>
  filter(!is.na(CHLAttack_mean)) |>
  arrange(TraceDate)

chart <- ggplot(tracesProfilesRaw, aes(x=CHLAttack_mean,y=SSTAttack_mean,group = UIN)) +
  geom_path(linewidth=0.1,color="grey40") +
#  geom_point(mapping = aes(x=CHLAttack_mean,y=SSTAttack_mean),color="grey40") +
  #geom_text(filter(tracesProfilesRaw, IncidentDate == TraceDate),mapping = aes(label=paste(TraceDate,"\n",round(CHLAttack_mean,1),round(SSTAttack_mean,1))),size=2,vjust=1) +
  geom_text(filter(tracesProfilesRaw, IncidentDate == TraceDate), mapping = aes(x=CHLAttack_mean,y=SSTAttack_mean, label = str_to_title(paste(Location,TraceDate))),size=3, nudge_x = 0,hjust = 0) +
  geom_point(filter(tracesProfilesRaw, IncidentDate == TraceDate),mapping = aes(x=CHLAttack_mean,y=SSTAttack_mean),size=3,color="darkorange") +
  xlab("mean CHL mg/m^3") + ylab("mean SST C") +
  scale_x_continuous(trans = "log",breaks = c(0.015,0.03,0.06,0.125,0.25,0.5,1,2,4,8,16)) +

  ggtitle("CHL concentrations, SST traces for NSW White Shark Attacks since 2004",
          paste("Trace for prior",traceDays,"days prior to atttack shown in orange")) +

  customTheme() +
  theme( axis.title.x = element_text()
  )
chart
saveSquareChart("SST-CHL-Traces",chart)
