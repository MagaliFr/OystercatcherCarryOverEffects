# Oystercatcher carry-over effects
Data and code for the manuscript from Frauendorf et al. entitled  xxx

Authors:
Magali Frauendorf, Andrew M. Allen, Henk-Jan van der Kolk, Sarah Cubaynes, Bruno J. Ens, Simon Verhulst, Eelke Jongejans, Hans de Kroon, Kees Oosterbeek, Karin Troost, Martijn van de Pol

File list and description:
- AgeMatrix.csv: age matrix for the MSM-SEM model
- StateMatrix.csv: state matrix for the MSM-SEM model with condition variables of each individual
- TCatch.csv: time step that individual was caught
- ID.csv: individual information of the bird
- COE_data.csv: carry-over effect dataset for the path analysis
- EnviornmentalVariables.csv: environmental variables for the MSM-SEM

Details on the data (variable names):
EnviornmentalVariables.csv:
- Kg_C_avail_Ind: kg cockles available per individual (within the 7-km radius around the catching location)
- Kg_M_avail_Ind: kg mussels available per individual (within the 7-km radius around the catching location)
- ObsCatchNr: catching location/capture ID (one date, time and location)
- PropGrassland: grassland proportion available (within the 7-km radius around the catching location)
- Density_km2: conspecific density (individuals per km2, within the 7-km radius around the catching location)
- Avg_exp_NovFeb: average exposure time for the period November-February (within the 7-km radius around the catching location)
- avgTemp_1M: average temperature (from the nearest weather station) for one month before capture event
- avgTemp_2M: average temperature (from the nearest weather station) for two month before capture event
- avgWC_1M: wind chill index (from the nearest weather station) for one month before capture event
- avgWC_2M: wind chill index (from the nearest weather station) for two month before capture event
- sumRH_month_1M: precipitation sum (mm/month) (from the nearest weather station) for one month before capture event
- sumRH_month_2M: precipitation sum (mm/month) (from the nearest weather station) for two month before capture event
- PrepAnom_month_1M: precipitation anomaly (difference from long-term average) (from the nearest weather station) for one month before capture event
- PrepAnom_month_2M: precipitation anomaly (difference from long-term average) (from the nearest weather station) for two month before capture event

COE_data.csv:
- Code: colour ring code of the individual
- CatchingArea: winter catching area
- NestID: id of the nest
- BirdId: individual bird id
- Sex: sex of the bird (M=male, F=female)
- LD: lay date (format: dd-mm-yyy)
- LD_Num: numeric lay date starting at 1st April of the year
- ClutchSize: maximum clutch size
- HatchDate: estimated/calculated/observed hatch date of the first hatchling (format dd-mm-yyyy)
- DayFound: date that nest is found
- LastActive: date that nest is active for last time
- LastCheck: date that the nest has been checked for the last time and determined failed or hatched
- ObsDay: number of days that nest was exposed
- Fail: nest failed (1=yes,0=no)
- NestSuccess: Daily clutch survival (mayfield)
- clutchNr: first clutch (1) or replacement clutch (>1)
- ClutchCertain: certainty of first or replacement clutch (1=certain, 2=uncertain)
- Habitat: habitat type (saltmarsh, cropfield, urban, grassland, nature)
- IC: coastal (<2km from coast) or inland (>2km from coast) nest location
- ChickSuccess: 0=no fledglings, 1= at least one hatchling fledged
- ChickSurvival: number of days the chick survived
- NrHatchlings: number of hatchlings
- NrFledglings: number of fledglings
  
AgeMatrix.csv:
- Age_1final – Age_40final: each time step of the multi-state model. Values indicate: 1=1 year birds, 2=2 and 3 year old birds, 3=adults (>3 years)
- each row is one individual.
  
StateMatrix.csv:
- SexN: sex of the bird, 1=female, 2=male
- AgeN: age class, 1=1 year old, 2=2 year old, 3=3 and >3 year old birds
- MS: body mass in gram
- LogitH: logit from proportion haematocrit
- LogitB: logit from proportion buffy coat
- TA: tarsus length (mm)
- TH: total head length (incl. bill) (mm)
- WL: wing length (mm)
- BH: Bill tip height (mm)
- HT: handling time (proportion of 24 hours time)
- DS: day in the season (1=1st November)
- T1 – T40 indicate the 18 time steps of the multi-state model. Number 1-10 mean the following: 9 alive states: D, P, N, B, V, T, S, R, X (see Fig. 3.2 from PhD thesis and text for explanation), 10 means recovered dead.
  
ID.csv:
- ObsCatchNr: capture event
- BirdId: ID of the bird
- Code: colour ring code
  
TCatch:
- TCatch: time step that the individual was caught (in order to apply time variant condition variable in MSM-SEM)
  
Note that the ID.csv file can be cbind with the TCatch, StateMatrix and AgeMatrix file, meaning that they have the correct order.

Data collection:
For details on the data collection, we refer to the manuscript.
