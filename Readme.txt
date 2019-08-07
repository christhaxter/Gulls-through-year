#############################################################################################
# Files to accompany paper: 
# Thaxter, C.B. et al. Avian Vulnerability to Wind Farm Collision Through the Year: 
# Insights from Lesser Black-backed Gulls (Larus fuscus) Tracked from  Multiple Breeding Colonies. 
# Journal of Applied Ecology
#############################################################################################

########################### SCRIPTS ###########################

######## ORFORD.R ########
# Full modelling script. note file sizes were very large and so have not been uploaded here, but models were specified
# as in this example - note called "ORFORD" but actually set to model the Walney colony, via toggle of colony name

######## Spatial plots.SEV.R ########
# Full mapping script to produce main graphical outputs within the paper. NOTE: Original files used are too big to be shared here. Please # contact me if these are required. 

########################### DATASETS ###########################

######## Walney.sens.raw.exam.bird4032.csv ########
# Raw data on distance travelled within the CRW with zero data included for each square per julian date

######## Walney.sp.agg.csv ########
# Saved aggregation of modelled GAMM sensitivity surface, aggregated across julian dates
F# or further plotting as quantiles across blank swathe of area use

######## n.turb.csv ########
csv file of number of turbines per grid square used as the exposure surface

######## p.coll.surface.csv ########
csv file of the probability of collision combined with the spatial model of distance travelled within the collision risk window to produce final sensitivity surfaces.



