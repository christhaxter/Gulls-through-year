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

######## country.zip ########
# Shapefiles for plotting base maps

######## predgrid_Walney.csv ... ########
# Prediction grids for each colony; note suffix of 'firstyear' for Orford Ness presenting the first year of tracking data in this paper

######## Walney.WR.csv ... ########
# Full raw data files on distance travelled within the CRW per square, bird and Julian date, feeding into modelling for sensitivity
# These files are large and have been uploaded as .gz zip files, available under the release tab

######## p.coll.surface.csv ########
# Probability of collision, for combining with the spatial model of distance travelled within the collision risk window to 
# produce final sensitivity surfaces.

######## n.turb.csv ########
# csv file of number of turbines per grid square used as the exposure surface

######## Walney.sp.agg.csv ########
# Saved aggregation of modelled GAMM sensitivity surface, aggregated across julian dates
# For further plotting as quantiles across blank swathe of area use

######## Walney.200.5_WR.csv ... ########
# Full model output files for each colony, numbers denoting degrees of freedom in space and time (model A = 200.5, model B = 25.100)
# These files are large and have been uploaded as .gz zip files, available under the release tab




