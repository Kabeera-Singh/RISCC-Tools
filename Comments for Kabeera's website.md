# RISCC TOOLS

**Regulatory Visualization** [https://www.riscctools.org/regulatory-visualization/](https://www.riscctools.org/regulatory-visualization/) 

- [ ] Update metadata in the ‘How to use this tool’  
      - [ ] **Updated text:** Choose a state from the dropdown to create a list of all regulated species in that state. Choose a species from the dropdown to create a list of all states where that species is regulated. The map will highlight your selections, and the table below will update.  
            Data sources: The data from this resource was compiled from the National Plant Board Website [here](https://nationalplantboard.org/regulated-plant-list/).  Learn more about regulatory level definitions in the Reg.Level Definitions tab [here](https://docs.google.com/spreadsheets/d/1XdmtsDxd4A3fURZixemEsAvCmYQ8_XM9d10WezWCWlg/edit?usp=sharing).  
- [ ] Update metadata in the Native Status text box  
      - [ ] **New text:** Native Status is derived from the USDA PLANTS Database and is written in the format Region(Status). Regions are as follow: AK=Alaska, CAN=Canada, GL=Greenland, HI=Hawaii, L48=Lower 48 states, NA=North America (only non-vascular plants and lichens have Native Status given at this level), NAV=Navassa Island (the sole Caribbean member of the United States Minor Outlying Islands), PB=Pacific Basin excluding Hawaii, PR=Puerto Rico, SPM=St. Pierre and Miquelon (France), VI \= U.S. Virgin Islands.   
            Status within the region are as follows: (N) \= Native, (I) \= Introduced, (W) \= Waif/Occasional. Question marks note probable status.  
- [ ] Add recommended citation at the bottom: Recommended Citation: Singh, K., J. Salva, M. Fertakos, and B.A., Bradley. RISCC Tools: State-based regulated plant data viewer. URL: https://www.riscctools.org/regulatory-visualization/ , access date.   
- [ ] Update compatibility with mobile phone use (the right side is hard to access), currently better in landscape mode than portrait

**Additional things that would be nice to consider:**

- [ ] How do we deal with and symbolize no data values?  
- [ ] Is it possible to have the mapframe rescale to where the species is present/regulated (or default to a whole-US view)? When you reset filters it should zoom back out to the original extents (full CONUS)  
- [ ] On the regulation map is there a way to see where species are present, regulated, and present AND regulated- or only regulated?’  
- [ ] For Puerto Rico specifically when the list of states is re-set to “All” the PR remains highlighted, even when you select “reset filters”  
- [ ] The underlying data for this tool should match [this database](https://docs.google.com/spreadsheets/d/1XdmtsDxd4A3fURZixemEsAvCmYQ8_XM9d10WezWCWlg/edit?gid=651617479#gid=651617479) (it might be slightly out of date).  We can also link to this database for people who want more underlying information.  
- [ ] Is it possible to have the map center the states where a species is regulated upon searching? So for a species that is widely regulated the map would just re-center, and for one that is only regulated in one state/region it would zoom? For example: *Cyperus asper* is only regulated in Rhode Island which is difficult to notice with the map centered.   
- [ ] Download file names \- if a state is selected the output filename should be \[state\]\_regulated\_invasive\_plants\_\[date\].  If a species is selected the output filename should be \[Genus\_species\]\_regulated\_states\_\[date\]

# C-Snap

# Home page \- updated text:

## **Why native plants?**

Native plants, defined as species historically found growing without human intervention in a given location, provide substantial ecological benefits including increasing the abundance of native birds and pollinators (*learn more [here](https://www.risccnetwork.org/research-summaries/impact-of-native-plants-on-bird-and-butterfly-biodiversity-in-suburban-landscapes?rq=tallamy) and [here](https://www.risccnetwork.org/research-summaries/tartaglia2024)*). Native plants are also far less likely to cause ecological harm than species introduced from other continents ([*learn more*](https://www.risccnetwork.org/research-summaries/the-natives-are-restless-but-not-often-and-mostly-when-disturbed-1)).   
Looking for sources of native plants? Try searching for vendors through directories [here](https://nativegardendesigns.wildones.org/nursery-list/), [here](https://www.audubon.org/native-plants), [here](https://homegrownnationalpark.org/directory/), or [here](https://xerces.org/pollinator-conservation/native-plant-nursery-and-seed-directory). Or if you’re looking specifically for nurseries that sell \*only\* native plants sourced from local/regional genotypes, find those [here](https://beechhollowfarms.com/native-plant-nurseries/). 

# Plant selection tool \- updated text:

# **Climate-Smart Plant Selection**

##### **How to Use This Tool**

Select your state and desired site and plant characteristics below. Filter columns must match for plants to appear in results. Sorting columns add to the match score to help rank plants by preference. This tool is still under construction. Resulting lists may not be fully correct.  
Match score: Each plant earns \+1 point for matching a sorting preference. The score helps rank plants by how well they fit your desired characteristics.  
Looking for sources of native plants? Try searching for vendors through directories [here](https://nativegardendesigns.wildones.org/nursery-list/), [here](https://www.audubon.org/native-plants), [here](https://homegrownnationalpark.org/directory/), or [here](https://xerces.org/pollinator-conservation/native-plant-nursery-and-seed-directory). Or if you’re looking specifically for nurseries that sell \*only\* native plants sourced from local/regional genotypes, find those [here](https://beechhollowfarms.com/native-plant-nurseries/). 

Plant Selection [https://www.climatesmartnativeplants.org/plant-selection/](https://www.climatesmartnativeplants.org/plant-selection/) 

- [ ] Add recommended citation at the bottom: Recommended Citation:. C-SNaP Tools: Singh, K., M.E. Fertakos, T.W.M. Nuhfer, J.M. Allen, E.M. Beaury, B.A. Bradley,. Climate-smart native plant selection. URL: https://www.climatesmartnativeplants.org/plant-selection/, access date.   
- [ ] In the same area as the recommended citation, add: This tool is based on the following publication: Fertakos, M.E., T.W.M. Nuhfer, E.M. Beaury, S. Birch, K. Singh, B.A. Bradley, C. Marshner, and J.M. Allen. 2026\. The-climate smart gardening database: Native and near-native garden plants for the northeastern United States. Ecology.  
- [ ] Remove the hardiness zone slider bar (it is confusing and we can sort by hardiness zone)  
- [ ] For the ‘wildlife services’ column, I know this is one where you combined a bunch of \-columns. It would be better to have NA instead of ‘None’ for those species which have no documented benefits. I don't want us to imply they have no benefits, when in reality this was just due to a data deficiency.   
- [ ] **Change text:** Sorting columns re-order plants based on a match score without reducing the list, putting "best match" species first. Match Score: The match score represents how many of your sorting criteria a given species meets. A higher match score is a better match to your preferences  
- [ ] **Change text** "Sorting Preferences (sorts by match score)" *\-\>* "Sorting Preferences (sorts by best match)"  
- [ ] The bloom period ranges got changed to commas making it unclear that it is a range now, try to bring back the dashes (-)   
- [ ] Downloading a PDF, the table is very cut off \- change the default to landscape instead of portrait orientation and/or split across multiple pages?   
- [ ] Is a clear all filters button easy? Maybe not necessary   
- [ ] Tooltip for search button that clarifies it searches only filtered data and only visible columns  
- [ ] Not a huge deal, but the spaces were removed after commas in a lot of the columns

**Stuff we need to revisit and figure out how to address:**

- [ ] Replace blank entries with NA, and it would be worth adding a description somewhere that NAs mean no data was available  
- [ ] I'm not really sure how/if we should incorporate height. It's a single value for some species and a range for others.   
- [ ] Change the hardiness zone slider to a single dropdown hardiness selection tool?

# RISCCTools

**Abundance Visualization**

- Can coordinates be derived by a pin on the map, or geocode lookup, or zipcode, or ecoregion selected by clicking? I think having to look up coordinates is tough  
  - Display all eco-regions to start, then click and select which eco-region you want for the table, then select a species  
- For both: Make sure the zoom on the map has a max  
- Ecoregion is limited to the state in which the coordinate is selected  
- Points can be different colors based on abundance vs occurrence  
- Not all points are showing (Occurrence data is clearly missing)  
- Ecoregions are cut to state boundaries \- This should not be the case (these should be based on EPA Level III ecoregions)  
- Do we need raw scientific name? I think just having the cleaned names would be fine  
- Define an abundant species somewhere?  
- Choose a species should ideally be alphabetical 	

# **Text for top**: This tool visualizes introduced plant occurrence and abundance (source Bradley et al. 2025 in Scholarworks) across the continental U.S.  Occurrence data are sourced from EDDMapS (link), iMap invasives (link), and twelve other state and regional databases. All data points include information about qualitative abundance (e.g., high), percent cover (from 0-100%), and cover class (the average of a range of percent cover e.g., 3 is the mean of 1-5%). NAs indicate no information about abundance (we only know that the species is present). ‘Species present’ indicates that the species has been reported as present at either unknown or low abundance. ‘Species abundant’ indicates that the species has been reported as present with a qualitative abundance value of X, a reported percent cover \>Y, or an average cover class \>Z.
