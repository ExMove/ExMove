# The Great Coding Workshop
This workshop is meant as an accessible introduction to analysing tracking data using R. We make use of the [tidyverse](https://www.tidyverse.org/packages/) collection and simple features ([sf](https://r-spatial.github.io/sf/index.html)) packages to read, clean and process multiple data files, ready for a range of analyses. You can view the workflow script directly on GitHub by clicking [here](R/Workflow.R)

#### _Authors_:

- Liam Langley :dancer: <a itemprop="sameAs" content="https://orcid.org/0000-0001-9754-6517" href="https://orcid.org/0000-0001-9754-6517" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID iD icon" style="width:1em;margin-right:.5em;"/></a>
- Luke Ozsanlav-Harris :dizzy_face: <a itemprop="sameAs" content="https://orcid.org/0000-0003-3889-6722" href="https://orcid.org/0000-0003-3889-6722" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID iD icon" style="width:1em;margin-right:.5em;"/></a>
- Alice Trevail :runner: <a itemprop="sameAs" content="https://orcid.org/0000-0002-6459-5213" href="https://orcid.org/0000-0002-6459-5213" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID iD icon" style="width:1em;margin-right:.5em;"/></a>
- Stephen Lang :neckbeard: <a itemprop="sameAs" content="https://orcid.org/0000-0001-5820-4346" href="https://orcid.org/0000-0001-5820-4346" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID iD icon" style="width:1em;margin-right:.5em;"/></a>

### Getting started

1.  Clone or download this repository to your machine[^1] (see: [setting up GitHub](https://intro2r.com/setup_git.html) | [cloning a repo](https://intro2r.com/setting-up-a-project-in-rstudio.html) | [using GitHub with RStudio](https://intro2r.com/use_git.html))
2.  Initiate the R Studio project by opening the `ExMove.Rproj` file (which will open a new instance of R Studio)
3.  Open the `Workflow.R` file, and start by running on one of our three example datasets (currently `RFB`, but can be changed to `GAN`, or `SAP`)
4.  For additional guidance on using the workflow code, you can look in `Documentation` for the .html version of the `User guide`
5.  If you want to run analyses on your own data, start by adding a folder of datafiles and metadata to `Data` (described in `User guide)
6.  If you run into issues when using your own data, the `Documentation/FAQs.html` file provides a starting point for common problems
7.  All outputs of this workflow (such as figures, summaries and processed data) are saved into folders of `DataOutputs`


### Code Description

#### Main workflow
- `R/Workflow.R` - WIP file for cleaning tracking data
- `app/Tracking data diagnostic app.R` - Shiny app for exploring how data filters/cleaning/re-sampling influences the data and derived stats

#### Optional/troubleshooting scripts
- `R/Optional_Processing_CP_trips.R` - Code to process trips for central place data
- `R/Optional_Processing_Resampling.R` - Code to resample data
- `R/Troubleshoot_Multiple_ID_columns.R` - Code for troubleshooting data with multiple ID columns

### Data Description
#### Tracking data files
- `Data/RFB` - folder containing GPS tracking data files from three Red-footed boobies, from two populations
- `Data/SAP` - folder containing GPS tracking data files from four South African penguins
- `Data/GWfG` - folder containing GPS tracking data files from four migrating Greenland White-fronted Geese

#### Metadata files
- `Data/RFB_Metadata.csv` - metadata file containing information on the Red-footed boobies dataset
- `Data/SAP_Metadata.csv` - metadata file containing information on the African penguin dataset
- `Data/GWfG_Metadata.csv` - metadata file containing information on the Greenland White-fronted Goose dataset
- `Data/RFB_CPshape` - folder containing shape files for central place of RFB data

[^1]: The entire repo can technically be downloaded without cloning (e.g. using 
`Code ▾` > `Download ZIP`), but any pipeline downloaded this way will be a static copy that won't receive future updates

