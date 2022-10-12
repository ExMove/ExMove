# The Great Coding Workflow
This code repository is meant as an accessible introduction to analysing tracking data using R. We make use of the [tidyverse](https://www.tidyverse.org/packages/) collection and simple features ([sf](https://r-spatial.github.io/sf/index.html)) packages to read, clean and process multiple data files, ready for a range of analyses. Go to the workflow script by clicking [here](R/test.R)

#### _Authors_:

- Liam Langley :dancer: <a itemprop="sameAs" content="https://orcid.org/0000-0001-9754-6517" href="https://orcid.org/0000-0001-9754-6517" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID iD icon" style="width:1em;margin-right:.5em;"/></a>
- Luke Ozsanlav-Harris :dizzy_face: <a itemprop="sameAs" content="https://orcid.org/0000-0003-3889-6722" href="https://orcid.org/0000-0003-3889-6722" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID iD icon" style="width:1em;margin-right:.5em;"/></a>
- Alice Trevail :runner: <a itemprop="sameAs" content="https://orcid.org/0000-0002-6459-5213" href="https://orcid.org/0000-0002-6459-5213" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID iD icon" style="width:1em;margin-right:.5em;"/></a>
- Stephen Lang :neckbeard: <a itemprop="sameAs" content="https://orcid.org/0000-0001-5820-4346" href="https://orcid.org/0000-0001-5820-4346" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID iD icon" style="width:1em;margin-right:.5em;"/></a>

### Getting started

1.  Clone or download this repository to your machine[^1] (see: [setting up GitHub](https://intro2r.com/setup_git.html) | [cloning a repo](https://intro2r.com/setting-up-a-project-in-rstudio.html) | [using GitHub with RStudio](https://intro2r.com/use_git.html))
2.  Navigate to the folder containing the downloaded/cloned files on your computer (i.e., don't open the below files from within github)
3.  Start by opening the `MoveExplore.Rproj` file , which will open a new instance of R studio and make sure that file paths work
4.  Open the `Workflow.R` file, and start by running on our example dataset (`RFB`)
5.  Open 'User_guide.html' from the 'Documentation' folder for additional guidance on using the workflow code
6.  If you want to run analyses on your own data, start by adding a folder of datafiles and metadata to `TestData` (described in `User guide)
7.  If you run into issues when using your own data, the `FAQs.html` file (also in 'Documentation' folder) provides a starting point for common problems
8.  All outputs of this workflow (such as figures, summaries and processed data) are saved into folders of `TestDataOutputs`

#### Main workflow
- `R/Workflow.R` - WIP file for cleaning tracking data

#### Optional & troubleshooting scripts
- `R/Optional_Processing_CP_trips.R` - Code to process trips for central place data
- `R/Optional_Processing_Resampling.R` - Code to resample data
- `R/Troubleshoot_Multiple_ID_columns.R` - Code for troubleshooting data with multiple ID columns

### Data Description
#### Tracking data files
- `TestData/RFB` - folder containing GPS tracking data files from three Red-footed boobies, from two populations

#### Metadata files
- `TestData/RFBTest_Metadata.csv` - metadata file containing information on the Red-footed boobies dataset

[^1]: The entire repo can be downloaded without cloning (e.g. using 
[![Code](https://img.shields.io/badge/-%20Code-brightgreen?style=flat-square)](https://github.com/AliceTrevail/Code-workshop) > `Download ZIP`), but any pipeline downloaded this way will be a static copy that won't receive future updates

