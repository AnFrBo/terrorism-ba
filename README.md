# The Influence of Terrorist Hotbeds on the Global Transformation of Terrorist Tactics

The interest in studies regarding the diffusion of transnational terrorism increases constantly. Nevertheless, the existing literature is scarce. Examining the influence of hotbeds on the global transformation of tactics is the objective of the repository. 

The transformation and diffusion of terrorist tactics is analyzed from 1977-2017. Two cluster analysis are applied with the objective to group similar observations. Also heatmaps are generated to visualize and demonstrate the diffusion of tactics. The causality of observed patterns is discussed in the enclosed thesis. Upcoming challenges regarding the analysis of categorical data are presented and possible approaches to solve those difficulties are given, such as the weighted Jaccard matrix. The **weighted Jaccard matrix** solves the problem of double-weighting variables with >2 levels if variables with 2 levels occur in the data set as well. For more information, please see chapter 5.1 in the attached thesis.


## Organization

__Author:__ Anna Franziska Bothe <br>
__Institute:__ Humboldt University Berlin, Ladislaus von Bortkiewicz Chair of Statistics & Institute for Security Policy at Kiel University <br>
__Semester:__ WS 2018/19 <br>


## Content

```
.
├── Bachelorthesis_ABothe.pdf            # PDF of final thesis
├── graphics                             # contains visualisation produced by code
├── R files                              # code pipeline
├── RData                                # for faster execution
├── README.md                            # this readme file
├── requirements.txt                     # contains all used libraries
├── setup.txt                            # describes execution of pipeline in detail

```

## Requirements

1. This project is implemented with R.
2. The final thesis is implemented in RStudio.
3. The required libraries are listed in requirements.txt


## Setup
```
1. Get data from the [Global Terrorism Database (GTD)](https://www.start.umd.edu/gtd/access/)
2. Open R files
3. Set "path_datasets" to data set directory
4. Run all R files in the order that is describe in setup.txt
```



