# Reflection Document - Milestone4

> Authors: Dustin Burnham, Javairia Raza, Rafael Pilliard Hellwig, Tanmay Sharma

The group was able to deploy a production-ready dashboard app as envisioned in the [proposal document](https://github.com/UBC-MDS/obesity-explorer-R/blob/main/doc/proposal.md). We decided to go with the R implementation from week 3 for our finalized version of the dashboard app. We feel our dashboard app presents an intuitive interface and a clear layout that help the viewer easily navigate between the different tabs and plots. We followed an iterative development process wherein we listed the bugs and future improvements for the milestones each week and this has helped us to implement most of our [desired functionality](https://github.com/UBC-MDS/obesity-explorer-R/milestone/2) in the final version of our app. 

## App details:
Our dashboard has three tabs, named ‘Country Standings’, ‘Trends’, and ‘Associations’. The ‘Country Standings’ tab shows a choropleth and a bar plot which help the viewer to compare obesity trends across countries since 1975. The ‘Trends’ tab shows a plot which presents a temporal view using trendlines of how the obesity rates have varied across individual countries and regions over the last few decades. The ‘Associations’ tab shows a scatter plot between obesity rate and different regressors such as literacy rate, unemployment rate, and smoking rate. The data in the scatter plot can be grouped by different variables such as sex, income level by country, and region.

## Decision to choose R for the final app version:
We decided to continue the development of the R version of the app. The choice was motivated by a desire to maintain a sense of design continuity since we’d implemented a lot of the feedback from milestones 1 and 2 in our milestone 3 version of the app. The group also felt that the R implementation had more optimized and dry code with appropriate use of functions and packages. 

- **Things that went well within the R eco-system:**
`usethis`, R-packaging, build tools, unit tests, `roxygen2`, `linting`, `styler`, and R-studio. 

- **Things that were challenging within the R eco-system:**
lack of documentation for R (`plotly`, `dash`), performance regression with an update to `dplyr` (1.0.3 to 1.0.4), lack of Heroku reviews functionality for R. 

## Feedback:
We received constructive feedback throughout our development process and have tried to incrementally improve our design, visualization, and code. The group has addressed all the feedback received from the TA and the peer group and effected appropriate changes to the final app as and where required. We found both the written and the in-person TA & instructor feedback helpful towards streamlining our development.

We chose not to implement bi-directional interactivity between the plots since having separate tabs for the plots made this issue moot (also global widgets serve interactivity in a way). Further, we also felt that an interactive table would clutter the design of our dashboard without adding any useful information. 

## Fixed issues from Milestone 3: 
- [Addressing the TA feedback from milestone 3](https://github.com/UBC-MDS/obesity-explorer-R/issues/61)
- [Acknowledging the peer feedback](https://github.com/UBC-MDS/obesity-explorer/issues/53)
