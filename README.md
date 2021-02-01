Data Visualisation 
J Component
Review 2

Youth Risk Behaviour Surveillance System

Members: 
Ritika Kayal - 18BCE2518
Aditi Tarigoppula - 18BCE0728

Steps to launch the shiny app through Rstudio:

Open Rstudio.
Open the YRBSS folder present in the project folder.
Select global.r or server.r to open in Rstudio.
Click on Run app.

Description of each file:

global.R
Contains the calculations for measures of central tendency and the plotly code that connects the visualisations with the dataset.

server.R
Contains code to control the way the data is presented on the user interface. It is responsible for formatting the data.

ui.R
Contains code that forms the structure of the user interface. It describes the layout of the interface which includes text, tabs, icons, size, relative proportions, etc.

viz_disc.R
Contains code that provide descriptions for the visualisation. It involves editing and formatting the data and using it as input for the various visualisation options offered by the ggplot2 package.

data folder 
Contains the dataset obtained after conducting the youth risk behaviour survey. It is divided into several .csv files that contain information like the attributes of the demographic that participated, responses to survey questions, etc.

www folder
Contains the bootstrap code that offers css framework for the front end development of the visualisation produced by the shinyapp.

rsconnect folder
Contains an executable file to facilitate the launch of the shinyapp package.
