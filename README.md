### Online-advice Shiny app

To run the shiny app, open R with the working directory set to the root directory of this repository, and run the following:

```r
library(shiny)
runApp("App")
```

If it is your first time using online-advice, the app will download on your local directory the last 5 years (2017-2021) of SAG data (around 13 MB) needed to display the ICES advice. This process will take several minutes but after the data is downloaded, the app will run much faster.

The utilities files and the shape file are in the Shiny folder in which some code is kept for development.
The official ui and server scripts are the ones in the temp folder.

<i>Source for fish illustrations: Food and Agriculture Organization of the United Nations, Original Scientific Illustrations Archive. Reproduced with permission <br/>
Source for placeholder fish icon: <a href="https://www.flaticon.com/free-icons/fish" title="fish icons">Fish icons created by vectorsmarket15 - Flaticon</a><i/>
Source for pdf icon: <a href="https://www.flaticon.com/free-icons/pdf" title="pdf icons">Pdf icons created by Creative Stall Premium - Flaticon</a>
Source for database icon: <a href="https://www.flaticon.com/free-icons/database" title="database icons">Database icons created by srip - Flaticon</a>
