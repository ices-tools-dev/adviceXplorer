### Online-advice Shiny app

To run the shiny app, open R with the working directory set to the root directory of this repository, and run the following:

```r
library(shiny)
runApp("App")
```

If it is your first time using online-advice, the app will download on your local directory the last 5 years (2017-2021) of SAG data (around 13 MB) needed to display the ICES advice. This process will take several minutes but after the data is downloaded, the app will run much faster.

All the official scripts and data to run the app are in the App folder. The Shiny folder is used to keep some code in development.

<b>Resources</b><br/>
<i>Source for fish illustrations: Food and Agriculture Organization of the United Nations (FAO), Original Scientific Illustrations Archive. Reproduced with permission <br/>
