### online-advice tool

To run the shiny app, open R with the working directory set to the root directory of this repository, and run the following:

```r
source("Shiny/advice_shiny_app_stable_version.r")
```

If it is your first time using online-advice, the app will download on your local directory the last 5 years (2017-2021) of SAG data (around 13 MB) needed to display the ICES advice. This process will take several minutes but after the data is downloaded, the app will run much faster.

The utilities files and the shape file are in the Shiny folder in which some code is kept for development.
The official ui and server scripts are the ones in the temp folder.

Source for fish illustrations: Food and Agriculture Organization of the United Nations, Original Scientific Illustrations Archive. Reproduced with permission <br />
Source for placeholder fish icon: <a href="https://www.flaticon.com/free-icons/fish" title="fish icons">Fish icons created by vectorsmarket15 - Flaticon</a>
