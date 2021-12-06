### online-advice tool

To run the shiny app, open R with the working directory set to the root directory of this repository, and run the following:

```r
source("Shiny/advice_shiny_app_stable_version.r")
```

If it is your first time using the app, uncomment the following line in the "advice_shiny_app_stable_version.r":

```r
source("Shiny/update_SAG_data.r")
```
The function in this file will download on your local directory the SAG data of the last 5 years (2017-2021) necessary to display the ICES advice. This process will take several minutes but after the data is downloaded, the app will run more quickly. When the app will be hosted on the server this process will be run automatically in the background so we will remove this feature.

The utilities files and the shape file are in the Shiny folder in which some code is kept for development.
The official ui and server scripts are the ones in the temp folder.
