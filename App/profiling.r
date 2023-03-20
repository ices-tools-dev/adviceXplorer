library(profvis)
library(shiny)
profvis({ runApp('App') }  
    , prof_output = '/App')

p <- profvis(prof_input = '/path_to_save_output/name.Rprof')
htmlwidgets::saveWidget(p, "/path_to_save_output/profile.html")