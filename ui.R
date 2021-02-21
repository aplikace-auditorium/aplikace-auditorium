###############################################################################
###############################################################################
###############################################################################

for(my_package in c(
    
    "shiny",
    "shinyjs",
    "RColorBrewer",
    "shinydashboard",
    "shinyBS",
    "randomForest"
    
)){
    
    if(
        !(
            my_package %in% rownames(installed.packages())
        )
    ){
        
        install.packages(
            my_package,
            dependencies = TRUE,
            repos = "http://cran.us.r-project.org"
        )
        
    }
    
    library(
        my_package,
        character.only = TRUE
    )
    
}


## vytvářím user interface aplikace -------------------------------------------

source(
    "./components/my_header.R",
    encoding = "UTF-8"
)

source(
    "./components/my_sidebar.R",
    encoding = "UTF-8"
)

source(
    "./components/my_body.R",
    encoding = "UTF-8"
)

my_ui <- dashboardPage(
    
    header = my_header,
    sidebar =  my_sidebar,
    body = my_body
    
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





