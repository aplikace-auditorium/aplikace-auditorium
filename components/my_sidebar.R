###############################################################################
###############################################################################
###############################################################################

## tvořím sidebar user interface aplikace -------------------------------------

my_sidebar <- dashboardSidebar(
    
    width = 250,
    
    ## definuji obsah odkazů na záložky v levém postranním panelu -------------
    
    sidebarMenu(
        
        ## odkaz na záložku "Informace o aplikaci" ----------------------------
        
        menuItem(
            
            text = "Informace o aplikaci",
            
            tabName = "info_tab",
            
            icon = icon("users")
            
        ),
        
        
        ## odkaz na záložku "Načtení dat" -------------------------------------
        
        menuItem(
            
            text = "Nahrání dat",
            
            tabName = "upload_tab",
            
            icon = icon("th"),
            
            startExpanded = FALSE,
            
            
            ## pododkaz na záložku "Načtení transakčních dat" -----------------
            
            menuSubItem(
                
                text = "Nahrání transakčních dat",
                
                tabName = "upload_transaction_data_tab"
                
            ),
            
            
            ## pododkaz na záložku "Načtení netransakčních dat" ---------------
            
            menuSubItem(
                
                text = "Nahrání netransakčních dat",
                
                tabName = "upload_non_transaction_data_tab"
                
            ),
            
            
            ## pododkaz na záložku "Načtení dat plánované budoucí sezóny" -----
            
            menuSubItem(
                
                text = "Nahrání dat plánované sezóny",
                
                tabName = "upload_fictive_non_transaction_data_tab"
                
            )
            
        ),
        
        
        ## odkaz na záložku "RFM analýza" -------------------------------------
        
        menuItem(
            
            text = "RFM analýza",
            
            tabName = "analysis_RFM_tab",
            
            icon = icon("dashboard"),
            
            startExpanded = FALSE,
            
            
            ## pododkaz na záložku "Segmentace zákazníků" ---------------------
            
            menuSubItem(
                
                text = "Segmentace zákazníků",
                
                tabName = "analysis_RFM_clients_segmentation_tab"
                
            ),
            
            
            ## pododkaz na záložku "Preference klíčového zákazníka" -----------
            
            menuSubItem(
                
                text = "Preference klíčového zákazníka",
                
                tabName = "analysis_RFM_clients_preferences_tab"
                
            )
            
        ),
        
        
        ## odkaz na záložku "CLV analýza" -------------------------------------
        
        menuItem(
            
            text = "CLV analýza",
            
            tabName = "analysis_CLV_tab",
            
            icon = icon("line-chart"),
            
            startExpanded = FALSE,
            
            
            ## pododkaz na záložku "CLV analýza transakčních dat" -------------
            
            menuSubItem(
                
                text = "Odhad budoucí útraty",
                
                tabName = "analysis_CLV_transaction_data_tab"
                
            ),
            
            
            ## pododkaz na záložku "CLV analýza netransakčních dat" -----------
            
            menuSubItem(
                
                text = HTML(
                    "Odhad budoucí útraty vzhledem<br>
                    k plánované sezóně"
                ),
                
                tabName = "analysis_CLV_non_transaction_data_tab"
                
            )
            
        ),
        
        #tags$br(),
        #tags$br(),
        
        HTML(
            "<center>
            <figure>
                <img src = '_prvky_oriznute_.png' width = '250px'>
            </figure>
            </center>"
        ),
        
        HTML(
            "<center>
            <figure>
                <img src = '_prvky_oriznute_.png' width = '250px'>
            </figure>
            </center>"
        ),
        
        tags$br(),
        tags$br()
        
        
    )
    
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





