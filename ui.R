shinyUI(fluidPage(theme = "yeti.css", #sandstone #slate good until final DT output # united good except orange
                  tagList(
                    singleton(tags$head(tags$script(src='//cdn.datatables.net/fixedheader/2.1.2/js/dataTables.fixedHeader.min.js',type='text/javascript'))),
                    singleton(tags$head(tags$link(href='//cdn.datatables.net/fixedheader/2.1.2/css/dataTables.fixedHeader.css',rel='stylesheet',type='text/css')))
                    ), 
                  shinyjs::useShinyjs(),
                  navbarPage('VDEQ Permit Tool',
                             navbarMenu("Background Metals Analysis",
                                        tabPanel("Targeted Monitoring (Unweighted) Data",
                                                 bootstrapPage(div(class="outer",
                                                                   tags$style(type ="text/css",".outer {position: fixed; top: 75px; left: 0; right: 0; bottom: 0; overflow-y: scroll; padding: 0}"),
                                                                   column(11,wellPanel(h4('This section uses all monitoring data available from the Probabilistic Monitoring programs, 
                                                                             facilities, and targeted monitoring stations, thus results from this page are unweighted 
                                                                             and represent the extent of monitored sites statewide.'))),
                                                                   leafletOutput("unweightedMap"),
                                                                   absolutePanel(top=150, left=60, draggable = F,bottom="auto",height=80,width=205,
                                                                                 textInput('targetlocationUN',"Search by Location",placeholder="Example: 37.564, -79.045"),
                                                                                 wellPanel(checkboxInput('supaBshape','Plot Superbasins',value=F),
                                                                                 checkboxInput('ecoshape','Plot Ecoregions',value=F),
                                                                                 checkboxInput('hucshape','Plot HUC8 Watersheds',value=F),style='padding: 3px;')),
                                                                   column(11,
                                                                          p("Use this section to help analyze background metals data for inclusion in forthcoming faciliy permit. 
                                                                            Type the facility's",strong("latitude")," and ",strong("longitude")," into the input box to add a marker 
                                                                            to the map at the permit location. ",strong("Remember to use a comma between latitude and longitude.")," Then review the layers available on the interactive map to select a 
                                                                            scale for analysis. You can adjust these as often as you like to analze the data at different resolutions. Note: if 
                                                                            the run statistics buttons are not available that means your latitude and longitude are not within the available
                                                                            data layers."),
                                                                          wellPanel(
                                                                            h5(strong("Unweighted Statistics")),
                                                                            fluidRow(column(4,
                                                                                            textInput('facilityUN','Facility:',placeholder='Latitude, Longitude')),
                                                                                     column(4,
                                                                                            selectInput('metalToPlotUN',label=strong('Choose a Metal'),
                                                                                                        choices=c("No Metals",capwords(tolower(levels(metalsSites_long$metal)))))),
                                                                                     column(4,
                                                                                            actionButton('runStats',"Plot Facility and Run Statistics"),
                                                                                            br(),br(),
                                                                                            actionButton('reviewstatsUN',"Review Statistics for all metals",class='btn-block'))),
                                                                            br(),br(),
                                                                            fluidRow(column(3,h6(strong('Superbasin Statistics'))),
                                                                                     column(9,DT::dataTableOutput('basinTable'))),
                                                                            fluidRow(column(3,h6(strong('Ecoregion Statistics'))),
                                                                                     column(9,DT::dataTableOutput('ecoTable'))),
                                                                            fluidRow(column(3,h6(strong('Superbasin Statistics'))),
                                                                                     column(9,DT::dataTableOutput('huc8Table')))
                                                                          )
                                                                          
                                                                   )))
                                        ),
                                        tabPanel("Probablilistic Monitoring (Weighted) Data",
                                                 bootstrapPage(div(class="outer",
                                                                   tags$style(type ="text/css",".outer {position: fixed; top: 75px; left: 0; right: 0; bottom: 0; overflow-y: scroll; padding: 0}"),
                                                                   column(11,wellPanel(h4('This section uses data from the Freshwater and Estuarine Probabilistic Monitoring Programs, 
                                                                             thus results from this page represent statistically valid weighted estimates of measurements 
                                                                             statewide.'))),
                                                                   leafletOutput("weightedMap"),#, width="75%", height="100%"),
                                                                   absolutePanel(top=150, left=60, draggable = F,bottom="auto",height=100,width=150,
                                                                                 selectInput('metalToPlot',label=strong('Choose a Metal'),
                                                                                             choices=c("No Metals",capwords(tolower(levels(metalsCDF$Indicator))))),
                                                                                 conditionalPanel("input.metalToPlot !== null && input.metalToPlot !=='No Metals'",
                                                                                                  selectInput('subpopToPlot',label=strong('Choose a Population'),
                                                                                                              choices=list(`All` = c("Virginia"),
                                                                                                                           `Ecoregion` = c("Blue Ridge Mountains",
                                                                                                                                           "Central Appalachian Ridges and Valleys",
                                                                                                                                           "Central Appalachians",
                                                                                                                                           "Northern Piedmont",
                                                                                                                                           "Piedmont","Southeastern Plains"),
                                                                                                                           `SuperBasin` = c("Clinch-Powell","Potomac-Shenandoah",
                                                                                                                                          "Rappahannock-York","Tennessee"),
                                                                                                                           `Basin` = c("Big Sandy","Chowan","Holston","James",
                                                                                                                                       "New","Potomac","Rappahannock","Roanoke",
                                                                                                                                       "Shenandoah","York"),
                                                                                                                           `Order` = c( "First Order","Second Order","Third Order",
                                                                                                                                        "Fourth Order","Fifth Order"))))),
                                                                   #absolutePanel(top=150, left=225, draggable = F,bottom="auto",height=50,width=175,
                                                                  #               textInput('targetlocationUnweighted',strong("Search by Location"),placeholder="Example: 37.5, -79.0")),
                                                                   fluidRow(
                                                                     column(7,style='padding:0px 10px 0px 30px;',
                                                                          h5(strong('Instructions:')),
                                                                          p("Use this section to help analyze background metals data for inclusion in forthcoming faciliy permit. 
                                                                            Use the drop down on the map to adjust the metal and subpopulation plotted on the map. You can click the markers 
                                                                            to find out more information about each data point. Review the associated summary statistics table below and 
                                                                            cdf curve to the right. You can specify multiple subpopulations that characterize your site in the table below to 
                                                                            review different statistical summaries simultaneously and copy or download them for future use."),
                                                                          fluidRow(column(6,
                                                                                          helpText('When you have selected the populations you want to review below, click
                                                                                                   the button to the right to see all (weighted) metal statistics in one table. You can
                                                                                                   download a report of all (weighted) metals statistics from that dialogue box.')),
                                                                            column(6,actionButton('reviewstats',"Review Statistics for all metals",class='btn-block')))
                                                                          ),
                                                                          column(5,
                                                                                 h5(strong('CDF Curve')),
                                                                                 plotOutput('weightedMetalsCDF', height="300px"))
                                                                                 ),
                                                                   column(12,
                                                                          h5(strong("Statistical Summary")),
                                                                          fluidRow(column(3,selectInput('basin',"Choose a Basin",
                                                                                                        choices = c("-","Big Sandy","Chowan","Holston","James","New",
                                                                                                                    "Potomac","Rappahannock","Roanoke","Shenandoah","York"))),
                                                                                   column(3,selectInput('superBasin','Choose a SuperBasin',
                                                                                                        choices = c("-","Clinch-Powell","Potomac-Shenandoah",
                                                                                                                    "Rappahannock-York","Tennessee"))),
                                                                                   column(3,selectInput('ecoregion',"Choose an Ecoregion",
                                                                                                        choices = c("-","Blue Ridge Mountains",
                                                                                                                    "Central Appalachian Ridges and Valleys",
                                                                                                                    "Central Appalachians","Northern Piedmont","Piedmont",
                                                                                                                    "Southeastern Plains"))),
                                                                                   column(3,selectInput('order','Choose a Stream Order',
                                                                                                        choices=c("-","First Order","Second Order",
                                                                                                                  "Third Order","Fourth Order","Fifth Order")))
                                                                          )),
                                                                  DT::dataTableOutput('weightedMetalsTable'))
                                                               ))
                                        ),
                             navbarMenu("Flow Analysis",
                                        tabPanel("Stream Gage Selection",
                                                 bootstrapPage(div(class="outer",
                                                                   tags$style(type ="text/css",".outer {position: fixed; top: 75px; left: 0; right: 0; bottom: 0; overflow-y: scroll; padding: 0}"),
                                                                   column(4,textInput('targetlocation',"Search by Location",placeholder="Example: 37.564, -79.045")),
                                                                   leafletOutput("GageMap"),#, width="75%", height="100%"),
                                                                   #absolutePanel(top=20, left=70, textInput('targetlocation',"Search by Location",placeholder="Example: 37.564, -79.045")),
                                                                   column(11,
                                                                          h5('Instructions:'),
                                                                          p('Review the map of current USGS gages for comparison to your watershed. You can zoom to a specific site or
                                                          simply scroll with your mouse. Clicking on a gage will bring up additional attributes and a link to the NWIS
                                                          site to review current flow data.'),
                                                                          p('Choose up to four gages to review compare to your watershed. Use the drop down box to select which gages you 
                                                           want to review after you have identified them on the map. Gage numbers are listed both in the attribute table 
                                                           as well as directly above the drop down menu.')),
                                                                   
                                                                   column(4,fluidRow(
                                                                     wellPanel(textOutput('gageText'),
                                                                               helpText('Use the drop down to select up to four gages to review further. You can scroll
                                                                             through the list or begin to type the gage number you want to keep to see a filtered
                                                                             list of gages based on your input.'),
                                                                               selectizeInput('gageList',h5(strong('Gages to Review')),choices=gageInfo@data$GageNo,multiple=T))
                                                                   )),
                                                                   column(6,
                                                                          tableOutput('gageInfoTable')
                                                                   )))),
                                        
                                        tabPanel("Stream Gage Statistics",
                                                 h5('Selected Gages'),
                                                 tableOutput('gageInfoTable2')
                                        )),
                            tabPanel('About',fluidRow(column(10,
                                                             h5("This tool was created to assist VDEQ  staff in the permit review process."),
                                                             p("Other stuff.")))),
                            tabPanel(HTML(" </a></li><li><a href=\'https://enviromapper.us/shiny/'>| Shiny Homepage |"))
                            
                  )
                  )
        )
#sidebarPanel(
#  ),
#mainPanel(