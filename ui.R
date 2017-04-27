shinyUI(fluidPage(theme = "yeti.css", #sandstone #slate good until final DT output # united good except orange
                  tagList(
                    singleton(tags$head(tags$script(src='//cdn.datatables.net/fixedheader/2.1.2/js/dataTables.fixedHeader.min.js',type='text/javascript'))),
                    singleton(tags$head(tags$link(href='//cdn.datatables.net/fixedheader/2.1.2/css/dataTables.fixedHeader.css',rel='stylesheet',type='text/css')))
                  ), 
                  navbarPage('VDEQ Permit Tool',
                             
                             navbarMenu("Background Metals Analysis",
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
                                                                     column(8,style='padding:0px 0px 0px 30px;',
                                                                          h5(strong('Instructions:')),
                                                                          p("Use this section to help analyze background metals data for inclusion in forthcoming faciliy permit. 
                                                                            Use the drop down on the map to adjust the metal and subpopulation plotted on the map. You can click markers 
                                                                            to find out more information about each data point. Then review the associated summary statistics table and 
                                                                            cdf curve. You can specify multiple subpopulations below to review different statistical summaries all at 
                                                                            once and copy or download them for future use.")),
                                                                          column(3,
                                                                                 h5('CDF Curve')
                                                                                 #plotOutput("p_dMetal"),
                                                                                 #plotOutput('weightedMetalsCDF'))
                                                                                 )),
                                                                   column(12,
                                                                          h5(strong("Statistical Summary")),
                                                                          fluidRow(column(3,selectInput('basin',"Choose a Basin",
                                                                                                        choices = c("","Big Sandy","Chowan","Holston","James","New",
                                                                                                                    "Potomac","Rappahannock","Roanoke","Shenandoah","York"))),
                                                                                   column(3,selectInput('superBasin','Choose a SuperBasin',
                                                                                                        choices = c("","Clinch-Powell","Potomac-Shenandoah",
                                                                                                                    "Rappahannock-York","Tennessee"))),
                                                                                   column(3,selectInput('ecoregion',"Choose an Ecoregion",
                                                                                                        choices = c("","Blue Ridge Mountains",
                                                                                                                    "Central Appalachian Ridges and Valleys",
                                                                                                                    "Central Appalachians","Northern Piedmont","Piedmont",
                                                                                                                    "Southeastern Plains"))),
                                                                                   column(3,selectInput('order','Choose a Stream Order',
                                                                                                        choices=c("","First Order","Second Order",
                                                                                                                  "Third Order","Fourth Order","Fifth Order")))
                                                                          )),
                                                                   DT::dataTableOutput('weightedMetalsTable'))
                                                               )),
                                        tabPanel("Targeted Monitoring (Unweighted) Data",
                                                 bootstrapPage(div(class="outer",
                                                                   tags$style(type ="text/css",".outer {position: fixed; top: 75px; left: 0; right: 0; bottom: 0; overflow-y: scroll; padding: 0}"),
                                                                   column(11,wellPanel(h4('This section uses all monitoring data available from the Probabilistic Monitoring programs, 
                                                                             facilities, and targeted monitoring stations, thus results from this page are unweighted 
                                                                             and represent the extent of monitored sites statewide.'))),
                                                                   leafletOutput("unweightedMap"),
                                                                   column(11,
                                                                          p("Use this section to help analyze background metals data for inclusion in forthcoming faciliy permit. 
                                                                            Type the facility's",strong("latitude")," and ",strong("longitude")," into the input box to add a marker 
                                                                            to the map at the permit location. Then review the layers available on the interactive map to select a 
                                                                            scale for analysis. You can adjust these as often as you like to analze the data at different resolutions."),
                                                                          wellPanel(
                                                                            h5("")
                                                                          )
                                                                          
                                                                          )))
                                                 )),
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
                                                             p("Other stuff."))))
                            
                  )
                  )
        )
#sidebarPanel(
#  ),
#mainPanel(