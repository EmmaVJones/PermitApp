shinyUI(fluidPage(theme = "yeti.css", #sandstone #slate good until final DT output # united good except orange
                  tagList(
                    singleton(tags$head(tags$script(src='//cdn.datatables.net/fixedheader/2.1.2/js/dataTables.fixedHeader.min.js',type='text/javascript'))),
                    singleton(tags$head(tags$link(href='//cdn.datatables.net/fixedheader/2.1.2/css/dataTables.fixedHeader.css',rel='stylesheet',type='text/css')))
                  ), 
                  withMathJax(),
                  shinyjs::useShinyjs(),
                  navbarPage('VDEQ Permit Tool',
                             navbarMenu("Flow Analysis",
                                        tabPanel("Existing Facility: Update Stream Gage Statistics",
                                                 h5(strong('Instructions')),
                                                 p('Use this tab to update stream gage statistics. First select the gage statistics you wish
                                                   to update. Then, you can either accept the gage information or apply a correction to the 
                                                   gage based on a previous flow frequency analysis.'),
                                                 fluidRow(
                                                   column(4,
 #change me when update final flow metrics
 selectizeInput('gageListupdatestats',h5(strong('Gage to update')),choices=gagestats$SITEID,selected= NA, multiple=F),
                #,choices=gageInfo@data$GageNo,selected= NA, multiple=F),
                                                          radioButtons("addFormula","Gage Statistics",c("No Correction","Add Formula"),selected = "No Correction",inline=T),
                                                          conditionalPanel("input.addFormula == 'Add Formula'",
                                                                           uiOutput('formulas')),
                                                          actionButton('updateFlowStats',"Update flow statistics"))),
                                                 br(),  
                                                 column(6,
                                                        DT::dataTableOutput('adjustedFlowStats'))),
                                        tabPanel("New Facility",
                                                 tabsetPanel(
                                                   tabPanel("Stream Gage Selection",
                                                            bootstrapPage(div(class="outer",
                                                                              tags$style(type ="text/css",".outer {position: fixed; top: 75px; left: 0; right: 0; bottom: 0; overflow-y: scroll; padding: 0}"),
                                                                              br(),br(),
                                                                              column(4,textInput('targetlocation',"Search by Location",placeholder="Example: 37.564, -79.045")),
                                                                              leafletOutput("GageMap"),#, width="75%", height="100%"),
                                                                              #absolutePanel(top=20, left=70, textInput('targetlocation',"Search by Location",placeholder="Example: 37.564, -79.045")),
                                                                              column(11,
                                                                                     h5(strong('Instructions:')),
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
                                                            tableOutput('gageInfoTable2'),
                                                            br(),hr(),br(),
                                                            h5(strong("Instructions:")),
                                                            span(p("Based on the results above, please select up to four stream gages, using the checkboxes below, to utilize in a correlation 
                                                                   analysis. Once you have made your selection, press the 'Get Gage Data' button. The app will pull mean daily discharge
                                                                   data from each of the selected gages. Proceed to the 'Correlation Analysis' tab to review data and compare 
                                                                   gage data to a target stream."),strong("Note:"),p("The records are merged by date among all the selected gages
                                                                                                                     so only common dates are returned in this table.")),
                                                            wellPanel(uiOutput('gageSelection')),
                                                            helpText('The data displayed below is limited to the the dates common to ALL selected gages.'),
                                                            DT::dataTableOutput('gageDataPreview')),
                                                   tabPanel("Correlation Analysis",
                                                            wellPanel(h5(strong('Instructions')),
                                                                      fluidRow(
                                                                        column(8,
                                                                               p("Upload your target stream data for comparison to the selected gages. When you upload a 
                                                                                 dataset (as .csv), the app will automatically merge it with the gage table below and 
                                                                                 output correlation coefficients among the target stream and all other selected streams. 
                                                                                 Make sure you follow the template. Click the 'Download Template' button to ensure your file
                                                                                 is in the correct format prior to uploading it to the app."),
                                                                               downloadButton('downloadTemplate',"Download template.csv"),
                                                                               fileInput('userFlowData',"Upload single site (flat file)",accept = '.csv',width='50%')
                                                                               ),
                                                                        column(4,DT::dataTableOutput('corrResult'),
                                                                               br(),br(),br(),br(),
                                                                               verbatimTextOutput('testthis'),
                                                                               uiOutput('selectGageFromCorrelationData'))
                                                                               #downloadButton('downloadTemplate',"Download template.csv"),
                                                                               #fileInput('userFlowData',"Upload single site (flat file)",accept = '.csv',width='100%'))),
                                                                      #DT::dataTableOutput('corrResult')
                                                                      )),
                                                            hr(),
                                                            helpText('Remember, the data displayed below is limited to the the dates common to ALL selected gages. Correlation
                                                                     results compare the uploaded dataset to the entire gage datasets.'),
                                                            DT::dataTableOutput('gageData')#,
                                                            #fluidRow(column(4,tableOutput('gageDataPreview1')),
                                                            #         column(4,tableOutput('gageDataPreview2')),
                                                             #        column(4,tableOutput('gageDataPreview3')),
                                                             #        column(4,tableOutput('gageDataPreview4')))
                                                            ),
                                                   tabPanel("Flow Frequency Analysis",
                                                            h4('Gage vs uploaded flow data regression'),
                                                            wellPanel(p('This part will output graph of  previously selected gage and user uploaded data,
                                                              displaying regression line and equation as well as regression statistics. Then,
                                                              the equation will be applied to the gage flow statistics and output in a table in
                                                              the lower right corner. There will also be a button to output summary report for 
                                                              this portion of the app, detailing gage analysis results, all tables, and graphics.')),
                                                            column(8,plotOutput('flowRegressionPlot')),
                                                            fluidRow(
                                                              column(6,
                                                                     h4("Flow Data"),
                                                                     DT::dataTableOutput('flowData')),
                                                              column(6,
                                                                     h4('Flow Frequencies'),
                                                                     DT::dataTableOutput('flowFrequencies')))
                                                            )))),
                             
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
                                                                     )),
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
                                                                            fluidRow(column(3,h6(strong('Basin Statistics'))),
                                                                                     column(9,DT::dataTableOutput('basinTable'))),br(),br(),
                                                                            fluidRow(column(3,h6(strong('Ecoregion Statistics'))),
                                                                                     column(9,DT::dataTableOutput('ecoTable'))),br(),br(),
                                                                            fluidRow(column(3,h6(strong('HUC8 Statistics'))),
                                                                                     column(9,DT::dataTableOutput('huc8Table')))
                                                                          )
                                                                          
                                                                          )))
                                                                   )
                                                 ),
                             tabPanel('About',fluidRow(column(10,
                                                              h5("This tool was created to assist VDEQ  staff in the permit review process."),
                                                              p("Other stuff.")))),
                             
                             tabPanel(HTML(" </a></li><li><a href=\'https://enviromapper.us/shiny/'>| Shiny Homepage |"))
                             
                                                   )
                                                            )
                             )