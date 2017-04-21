shinyUI(fluidPage(theme = "yeti.css", #sandstone #slate good until final DT output # united good except orange
                  tagList(
                    singleton(tags$head(tags$script(src='//cdn.datatables.net/fixedheader/2.1.2/js/dataTables.fixedHeader.min.js',type='text/javascript'))),
                    singleton(tags$head(tags$link(href='//cdn.datatables.net/fixedheader/2.1.2/css/dataTables.fixedHeader.css',rel='stylesheet',type='text/css')))
                  ), 
                  navbarPage('VDEQ Permit Tool',
                              tabPanel("Tab1",
                                      bootstrapPage(div(class="outer",
                                                        tags$style(type ="text/css",".outer {position: fixed; top: 75px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                                        leafletOutput("GageMap"),#, width="75%", height="100%"),
                                                        #absolutePanel(top=20, left=70, textInput("target_zoom", "" , "Latitude, Longitude")),
                                                        h5('Instructions:'),
                                                        p('Review the map of current USGS gages for comparison to your watershed. You can zoom to a specific site or
                                                          simply scroll with your mouse. Clicking on a gage will bring up additional attributes and a link to the NWIS
                                                          site to review current flow data.'),
                                                        p('Choose up to four gages to review compare to your watershed. Use the drop down box to select which gages you 
                                                           want to review after you have identified them on the map. Gage numbers are listed both in the attribute table 
                                                           as well as directly above the drop down menu.'),
                                                        column(2,
                                                               actionButton('zoomButton',"Zoom to Lat/Long"),#,class='btn-block'),
                                                               numericInput('lat',"Latitude",value=NA),
                                                               numericInput('long',"Longitude",value = NA)),
                                                        
                                                        column(4,fluidRow(
                                                          wellPanel(textOutput('gageText'),
                                                                    helpText('Use the drop down to select up to four gages to review further. You can scroll
                                                                             through the list or begin to type the gage number you want to keep to see a filtered
                                                                             list of gages based on your input.'),
                                                                    selectizeInput('gageList',h5(strong('Gages to Review')),choices=gageInfo@data$GageNo,multiple=T))
                                                         )),
                                                        column(4,
                                                               tableOutput('gageInfoTable'))))),
                             #bsModal('zoom',"Zoom to Specific Latitude & Longitude",'zoomButton',size='small',
                            #         numericInput('lat',"Latitude",value=NA),
                             #        numericInput('long',"Longitude",value = NA),
                              #       actionButton('submitZoom','Zoom')),
                             tabPanel("Tab2"),
                            tabPanel('About',fluidRow(column(10,
                                                             h5("This tool was created to assist VDEQ  staff in the permit review process."),
                                                             p("Other stuff."))))
                            
                  )
                  )
        )
#sidebarPanel(
#  ),
#mainPanel(