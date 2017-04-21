shinyUI(fluidPage(theme = "yeti.css", #sandstone #slate good until final DT output # united good except orange
                  tagList(
                    singleton(tags$head(tags$script(src='//cdn.datatables.net/fixedheader/2.1.2/js/dataTables.fixedHeader.min.js',type='text/javascript'))),
                    singleton(tags$head(tags$link(href='//cdn.datatables.net/fixedheader/2.1.2/css/dataTables.fixedHeader.css',rel='stylesheet',type='text/css')))
                  ), 
                  navbarPage('VDEQ Permit Tool',
                             tabPanel('About',fluidRow(column(10,
                                                              h5("This tool was created to assist VDEQ  staff in the permit review process."),
                                                              p("Other stuff.")))),
                             tabPanel("Tab1",
                                      bootstrapPage(div(class="outer",
                                                        tags$style(type ="text/css",".outer {position: fixed; top: 75px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                                        leafletOutput("GageMap"),#, width="75%", height="100%"),
                                                        #absolutePanel(top=20, left=70, textInput("target_zoom", "" , "Latitude, Longitude")),
                                                        h3('Instructions:'),
                                                        p('Detailed instructions.'),
                                                        shinyDirButton("dir", "Chose directory", "Upload Shapefiles from Directory"),
                                                        #fileInput('watershedShapefile',"Input Watershed (shapefile)",
                                                        #          accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj'),
                                                        #          multiple = T),
                                                        actionButton('zoomButton',"Zoom to Lat/Long",class='btn-block'),
                                                        verbatimTextOutput('submitZoom')))),
                             bsModal('zoom',"Zoom to Specific Latitude & Longitude",'zoomButton',size='small',
                                     numericInput('lat',"Latitude",value=NA),
                                     numericInput('long',"Longitude",value = NA),
                                     actionButton('submitZoom','Zoom')),
                             tabPanel("Tab2")
                  )
                  )
        )
#sidebarPanel(
#  ),
#mainPanel(