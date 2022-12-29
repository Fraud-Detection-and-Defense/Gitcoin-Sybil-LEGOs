## Loading Libraries
library(shiny)
library(shinydashboard)
library(bslib)
library(shinycssloaders)
library(networkD3)
library(DT)
library(shinyWidgets)

fluidPage(theme = bs_theme(bootswatch = "sandstone"),useShinydashboard(),
    sidebarLayout(

        ########################################################################
        ## Side Panel
        ########################################################################
        sidebarPanel(width=3,
            h2("Sybil Scoring",align="center"),
            hr(),

            ## GTC Logo
            tags$div(style = "text-align: center;", tags$img(src = "gc.png", width=100)),
            
            ## Description
            h4("Introduction",align="center"),
            helpText("Select a handle or collection of handles below to graph them along our Sybil Scoring LEGO components. Red regions indicate deviant behavior along that variable, while the green regions highlight positive behaviors that are likely to be indicative of non-sybil behavior. Note that data for a handle may be missing and won’t be displayed on the graph."),
            hr(),

            ## Handle UI
            conditionalPanel(
                                condition = "input.tabs1 == 'Account Inspection'",
                                selectizeInput('handle_sel', "Select Handles (Max 5)", multiple = TRUE, choices = NULL)
            ),

            ## Grant UI
            conditionalPanel(
                                condition = "input.tabs1 == 'Grant Inspection'",
                                selectizeInput('grant_sel', "Select Grants (Max 5)", multiple = TRUE, choices = NULL)
            ),
            
            ## Export UI
            conditionalPanel(
                                condition = "input.tabs1 == 'Account Filtering'",
                                column(12,
                                    br(),
                                    align = "center"
                                )
            )
        ),
        ########################################################################
        ########################################################################


        ########################################################################
        ## Main Panel
        ########################################################################
        mainPanel(width=9,
            tabsetPanel(id = "tabs1",
                ########################################################################
                ## Account Inspection
                ########################################################################
                tabPanel("Account Inspection",
                            br(),
                            tabsetPanel(id = "tabs2",
                                tabPanel("All Legos",
                                            withSpinner(plotOutput("legoplot",width="auto",height="600px"))
                                ),
                                tabPanel("Levenshtein",
                                            withSpinner(forceNetworkOutput("lev_network"))
                                ),
                                tabPanel("Donor DNA",
                                            fluidRow(
                                                column(6,
                                                    sliderInput("ddna_network_cutoff", label = "DDNA Distance Cutoff", min = 0, max = .25, value = .1,step=.01)
                                                ),
                                                column(6,
                                                    selectizeInput('ddna_network_handle', "Inspect User DDNA", multiple = FALSE, choices = NULL)
                                                )
                                            ),
                                            fluidRow(
                                                column(6,
                                                    withSpinner(forceNetworkOutput("ddna_network"))
                                                ),
                                                column(6,
                                                    withSpinner(dataTableOutput("ddna_table"))
                                                )
                                            )
                                ),
                                tabPanel("IPs Shared",
                                            fluidRow(
                                                        column(5,selectizeInput('ips_network_handle', "Inspect User IP Profile", multiple = FALSE, choices = NULL)),
                                                        column(7,fluidRow(
                                                                    column(6,sliderInput("ips_network_cutoff", label = "IP Shared with no. of Users between",min=1,max=100,value=c(1,15))),
                                                                    column(4,br(),actionBttn("ips_network_cutoff_update", label = "Update Cutoffs",width=200,color="success",style="simple"),align = "center")
                                                                )
                                                        )
                                            ),
                                            fluidRow(
                                                column(9,withSpinner(forceNetworkOutput("ips_network"))),
                                                column(3,
                                                        tags$style(".small-box.bg-olive { background-color: #02E2AC !important; color: #000000 !important; }"),
                                                        tags$head(tags$style(HTML(".small-box {height: 100px}"))),
                                                        valueBoxOutput("num_ips",width=NULL),
                                                        valueBoxOutput("num_ips_shared",width=NULL),
                                                        valueBoxOutput("num_ips_shared1",width=NULL),
                                                        valueBoxOutput("num_ips_shared2",width=NULL)
                                                )
                                            )
                                )
                            )
                ),
                ########################################################################
                ########################################################################


                ########################################################################
                ## Grant Inspection
                ########################################################################
                tabPanel("Grant Inspection",
                            br(),
                            tabsetPanel(id = "tabs3",
                                tabPanel("All Legos",
                                            withSpinner(plotOutput("glegoplot",width="auto",height="600px"))
                                ),
                                tabPanel("Grant DNA",
                                            fluidRow(
                                                column(6,
                                                    sliderInput("gdna_network_cutoff", label = "GDNA Distance Cutoff", min = 0, max = .25, value = .1,step=.01)
                                                ),
                                                column(6,
                                                    selectizeInput('gdna_network_grant', "Inspect Grant GDNA", multiple = FALSE, choices = NULL)
                                                )
                                            ),
                                            fluidRow(
                                                column(6,
                                                    withSpinner(forceNetworkOutput("gdna_network"))
                                                ),
                                                column(6,
                                                    withSpinner(dataTableOutput("gdna_table"))
                                                )
                                            )
                                ),
                                tabPanel("Grant Donors Inspection",
                                            fluidRow(
                                                        # column(5,uiOutput("gddna_cluster_selUI"),align="center"),
                                                        column(5,selectizeInput("gddna_cluster_sel", label = "Select Cluster (Max 5)", multiple = TRUE, choices = NULL),align="center"),
                                                        column(7,fluidRow(
                                                                    column(6,sliderInput("grant_ddna_network_cutoff", label = "Grant Donors DDNA Cutoff", min = 0, max = .1, value = 0,step=.01)),
                                                                    column(6,br(),actionBttn("grant_ddna_network_cutoff_update", label = "Compute Clusters",width=200,color="success",style="simple"),align = "center")
                                                                )
                                                        )
                                            ),
                                            fluidRow(
                                                column(7,
                                                    withSpinner(forceNetworkOutput("gddna_network"))
                                                ),
                                                column(5,
                                                    withSpinner(dataTableOutput("gddna_table"))
                                                )
                                            )

                                )
                            )
                ),
                ########################################################################
                ########################################################################


                ########################################################################
                ## Sybil Clusters
                ########################################################################
                tabPanel("Sybil Clusters",
                    fluidRow(
                        column(5,selectizeInput("sybil_cluster_sel", label = "Select Cluster (Max 5)", multiple = TRUE, choices = NULL),align="center")
                    ),
                    fluidRow(
                        column(7,withSpinner(forceNetworkOutput("sybil_network"))),
                        column(5,withSpinner(dataTableOutput("sybil_table")))
                    )

                ),
                ########################################################################
                ########################################################################


                ########################################################################
                ## Account Filtering
                ########################################################################
                tabPanel("Account Filtering",
                            fluidRow(
                                column(5,box(width=12,
                                    column(12,h5("Create a Selection Criterion"),align="center"),
                                    hr(),
                                    br(),
                                    uiOutput("filterUI1"),
                                    uiOutput("filterUI2"),
                                    hr(),
                                    uiOutput("filterUI3"),
                                    column(12,
                                        actionBttn("filter_sel_add", label = "Add Condition",width=200,color="success",style="simple"),
                                        actionBttn("filter_sel_rem", label = "Remove Condition",width=200,color="danger",style="simple"),
                                        align="center"
                                    )
                                )),
                                column(7,
                                    fluidRow(
                                        tags$head(tags$style(HTML("#filterShow {font-size: 10px;}"))),
                                        fluidRow(br()),
                                        verbatimTextOutput("filterShow")
                                    ),
                                    fluidRow(
                                        column(6,
                                            valueBoxOutput("filter_setsize_1",width=NULL),
                                            align="center"
                                        ),
                                        column(6,
                                            valueBoxOutput("filter_setsize_2",width=NULL),
                                            align="center"
                                        )
                                    ),
                                    fluidRow(
                                        column(12,uiOutput("filterexp"),align="center")
                                    )
                                )
                            )
                )
                ########################################################################
                ########################################################################                
           )
        )
    )
)