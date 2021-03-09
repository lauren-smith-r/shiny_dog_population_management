# required packages
require(shiny)
require(data.table)
require(ggplot2)
require(deSolve)
require(tidyr)
require(shinythemes)
require(readxl)
require(tidyverse)
require(Rcapture)
require(dplyr)
require(DT)


saveData <- function(data) { # saves the output of systems model simulation each time simulation is run
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- cbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() { # loads in the output of the systems model for each simulation that is run
  if (exists("responses")) {
    responses
  }
}

ui <- fluidPage(theme = shinytheme("flatly"), # sets website theme
                
                tags$head(includeHTML("google-analytics.html")), # includes ability to track website use through google analytics
                
                titlePanel("Assessing your dog population management strategies"), # adds website title
                navbarPage("Get started",
                           
                           tabPanel(icon("home"), # adds a home icon
                                    fluidRow(
                                      column(12, align="center", # sets text and images in centre of page
                                             h3("Welcome to the dog population management assessment website!"), # adds header text
                                             br(), # adds paragraph break
                                             br(),
                                             p("There is a growing need to assess the impact of dog population management interventions."), # adds paragraph
                                             p("This website helps you to", strong( "estimate"), "local dog population sizes and", strong("assess"), "the potential impact of your interventions."),
                                             br(),
                                             p(strong("Select from the tabs above to get started!")), # adds bold text in paragraph
                                             br(),
                                             br(),
                                             img(src = "dog_photo.jpg", height = 200, width = 300), # adds dog images
                                             img(src = "dog_1.jpg", height = 200, width = 300), 
                                             img(src = "DSC_0337.JPG", height = 200, width = 300)
                                      )
                                    )
                           ),
                           
                           navbarMenu(title = "Estimate dog population size", # adds first tab
                                      tabPanel("User instructions", # adds drop-down options in first tab
                                               h4(strong("Overview")),
                                               p("Understanding the size of your dog population is important for measuring the impact of interventions."),
                                               p("This website allows you to estimate the size of your local dog population using two methods: (1) ", strong( 
                                                 "the Lincoln-Petersen estimator"), "and (2)", strong(" closed mark-recapture.")),
                                               p("Both methods require that dogs are identified during street surveys, so that you can distinguish dogs that you observed in the first survey
                    from dogs you observe in subsequent surveys.
                    Dogs are usually identified by marking (e.g. with red dye) or by taking photographs of every dog you see.
                    Street surveys are normally carried out by one or two people who travel along a predetermined route covering the study area.
                    These surveyors record every dog that they see by either taking photographs for individual identification, or by marking dogs (e.g. using dye).
                      For more information on how to conduct street surveys, please see the", strong("'How to conduct a street survey'"), " section and refer to the 
                      International Companion Animal Management (ICAM) Coalition handbook on Humane Dog Population Management."),
                                               tags$a("ICAM dog population management handbook",
                                                      href="https://www.icam-coalition.org/downloads/"), # adds website link to ICAM's guidebook
                                               br(),
                                               
                                               tags$hr(),
                                               
                                               br(),
                                               h4(strong("Lincoln-Petersen Estimator")),
                                               p("The Lincoln-Petersen method requires that you have carried out", strong("two"), " street surveys over two days.
                    Dogs are marked on the first day and the proportion of dogs that are counted on the second day within the same survey area that are marked is used to estimate the number of
                      dogs in the area."),
                                               p("Surveys should be carried out on subsequent days (i.e. one day following another) or in as short a period of time as possible. Select",
                                                 strong(em("Lincoln-Petersen Estimator")), "to use this method of calculating the number of dogs in your area if you have conducted", strong( "two surveys"), "over",
                                                 strong("two"), "days."),
                                               br(),
                                               p("The Lincoln-Petersen estimator uses the below formula:"),
                                               p("N = (K + 1) X (n + 1) / ((k+1) - 1)"),
                                               p("where:"),
                                               p("N = the number of dogs in your population"),
                                               p("n = the number of dogs observed and identified (e.g. through marking or photograph) on the first survey"),
                                               p("K = the number of dogs observed on the second survey"),
                                               p("k = the number of dogs observed during the", strong("first"), "and the", strong("second"), "survey"),
                                               tags$hr(),
                                               br(),
                                               h4(strong("Closed mark-recapture method")),
                                               p("The closed mark-recapture method that is allowed in this website requires that you have conducted", strong("three "), 
                                                 "street surveys carried out over three days. The closed mark-recapture method estimates the number of dogs in an area using statistical methods,
                      providing more robust estimates of the true number of dogs in the area."),
                                               p("Surveys should be carried out on subsequent days (i.e. three days in a row) or in as short a period of time as possible.
                      Select", strong(em("Closed mark-recapture")), "to use this method of calculating the number of dogs in your area if you have conducted", strong(" three"), "surveys
                      over ", strong("three"), "days."),
                                               tags$hr(),
                                               br(),
                                               h5(strong("How to estimate your population size")),
                                               HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/kdmBnk1HLQQ" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'), # adds link to youtube video
                                               br(),
                                               br(),
                                               br(),
                                      ),
                                      
                                      tabPanel("How to conduct a street survey", # adds drop-down option in first tab
                                               h5(strong("Selecting study areas within a larger town/city")),
                                               br(),
                                               tags$ul( # creates a bullet pointed list
                                                 tags$li("If the town/city is too large to survey the total area in a couple of hours, you need to select several 
                              ‘study areas’ within your town/city."), # list item
                                                 tags$li(tags$div(HTML(paste("Study areas should be approximately 2km", tags$sup(2), sep = "")), "and you should be able to complete a survey of the area in 
                              less than three hours.")), # list item
                                                 tags$li(tags$div(HTML(paste("To select study areas, start by splitting your town/city into contiguous blocks (blocks that touch boundaries
                              with each other). You can use natural roads or streets to define your blocks. These blocks should be approximately 2km", tags$sup(2), sep = "")), ".")), # list item
                                                 tags$li("Tally up the total number of blocks in your town/city and randomly select approximately 10% of these blocks. It is important
                              that you select the blocks randomly, do not select areas where you know there are free-roaming dogs, as this can lead to overestimates of the number of dogs in your town/city."), # list item
                                                 tags$li("These randomly selected blocks will be your study areas where you will carry out the street surveys."), # list item
                                               ),
                                               br(),
                                               h5(strong("Designing survey routes")),
                                               br(),
                                               tags$ul( # Creates a bullet pointed list
                                                 tags$li("Before you begin surveying your study area, you need to design your survey route. Your route should cover as much of the study area as possible."),
                                                 tags$li("You should try to move through the streets in a systematic way, with little back-tracking over streets you have already covered (this will depend on the street layout in your study area)."),
                                                 tags$li("Consider avoiding enclosed areas to reduce the risk of cornering and intimidating dogs. You may also wish to exclude any areas without a pavement as a traffic safety measure."),
                                                 tags$li("Plan and test out your survey route ahead of beginning the street surveys."),
                                               ),
                                               br(),
                                               h5(strong("Carrying out street surveys")),
                                               br(),
                                               tags$ul( # Creates a bullet pointed list
                                                 tags$li("Always conduct the street surveys at the same time of day, either in the morning or afternoon, e.g. between 7am and 9am, or between 3pm and 5pm."),
                                                 tags$li("Always follow the same route."),
                                                 tags$li("Move along the survey route and record every dog that your observe."),
                                                 tags$li("Ensure the same number of people carry out the survey, e.g. one or two observers. Changing the number of observers between surveys can lead to over- or under-estimates of dog population size."),
                                                 tags$li("Try to maintain observer effort for seeing dogs as much as possible throughout the survey (i.e. constantly watch for dogs during the survey)."),
                                                 tags$li("Do not encourage dogs with food, or make extra effort to observe dogs in areas you have previously seen dogs on the route, as this can also lead to over- or under-estimates of dog population size."),
                                                 tags$li("Plan to conduct the street survey during daylight hours, as daylight can affect your chances of observing dogs."),
                                                 tags$li("Consider terminating surveys on days when weather is particularly bad, such as very low or high temperatures, 
                      or days with prolonged heavy rainfall."),
                                               ),
                                               br(),
                                               p(strong("Recording observations of dogs")),
                                               br(),
                                               tags$ul( # Creates a bullet pointed list
                                                 tags$li("Record every free-roaming dog that you see. Free-roaming dogs are classified as free-roaming if (i) they are not in an enclosed private property, (ii) not on a lead or (iii) under the control of a person (i.e. not on a lead, but under the watch and responsibility of someone)."),
                                                 tags$li("You may also be interested in measuring other indicators of management impact, such as the ages and sexes of the dogs, body condition scores, and prevalence of skin conditions or injuries."),
                                               ),
                                               br(),
                                               p(strong("Using your data to estimate the town/city wide population size")),
                                               tags$ul( # Creates a bullet pointed list
                                                 tags$li("You will need to estimate the size of the population (using either the Lincoln-Petersen estimator or closed mark-recapture) for each of your study areas."),
                                                 tags$li("You can extrapolate this estimate to a city or town wide estimate by taking an average of the number of dogs estimate in each of your study sites and multiplying by the total number of study areas."),
                                                 tags$li("Alternatively, you can also work out the average number of dogs per km2 and multiply this by the number of km2 in your town/city."),
                                               ),
                                               tags$hr(),
                                               br(),
                                               h5(strong("How mark-recapture works")),
                                               HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/UXTmlWt_cXU" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'), # adds link to youtube video
                                               br(),
                                               br()
                                      ),
                                      
                                      tabPanel("Lincon-Petersen Estimator", # adds drop-down list option to first tab
                                               h4("Fill out below:"),
                                               br(),
                                               tags$ol( # creates a numbered list
                                                 tags$li("the ", strong("number of dogs "), "that you observed during the ", strong("first survey.")),
                                                 tags$li("the ", strong("number of dogs "), "that you observed during the", strong("second survey.")),
                                                 tags$li("the ", strong("number of dogs"), "that you observed on the ", strong("second survey"), "that you", strong(em("also")), "observed on the", strong("first survey."))
                                               ),
                                               br(),
                                               p("Click 'Estimate population size' to calculate the total number of dogs in your survey area."),
                                               br(),
                                               br(),
                                               sidebarPanel(
                                                 
                                                 numericInput("n", label = h4("1) Number of dogs observed and marked during first survey"), value = 0), # allows user to insert a number - calls number n
                                                 numericInput("K", label = h4("2) Number of dogs observed during second survey"), value = 0), # allows user to insert a number - calls number K
                                                 numericInput("k", label = h4("3) Number of dogs observed in second survey that were observed (marked) in first survey"), value = 0), # allows user to insert a number - calls number k
                                                 actionButton(inputId = "estimate", label = "Estimate population size"), # will only simulate results when estiamte population size button is clicked
                                               ),
                                               
                                               mainPanel(
                                                 verbatimTextOutput("number_of_dogs_LP") # outputs the estimated number of dogs as text output
                                               )
                                               
                                      ),
                                      
                                      tabPanel("Closed mark-recapture", # adds drop-down list option to first tab
                                               h4("Overview"),
                                               p("Closed mark-recapture methods use statistical modelling to estimate the number of dogs in an area. To use this method to caculate how many dogs are in 
                    an area you must record observations of dogs over three surveys and upload a csv file of your observations of the dogs."),
                                               br(),
                                               h4("Formatting your file for closed mark-recapture estimation"),
                                               p("Your observations of individual dogs must be in a 1/0 format, where 1 indicates that the individual dog was observed, and 0
                       indicates you did not see that particular dog during the survey. Use one row for each individual dog you observed. Three columns should be filled in (for surveys 1, 2 and 3 in order),
                      the number of rows you have should be the same as the number of dogs you observed."),
                                               p("You may find it easiest to create your spreadsheets of dog observations in Microsoft Excel. Microsoft Excel files can be easily converted to csv format
                    by selecting ", strong(em("save as")), " and saving the file as a", strong(em(" comma-separated value")), " from the drop-down menu."),
                                               h4("Example"),
                                               p("You have carried out three days of mark-recapture surveys and during the surveys you saw three dogs.
                      The first dog you saw on all three days, the second dog you saw on the first and last day, and the third dog you 
                      only saw on the last day. Your spreadsheet would then be:"),
                                               br(),
                                               tableOutput("table_example_1"), # outputs table
                                               br(),
                                               h4("Ready to calculate your population size!"),
                                               p("Select the", strong(" browse"), " button to upload your csv file and select", strong(" 'Estimate population size'"), "to calculate the number of dogs in your area 
                      and the 95% confidence interval."),
                                               p(strong("Note: Spreadsheets must not have column or row names, they must include only 1's and 0's with three columns (one for each
                     of the three surveys) and one row per individual dog. For example:")),
                                               br(),
                                               tableOutput("table_example_2"), # outputs table
                                               
                                               br(),
                                               h5("Please upload your mark-recapture file in CSV format."),
                                               sidebarPanel(
                                                 fileInput("file", "Choose CSV file", accept = ".csv"), # allows users to input a csv file from their desktop
                                                 actionButton(inputId = "estimate_abundance", label = "Estimate population size") # only simulates results when "Estimate population size" button is clicked
                                                 
                                               ),
                                               mainPanel(
                                                 tableOutput("abundance_estimate") # outputs mark-recapture results as a table
                                               )
                                      ),
                                      
                                      tabPanel("Frequently Asked Questions", # adds drop-down list option to first tab
                                               h4("FAQs: Methods for estimating population size"),
                                               br(),
                                               tags$ol( # creates a numbered list
                                                 tags$li("What method should I use?"),
                                                 tags$ul( # Creates a bullet pointed list
                                                   
                                                   tags$li("Selecting the most appropriate method will depend on whether you wish to estimate the total number of dogs in the area,
                                                           or you simply want to track trends in population size."),
                                                   tags$li("You may need to estimate the total number of dogs in your area if you want to:",
                                                     tags$ul( # Creates a sub-bullet pointed list
                                                       tags$li("Plan dog population management interventions, such as those aiming to reduce free-roaming dog population size.
                                                               You need an estimate of the dog population size so you can plan resources to ensure a high dog population management coverage."),
                                                       tags$li("Plan public health interventions, such as mass rabies vaccination. You need an estimate of the dog population size so that you
                                                               can plan resources to ensure a high vaccination coverage.")
                                                     ),
                                                   tags$li("Population estimation methods include:"),
                                                           tags$ul( # Creates a sub-bullet pointed list
                                                              tags$li("Lincoln-Petersen Estimator – method available in this website."),
                                                              tags$li("Closed mark-recapture – method available in this website."),
                                                              tags$li(tags$a("Application Super-Duplicates",
                                                                  href="https://www.frontiersin.org/articles/10.3389/fvets.2018.00104/full")), # adds website link to Tiwari et al, 2018 paper
                                                           )
                                                     ),
                                                   tags$li("You may wish to use simple count methods to monitor trends in population size, for example after you have applied your dog population 
                                                   management/mass vaccination intervention. Guidance on how to apply simple count methods are available in the ",
                                                           tags$a("ICAM dog population management handbook",
                                                                  href="https://www.icam-coalition.org/downloads/") # adds website link to ICAM's guidebook
                                                           ),
                                                   tags$li("In this website, if you are using photographic methods of identifying dogs, you can use either the Lincoln-Petersen or closed mark-recapture
                                                           method. If you are using a physical marking method (such as red dye), you should use the Lincoln-Petersen method. Note: the Lincoln-Petersen
                                                           method requires two days of surveys and the closed mark-recapture method requires three days of surveys. The closed mark-recapture method 
                                                           provides more accurate estimates of the true dog population size."),
                                                   tags$li("Type of area (urban/rural):",
                                                           tags$ul( # Creates a sub-bullet pointed list
                                                             tags$li("The population estimation methods and simple count methods are suitable for both urban and rural areas.")
                                                           )
                                                   )
                                                    ),
                                                 br(),
                                                 tags$li("What time of day should I conduct surveys?"),
                                                 tags$ul( # Creates a sub-bullet pointed list
                                                   tags$li("Research suggests that dog activity varies depending on the time of day and dog populations are likely to behave differently in different geographic locations. 
                                                           These factors are likely to affect the observability (i.e., the visibility) of dogs. "),
                                                   tags$li("It is important that surveys are conducted at the same time of day consistently, regardless of whether surveys are conducted in the morning, afternoon, or 
                                                           evening. Keeping the survey time consistent ensures factors affecting dog observability remain constant. This ensures your results are comparable over time for that specific area."),
                                                   tags$li("It is important to remember that surveys should be completed without making extra effort to observe dogs in areas where you have seen dogs before, as this can lead to over-estimates 
                                                           of dog population size. You should survey your pre-determined route in an unbiased manner and maintain a consistent effort to observe and record dogs throughout the survey. See the ",
                                                           strong("How to conduct a street survey"), "section of this website and watch our video for more details.")
                                                   ),
                                                 br(),
                                                 tags$li("How much of the survey area should I survey?"),
                                                 tags$ul( # Creates a sub-bullet pointed list
                                                   tags$li("If your town/city/area is small enough, you can conduct a survey covering the whole area. If your town/city/area is too large to survey the total area, you need to select several 
                                                           survey areas, aiming to cover about 10% of the total area."),
                                                   tags$li("Details on how to split a larger area into smaller block are available in the", strong("How to conduct a street survey” section of the website."))
                                                 )
                                               )
                                      )
                           ),
                           
                           navbarMenu(title = "Assess the impact of your interventions", # second tab in website
                                      tabPanel("User instructions", # adds drop-down list option to second tab
                                               h4(strong("Overview")),
                                               p("Our simulation model allows you to estimate the effect of neutering interventions on your local dog population
                      by providing you with predictions of the effects on dog population size, welfare, and the proportional costs. These
                       predictions are created by modelling the system of dog populations using mathematical equations. The system of dog 
                       populations considers the interactions between dogs on the street, in shelters, and those that are owned (e.g. pet dogs)."),
                                               p("To get started, ", strong("read the below instructions "), "and start assessing your dog population management impact in the", strong("Assess your impact"), "section."),
                                               br(),
                                               h4(strong("Instructions for simulating interventions")),
                                               br(),
                                               tags$ol( # creates a numbered list
                                                 tags$li(h5("Applying input parameters")),
                                                 p("To assess the impact of your interventions, you must first include information about your local dog populations,
                         whether they have low, medium or high rates of movement, and details of your planned intervention. This simulation model 
                        allows you to assess the effects of", strong(" catch-neuter-release "), ("interventions, but you may also wish simulate the impact of"), strong("responsible 
                        ownership "), "interventions by reducing the rate at which dogs are ", strong("abandoned from homes to the street"), 
                                                   "and increasing the rate dogs are ", strong("adopted to homes from shelters"),"."),
                                                 p(""),
                                                 br(),
                                                 tags$li(h5("Simulating your results")),
                                                 p("Once you have input your estimated local parameters, click", strong("Simulate results"), " in the lower right hand corner to predict the outcomes of your intervention.
                        The model may take up to one minute to simulate your predictions, particularly if you want to run your simulation for a long period of time (e.g. 50 years). Once your 
                        results have been simulated, you can view the outputs in three tabs:"),
                                                 tags$ul( # Creates a bullet pointed list
                                                   tags$li(p(strong("'Estimate population size impact'"), " allows you to view the estimated impact 
                        of your intervention on the sizes of (i) the total street dog population (the sum of intact and neutered street dogs), (ii) number of intact (not neutered) street dogs, and (iii) the number
                        of neutered street dogs over time.")),
                                                   tags$li(p(strong("'Estimated cost and welfare impact'"), " allows you to view the estimated impact of your intervention on welfare and costs in terms of staff-time.")),
                                                   tags$li(p(strong("'Log of simulations'"), " allows you to view your previously run simulations, allowing you to compare different interventions.")),
                                                 ),
                                                 br(),
                                                 tags$li(h5("Understanding the outputs")),
                                                 tags$ul( # Creates a bullet pointed list
                                                   tags$li(p(strong("Estimated population size impact")),
                                                           p("This graph displays the predicted impact of your intervention on dog population size. Below the graph, the total population size change is printed for
                        (i) the end of the intervention (i.e. when you stop neutering), and (ii) the end of the simulation (i.e. when the simulation ends).
                        These values will be the same if your intervention is applied for the duration of your simulations (e.g. 5 years of intervention in a 5 year simulation.
                        A negative value indictes a reduction in total population size, a positive value indicates an increase in population size."),
                                                   ),
                                                   br(),
                                                   tags$li(p(strong("Estimated cost and welfare impact")),
                                                           p("Welfare impact is scored on a 1 to 5 scale, where 5 indicates the highest possible welfare. The scoring is based on the proportion of dogs that are
                        kept in shelters, owned, and intact street or neutered street."),
                                                           p("The simulation provides a means of comparing the resources required for your interventions by estimating the staff-time (veterinarians, veterinary nurses, and dog catchers) required to complete the intervention.
                      Proportional staff costs are calculated using these estimations, allowing you to compare costs between different interventions.
                        The cost estimations provided", strong(em("do not")), "estimate the true staff costs for your intervention, as these vary depending on the country. The cost estimations", strong(em("do not")),
                                                             "incorporate the full costs of the intervention, as many costs are not accounted for (e.g. equipment,
                      logistical costs, facilities). "),
                                                           p("For more information on the cost and welfare scoring, see", em("Smith L (2020). Chapter 5: Assessing the effectivenes of 
                        dog population management through systems modelling. PhD Thesis. The University of Leeds.")),
                                                   ),
                                                   br(),
                                                   tags$li(p(strong("Log of simulations")),
                                                           p("A log of each simulation is kept for each session you are on the website. Each column represents a model simulation."),
                                                   )),
                                                 br(),
                                                 br(),
                                                 h5(strong("How to assess your impact")),
                                                 HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/xqdf3f3MMhI" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'), # adds link to youtube video
                                                 br(),
                                                 br(),
                                                 br(),
                                               ),
                                               h5(em("Please note that this simulation is a research tool. It cannot guarantee any predicted outcome, but instead provides an estimate of the possible effects of an intervention and a means
                            of comparing different interventions. Users of the tool do so at their own risk.  The University of Leeds is not liable for any use of the tool by any users whatsoever."))
                                      ),
                                      tabPanel("Assess your impact", # adds drop-down list option to second tab
                                               tabsetPanel(
                                                 
                                                 tabPanel("Estimated population size impact", # first tab of results "Estimated population size impact"
                                                          h4("Impact of intervention graph will appear here."),
                                                          em("Please allow up to a minute for model to simulate results."),
                                                          
                                                          plotOutput("graph1"), # outputs graph of estimated population size change
                                                          h4("Population size change:"),
                                                          tableOutput("population_estimates"), # outputs table of results for population size change
                                                          tableOutput("pop_size_table") # outputs table of results of dog population size for every year simulated
                                                 ),
                                                 tabPanel("Estimated cost and welfare impact", # second tab of results "Estimated cost and welfare impact"
                                                          tableOutput("costs_table"), # outputs table of estimated costs
                                                          tableOutput("welfare_table") # outputs table of estimated welfare impact
                                                 ),
                                                 tabPanel("Log of simulations", # third tab of results "Log of simulations"
                                                          dataTableOutput("responses") # outputs each logged simulation for that period of activity on the website
                                                          
                                                 )
                                               ),
                                               
                                               hr(),
                                               
                                               # Sidebar with the widgets to amend parameters
                                               fluidRow(
                                                 
                                                 column(3,
                                                        
                                                        h4("Input parameters"),
                                                        
                                                        # numeric inputs of the different population sizes, starts with value = 
                                                        numericInput("St", label = h4("Provide an estimate of the number of intact street dogs in the population."), value = 23650),
                                                        numericInput("Sh", label = h4("Provide an estimate of the number of dogs in shelters in the population."), value = 2086),
                                                        numericInput("Ow", label = h4("Provide an estimate of the number of owned dogs in the population."), value = 98358),
                                                        numericInput("Ne", label = h4("Provide an estimate of the number of neutered street dogs in the population."), value = 0)
                                                 ),
                                                 
                                                 column(4, offset = 1, # sets second column of input parameters
                                                        
                                                        # allows user to input low-high rates using radiobuttons
                                                        radioButtons("r_st", h4("Estimated rate the street dog population grows (e.g. through births and immigration)"), 
                                                                     choices = list("Low" = "r_st_low", "Medium" = "r_st_med","High" = "r_st_high"), select = "r_st_med"),
                                                        radioButtons("abandonment_rate", h4("Estimated rate dogs are abandoned from homes to the street *"),
                                                                     choices = list("Very Low" = "abandonment_very_low", "Low" = "abandonment_low", "Medium" = "abandonment_medium",
                                                                                    "High" = "abandonment_high"), selected = "abandonment_medium"),
                                                        radioButtons("st_adoption", h4("Estimated rate dogs are adopted to home from the street"),
                                                                     choices = list("Low" = "st_ad_low", "Medium" = "st_ad_med",
                                                                                    "High" = "st_ad_high"), selected = "st_ad_med"),
                                                        radioButtons("shelter_adoption_rate", h4("Estimated rate dogs are adopted from local shelters **"),
                                                                     choices = list("Low" = "shelter_adoption_low", "Medium" = "shelter_adoption_medium",
                                                                                    "High" = "shelter_adoption_high", "Very high" = "shelter_adoption_very_high"), selected = "shelter_adoption_medium"),
                                                        radioButtons("relinquish_rate", h4("Estimated rate dogs abandoned from homes to the shelters"),
                                                                     choices = list("Low" = "rel_low", "Medium" = "rel_med",
                                                                                    "High" = "rel_high"), selected = "rel_med"),
                                                        p("* select low rates to simulate responsible ownership interventions"), # adds notes on how to apply a responsible ownership intervention
                                                        p("** select high rates to simulate responsible ownership interventions")
                                                 ),
                                                 
                                                 column(4, # third column
                                                        # allows user to select value between 0 and 1 using a slider at 0.05 step-size intervals
                                                        sliderInput("neuter_rate", label = h4("Proportion of population you aim to neuter over one year", p(em("(e.g., 0.5 = 50%)"))), min = 0,
                                                                    max = 1, value = 0.5, step = 0.05),
                                                        
                                                        # adds radio-button options for intervention input parameters
                                                        radioButtons("int_length", h4("How long to you plan to neuter this percentage of the population per year?"), 
                                                                     choices = list("1 year" = "int_length_1_year",
                                                                                    
                                                                                    "2 years" = "int_length_2_years",
                                                                                    
                                                                                    "3 years" = "int_length_3_years",
                                                                                    
                                                                                    "5 years" = "int_length_5_years",
                                                                                    
                                                                                    "10 years" = "int_length_10_years",
                                                                                    
                                                                                    "Ongoing (continuous intervention)" = "int_length_continuous"),
                                                                     selected = "int_length_5_years"),
                                                        
                                                        radioButtons("periodicity", h4("Over what timeline do you plan to neuter dogs (i.e. neutering periodicity)?"),
                                                                     choices = list("Continuously (neutering dogs each month throughout the year)" = "periodicity_continuous",
                                                                                    "Annually (one neutering event per year)" = "periodicity_annual"),
                                                                     selected = "periodicity_annual"),
                                                        
                                                        radioButtons("length", h4("How long would you like the simulation to run for?"),
                                                                     choices = list("5 years" = "length_5_years", "10 years" = "length_10_years",
                                                                                    "15 years" = "length_15_years", "25 years" = "length_25_years",
                                                                                    "50 years" = "length_50_years"), selected = "length_5_years"),
                                                        actionButton(inputId = "go", label = "Simulate results") # will only simulate results when go button is pressed
                                                 ),
                                                 
                                                 
                                                 br(),
                                               )
                                      )
                           ),
                           
                           tabPanel("Further information",
                                    
                                    h4("Project details"),
                                    
                                    h5("The", em(strong("Assess the impact of your interventions")), "simulation is based on a mathematical model developed by the STRAYS project, a study conducted by the University of Leeds, VIER PFOTEN International, 
                                    and Istituto Zooprofilattico Sperimentale dell’Abruzzo e del Molise “Giuseppe Caporale”. The STRAYS project was funded by VIER PFOTEN International. VIER PFOTEN relies on donations to fund projects focused on 
                                    animal rescue and welfare. You can donate", tags$a("here.", href="https://secure.four-paws.org/donate?utm_source=smith-university-of-leeds&utm_medium=affiliate&utm_campaign=researchproject")),
                                    br(),
                                    h5("All modelling code for both the", (em(strong("Assess the impact of your interventions"))), " simulation and ", (em(strong("Estimate dog population size"))), "can be found on ", tags$a("GitHub", href="https://github.com/lauren-smith-r/shiny_dog_population_management")),
                                    br(),
                                    br(),
                                    br(),
                                    img(src = "leeds_logo.jpg", height = 100, width = 250), # University of Leeds logo
                                    img(src = "FP_logo.png", height = 100, width = 250), # VIER PFOTEN International logo
                                    br(),
                                    br(),
                                    br(),
                                    tags$hr(),
                                    h5("Created by:"),
                                    tags$a("Animal Welfare Epidemiology Lab, University of Leeds",
                                           href="http://welfare-epicentre.weebly.com/#"), # adds link to research group website
                                    h5("For more information, please contact:"),
                                    h5("Dr Lauren Smith, Faculty of Biological Sciences, University of Leeds"),
                                    h5("L.M.Smith1@leeds.ac.uk"),
                                    br(),
                                    br(),
                           )
                           
                ))

server <- function(input, output, session) {
  
  lincoln_data <- eventReactive(input$estimate, { # creates a reactive input called lincoln_data
    
    (((input$K + 1)*(input$n + 1))/input$k+1)-1 # formula for Lincoln-Petersen estimator using the input values from the ui
    
  })
  
  output$number_of_dogs_LP <- renderPrint({ # prints the results of the Lincoln-Petersen estimator
    
    round(lincoln_data(),digits=0) # prints lincoln-petersen results and rounds to zero decimal places
    
  })
  
  MR_data <- eventReactive(input$estimate_abundance, { # creates a reactive object that runs below code when estimate_abundance is clicked
    
    file <- input$file # creates an object for the users csv file of the mark-recapture histories
    ext <- tools::file_ext(file$datapath) # allows the users csv file to be input from the users computer
    req(file) # calls the file
    validate(need(ext == "csv", "Please upload a csv file")) # ensures that the file is csv format
    mark_data <- read.csv(file$datapath, header = FALSE) # creates an object for the users csv file
    
    # Runs the different types of closed mark-recapture models using closedpCI.t
    M0 <- closedpCI.t(mark_data, m = "M0") # constant capture probability
    Mt <- closedpCI.t(mark_data, m = "Mt") # time varying capture probability
    Mh <- closedpCI.t(mark_data, m = "Mh") # heterogenous capture probability
    Mth <- closedpCI.t(mark_data, m = "Mth") # time varying and heterogeneous capture probability
    
    MR_models <- rbind.data.frame(M0[["results"]], Mt[["results"]], Mh[["results"]], Mth[["results"]]) # makes a dataframe by binding the results of all the models run
    MR_models_no_warning <- subset(MR_models, infoFit == 0) # ensures there are no models included that have errors or warnings by subsetting for 0 warnings
    MR_outputs <- setDT(MR_models_no_warning, keep.rownames = TRUE)[] # includes the rownames as a column in the dataframe
    MR_outputs <- MR_outputs %>% arrange(AIC) # orders the models by the lowest AIC
    
    # Create a new column with the model name. This refers to the type of model and is used to call correct model for confidence intervals.
    MR_outputs[, "model"] <- ifelse(MR_outputs[["rn"]] == "M0", "M0",
                                    ifelse(MR_outputs[["rn"]] == "Mt", "Mt",
                                           ifelse(MR_outputs[["rn"]] == "Mh", "Mh",
                                                  ifelse(MR_outputs[["rn"]] == "Mh Darroch", "Mh",
                                                         ifelse(MR_outputs[["rn"]] == "Mh Gamma3.5", "Mh",
                                                                ifelse(MR_outputs[["rn"]] == "Mh Poisson2", "Mh",
                                                                       ifelse(MR_outputs[["rn"]] == "Mh Chao (LB)", "Mh",
                                                                              ifelse(MR_outputs[["rn"]] == "Mth", "Mth",
                                                                                     ifelse(MR_outputs[["rn"]] == "Mth Poisson2", "Mth",
                                                                                            ifelse(MR_outputs[["rn"]] == "Mth Darroch", "Mth",
                                                                                                   ifelse(MR_outputs[["rn"]] == "Mth Gamma3.5", "Mth",
                                                                                                          ifelse(MR_outputs[["rn"]] == "Mth Chao (LB)", "Mth",
                                                                                                                 ifelse(MR_outputs[["rn"]] == "Mb", "Mb",
                                                                                                                        ifelse(MR_outputs[["rn"]] == "Mbh", "Mbh",
                                                                                                                               NA))))))))))))))
    
    
    # Makes a dataframe for each of the different models that have been run, their abundance estimate and confidence interval
    M0_results <- as.data.frame(M0[["CI"]])
    Mt_results <- as.data.frame(Mt[["CI"]])
    Mh_results <- as.data.frame(Mh[["CI"]])
    Mth_results <- as.data.frame(Mth[["CI"]])
    
    # Extracts the abundance estimate for the model with the lowest AIC by calling the result for row 1, column 9 = MR_outputs[1,9]
    abundance <- ifelse(MR_outputs[1,9] == "M0", M0_results[["abundance"]],
                        ifelse(MR_outputs[1,9] == "Mt", Mt_results[["abundance"]],
                               ifelse(MR_outputs[1,9] == "Mh", Mh_results[["abundance"]],
                                      ifelse(MR_outputs[1,9] == "Mth", Mth_results[["abundance"]], NA))))
    # Extracts the lower confidence interval estimate for the model with the lowest AIC row 1, column 9 = MR_outputs[1,9]
    Lower_CI <- ifelse(MR_outputs[1,9] == "M0", M0_results[["infCL"]],
                       ifelse(MR_outputs[1,9] == "Mt", Mt_results[["infCL"]],
                              ifelse(MR_outputs[1,9] == "Mh", Mh_results[["infCL"]],
                                     ifelse(MR_outputs[1,9] == "Mth", Mth_results[["infCL"]], NA))))
    # Extracts the upper confidence interval estimate for the model with the lowest AIC row 1, column 9 = MR_outputs[1,9]
    Upper_CI <- ifelse(MR_outputs[1,9] == "M0", M0_results[["supCL"]],
                       ifelse(MR_outputs[1,9] == "Mt", Mt_results[["supCL"]],
                              ifelse(MR_outputs[1,9] == "Mh", Mh_results[["supCL"]],
                                     ifelse(MR_outputs[1,9] == "Mth", Mth_results[["supCL"]], NA))))
    
    MR_results <- data.frame(abundance, Lower_CI, Upper_CI) # makes a dataframe for the abundance, lower and upper confidence intervals
    colnames(MR_results)<- c("Abundance","Lower confidence level","Upper confidence level") # defines column names
    list(MR_results=MR_results) # ensures this dataframe is available to be called by shiny app
    
  })
  
  output$abundance_estimate <- renderTable({  # outputs the table of mark-recapture results
    MR_data()[["MR_results"]] 
    
  })
  
  # Code for systems model - system of dog subpopulations
  systems_data <- eventReactive(input$go, { # creates a reactive object - below script is run when "go" is clicked
    
    # This section of code creates the lookup function for the death rate of neutered dogs
    x.stock <- seq(0, 20000, by = 2000)
    y.rate <- c(0.019, 0.0192, 0.0194, 0.0196, 0.0199, 0.0202, 0.0204, 0.0208, 0.022, 0.026, 0.032)
    my_death_rate_function <- approxfun(x=x.stock, y=y.rate, rule = 2)
    
    # function defining the systems model
    dogs_m <- function(t, y, p)
    {
      with(as.list(c(y,p)), {
        
        # functions allowing the sheltering, culling and catch-neuter-release intervention to be applied during intervention months (i.e., annually, or continuously)
        sheltering_I <- sheltering_on_fun(t) 
        culling_I <- culling_on_fun(t)
        CNR_I <- CNR_on_fun(t)
        
        # rate neutered dogs are removed through deaths
        my_death_rate_function <- approxfun(x=x.stock, y=y.rate, rule = 2)
        
        #  differential equations describing the flows between the street, shelter, and owned dog populations
        dSt_dt <- r_st*St*(1 - (St+Ne)/K_st) + abandonment_rate*Ow - st_adoption*St - (sheltering_I * sheltering * St) - (culling_I * cull_rate * St) - (CNR_I * neuter_rate * St) # # equation for intact street dog population
        dSh_dt <- relinquish_rate*Ow - shelter_adoption_rate*Sh - shelter_death*Sh + (sheltering_I * sheltering * St) # equation for shelter dog population
        dOw_dt <- r_ow*Ow*(1 - Ow/K_ow) + shelter_adoption_rate*Sh + st_adoption*St - abandonment_rate*Ow - relinquish_rate*Ow # equation for owned dog population
        dNe_dt <- - my_death_rate_function(Ne+St) * Ne - st_adoption*Ne + (CNR_I * neuter_rate * St) # equation for neutered street dog population
        
        return(list(c(dSt_dt, dSh_dt, dOw_dt, dNe_dt),
                    sheltering_I = sheltering_I,
                    culling_I = culling_I,
                    CNR_I = CNR_I
        )
        )
      })
    }
    
    # Input parameters
    state = c(St = input$St, Sh = input$Sh, Ow = input$Ow, Ne=input$Ne) # input parameters for intact street (St), shelter (Sh), owned (Ow), and neutered street (Ne) dog populations
    
    period = (switch(input$periodicity,
                     "periodicity_continuous" = 12,
                     "periodicity_annual" = 1)) # defines the periodicity for the intervention coverage to be altered to account for annual or continuous intervention
    
    # list of input parameters for the above systems model
    parameters = list(r_st = switch(input$r_st, "r_st_low" = 0.01, "r_st_med" = 0.025, "r_st_high" = 0.04), # Maximum growth rate of street dog population
                      r_ow = 0.07, # Maximum growth rate of owned dog population
                      K_st=input$St, # Carrying capacity of street dog population
                      K_ow=input$Ow, # Carrying capacity of owned dog population
                      abandonment_rate = switch(input$abandonment_rate, "abandonment_very_low" = 0.0001, "abandonment_low" = 0.001, "abandonment_medium" = 0.003, "abandonment_high" = 0.009), # Abandonment rate of owned dogs to street dog population
                      st_adoption = switch(input$st_adoption, "st_ad_low" = 0.004, "st_ad_med" = 0.007, "st_ad_high" = 0.01), # Adoption rate of street dogs to owned dog population
                      shelter_adoption_rate = switch(input$shelter_adoption_rate, "shelter_adoption_low" = 0.005, "shelter_adoption_medium" = 0.025, "shelter_adoption_high" = 0.0475, "shelter_adoption_very_high" = 0.08), # Adoption rate of shelter dogs to owned dog popualation
                      relinquish_rate = switch(input$relinquish_rate, "rel_low" = 0.0004, "rel_med" = 0.0007, "rel_high" = 0.001), # Relinquishment rate of owned dogs to shelter population
                      shelter_death = 0.008, # Death rate of shelter dogs
                      sheltering =  0, # Rate dogs are moved from street to shelter dog population (sheltering intervention) - not applied in the shiny app
                      cull_rate = 0, # Rate street dogs are removed through culling intervention - not applied in the shiny app
                      neuter_rate =  (input$neuter_rate)/period) # Rate intact street dogs are moved to neutered street dog population (neutering intervention) corrected for periodicity (annual/continuous)
    
    neuter_rate =  (input$neuter_rate)/period # Rate intact street dogs are moved to neutered street dog population (neutering intervention) corrected for periodicity (annual/continuous)
    
    length = (switch(input$length, "length_5_years" = (12*5), "length_10_years" = (12*10), "length_15_years" = (12*15),
                     "length_25_years" = (12*25), "length_50_years" = (12*50))) # length of simulation, as defined by user
    
    times = seq(0, length, by = 0.01) # times of simulation, as defined by user, by 0.01 time step intervals
    
    sheltering_times <- data.frame(
      times = times,
      sheltering_on = 0)  # no sheltering intervention
    
    culling_times <- data.frame(
      times = times,
      culling_on = 0) # no culling intervention
    
    intervention_length = (switch(input$int_length, "int_length_1_year" = (1*12), # specifies length intervention is applied, as defined by user
                                  "int_length_2_years" = (2*12),
                                  "int_length_3_years" = (3*12),
                                  "int_length_5_years" = (5*12),
                                  "int_length_10_years" = (10*12),
                                  "int_length_continuous" = length))
    
    CNR_pulse <- seq(0, intervention_length, by=13)  # specifies a sequence of values between 0 and the intervention length 
    # by 13 months to allow an annual (or pulsed) intervention to run 
    # (CNR intervention is applied for one month every 13 months)
    
    #  sets CNR intervention to be applied for one month every 13 months to simulate annual intervention and continuously for continuous interventions      
    CNR_times <- data.frame(
      times = times,
      CNR_on = (switch(input$periodicity,
                       "periodicity_continuous" = (ifelse(times < intervention_length, 1,0)),
                       "periodicity_annual" = (ifelse( floor(times) %in% CNR_pulse, 1, 0)))))
    
    
    sheltering_on_fun <- approxfun(sheltering_times, rule = 2)
    culling_on_fun <- approxfun(culling_times, rule = 2)
    CNR_on_fun <- approxfun(CNR_times, rule = 2)
    
    # runs the ordinary differential equations using package deSolve and saves as a dataframe
    model_output_max <- as.data.frame(
      ode(y = state, times = times,
          func = dogs_m,
          parms = parameters, 
          method = "rk4"))
    
    model_output_max[, "FRD"] <- model_output_max$St + model_output_max$Ne # creates a new column for the total street dog population (intact street + neutered street)
    model_output_min_no_melt <- model_output_max # creates a new dataframe where the unnecessary columns will be deleted
    # deletes unnecessary columns
    model_output_min_no_melt$sheltering_I <- NULL 
    model_output_min_no_melt$culling_I <- NULL
    model_output_min_no_melt$CNR_I <- NULL
    model_output_min_no_melt$Ow <- NULL
    model_output_min_no_melt$Sh <- NULL
    
    
    model_output_min <- melt(setDT(model_output_min_no_melt), id.vars = "time", # melts the columns together by population to make two columns of time and population
                             variable.name = "pop")
    # divides the time (which is in months) by 12 to create time in years
    model_output_min$time <- model_output_min$time/12
    model_output_min_no_melt$time <- model_output_min_no_melt$time/12
    
    results <- data.frame(model_output_min) # creates a dataframe of the melted minimum dataframe (with only two columns) and calls it results
    
    pop_size_table <- data.frame(model_output_min_no_melt) # creates a dataframe that isn't melted by population so that this can be output on the user interface
    
    # creates a sequence of the times in 1 year intervals, so that a table of outputs can be created for only the one year time points (instead of all time-steps at 0.01 intervals)
    year_times <- seq(0, 50, by=1)
    pop_size_table[, "Year"] <- ifelse(pop_size_table$time %in% year_times, 1, 0)
    pop_size_table <- subset(pop_size_table, Year == 1)
    pop_size_table$Year = NULL
    # formats the table by adding in percentages and amended values to 0 decimal places
    pop_size_table[, "Intact percentage"] <- format(round(( pop_size_table$St / pop_size_table$FRD ) * 100, 0), nsmall=0)
    pop_size_table[, "Neutered percentage"] <- format(round(( pop_size_table$Ne / pop_size_table$FRD ) * 100, 0), nsmall=0)
    pop_size_table$time <- format(round(pop_size_table$time, 0), nsmall=0)
    pop_size_table$St <- format(round(pop_size_table$St, 0), nsmall=0)
    pop_size_table$Ne <- format(round(pop_size_table$Ne, 0), nsmall=0)
    pop_size_table$FRD <- format(round(pop_size_table$FRD, 0), nsmall=0)
    # names the columns
    colnames(pop_size_table) <- c("time", "Intact", "Neutered", "Total", "Intact_percentage", "Neutered_percentage")
    # creates dataframe for population size table output at 1 year intervals
    pop_size_table2 <- data.frame(pop_size_table$time, pop_size_table$Intact, pop_size_table$Intact_percentage, pop_size_table$Neutered,
                                  pop_size_table$Neutered_percentage, pop_size_table$Total)
    # names the columsn
    colnames(pop_size_table2) <- c("Time (years)", "Intact",  "(%)", "Neutered", "(%)", "Total")
    
    # Calculating the population size change
    FRD_pop <- subset(results, pop =="FRD") # subsetting results for FRD only
    end_int_size <- subset(FRD_pop, time == (intervention_length / 12)) # creating an object by selecting the population size at the end of the intervention length (/12 to account for years instead of months)
    end_sim_size <- subset(FRD_pop, time == (length / 12)) # creating an object by selecting the population size at the end of the simulation (/12 to account for years instead of months)
    pop_change_int <- ((end_int_size[1,3] - FRD_pop[1,3]) / (FRD_pop[1,3]) * 100) # Calculating the population size change at the end of the intervention.
    pop_change_sim <- ((end_sim_size[1,3] - FRD_pop[1,3]) / (FRD_pop[1,3]) * 100) # Calculating the population size change at the end of the simulation.
    # formatting the values to include 0 decimal places
    pop_change_sim <- format(round(pop_change_sim, 2), nsmall=2) 
    pop_change_int <- format(round(pop_change_int, 2), nsmall=2)
    
    pop_estimates <- data.frame(pop_change_int, pop_change_sim) # creating a table of the results for the population size change at the end of intervention and simulation
    colnames(pop_estimates) <- c("at end of intervention (%)", "at end of simulation (%)") # naming columns
    
    # ### Costs analysis ###
    cost_periodicity <- switch(input$periodicity,
                               "periodicity_continuous" = 1,
                               "periodicity_annual" = 13)
    cost_rows <- seq(0, intervention_length, by = cost_periodicity) # Delete the rows so only full time-steps are included
    costs_results <- subset(model_output_max, time %in% cost_rows) # select only the rows for months
    costs_results[, "No_dogs_neutered"] <- costs_results$St*neuter_rate # Calculates the number of intact dogs neutered at each time step
    costs_results[, "dogs_per_day"] <- costs_results$No_dogs_neutered/24 # Calculates the number of dogs neutered per day, given a standard 30 day month with 6 days off
    costs_results[, "vets_per_day"] <- costs_results$dogs_per_day/6 # Calculates the number of vets required per day, given that each vet can neuter 6 dogs per day
    costs_results[, "vet_nurses_per_day"] <- costs_results$dogs_per_day/8 # Calculates the number of veterinary nurses required per day, given that each veterinary nurse can care for 8 dogs per day
    costs_results[, "dog_catchers_per_day"] <- costs_results$dogs_per_day/13 # Calculates the number of dog catchers required per day, given that each dog catcher can catch 13 dogs per day
    costs_results[, "kennel_staff_per_month"] <- costs_results$Sh/13 # Calculates the number of shelter workers required per day, given that each shelter worker can care for 13 dogs per day
    vets <- sum(costs_results$vets_per_day) # Sums vet months
    vet_nurse <- sum(costs_results$vet_nurses_per_day)  # Sums vet nurses months
    dog_catch <- sum(costs_results$dog_catchers_per_day)  # Sums dog catchers months
    kennel_staff <- sum(costs_results$kennel_staff_per_month) # Sums kennel staff months
    total_staff_time <- (vets + vet_nurse + dog_catch + kennel_staff) # Sums the total staff time and change from month to year
    proportional_cost <- format(round( ( (vets*1) + (vet_nurse*0.65) + (dog_catch*0.56) * 20000), 0), nsmall=0) # Calculates the proportional costs of each staff type
    total_costs <- data.frame(vets, vet_nurse, dog_catch, total_staff_time, proportional_cost) # Creates a dataframe of the total costs
    # formats values to 0 decimal places
    total_costs$vets <- format(round(total_costs$vets, 0), nsmall=0)
    total_costs$vet_nurse <- format(round(total_costs$vet_nurse, 0), nsmall=0)
    total_costs$dog_catch <- format(round(total_costs$dog_catch, 0), nsmall=0)
    total_costs$total_staff_time <- format(round(total_costs$total_staff_time, 0), nsmall=0)
    # names columns
    colnames(total_costs) <- c("Vet hours/year","Vet nurse hours/year","Dog catcher hours/year", "Total staff hours/year", "Proportional costs (€)")
    
    #### Welfare analysis ###
    welfare_rows <- seq(0, length, by = 1) # Deletes the rows so only full time-steps are included
    welfare_results <- subset(model_output_max, time %in% welfare_rows) # selects only the rows for months
    # calculates the welfare scores for each dog subpopulation type
    welfare_results[, "score_intact"] <- welfare_results$St*2.3
    welfare_results[, "score_neutered"] <- welfare_results$Ne*3.1
    welfare_results[, "score_shelter"] <- welfare_results$Sh*2.8
    welfare_results[, "score_owned"] <- welfare_results$Ow*3.5
    # calculates an overall welfare score
    welfare_results[, "score_overall"] <- (welfare_results$score_intact + welfare_results$score_neutered + welfare_results$score_shelter + welfare_results$score_owned)/(welfare_results$St + welfare_results$Ne + welfare_results$Sh + welfare_results$Ow)
    total_welfare <- format(round((sum(welfare_results$score_overall))/length, 2), nsmall=2) # formats value to 0 decimal places
    total_welfare <- data.frame(total_welfare) # creates a dataframe for total welfare
    colnames(total_welfare) <- ("Welfare score") # names columns
    fields <- c("neuter_rate", "length", "periodicity")
    
    # lists all the objects that will be output from this section of code
    list(pop_estimates=pop_estimates, pop_change_sim = pop_change_sim, pop_change_int=pop_change_int, total_welfare=total_welfare, total_costs=total_costs, results=results, time=results$time, value=results$value, pop=results$pop, fields=fields, proportional_cost=proportional_cost,
         pop_size_table2 = pop_size_table2)
    
  })
  
  # Saving what data was filled in so can be listed in log of simulations
  form_Data <- reactive({
    form_in <- sapply((systems_data()[["fields"]]), function(x) input[[x]])
    form_out <- (systems_data()[["total_costs"]])
    form <- data.frame(input$neuter_rate, input$length, input$int_length, input$periodicity, input$abandonment_rate, input$shelter_adoption_rate, (systems_data()[["proportional_cost"]]), (systems_data()[["total_welfare"]]), (systems_data()[["pop_change_int"]]), (systems_data()[["pop_change_sim"]]))
    colnames(form) <- c("Neutering coverage (0.5 = 50%)", "Length of simulation", "Length of intervention", "Periodicity of intervention", "Abandonment rate", "Shelter adoption rate", "Proportional costs", "Welfare score", "Population change at end of intervention", "Population change at end of simulation")
    form
  })
  
  # When the Submit button is clicked, form data is saved
  observeEvent(input$go, {
    saveData(form_Data())
  })
  
  # Shows the previous responses
  # (update with current response when simulate results is clicked)
  output$responses <- renderDataTable({
    input$go
    loadData()
  })
  
  # outputs graph of dog population sizes over time
  output$graph1 <- renderPlot({
    p = ggplot(systems_data()[["results"]], aes(x=systems_data()[["time"]]), y=systems_data()[["value"]]) +
      xlab("Time (years)") +
      ylab("Estimated population size")
    p = p + geom_line(aes(y=systems_data()[["value"]], color=systems_data()[["pop"]]), size=1.5)
    p = p + scale_color_manual(name="Population", 
                               labels = c("Intact", 
                                          "Neutered", 
                                          "Total"), 
                               values = c("St"="red", 
                                          "Ne"="darkgreen", 
                                          "FRD"="blue"))
    
    print(p)
    
  })
  
  # outputs table of costs
  output$costs_table <- renderTable({
    systems_data()[["total_costs"]]
  })
  
  # outputs welfare table
  output$welfare_table <- renderTable({
    systems_data()[["total_welfare"]]
  })
  
  # outputs table of population estimates
  output$population_estimates <- renderTable({
    systems_data()[["pop_estimates"]]
  })
  
  # outputs an example table for mark-recapture CSV formatting with column and row names
  output$table_example_1 <- renderTable({
    
    survey_1 <- c(1,1,0)
    survey_2 <- c(1,0,0)
    survey_3 <- c(1,1,1)
    
    example_table <- format(round(data.frame(survey_1, survey_2, survey_3), 0), nsmall=0)
    row.names(example_table) <- c("Dog 1", "Dog 2", "Dog 3")
    colnames(example_table) <- c("Survey 1", "Survey 2", "Survey 3")
    example_table
    
  }, rownames = TRUE, colnames = TRUE)
  
  # outputs example table for mark-recapture CSV formatting without column or row names
  output$table_example_2 <- renderTable({
    
    survey_1 <- c(1,1,0)
    survey_2 <- c(1,0,0)
    survey_3 <- c(1,1,1)
    
    example_table <- format(round(data.frame(survey_1, survey_2, survey_3), 0), nsmall=0)
    row.names(example_table) <- c("Dog 1", "Dog 2", "Dog 3")
    colnames(example_table) <- c("Survey 1", "Survey 2", "Survey 3")
    example_table
    
  }, rownames = FALSE, colnames = FALSE)
  
  # outputs the population size table
  output$pop_size_table <- renderTable({
    
    systems_data()[["pop_size_table2"]]
    
  })
  
}


shinyApp(ui = ui, server = server)
