
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(reshape2)
library(scales)
options(scipen = 999)

# Define UI for slider demo app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Calculateur d'annuités"),
    h4(tags$a(href = "https://www.poitoucharentes.cerfrance.fr", "Guillaume Verdier")),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar to demonstrate various slider options ----
        sidebarPanel(

            img(src = "LogoCefrancePCH251X100.jpg",.noWS = NULL),
            # br()  ► espace
            
            # Input: Simple integer interval ----
            numericInput("principal", "Montant du prêt (EUR)", 100000, min = 0, step = 1000),
            hr(),
            numericInput("interest", "taux d'intérêt (%)", 2.15, min = 0, max = 100, step = 0.01),
            hr(),
            sliderInput("length", "durée du prêt (année(s))",
                        min = 0,
                        max = 30,
                        value = 10,
                        step = 1
            ),
            hr(),
            checkboxInput("plot", "Affichage du graphique ?", TRUE),
            hr(),
            HTML('<p>Rapporter un <a href="https://github.com/CerfrancePCH/Calculs_financiers/issues">bug</a> ou voir le <a href="https://github.com/CerfrancePCH/Calculs_financiers">code</a>.                      Back to <a href="https://www.poitoucharentes.cerfrance.fr/">www.poitoucharentes.cerfrance.fr</a>.</p>')
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Table summarizing the values entered ----
            uiOutput("text"),
            br(),
            plotOutput("distPlot"),
            br(),
            DT::dataTableOutput("tbl"),
            br(),
            p(em("Attention: Cette application n'inclut aucun conseil ou recommendation en terme d'investissement, ni d'analyse financiere. Cette application n'a d'autre but qu'une simple information financière, détachée de tout investissement. Cerfrance PCH ne saurait être tenu responsable de décisions d'investissements basées sur les seules informations de cette application, ou de son utilisation par des tiers. Tout emprunt engage celui qui le réalise et doit etre remboursé.")),
            p(em("Cette application R Shiny est tres largement inspiree du code R de Antoine Soetewey, www.antoinesoetewey.com.")),
            br(),
            br()
        )
    )
)

# Define server logic for slider examples ----
server <- function(input, output) {
    mortgage <- function(P = 500000, I = 6, L = 30, amort = TRUE, plotData = TRUE) {
        J <- I / (12 * 100)
        N <- 12 * L
        M <- P * J / (1 - (1 + J)^(-N))
        monthPay <<- M
        # Calculate Amortization for each Month
        if (amort == TRUE) {
            Pt <- P # current principal or amount of the loan
            currP <- NULL
            while (Pt >= 0) {
                H <- Pt * J # this is the current monthly interest
                C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
                Q <- Pt - C # this is the new balance of your principal of your loan
                Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
                currP <- c(currP, Pt)
            }
            monthP <- c(P, currP[1:(length(currP) - 1)]) - currP
            aDFmonth <<- data.frame(
                Month = 1:length(currP),
                Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
                Balance = c(currP[1:(length(currP))]),
                Payment = monthP + c((monthPay - monthP)[1:(length(monthP))]),
                Principal = monthP,
                Interest = c((monthPay - monthP)[1:(length(monthP))])
            )
            aDFmonth <<- subset(aDFmonth, Year <= L * 12)
            aDFyear <- data.frame(
                Amortization = tapply(aDFmonth$Balance, aDFmonth$Year, max),
                Annual_Payment = tapply(aDFmonth$Payment, aDFmonth$Year, sum),
                Annual_Principal = tapply(aDFmonth$Principal, aDFmonth$Year, sum),
                Annual_Interest = tapply(aDFmonth$Interest, aDFmonth$Year, sum),
                Year = as.factor(na.omit(unique(aDFmonth$Year)))
            )
            aDFyear <<- aDFyear
        }
        if (plotData == TRUE) {
            aDFyear2 <- aDFyear %>%
                rename(
                    Interest = Annual_Interest,
                    Payment = Annual_Payment,
                    Principal = Annual_Principal
                )
            aDFyear2$Year <- as.factor(aDFyear2$Year)
            aDFyear2 <- melt(aDFyear2[, c("Interest", "Principal", "Year")], id.vars = "Year")
            
            ggplot(aDFyear2, aes(x = Year, y = value, fill = variable)) +
                geom_bar(position = "fill", stat = "identity") +
                labs(y = "Payment") +
                scale_y_continuous(labels = percent) +
                theme_minimal() +
                theme(legend.title = element_blank(), legend.position = "top")
        }
    }
    
    output$text <- renderUI({
        mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
        HTML(paste0(
            "<h3>", "Recapitulatif", "</h3>",
            "Montant emprunté       : ", format(round(input$principal, 2), big.mark = " "),
            "<br>",
            "Taux d'interêt         : ", input$interest, "%",
            "<br>",
            "Durée d'emprunt        : ", input$length, " année (", input$length * 12, " mois)",
            "<br>",
            "<b>", "Paiement mensuel: ", format(round(monthPay, digits = 0), big.mark = " "), "</b>",
            "<br>",
            "<b>", "Paiement 12 mois: ", format(round(monthPay * 12, digits = 0), big.mark = " "), "</b>",
            "<br>",
            "<b>", "Total remboursé : ", "</b>", format(round(input$principal, 0), big.mark = " "), " (capital) + ", format(round(monthPay * 12 * input$length - input$principal, 0), big.mark = " "), " (interêts) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 0), big.mark = " "), "</b>"
        ))
    })
    
    output$distPlot <- renderPlot({
        mortgage(P = input$principal, I = input$interest, L = input$length, plotData = input$plot)
    })
    
    # Data output
    output$tbl <- DT::renderDataTable({
        mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
        df_month <- DT::datatable(data.frame(round(aDFmonth, 2)),
                                  extensions = "Buttons",
                                  options = list(
                                      lengthChange = TRUE,
                                      dom = "Blrtip",
                                      buttons = c("copy", "csv", "excel", "pdf", "print"),
                                      
                                      lengthMenu = list(c(-1, 10, 12, 15, 25, 50, 100), c("All", "10", "12", "15", "25", "50", "100"))
                                  ),
                                  rownames = FALSE
        ) %>%
            formatCurrency(c("Balance", "Payment", "Principal", "Interest"), currency = "", interval = 3, mark = " ")
    })
}

# Run the application
shinyApp(ui = ui, server = server)