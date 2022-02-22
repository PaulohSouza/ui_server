######################################################
#  Copyright 2021 - Author(s):                       #
######################################################
#                                                    #
######################################################

######################################################
#              bibliotecas utilizadas                #
######################################################

library(shiny)
library(shinyalert)
library(ExpDes.pt)
library(rmarkdown)
library(labestData)
library(plotly)
library(xtable)
library(shinythemes)
library(pracma)
library(stringr)
library(ds)
library(car)
library(tidyverse)
library(ggplot2)
library(pacman)
library(agricolae)
library(ggthemes)
library(multcompView)
library(dplyr)
library(easyanova)
library(xlsx)
library(correlation)
library(factoextra)
library(ggraph)
library(Hmisc)
library(see)
library(corrplot)
library(RColorBrewer)
library(highcharter)
library(grDevices)
library(Tratamentos.ad)


######################################################
#        ui - parte cliente do shiny                 #
######################################################
ui <-navbarPage("FMT Análises", windowTitle = 'FMT Análises', collapsible = TRUE, theme = shinytheme("paper"),
                useShinyalert(),
                tabPanel
                (
                  ":",
                  sidebarLayout
                  (
                    sidebarPanel

                    ######################################################
                    #              scripts dos botoes avancar            #
                    ######################################################
                    (tags$head(tags$script('
                                           Shiny.addCustomMessageHandler("myCallbackHandler",
                                           function(typeMessage) {console.log(typeMessage)
                                           if(typeMessage == 1){
                                           $("a:contains(Conjunto de dados)").click();
                                           }
                                           if(typeMessage == 2){
                                           $("a:contains(Escolha da análise)").click();
                                           }
                                           if(typeMessage == 3){
                                           $("a:contains(ANAVA DIC)").click();
                                           }
                                           if(typeMessage == 4){
                                           $("a:contains(ANAVA DBC)").click();
                                           }
                                           if(typeMessage == 5){
                                           $("a:contains(CSV)").click();
                                           }
                                           if(typeMessage == 6){
                                           $("a:contains(LABEST)").click();
                                           }
                                           if(typeMessage == 7){
                                           $("a:contains(Conjunto de dados)").click();
                                           }
                                           if(typeMessage == 8){
                                           $("a:contains(Escolha da análise)").click();
                                           }
                                           if(typeMessage == 9){
                                           $("a:contains(ANAVA DQL)").click();
                                           }
                                           if(typeMessage == 10){
                                           $("a:contains(Escolha da análise)").click();
                                           }
                                           if(typeMessage == 12){
                                           $("a:contains(F)").click();
                                           }
                                           if(typeMessage == 13){
                                           $("a:contains(G)").click();
                                           }
                                           if(typeMessage == 14){
                                           $("a:contains(H)").click();
                                           }
                                           if(typeMessage == 15){
                                           $("a:contains(I)").click();
                                           }
                                           if(typeMessage == 16){
                                           $("a:contains(J)").click();
                                           }
                                           if(typeMessage == 17){
                                           $("a:contains(K)").click();
                                           }
                                           if(typeMessage == 18){
                                           $("a:contains(L)").click();
                                           }
                                           if(typeMessage == 19){
                                           $("a:contains(M)").click();
                                           }
                                           if(typeMessage == 20){
                                           $("a:contains(N)").click();
                                           }
                                           if(typeMessage == 21){
                                           $("a:contains(O)").click();
                                           }
                                           if(typeMessage == 22){
                                           $("a:contains(P)").click();
                                           }
                                           if(typeMessage == 23){
                                           $("a:contains(P)").click();
                                           }
                                           if(typeMessage == 24){
                                           $("a:contains(Escolha da análise)").click();
                                           }
                                          if(typeMessage == 25){
                                           $("a:contains(Q)").click();
                                           }

                                           });
                                           ')),

                     ######################################################
                     # botao iniciar, fechar e seus alinhamentos em html  #
                     ######################################################

                     # Linha horizontal ----
                     tags$hr(),

                     tags$div(align="center", 
                              actionButton("action7", label="Iniciar a análise", class="btn btn-success", icon= icon("angle-right"), width="65%")
                     ),

                     br(),
                     tags$div(align="center",
                              actionButton("action5", label=" Instruções de uso", onclick ="window.open('https://docs.google.com/document/d/1to1ORIJu0VtCZZFE2uiE7eAFz8Y2zbINuf6AE9NHkHY', '_blank')",
                                           class="btn btn-success", icon=icon("question-circle"), width="65%")
                     ),
                  
                    br(),
                    tags$div(align="center",
                              actionButton("action25", label=" Atualizações", onclick ="window.open('https://docs.google.com/document/d/1fJ2NZZLep1dV0TSEmhACCj5EH0WQdpyAZTjonXBJC1c', '_blank')",
                                           class="btn btn-success", icon=icon("table"), width="65%")

                    ),

                     br(),
                     tags$div(align="center",
                              actionButton("fechar", label=" Fechar", class="btn btn-success", icon=icon("power-off"), onclick = "setTimeout(function(){window.close();},500);", width="65%")
                     ),

                     # Linha horizontal ----
                     tags$hr()

                    ),
                    mainPanel(tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='560')),
                              tags$div( align="center", 
                                             br(),
                                             HTML(" FMT Análises - 2021. versão 1.5"),
                                             br(),
                                             HTML(" Centro de Pesquisa Dario Minoru Hiromoto"),
                                             br(),
                                             HTML(" Av. Antônio Teixeira dos Santos, 1559"),
                                             HTML(" - Parque Universitário"),
                                             br(),
                                             br(), 
                                             HTML(" Tel: (66) 3439-4100 - Rondonópolis - MT"),
                                             br(),
                                             br()
                              ))
                    )

                  ),

                tabPanel
                (
                  "Conjunto de dados", icon=icon("arrow-up"),
                  sidebarLayout
                  (
                    sidebarPanel(

                     # Linha horizontal ----
                     tags$hr(),

                     # Input: Seleciona o separador ----
                     radioButtons("conj", "O Conjunto de dados analisados será obtido via:",
                                  choices = c('Importação de arquivo csv [Recomendado]' = "csv",
                                              'Importação de arquivo xlsx' = 'xlsx',
                                              'Dados Exemplos - (Para Treinamento)' = "labest"),
                                  selected = "csv"),

                     # Linha horizontal ----
                     tags$hr(),


                     ######################################################
                     #     botao avancar e seus alinhamentos em html      #
                     ######################################################
                     tags$div(align="center", border="0",

                              actionButton("action11", label = "Avançar", class="btn btn-success", icon= icon("angle-right"), width="100")
                     )

                    ),
                    mainPanel(tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='560'), br()),
                              br()
                    )
                    )

                  ),

                navbarMenu("Banco de Dados", icon=icon("arrow-up"),
                tabPanel
                (
                  "CSV", icon=icon("arrow-up"),
                  sidebarLayout
                  (
                    sidebarPanel

                    ######################################################
                    #              scripts dos botoes avancar            #
                    ######################################################
                    (

                      # Input: Seleciona o arquivo ----
                      fileInput("file1", "Escolha o arquivo CSV/xlsx",
                                multiple = FALSE,
                                buttonLabel = "Procurar...",
                                placeholder = "Dados não selecionados"),

                      # Input: Seleciona o separador ----
                      radioButtons("sep", "Separador de valores",
                                   choices = c('Virgula' = ",",
                                               'Ponto e virgula' = ";",
                                               'Tabulação' = "\t"),
                                   selected = ";"),

                      # Input: Select  ----
                      radioButtons("dec", "Separador Decimal",
                                   choices = c('Ponto' = ".",
                                               "Virgula" = ","),
                                   selected = ','),

                      # Linha horizontal ----
                      tags$hr(),



                      ######################################################
                      #     botao avancar e seus alinhamentos em html      #
                      ######################################################

                      tags$div(align="center",  border="0",
                               actionButton("action1", label = "< Voltar", class="btn btn-success", width="100"),
                               actionButton("action8", label = "Avançar >", class="btn btn-success", width="100")
                      )

                    ),
                    # mainPanel(verbatimTextOutput("tabela")
                    mainPanel(tableOutput("tabela"))
                    
                    ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()

                  ),

                tabPanel
                (
                  "LABEST", icon=icon("arrow-up"),

                  sidebarLayout
                  (

                    sidebarPanel
                    (
                      htmlOutput("menu17"),
                      includeCSS("palatino.css"),

                      ######################################################
                      #     botao avancar e seus alinhamentos em html      #
                      ######################################################
                           tags$div(align="center", 
                               actionButton("action23", label = "< Voltar", class="btn btn-success", width="45%"),
                               actionButton("action24", label = "Avançar >", class="btn btn-success", width="45%")
                           )
                    ),
                    mainPanel(uiOutput("TABLE_DOC"))
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                          ),
                  br()
                )
                ),

                ######################################################
                #              painel de escolha da analise          #
                ######################################################
                tabPanel
                (
                  "Escolha da análise", icon=icon("check-circle"),
                  sidebarLayout
                  (
                    sidebarPanel
                    (
                      tags$form(id="formulario", action="dados",
                                # Input: Seleciona o Delineamento ----

                                selectInput("deli", label = "Selecione o Delineamento",
                                             choices = c('DIC' = "dic",
                                                         'DBC' = "dbc",
                                                         'DQL' = "dql",
                                                         'Faixas' = "faixas",
                                                         'Fatorial duplo com um tratamento adicional DBC' = "fat2addbc",
                                                         'Fatorial duplo com um tratamento adicional DBC com DUNNET' = "fat2addunnet",
                                                         'Fatorial duplo com um tratamento adicional DIC' = "fat2addic",
                                                         'Fatorial duplo em DBC' = "fat2dbc",
                                                         'Fatorial duplo em DIC' = "fat2dic",
                                                         'Fatorial triplo com um tratamento adicional em DBC' = "fat3addbc",
                                                         'Fatorial triplo com um tratamento adicional em DIC' = "fat3addic",
                                                         'Fatorial triplo em DBC' = "fat3dbc",
                                                         'Fatorial triplo em DIC' = "fat3dic",
                                                         'Parcelas subdivididas em DBC' = "psub2dbc",
                                                         'Parcelas subdivididas em DIC' = "psub2dic"),
                                             selected = "dic"),


                                htmlOutput("menu"), htmlOutput("menu9"),
                                htmlOutput("menu7"), htmlOutput("menu10"), htmlOutput("menu8"), htmlOutput("menu11"), htmlOutput("menu15"), htmlOutput("menu16"),
                                htmlOutput("menu1"), htmlOutput("menu2"), htmlOutput("menu3"), htmlOutput("menu4"), htmlOutput("menu14"),
                                htmlOutput("menu13"), htmlOutput("menu18"), htmlOutput("linear"), htmlOutput("linear1"),
                                tags$hr(),

                                tags$div(align="center",
                                         actionButton("action", label = "< Voltar", class="btn btn-success", width="100"),
                                         actionButton("action2", label = "Avançar >", class="btn btn-success", width="100")
                                )
                      )

                    ),

                    ######################################################
                    #    impressao da estrutura dos dados                #
                    ######################################################
                    mainPanel(
                      tabsetPanel(
                         tabPanel("Estrutura",  verbatimTextOutput("structure"))
                         )
                      )
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()

                ),

                ######################################################
                #                   analise em dic                  #
                ######################################################
                navbarMenu("Delineamentos", icon=icon("bars"),
                tabPanel
                (
                  "ANAVA DIC", icon=icon("bars"),
                  sidebarLayout
                  (
                    conditionalPanel(condition = "input.deli == 'dic'",
                    sidebarPanel

                    (
                      
                      tags$div(align="left",
                               actionButton("action3", label = "< Voltar", class="btn btn-sucess", width="30%")
                      ),
                      br(),
                      # Input: Seleciona o teste de comparacao múltipla ----
                      htmlOutput("testecomparacao"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigF", "Significância a ser adotada pelo teste F: ", "0.05"),

                      tags$hr(),

                      # Input: Seleciona o teste de homogeneidade ----
                      htmlOutput("homogeneidade"),


                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigT", "Significância a ser adotada pelo teste de comparacao múltipla de médias: ", "0.05"),

                      tags$hr(),

                      # Defina um titulo para os gráficos no eixo X
                      
                      textInput("labelX", "Defina um título para o eixo X dos gráficos: ", "Tratamentos"),
                      # Defina um titulo para os gráficos no eixo y
                      textInput("labelY", "Defina um título para o eixo Y dos gráficos: ", "Resposta"),

                      # Valores para o gráfico

                      ######################################################
                      #     botao voltar e seus alinhamentos em html       #
                      ######################################################

                      br(),
                      ######################################################
                      #botao para imprimir valores no formato escolhido DIC#
                      ######################################################


                      radioButtons('format', 'Formato do documento', c(  'HTML', 'DOCX'),
                                   inline = TRUE),
                      downloadButton('downloadReportDIC')
                     )


                    ),
                    mainPanel(

                      ######################################################
                      #              painel de impressao do dic            #
                      ######################################################
                      conditionalPanel(condition = "input.deli == 'dic'",
                      htmlOutput("uidicmodelos")
                    )
                    )
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()

                ),

                ######################################################
                #                analise em dbc                      #
                ######################################################

                tabPanel
                (
                  "ANAVA DBC", icon=icon("bars"),
                  sidebarLayout
                  (
                    conditionalPanel(condition = "input.deli == 'dbc'",
                    sidebarPanel
                    (
                      tags$div(align="left",
                               actionButton("action4", label = "< Voltar", class="btn btn-sucess", width="30%")
                      ),
                      br(),
                      # Input: Seleciona o teste de hipotese ----

                      radioButtons("mcomp1", "Selecione o teste de comparacao múltipla:",
                                   choices = c('Tukey' = "tukey",
                                               'LSD' = "lsd",
                                               'Duncan' = "duncan",
                                               'SNK' = "snk"),
                                               #'Calinski e Corsten' = "ccf",
                                               # 'Scott-Knott' = "sk"),
                                   selected = "lsd"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigF1", "Significância a ser adotada pelo teste F: ", "0.05"),

                      # Input: Seleciona o teste de hipotese ----
                      radioButtons("hvar1", "Selecione o teste de homogeneidade de variancias:",
                                   choices = c('Han' = "han",
                                               'Ascombe e Tukey' = "anscombetukey",
                                               'ONeill e Mathews' = "oneillmathews"),
                                   selected = "oneillmathews"),


                      # Input: Significância a ser adotada pelo teste F ----

                      textInput("sigT1", "Significância a ser adotada pelo teste de comparacao múltipla de médias: ", "0.05"),

                      htmlOutput("menu5"),
                      htmlOutput("menu6"),
                      tags$hr(),
                      
                      # Defina um titulo para os gráficos no eixo X
                      
                      textInput("labelXDBC", "Defina um título para o eixo X dos gráficos: ", "Tratamentos"),
                      # Defina um titulo para os gráficos no eixo y
                      textInput("labelYDBC", "Defina um título para o eixo Y dos gráficos: ", "Resposta"),


                      ######################################################
                      #     botao voltar e seus alinhamentos em html       #
                      ######################################################

                      
                      br(),
                      ######################################################
                      #botao para imprimir valores no formato escolhido DBC#
                      ######################################################


                      radioButtons('format', 'Formato do documento', c( 'HTML', 'DOCX'),
                                   inline = TRUE),
                      downloadButton('downloadReportDBC')
                    )
                    ),
                    mainPanel(

                      ######################################################
                      #              painel de impressao do dbc            #
                      ######################################################
                      conditionalPanel(condition = "input.deli == 'dbc'",
                      htmlOutput("uidbcmodelos")
                    )
                    )
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()

                ),

                ######################################################
                #                analise dql                         #
                ######################################################

                tabPanel
                (
                  "ANAVA DQL", icon=icon("bars"),
                  sidebarLayout
                  (
                    conditionalPanel(condition = "input.deli == 'dql'",
                    sidebarPanel
                    (
                       tags$div(align="left",
                               actionButton("action10", label = "< Voltar", class="btn btn-success", width="30%")
                      ),
                      br(),
                      # Input: Seleciona o teste de hipotese ----

                      radioButtons("mcomp2", "Selecione o teste de comparacao múltipla:",
                                   choices = c('Tukey' = "tukey",
                                               'LSD' = "lsd",
                                               'LSDB' = "lsdb",
                                               'Duncan' = "duncan",
                                               'SNK' = "snk",
                                               'Scott-Knott' = "sk"),
                                               #'Bootstrap' = "ccboot",
                                               #'Calinski e Corsten' = "ccf"),
                                   selected = "tukey"),

                      # Input: Significância a ser adotada pelo teste F ----

                      textInput("sigF2", "Significância a ser adotada pelo teste F: ", "0.05"),

                      # Input: Significância a ser adotada pelo teste F ----

                      textInput("sigT2", "Significância a ser adotada pelo teste de comparacao múltipla de médias: ", "0.05"),

                      tags$hr(),

                      ######################################################
                      #     botao voltar e seus alinhamentos em html       #
                      ######################################################

                     
                      br(),
                      ######################################################
                      #botao para imprimir valores no formato escolhido DQL#
                      ######################################################


                      radioButtons('format', 'Formato do documento', c(  'HTML', 'DOCX'),
                                   inline = TRUE),
                      downloadButton('downloadReportDQL')
                    )
                    ),
                    mainPanel(

                      ######################################################
                      #              painel de impressao dql               #
                      ######################################################
                      conditionalPanel(condition = "input.deli == 'dql'",
                      tabsetPanel(
                        tabPanel("Box Plot",           plotlyOutput("plotar2")),
                        tabPanel("Análise de resíduo",          plotOutput("resid_plot2")),
                        tabPanel("ANAVA",       verbatimTextOutput("anovatable2"))
                      )
                    )
                    )
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()

                ),

                ######################################################
                #                analise faixas                      #
                ######################################################

                tabPanel
                (
                  "F", icon=icon("bars"),
                  sidebarLayout
                  (
                    conditionalPanel(condition = "input.deli == 'faixas'",
                    sidebarPanel
                    (
                      # Input: Seleciona o teste de hipotese ----

                      radioButtons("mcomp3", "Selecione o teste de comparacao múltipla:",
                                   choices = c('Tukey' = "tukey",
                                               'LSD' = "lsd",
                                               'LSDB' = "lsdb",
                                               'Duncan' = "duncan",
                                               'SNK' = "snk",
                                               'Scott-Knott' = "sk",
                                               'Bootstrap' = "ccboot",
                                               'Calinski e Corsten' = "ccf"),
                                   selected = "tukey"),

                      # Input: Significância a ser adotada pelo teste F ----

                      textInput("sigF3", "Significância a ser adotada pelo teste F: ", "0.05"),

                      # Input: Significância a ser adotada pelo teste F ----

                      textInput("sigT3", "Significância a ser adotada pelo teste de comparacao múltipla de médias: ", "0.05"),

                      tags$hr(),

                      ######################################################
                      #     botao voltar e seus alinhamentos em html       #
                      ######################################################

                      tags$div(align="left",
                               actionButton("action10", label = "< Voltar", class="btn btn-sucess", width="50%")
                      ),
                      br(),

                      #########################################################
                      #botao para imprimir valores no formato escolhido FAIXAS#
                      #########################################################


                      radioButtons('format', 'Formato do documento', c(  'HTML', 'DOCX'),
                                   inline = TRUE),
                      downloadButton('downloadReportFAIXAS')
                    )
                    ),
                    mainPanel(

                      ######################################################
                      #              painel de impressao faixas            #
                      ######################################################
                      conditionalPanel(condition = "input.deli == 'faixas'",
                      tabsetPanel(
                        tabPanel("Box Plot",           plotlyOutput("plotar3")),
                        tabPanel("Análise de resíduo",          plotOutput("resid_plot3")),
                        tabPanel("ANAVA",       verbatimTextOutput("anovatable3"))
                      )
                    )
                    )
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()

                ),

                ######################################################
                # analise fat duplo com 1 trat adicional em DBC      #
                ######################################################

                tabPanel
                (
                  "G", icon=icon("bars"),
                  sidebarLayout
                  (
                    conditionalPanel(condition = "input.deli == 'fat2addbc'",
                    sidebarPanel
                    (
                      # Input: Seleciona o teste de hipotese ----

                      radioButtons("mcomp4", "Selecione o teste de comparacao múltipla:",
                                   choices = c('Tukey' = "tukey",
                                               'LSD' = "lsd",
                                               'LSDB' = "lsdb",
                                               'Duncan' = "duncan",
                                               'SNK' = "snk",
                                               'Scott-Knott' = "sk",
                                               'Bootstrap' = "ccboot",
                                               'Calinski e Corsten' = "ccf"),
                                   selected = "tukey"),

                      # Input: Significância a ser adotada pelo teste F ----

                      textInput("sigF4", "Significância a ser adotada pelo teste F: ", "0.05"),

                      # Input: Significância a ser adotada pelo teste F ----

                      textInput("sigT4", "Significância a ser adotada pelo teste de comparacao múltipla de médias: ", "0.05"),

                      tags$hr(),

                      ######################################################
                      #     botao voltar e seus alinhamentos em html       #
                      ######################################################

                      tags$div(align="left",
                               actionButton("action10", label = "< Voltar", class="btn btn-sucess", width="50%")
                      ),
                      br(),
                      ################################################################
                      #botao para imprimir valores no formato escolhido fat 1adc dbc #
                      ################################################################


                      radioButtons('format', 'Formato do documento', c(  'HTML', 'DOCX'),
                                   inline = TRUE),
                      downloadButton('downloadReportfat1adcdbc')
                    )
                    ),
                    mainPanel(

                      ######################################################
                      #    analise fat duplo com 1 trat adicional em DBC   #
                      ######################################################
                      conditionalPanel(condition = "input.deli == 'fat2addbc'",
                      tabsetPanel(
                        tabPanel("Box Plot",           plotlyOutput("plotar4")),
                        tabPanel("Análise de resíduo",          plotOutput("resid_plot4")),
                        tabPanel("ANAVA",       verbatimTextOutput("anovatable4"))
                      )
                    )
                    )
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()

                ),
                
                tabPanel(
                  "Q",
                  sidebarLayout(
                    sidebarPanel(
                      h5("Menu de opções em desenvolvimento \n
                           Estou atualizando as opções, \n
                           Anova já visivel")
                    ),
                    mainPanel(
                      tabsetPanel(
                        tabPanel("ANAVA",       verbatimTextOutput("anovatable15"))
                        )
                    )
                  )

                ),

                ######################################################
                # analise fat duplo com 1 trat adicional em DIC      #
                ######################################################

                tabPanel
                (
                  "H", icon=icon("bars"),
                  sidebarLayout
                  (
                    conditionalPanel(condition = "input.deli == 'fat2addic'",
                    sidebarPanel
                    (
                      # Input: Seleciona o teste de hipotese ----

                      radioButtons("mcomp5", "Selecione o teste de comparacao múltipla:",
                                   choices = c('Tukey' = "tukey",
                                               'LSD' = "lsd",
                                               'LSDB' = "lsdb",
                                               'Duncan' = "duncan",
                                               'SNK' = "snk",
                                               'Scott-Knott' = "sk",
                                               'Bootstrap' = "ccboot",
                                               'Calinski e Corsten' = "ccf"),
                                   selected = "tukey"),

                      # Input: Significância a ser adotada pelo teste F ----

                      textInput("sigF5", "Significância a ser adotada pelo teste F: ", "0.05"),

                      # Input: Significância a ser adotada pelo teste F ----

                      textInput("sigT5", "Significância a ser adotada pelo teste de comparacao múltipla de médias: ", "0.05"),

                      tags$hr(),

                      ######################################################
                      #     botao voltar e seus alinhamentos em html       #
                      ######################################################

                      tags$div(align="left",
                               actionButton("action10", label = "< Voltar", class="btn btn-sucess", width="50%")
                      ),
                      br(),
                      ################################################################
                      #botao para imprimir valores no formato escolhido fat 1adc dic #
                      ################################################################


                      radioButtons('format', 'Formato do documento', c(  'HTML', 'DOCX'),
                                   inline = TRUE),
                      downloadButton('downloadReportfat1adcdic')
                    )
                    ),
                    mainPanel(

                      ######################################################
                      #    analise fat duplo com 1 trat adicional em DIC   #
                      ######################################################
                      conditionalPanel(condition = "input.deli == 'fat2addic'",
                      tabsetPanel(
                        tabPanel("Box Plot",           plotlyOutput("plotar5")),
                        tabPanel("Análise de resíduo",          plotOutput("resid_plot5")),
                        tabPanel("ANAVA",       verbatimTextOutput("anovatable5"))
                      )
                    )
                    )
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()

                ),


                ######################################################
                # analise fat duplo  em DBC                          #
                ######################################################

                tabPanel
                (
                  "I", icon=icon("bars"),
                  sidebarLayout
                  (
                    conditionalPanel(condition = "input.deli == 'fat2dbc'",
                    sidebarPanel
                    (
                      # Input: Seleciona o teste de hipotese ----

                      radioButtons("mcomp6", "Selecione o teste de comparacao múltipla:",
                                   choices = c('Tukey' = "tukey",
                                               'LSD' = "lsd",
                                               'LSDB' = "lsdb",
                                               'Duncan' = "duncan",
                                               'SNK' = "snk",
                                               'Scott-Knott' = "sk",
                                               'Bootstrap' = "ccboot",
                                               'Calinski e Corsten' = "ccf"),
                                   selected = "tukey"),

                      # Input: Significância a ser adotada pelo teste F ----

                      textInput("sigF6", "Significância a ser adotada pelo teste F: ", "0.05"),

                      # Input: Significância a ser adotada pelo teste F ----

                      textInput("sigT6", "Significância a ser adotada pelo teste de comparacao múltipla de médias: ", "0.05"),

                      tags$hr(),

                      ######################################################
                      #     botao voltar e seus alinhamentos em html       #
                      ######################################################

                      tags$div(align="left",
                               actionButton("action10", label = "< Voltar", class="btn btn-sucess", width="50%")
                      ),
                      br(),
                      ################################################################
                      #botao para imprimir valores no formato escolhido fat duploDBC #
                      ################################################################


                      radioButtons('format', 'Formato do documento', c(  'HTML', 'DOCX'),
                                   inline = TRUE),
                      downloadButton('downloadReportfatduploDBC')
                    )
                    ),
                    mainPanel(

                      ######################################################
                      #    analise fat duplo em DBC                        #
                      ######################################################
                      conditionalPanel(condition = "input.deli == 'fat2dbc'",
                      tabsetPanel(
                        tabPanel("Box Plot",           plotlyOutput("plotar6")),
                        tabPanel("Análise de resíduo",          plotOutput("resid_plot6")),
                        tabPanel("ANAVA",       verbatimTextOutput("anovatable6"))
                      )
                    )
                    )
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()

                ),

                ######################################################
                # analise fat duplo  em DIC                          #
                ######################################################

                tabPanel
                (
                  "J", icon=icon("bars"),
                  sidebarLayout
                  (
                    conditionalPanel(condition = "input.deli == 'fat2dic'",
                    sidebarPanel
                    (
                      # Input: Seleciona o teste de hipotese ----

                      radioButtons("mcomp7", "Selecione o teste de comparacao múltipla:",
                                   choices = c('Tukey' = "tukey",
                                               'LSD' = "lsd",
                                               'LSDB' = "lsdb",
                                               'Duncan' = "duncan",
                                               'SNK' = "snk",
                                               'Scott-Knott' = "sk",
                                               'Bootstrap' = "ccboot",
                                               'Calinski e Corsten' = "ccf"),
                                   selected = "tukey"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigF7", "Significância a ser adotada pelo teste F: ", "0.05"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigT7", "Significância a ser adotada pelo teste de comparacao múltipla de médias: ", "0.05"),

                      tags$hr(),

                      ######################################################
                      #     botao voltar e seus alinhamentos em html       #
                      ######################################################

                      tags$div(align="left",
                               actionButton("action10", label = "< Voltar", class="btn btn-sucess", width="50%")
                      ),
                      br(),
                      ################################################################
                      #botao para imprimir valores no formato escolhido fat duploDiC #
                      ################################################################


                      radioButtons('format', 'Formato do documento', c(  'HTML', 'DOCX'),
                                   inline = TRUE),
                      downloadButton('downloadReportfatduploDIC')
                    )
                    ),
                    mainPanel(

                      ######################################################
                      #    analise fat duplo em DIC                        #
                      ######################################################
                      conditionalPanel(condition = "input.deli == 'fat2dic'",
                      tabsetPanel(
                        tabPanel("Box Plot",           plotlyOutput("plotar7")),
                        tabPanel("Análise de resíduo",          plotOutput("resid_plot7")),
                        tabPanel("ANAVA",       verbatimTextOutput("anovatable7"))
                      )
                    )
                    )
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1- 2021")
                  ),
                  br()

                ),

                ######################################################
                # Fatorial Triplo com um tratamento adicional em DBC #
                ######################################################

                tabPanel
                (
                  "K", icon=icon("bars"),
                  sidebarLayout
                  (
                    conditionalPanel(condition = "input.deli == 'fat3addbc'",
                    sidebarPanel
                    (
                      # Input: Seleciona o teste de hipotese ----

                      radioButtons("mcomp8", "Selecione o teste de comparacao múltipla:",
                                   choices = c('Tukey' = "tukey",
                                               'LSB' = "lsd",
                                               'LSDB' = "lsdb",
                                               'Duncan' = "duncan",
                                               'SNK' = "snk",
                                               'Scott-Knott' = "sk",
                                               'Bootstrap' = "ccboot",
                                               'Calinski e Corsten' = "ccf"),
                                   selected = "tukey"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigF8", "Significância a ser adotada pelo teste F: ", "0.05"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigT8", "Significância a ser adotada pelo teste de comparacao múltipla de médias: ", "0.05"),

                      tags$hr(),

                      ######################################################
                      #     botao voltar e seus alinhamentos em html       #
                      ######################################################

                      tags$div(align="left",
                               actionButton("action10", label = "< Voltar", class="btn btn-sucess", width="50%")
                      ),
                      br(),

                      #####################################################################
                      #botao para imprimir valores no formato escolhido fat triplo 1ddDBC #
                      #####################################################################


                      radioButtons('format', 'Formato do documento', c(  'HTML', 'DOCX'),
                                   inline = TRUE),
                      downloadButton('downloadReportfattriploaddDBC')
                    )
                    ),
                    mainPanel(

                      ######################################################
                      # Fatorial Triplo com um tratamento adicional em DBC #
                      ######################################################
                      conditionalPanel(condition = "input.deli == 'fat3addbc'",
                      tabsetPanel(
                        tabPanel("Box Plot",           plotlyOutput("plotar8")),
                        tabPanel("Análise de resíduo",          plotOutput("resid_plot8")),
                        tabPanel("ANAVA",       verbatimTextOutput("anovatable8"))
                      )
                    )
                    )
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()

                ),

                ######################################################
                # Fatorial Triplo com um tratamento adicional em DIC #
                ######################################################

                tabPanel
                (
                  "L", icon=icon("bars"),
                  sidebarLayout
                  (
                    conditionalPanel(condition = "input.deli == 'fat3addic'",
                    sidebarPanel
                    (
                      # Input: Seleciona o teste de hipotese ----

                      radioButtons("mcomp9", "Selecione o teste de comparacao múltipla:",
                                   choices = c('Tukey' = "tukey",
                                               'LSB' = "lsd",
                                               'LSDB' = "lsdb",
                                               'Duncan' = "duncan",
                                               'SNK' = "snk",
                                               'Scott-Knott' = "sk",
                                               'Bootstrap' = "ccboot",
                                               'Calinski e Corsten' = "ccf"),
                                   selected = "tukey"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigF9", "Significância a ser adotada pelo teste F: ", "0.05"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigT9", "Significância a ser adotada pelo teste de comparacao múltipla de médias: ", "0.05"),

                      tags$hr(),

                      ######################################################
                      #     botao voltar e seus alinhamentos em html       #
                      ######################################################

                      tags$div(align="left",
                               actionButton("action10", label = "< Voltar", class="btn btn-sucess", width="50%")
                      ),
                      br(),
                      #####################################################################
                      #botao para imprimir valores no formato escolhido fat triplo 1ddDIC #
                      #####################################################################


                      radioButtons('format', 'Formato do documento', c(  'HTML', 'DOCX'),
                                   inline = TRUE),
                      downloadButton('downloadReportfattriploaddDIC')
                    )
                    ),
                    mainPanel(

                      ######################################################
                      # Fatorial Triplo com um tratamento adicional em DIC #
                      ######################################################
                      conditionalPanel(condition = "input.deli == 'fat3addic'",
                      tabsetPanel(
                        tabPanel("Box Plot",           plotlyOutput("plotar9")),
                        tabPanel("Análise de resíduo",          plotOutput("resid_plot9")),
                        tabPanel("ANAVA",       verbatimTextOutput("anovatable9"))
                      )
                    )
                    )
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()

                ),

                ######################################################
                #               Fatorial Triplo em DBC               #
                ######################################################

                tabPanel
                (
                  "M", icon=icon("bars"),
                  sidebarLayout
                  (
                    conditionalPanel(condition = "input.deli == 'fat3dbc'",
                    sidebarPanel
                    (
                      # Input: Seleciona o teste de hipotese ----

                      radioButtons("mcomp10", "Selecione o teste de comparacao múltipla:",
                                   choices = c('Tukey' = "tukey",
                                               'LSB' = "lsd",
                                               'LSDB' = "lsdb",
                                               'Duncan' = "duncan",
                                               'SNK' = "snk",
                                               'Scott-Knott' = "sk",
                                               'Bootstrap' = "ccboot",
                                               'Calinski e Corsten' = "ccf"),
                                   selected = "tukey"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigF10", "Significância a ser adotada pelo teste F: ", "0.05"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigT10", "Significância a ser adotada pelo teste de comparacao múltipla de médias: ", "0.05"),

                      tags$hr(),

                      ######################################################
                      #     botao voltar e seus alinhamentos em html       #
                      ######################################################

                      tags$div(align="left",
                               actionButton("action10", label = "< Voltar", class="btn btn-sucess", width="50%")
                      ),
                      br(),
                      #####################################################################
                      #botao para imprimir valores no formato escolhido fat triplo DBC    #
                      #####################################################################


                      radioButtons('format', 'Formato do documento', c(  'HTML', 'DOCX'),
                                   inline = TRUE),
                      downloadButton('downloadReportfattriploDBC')
                    )

                    ),
                    mainPanel(

                      ######################################################
                      #               Fatorial Triplo em DBC               #
                      ######################################################
                      conditionalPanel(condition = "input.deli == 'fat3dbc'",
                      tabsetPanel(
                        tabPanel("Box Plot",           plotlyOutput("plotar10")),
                        tabPanel("Análise de resíduo",          plotOutput("resid_plot10")),
                        tabPanel("ANAVA",       verbatimTextOutput("anovatable10"))
                      )
                    )
                    )
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()

                ),

                ######################################################
                #               Fatorial Triplo em DIC               #
                ######################################################

                tabPanel
                (
                  "N", icon=icon("bars"),
                  sidebarLayout
                  (
                    conditionalPanel(condition = "input.deli == 'fat3dic'",
                    sidebarPanel
                    (
                      # Input: Seleciona o teste de hipotese ----

                      radioButtons("mcomp11", "Selecione o teste de comparacao múltipla:",
                                   choices = c('Tukey' = "tukey",
                                               'LSB' = "lsd",
                                               'LSDB' = "lsdb",
                                               'Duncan' = "duncan",
                                               'SNK' = "snk",
                                               'Scott-Knott' = "sk",
                                               'Bootstrap' = "ccboot",
                                               'Calinski e Corsten' = "ccf"),
                                   selected = "tukey"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigF11", "Significância a ser adotada pelo teste F: ", "0.05"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigT11", "Significância a ser adotada pelo teste de comparacao múltipla de médias: ", "0.05"),

                      tags$hr(),

                      ######################################################
                      #     botao voltar e seus alinhamentos em html       #
                      ######################################################

                      tags$div(align="left",
                               actionButton("action10", label = "< Voltar", class="btn btn-sucess", width="50%")
                      ),
                      br(),
                      #####################################################################
                      #botao para imprimir valores no formato escolhido fat triplo DIC    #
                      #####################################################################


                      radioButtons('format', 'Formato do documento', c(  'HTML', 'DOCX'),
                                   inline = TRUE),
                      downloadButton('downloadReportfattriploDIC')
                    )
                    ),

                    conditionalPanel(condition = "input.deli == 'fat3dic'",
                    mainPanel(
                      ######################################################
                      #               Fatorial Triplo em DIC               #
                      ######################################################
                      tabsetPanel(
                        tabPanel("Box Plot",           plotlyOutput("plotar11")),
                        tabPanel("Análise de resíduo",          plotOutput("resid_plot11")),
                        tabPanel("ANAVA",       verbatimTextOutput("anovatable11"))
                      )
                    )
                    )
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()

                ),

                ######################################################
                #           Parcelas Subdivididas em DBC             #
                ######################################################

                tabPanel
                (
                  "O", icon=icon("bars"),
                  sidebarLayout
                  (
                    conditionalPanel(condition = "input.deli == 'psub2dbc'",
                    sidebarPanel
                    (
                      # Input: Seleciona o teste de hipotese ----

                      radioButtons("mcomp12", "Selecione o teste de comparacao múltipla:",
                                   choices = c('Tukey' = "tukey",
                                               'LSB' = "lsd",
                                               'LSDB' = "lsdb",
                                               'Duncan' = "duncan",
                                               'SNK' = "snk",
                                               'Scott-Knott' = "sk",
                                               'Bootstrap' = "ccboot",
                                               'Calinski e Corsten' = "ccf"),
                                   selected = "tukey"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigF12", "Significância a ser adotada pelo teste F: ", "0.05"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigT12", "Significância a ser adotada pelo teste de comparacao múltipla de médias: ", "0.05"),

                      tags$hr(),

                      ######################################################
                      #     botao voltar e seus alinhamentos em html       #
                      ######################################################

                      tags$div(align="left",
                               actionButton("action10", label = "< Voltar", class="btn btn-success", width="50%")
                      ),
                      br(),
                      ######################################################
                      #botao para imprimir valores no formato escolhido DIC#
                      ######################################################


                      radioButtons('format', 'Formato do documento', c(  'HTML', 'DOCX'),
                                   inline = TRUE),
                      downloadButton('downloadReportPARSUBDBC')
                    )
                    ),

                    conditionalPanel(condition = "input.deli == 'psub2dbc'",
                    mainPanel(
                      ######################################################
                      #           Parcelas Subdivididas em DBC             #
                      ######################################################
                      tabsetPanel(
                        tabPanel("Box Plot",           plotlyOutput("plotar12")),
                        tabPanel("Análise de resíduo",          plotOutput("resid_plot12")),
                        tabPanel("ANAVA",       verbatimTextOutput("anovatable12"))
                      )
                    )
                    )
                  ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()

                ),
                
                
                ######################################################
                #           Parcelas Subdivididas em DIC             #
                ######################################################
                tabPanel
                (
                  "P", icon=icon("bars"),
                  sidebarLayout
                  (
                    conditionalPanel(condition = "input.deli == 'psub2dic'",
                    sidebarPanel
                    (
                      # Input: Seleciona o teste de hipotese ----

                      radioButtons("mcomp13", "Selecione o teste de comparacao múltipla:",
                                   choices = c('Tukey' = "tukey",
                                               'LSB' = "lsd",
                                               'LSDB' = "lsdb",
                                               'Duncan' = "duncan",
                                               'SNK' = "snk",
                                               'Scott-Knott' = "sk",
                                               'Bootstrap' = "ccboot",
                                               'Calinski e Corsten' = "ccf"),
                                   selected = "tukey"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigF13", "Significância a ser adotada pelo teste F: ", "0.05"),

                      # Input: Significância a ser adotada pelo teste F ----
                      textInput("sigT13", "Significância a ser adotada pelo teste de comparacao múltipla de médias: ", "0.05"),

                      tags$hr(),

                      ######################################################
                      #     botao voltar e seus alinhamentos em html       #
                      ######################################################

                      tags$div(align="left",
                               actionButton("action10", label = "< Voltar", class="btn btn-sucess", width="50%")
                      ),
                      br(),
                      ######################################################
                      #botao para imprimir valores no formato escolhido    #
                      ######################################################


                      radioButtons('format', 'Formato do documento', c(  'HTML', 'DOCX'),
                                   inline = TRUE),
                      downloadButton('downloadReportPARSUBDIC')
                    )
                ),

                    conditionalPanel(condition = "input.deli == 'psub2dic'",
                    mainPanel(
                      ######################################################
                      #           Parcelas Subdivididas em DIC             #
                      ######################################################
                      tabsetPanel(
                        tabPanel("Box Plot",           plotlyOutput("plotar13")),
                        tabPanel("Análise de resíduo",          plotOutput("resid_plot13")),
                        tabPanel("ANAVA",       verbatimTextOutput("anovatable13"))
                      )
                    )
                 )
                ),
                  # FOOTER CREDITOS
                  hr(),
                  tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                  tags$div( align="center",
                            HTML("Ver. 1.0.1 - 2021")
                  ),
                  br()
              )

        )

)
