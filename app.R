#setwd("C:/Users/User/Desktop/IBICT/observatorio empreendorismo/app 2/Observatorio2")
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(data.table)
library(formattable)
library(DT)
library(readr)
library(readxl)
library(tidyverse)
library(scales)
library(plotly)

Rank <- read_excel("Rank.xlsx")
gem=readRDS("Individual.rds")
atitudes=data.frame(`Atitudes e Percepções`=c("Conhece alguém que iniciou um novo negócio",
                                              "Boas oportunidades para iniciar um negócio na minha área",
                                              "É fácil começar um negócio",
                                              "Pessoalmente, tem as habilidades e conhecimentos",
                                              "Medo do fracasso"))

atitudes1=data.frame(`Atitudes e Percepções`=c("Conhece alguém que iniciou um novo negócio",
                                               "Boas oportunidades para iniciar um negócio na minha área",
                                               "Pessoalmente, tem as habilidades e conhecimentos",
                                               "Medo do fracasso"))
motivos1518=readRDS("motivos1518.rds")
motivos1920=readRDS("motivos1920.rds")
mot=data.frame(motivação=c("Fazer a Diferença no Mundo",
                           "Para construir uma grande riqueza ou uma renda muito alta",
                           "Para continuar uma tradição familiar",
                           "Para ganhar a vida porque os empregos são escassos"))

motivo1=function(ano,sexo1){
  x1=motivos1518%>%
    filter(TEAyy=="Yes",!is.na(TEAMOTIV))%>%
    group_by(TEAMOTIV,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(TEA=round(n/sum(n)*100,2))%>%
    filter( yrsurv %in% ano,gender %in% sexo1)%>%
    mutate(MOTIVO=TEAMOTIV)
  x2=motivos1518%>%
    filter(TEAyy=="Yes",!is.na(SU_MOTIV))%>%
    group_by(SU_MOTIV,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(SU=round(n/sum(n)*100,2))%>%
    filter( yrsurv %in% ano,gender %in% sexo1)%>%
    mutate(MOTIVO=SU_MOTIV)
  x3=motivos1518%>%
    filter(TEAyy=="Yes",!is.na(BB_MOTIV))%>%
    group_by(BB_MOTIV,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(BB=round(n/sum(n)*100,2))%>%
    filter( yrsurv %in% ano,gender %in% sexo1)%>%
    mutate(MOTIVO=BB_MOTIV)
  x4=merge(x1[,c(5,6)], x2[,c(5,6)], by="MOTIVO", all = T)
  x4=merge(x4,x3[,c(5,6)], by="MOTIVO", all = T)
  
  names(motivo)=c("motivação","Iniciais","Nascente","Novos")
  datatable(motivo,
            rownames = FALSE, 
            options = list(pageLength = 10, dom = 't',
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': 'darkblue', 'color':'#E69F00'});",
                             "}")))%>%
    
    formatStyle(columns = "Nascente",target = "cell", backgroundColor = "#B0C4DE")%>%
    formatStyle(columns = "Iniciais", target = "cell", backgroundColor = "#B0C4DE")%>%
    formatStyle(columns = "Novos", target = "cell", backgroundColor = "#B0C4DE")%>%
    formatStyle(columns = "motivação", target = "cell", backgroundColor = "#B0C4DE")
}
motivo2=function(ano,sexo1){
  ano=ano
  x1=motivos1920%>%filter(TEAyy=="Yes",!is.na(SU_yyMOT1yes))%>%
    group_by(SU_yyMOT1yes,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(SU=round(n/sum(n),2))%>%
    filter(SU_yyMOT1yes=="Yes", yrsurv %in% ano,gender %in% sexo1)
  x2=motivos1920%>%filter(TEAyy=="Yes",!is.na(TEAyyMOT1yes))%>%
    group_by(TEAyyMOT1yes,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(TEA=round(n/sum(n),2))%>%
    filter(TEAyyMOT1yes=="Yes", yrsurv %in% ano,gender %in% sexo1)
  x3=motivos1920%>%filter(TEAyy=="Yes",!is.na(BB_yyMOT1yes))%>%
    group_by(BB_yyMOT1yes,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(BB=round(n/sum(n),2))%>%
    filter(BB_yyMOT1yes=="Yes", yrsurv %in% ano,gender %in% sexo1)
  mot1=cbind(x1[,5],x2[,5],x3[,5])
  rm(x1);rm(x2);rm(x3);
  x1=motivos1920%>%filter(TEAyy=="Yes",!is.na(SU_yyMOT2yes))%>%
    group_by(SU_yyMOT2yes,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(SU=round(n/sum(n),2))%>%
    filter(SU_yyMOT2yes=="Yes", yrsurv %in% ano,gender %in% sexo1)
  x2=motivos1920%>%filter(TEAyy=="Yes",!is.na(TEAyyMOT2yes))%>%
    group_by(TEAyyMOT2yes,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(TEA=round(n/sum(n),2))%>%
    filter(TEAyyMOT2yes=="Yes", yrsurv %in% ano,gender %in% sexo1)
  x3=motivos1920%>%filter(TEAyy=="Yes",!is.na(BB_yyMOT2yes))%>%
    group_by(BB_yyMOT2yes,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(BB=round(n/sum(n),2))%>%
    filter(BB_yyMOT2yes=="Yes", yrsurv %in% ano,gender %in% sexo1)
  mot2=cbind(x1[,5],x2[,5],x3[,5])
  rm(x1);rm(x2);rm(x3);
  x1=motivos1920%>%filter(TEAyy=="Yes",!is.na(SU_yyMOT3yes))%>%
    group_by(SU_yyMOT3yes,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(SU=round(n/sum(n),2))%>%
    filter(SU_yyMOT3yes=="Yes", yrsurv %in% ano,gender %in% sexo1)
  x2=motivos1920%>%filter(TEAyy=="Yes",!is.na(TEAyyMOT3yes))%>%
    group_by(TEAyyMOT3yes,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(TEA=round(n/sum(n),2))%>%
    filter(TEAyyMOT3yes=="Yes", yrsurv %in% ano,gender %in% sexo1)
  x3=motivos1920%>%filter(TEAyy=="Yes",!is.na(BB_yyMOT3yes))%>%
    group_by(BB_yyMOT3yes,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(BB=round(n/sum(n),2))%>%
    filter(BB_yyMOT3yes=="Yes", yrsurv %in% ano,gender %in% sexo1)
  mot3=cbind(x1[,5],x2[,5],x3[,5])
  rm(x1);rm(x2);rm(x3);
  x1=motivos1920%>%filter(TEAyy=="Yes",!is.na(SU_yyMOT4yes))%>%
    group_by(SU_yyMOT4yes,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(SU=round(n/sum(n),2))%>%
    filter(SU_yyMOT4yes=="Yes", yrsurv %in% ano,gender %in% sexo1)
  x2=motivos1920%>%filter(TEAyy=="Yes",!is.na(TEAyyMOT4yes))%>%
    group_by(TEAyyMOT4yes,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(TEA=round(n/sum(n),2))%>%
    filter(TEAyyMOT4yes=="Yes", yrsurv %in% ano,gender %in% sexo1)
  x3=motivos1920%>%filter(TEAyy=="Yes",!is.na(BB_yyMOT4yes))%>%
    group_by(BB_yyMOT4yes,yrsurv,gender)%>%
    summarise(n=sum(weight))%>%mutate(BB=round(n/sum(n),2))%>%
    filter(BB_yyMOT4yes=="Yes", yrsurv %in% ano,gender %in% sexo1)
  mot4=cbind(x1[,5],x2[,5],x3[,5])
  
  motivo=cbind(mot,rbind(mot1,mot2,mot3,mot4))
  
  names(motivo)=c("motivação","Nascente","Iniciais","Novos")
  datatable(motivo,
            rownames = FALSE, 
            options = list(pageLength = 10, dom = 't',
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': 'darkblue', 'color':'#E69F00'});",
                             "}")))%>%
    
    formatStyle(columns = "Nascente",target = "cell", backgroundColor = "#B0C4DE")%>%
    formatStyle(columns = "Iniciais", target = "cell", backgroundColor = "#B0C4DE")%>%
    formatStyle(columns = "Novos", target = "cell", backgroundColor = "#B0C4DE")%>%
    formatStyle(columns = "motivação", target = "cell", backgroundColor = "#B0C4DE")
  
}
motivo=function(ano,sexo1){
  if(ano>=2019){
    motivos=motivo2(ano,sexo1)
  } else {
    motivos=motivo1(ano,sexo1)
    
  }
  motivos
}
data<- read_csv("export-nes-2024-07-30_14-06-11.csv", col_names = FALSE, skip = 1)

names(data)=data[1,]
data=data[-1,-1]
names=c("País","Ano",
        "Acesso a Financiamentos",
        "Apoio e políticas governamentais",
        "Impostos e burocracia",
        "Programas governamentais",
        "Educação empreendedora na escola básica",
        "Educação empreendedora pós-escola",
        "Pesquisa e transferência de desenvolvimento",
        "Infraestrutura comercial e profissional",
        "Dinâmica do mercado interno",
        "Abertura do mercado interno",
        "Infraestrutura física e de serviços",
        "Normas culturais e sociais")
names(data)=names


Empresas_ativas=read_excel("Empresas ativas.xlsx", 
                           col_types = c("text", "text", "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", "numeric"))
Pessoal=read_excel("Pessoal Assalariado.xlsx", 
                   col_types = c("text", "text", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric"))
Salario=read_excel("Salario Médio.xlsx", 
                   col_types = c("text", "text", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric"))
idademedia1 <- read_excel("idademedia1.xlsx", 
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric"))
idademedia2 <- read_excel("idademedia2.xlsx", 
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric"))
gazelas <- read_excel("gazelas.xlsx", col_types = c("text", 
                                                    "numeric", "numeric", "numeric"))

idademedia1$socio=idademedia1$Total-idademedia1$Assalariado

Receita <- read_delim("Receita.csv", delim = ";", 
                      escape_double = FALSE, col_types = cols(...1 = col_skip()), 
                      locale = locale(encoding = "ISO-8859-1"), 
                      trim_ws = TRUE)



ui=navbarPage(shinyWidgets::useShinydashboard(),title="OEMFE", header = includeCSS('style.css'),
              #########################TABPNAEL###############################
              tabPanel("GEM",
                       inputPanel(
                         selectInput("ano1","Ano",
                                     choices =sort(unique(gem$yrsurv)),
                                     selected = 2019),
                         pickerInput("sexo1","sexo",options = list(`actions-box` = TRUE),multiple = T,
                                     choices =sort(unique(gem$gender)),selected = "Feminino")),
                       
                       
                       mainPanel(
                         fluidRow(
                           valueBoxOutput(outputId = "box_1",
                                          width = 4),
                           valueBoxOutput(outputId = "box_2",
                                          width = 4),
                           valueBoxOutput(outputId = "box_5",
                                          width = 4))),
                       
                       mainPanel(
                         fluidRow(div(column(6,div(style="width:600px;",plotOutput("plot1"))),style="position: relative;left: 15px;bottom:-35px;"),
                                  div(column(6,div(style="width:600px;",plotOutput("plot2"))),style="position: relative;left: 250px;bottom:-35px;")),
                         
                         fluidRow(div(column(6,div(style="width:600px;",DTOutput("table1"))),style="position: relative;left: -200px;bottom:-50px;"),
                                  div(column(6,div(style="width:600px;position: relative;bottom:-50px;",plotOutput("plot3")))),style="position: relative;left: 250px;bottom:-50px;"),
                         
                         fluidRow(div(column(6,div(style="width:600px;",plotOutput("plot4")),
                                             p("Avaliação das condições do empreendorismo por especialistas",style = "font-size:15px")),
                                      style="position: relative;left: 15px;bottom:-150px;"),
                                  div(column(6,div(style="width:600px;",DTOutput("table2"))),style="position: relative;left: 250px;bottom:-150px;")),
                         
                         fluidRow(div(column(6,div(style="width:600px;",plotlyOutput("plot5"))),style="position: relative;left: 15px;bottom:-150px;"),
                                  div(column(6,div(style="width:600px;",DTOutput("table3"))),style="position: relative;left: 250px;bottom:-150px;")),
                       )
                       
              ),
              ############################### TABPANEL 2##########################################         
              
              tabPanel("Cempre",
                       inputPanel(
                         selectInput("ano2","Ano",choices =sort(unique(Empresas_ativas$Ano)),selected = 2021),
                         selectInput("cnae","CNAE",choices =unique(Empresas_ativas$`Seções da classificação de atividades`)),
                         selectInput("porte","Faixa de Ocupados",
                                     choices =unique(Empresas_ativas$`Faixas de pessoal ocupado assalariado`),selected = "Total")
                       ),
                       mainPanel(
                         
                         valueBoxOutput(outputId = "box_3",width = 6),
                         valueBoxOutput(outputId = "box_4",width = 6),
                         fluidRow(div(column(6,div(style="width:600px;",plotOutput("plot6"))),style="position: relative;left: 15px;bottom:-35px;"),
                                  div(column(6,div(style="width:600px;",plotOutput("plot7"))),style="position: relative;left: 250px;bottom:-35px;")),
                         
                         fluidRow(div(column(6,div(style="width:600px;",plotOutput("plot8"))),style="position: relative;left: 15px;bottom:-35px;"),
                                  div(column(6,div(style="width:600px;",plotOutput("plot9"))),style="position: relative;left: 250px;bottom:-35px;")),
                         
                         fluidRow(div(column(6,div(style="width:600px;",plotOutput("plot10"))),style="position: relative;left: 15px;bottom:-35px;"),
                                  div(column(6,div(style="width:600px;",plotOutput("plot11"))),style="position: relative;left: 250px;bottom:-35px;")),
                         
                         fluidRow(div(column(6,div(style="width:600px;",plotOutput("plot12"))),style="position: relative;left: 15px;bottom:-35px;"),
                                  div(column(6,div(style="width:600px;",plotOutput("plot13"))),style="position: relative;left: 250px;bottom:-35px;")),
                         
                         fluidRow(div(column(6,div(style="width:600px;",plotOutput("plot14"))),style="position: relative;left: 15px;bottom:-35px;"),
                                  div(column(6,div(style="width:600px;",plotOutput("plot15"))),style="position: relative;left: 250px;bottom:-35px;"))
                       )
                       
              ),
              
              ############################TABPANEL 3
              tabPanel("Receita Federal",
                       inputPanel(
                         pickerInput("cnae1","CNAE",choices = sort(unique(Receita$CNAE1)),
                                     options = list(`actions-box` = TRUE),multiple = T,
                                     selected = unique(Receita$CNAE1)),
                         
                         pickerInput("uf3","UF",options = list(`actions-box` = TRUE),multiple = T,
                                     choices= list("Norte" = list("AC","AM","AP","TO","PA","RR","RO"),
                                                   "Nordeste" = list("CE","AL","BA","MA","PB","PE",
                                                                     "PI","RN","SE"),
                                                   "Centro-Oeste" = list("GO","MT","MS","DF"),
                                                   "Sudeste"=list("ES","MG","RJ","SP"),
                                                   "Sul"=list("PR","SC","RS")),selected = unique(Receita$X20)),
                       ),
                       mainPanel(plotOutput("plot16"),
                                 fluidRow(div(column(6,div(style="width:600px;",imageOutput("image1"))),style="position: relative;left: 15px;bottom:-35px;"),
                                          div(column(6,div(style="width:600px;",imageOutput("image2"))),style="position: relative;left: 250px;bottom:-35px;")),
                                 
                       )
              ),
              #############################TABPANEL 3
              tabPanel("World Bank",
                       mainPanel(DTOutput("table5"))
                       
              ),
              #############################TABPANEL 3
              tabPanel("PNAD",
                       h4("Rasunho da PNAD", a("Link:", href = "https://larissamoreno.shinyapps.io/observatorio/"))
              )
)
server <- function(input, output, session) {
  output$box_1 <- renderValueBox({
    razão=gem%>%group_by(gender,yrsurv,ESTBBUSM)%>%
      summarise(n=sum(weight),)%>%mutate(prop=n/sum(n))%>%
      filter(ESTBBUSM=="Yes", yrsurv %in% input$ano1)%>%ungroup()%>%
      select(prop)%>%mutate(razão = round(prop[2L]/prop[1L],2))
    valueBox(as.numeric(razão[1,2]),
             HTML("Razão entre Taxas Estabelecidos<br>de homens e mulheres"),
             color = "green")
  })
  
  output$box_2 <- renderValueBox({
    razão1=gem%>%group_by(gender,age7c,yrsurv,TEAyy)%>%
      summarise(n=sum(weight))%>%mutate(prop=n/sum(n))%>%
      filter(TEAyy=="Yes",age7c=="45-54", yrsurv==2019)%>%ungroup()%>%
      select(prop)%>%mutate(razão = round(prop[2L]/prop[1L],2))
    
    valueBox(as.numeric(razão1[1,2]),
             HTML("Razão entre Taxas Iniciais<br>de homens e mulheres"),
             color = "green")
  })
  output$box_5 <- renderValueBox({
    
    valueBox("?",
             HTML("Coletar Projeção da População"),
             color = "green")
  })
  
  
  react1=reactive({
    x1=gem%>%filter(gender %in% input$sexo1)%>%
      group_by(yrsurv,ESTBBUSM)%>%#Estabelecidos
      summarise(n=sum(weight))%>%mutate(Estabelecidos=n/sum(n)*100)%>%
      filter(ESTBBUSM %in% "Yes")
    
    x2=gem%>%filter(gender %in% input$sexo1)%>%
      group_by( yrsurv,TEAyy)%>%
      summarise(n=sum(weight))%>%mutate(Inicial=n/sum(n)*100)%>%
      filter(TEAyy %in% "Yes")#Inicial
    x3=gem%>%filter(gender %in% input$sexo1)%>%
      group_by( yrsurv,SUBOANW)%>%
      summarise(n=sum(weight))%>%mutate(Nascentes=n/sum(n)*100)%>%
      filter(SUBOANW %in% "Yes")#Nascentes
    x4=gem%>%filter(gender %in% input$sexo1)%>%
      group_by( yrsurv,BABYBUSO)%>%
      summarise(n=sum(weight))%>%mutate(Novos=n/sum(n)*100)%>%
      filter(BABYBUSO %in% "Yes")#Novos
    x5=gem%>%filter(gender %in% input$sexo1)%>%
      group_by( yrsurv,ANYBUSOW)%>%
      summarise(n=sum(weight))%>%mutate(Total=n/sum(n)*100)%>%
      filter(ANYBUSOW %in% "Yes")#TOTAL
    x6=gem%>%filter(gender %in% input$sexo1)%>%
      group_by( yrsurv,FUTSUPyy)%>%
      summarise(n=sum(weight))%>%mutate(Potencial=n/sum(n)*100)%>%
      filter(FUTSUPyy %in% "Yes")#potenciais
    
    data=cbind(x1[,c(1,4)],x2[,4],x3[,4],x4[,4],x5[,4],x6[,4])
    data
    
  })
  
  output$plot1=renderPlot({
    
    ggplot(react1(),aes(x=yrsurv))+
      geom_line(aes(y = Total,colour = "Total")) +
      geom_text(aes(y=Total,label=paste0(round(Total,2),"%")),vjust=-0.3, size=2.8)+
      geom_line(aes(y = Estabelecidos,colour ="Estabelecidos"))+
      geom_text(aes(y=Estabelecidos,label=paste0(round(Estabelecidos,2),"%")),vjust=-0.3, size=2.8)+
      geom_line(aes(y = Inicial,colour ='Inicial'))+
      geom_text(aes(y=Inicial,label=paste0(round(Inicial,2),"%")),vjust=-0.3, size=2.8)+
      ggtitle("Taxas de Empreendorismo Total, Estabelecidos e Inicial")+
      scale_colour_manual(values = c("red","blue","orange"))+
      guides(colour=guide_legend(title=" "))
    
    
  })
  output$plot2=renderPlot({
    ggplot(react1(),aes(x=yrsurv))+
      geom_line(aes(y = Nascentes,colour = "Nascentes")) +
      geom_text(aes(y=Nascentes,label=paste0(round(Nascentes,2),"%")),vjust=-0.3, size=2.8)+
      geom_line(aes(y = Novos,colour ="Novos"))+
      geom_text(aes(y=Novos,label=paste0(round(Novos,2),"%")),vjust=-0.3, size=2.8)+
      geom_line(aes(y = Potencial,colour ='Potenciais'))+
      geom_text(aes(y=Potencial,label=paste0(round(Potencial,2),"%")),vjust=-0.3, size=2.8)+
      ggtitle("Taxas de Empreendorismo Total, Estabelecidos e Inicial")+
      scale_colour_manual(values = c("red","blue","orange"))+
      guides(colour=guide_legend(title=" "))
  })
  output$plot3=renderPlot({
    informal=gem%>%filter(yrsurv %in% input$ano1,gender %in% input$sexo1)%>%
      group_by(BUSANGVL)%>%
      summarise(total=sum(weight))%>%mutate(prop=total/sum(total))
    ggplot(informal,aes(x="",y=prop,fill=BUSANGVL))+
      geom_bar(stat="identity",width=1)+
      coord_polar("y", start=0)+
      geom_text(aes(y=prop,label=paste0(round(100*prop,2),"%")), position = position_stack(vjust = .5))+
      scale_fill_manual(values=c("blue",'#E69F00'))+
      ggtitle("Proporção de Informais")+
      theme_void()+theme(legend.text=element_text(size=12))+
      guides(fill=guide_legend(title=" "))
  })
  output$plot4=renderPlot({
    saida=gem%>%filter(yrsurv %in% input$ano1,gender %in% input$sexo1)%>%
      group_by(EXIT_RS)%>%filter(!is.na(EXIT_RS))%>%
      summarise(total=sum(weight))%>%mutate(prop=total/sum(total)*100)
    
    ggplot(saida,aes(x=EXIT_RS,y=prop,fill=EXIT_RS))+ geom_bar(stat = "identity") +
      coord_flip()+
      xlab("")+ylab("Porcentagem")+
      geom_text(aes(label=paste0(round(prop,2),"%")), vjust=1.6,
                position = position_dodge(0.9), size=3)+
      scale_fill_manual(values=c("#0047e6",'#E69F00',"#3472ff","#ffd067",
                                 "#002f9a","#9a6a00","#00184e","#ffc034",
                                 "#81a7ff","#cd8d00"),name="")+
      theme_minimal()+
      ggtitle("Porcentagem das razões da saída do negócio")+
      theme(legend.position = "none")
  })  
  react3=reactive({
    if(input$ano1 %in% c(2015,2019,2020)){
      x1=gem%>%filter(!is.na(KNOWENyy),yrsurv %in% input$ano1,gender %in% input$sexo1)%>%
        group_by(KNOWENyy)%>%
        summarise(n=sum(weight))%>%mutate(prop=n/sum(n)*100)%>%
        filter(KNOWENyy=="Yes")
      x2=gem%>%filter(!is.na(OPPORTyy),yrsurv %in% input$ano1,gender %in% input$sexo1)%>%
        group_by(OPPORTyy)%>%
        summarise(n=sum(weight))%>%mutate(prop=n/sum(n)*100)%>%
        filter(OPPORTyy=="Yes")
      x3=gem%>%filter(!is.na(EASYSTyy),yrsurv %in% input$ano1,gender %in% input$sexo1)%>%
        group_by(EASYSTyy)%>%
        summarise(n=sum(weight))%>%mutate(prop=n/sum(n)*100)%>%
        filter(EASYSTyy=="Yes")
      x4=gem%>%filter(!is.na(SUSKILyy),yrsurv %in% input$ano1,gender %in% input$sexo1)%>%
        group_by(SUSKILyy)%>%
        summarise(n=sum(weight))%>%mutate(prop=n/sum(n)*100)%>%
        filter(SUSKILyy=="Yes")
      x5=gem%>%filter(!is.na(FRFAILOP),yrsurv %in% input$ano1,gender %in% input$sexo1)%>%
        group_by(FRFAILOP)%>%
        summarise(n=sum(weight))%>%mutate(prop=n/sum(n)*100)%>%
        filter(FRFAILOP=="Yes")
      x=rbind(x1[,3],x2[,3],x3[,3],x4[,3],x5[,3])
      x=x%>%mutate_if(is.numeric, ~round(.,2))
      data=cbind(atitudes,x)
      names(data)=c("Atitudes e Percepções","% Adultos")
      
      datatable(data,
                rownames = FALSE, 
                options = list(pageLength = 10, dom = 't',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': 'darkblue', 'color':'#E69F00'});",
                                 "}")))%>%
        
        formatStyle(columns = "Atitudes e Percepções",target = "cell", backgroundColor = "#B0C4DE")%>%
        formatStyle(columns = "% Adultos", target = "cell", backgroundColor = "#B0C4DE")  
    }else{
      x1=gem%>%filter(!is.na(KNOWENyy),yrsurv %in% input$ano1,gender %in% input$sexo1)%>%
        group_by(KNOWENyy)%>%
        summarise(n=sum(weight))%>%mutate(prop=n/sum(n)*100)%>%
        filter(KNOWENyy=="Yes")
      x2=gem%>%filter(!is.na(OPPORTyy),yrsurv %in% input$ano1,gender %in% input$sexo1)%>%
        group_by(OPPORTyy)%>%
        summarise(n=sum(weight))%>%mutate(prop=n/sum(n)*100)%>%
        filter(OPPORTyy=="Yes")
      x4=gem%>%filter(!is.na(SUSKILyy),yrsurv %in% input$ano1,gender %in% input$sexo1)%>%
        group_by(SUSKILyy)%>%
        summarise(n=sum(weight))%>%mutate(prop=n/sum(n)*100)%>%
        filter(SUSKILyy=="Yes")
      x5=gem%>%filter(!is.na(FRFAILOP),yrsurv %in% input$ano1,gender %in% input$sexo1)%>%
        group_by(FRFAILOP)%>%
        summarise(n=sum(weight))%>%mutate(prop=n/sum(n)*100)%>%
        filter(FRFAILOP=="Yes")
      x=rbind(x1[,3],x2[,3],x4[,3],x5[,3])
      x=x%>%mutate_if(is.numeric, ~round(.,2))
      data=cbind(atitudes1,x)
      names(data)=c("Atitudes e Percepções","% Adultos")
      
      datatable(data,
                rownames = FALSE, 
                options = list(pageLength = 10, dom = 't',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': 'darkblue', 'color':'#E69F00'});",
                                 "}")))%>%
        
        formatStyle(columns = "Atitudes e Percepções",target = "cell", backgroundColor = "#B0C4DE")%>%
        formatStyle(columns = "% Adultos", target = "cell", backgroundColor = "#B0C4DE")
    }
    
  })
  
  
  output$table1=renderDT({
    react3()
    
  })
  react2=reactive({
    motivo(ano=input$ano1,sexo1=input$sexo1)  
    
  })
  output$table2=renderDT({
    react2()
  })
  output$plot5=renderPlotly({
    
    teste=data%>%filter(País=="Brazil",Ano %in% input$ano1)
    teste=teste[3:14]
    teste=rbind(rep(10,12),rep(0,12),teste) #max=10 , min=12
    teste=teste%>%mutate_if(is.character,~as.numeric(.))
    x=as.numeric(teste[3,])
    y=as.character(colnames(teste))
    
    fig <- plot_ly(type = 'scatterpolar', r = x,theta =y,fill = 'toself') 
    fig <- fig %>%layout(polar = list(radialaxis = list(visible = T,range = c(0,10))),showlegend = F)
    
    fig
    
  })  
  output$table3=renderDT({
    data1=data%>%filter(Ano %in% input$ano1)
    data1$rank1=paste0(rank(data1$`Acesso a Financiamentos`),"/",nrow(data1))
    data1$rank2=paste0(rank(data1$`Apoio e políticas governamentais`),"/",nrow(data1))
    data1$rank3=paste0(rank(data1$`Impostos e burocracia`),"/",nrow(data1))
    data1$rank4=paste0(rank(data1$`Programas governamentais`),"/",nrow(data1))
    data1$rank5=paste0(rank(data1$`Educação empreendedora na escola básica`),"/",nrow(data1))
    data1$rank6=paste0(rank(data1$`Educação empreendedora pós-escola`),"/",nrow(data1))
    data1$rank7=paste0(rank(data1$`Pesquisa e transferência de desenvolvimento`),"/",nrow(data1))
    data1$rank8=paste0(rank(data1$`Infraestrutura comercial e profissional`),"/",nrow(data1))
    data1$rank9=paste0(rank(data1$`Dinâmica do mercado interno`),"/",nrow(data1))
    data1$rank10=paste0(rank(data1$`Abertura do mercado interno`),"/",nrow(data1))
    data1$rank11=paste0(rank(data1$`Infraestrutura física e de serviços`),"/",nrow(data1))
    data1$rank12=paste0(rank(data1$`Normas culturais e sociais`),"/",nrow(data1))
    teste2=data1%>%filter(País=="Brazil")
    teste2=teste2[,-c(1,2)]
    rank=teste2[,13:24]
    rank=as.data.frame(t(rank))
    rownames(rank)=colnames(teste2[1:12])
    y=as.data.frame(rownames(rank))
    rank=cbind(y,rank)
    names(rank)=c("Razões da Saída","Rank")
    
    datatable(rank,
              rownames = FALSE, 
              options = list(pageLength = 10, dom = 't',
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': 'darkblue', 'color':'#E69F00'});",
                               "}")))%>%
      
      formatStyle(columns = "Rank", target = "cell", backgroundColor = "#B0C4DE")%>%
      formatStyle(columns = "Razões da Saída", target = "cell", backgroundColor = "#B0C4DE")
  })
  
  output$plot6=renderPlot({
    x=Empresas_ativas%>%
      filter(`Seções da classificação de atividades` %in% input$cnae,
             `Faixas de pessoal ocupado assalariado` %in% input$porte)%>%
      group_by(Ano)%>%
      summarise(tx.entrada=sum(Entradas,na.rm=T)/sum(`Empresas Ativas`,na.rm = T)*100,
                tx.saida=sum(Saídas,na.rm=T)/sum(`Empresas Ativas`,na.rm = T)*100)
    
    ggplot(x,aes(x=Ano))+
      geom_line(aes(y = tx.entrada,colour = "Taxa de Entrada")) +
      geom_text(aes(y=tx.entrada,label=paste0(round(tx.entrada,2),"%")),vjust=-0.3, size=2.8)+
      geom_line(aes(y = tx.saida,colour ="Taxa de Saída"))+
      geom_text(aes(y=tx.saida,label=paste0(round(tx.saida,2),"%")),vjust=-0.3, size=2.8)+
      ggtitle("Taxas de Entrada e Saída de Empresas")+
      scale_colour_manual(values = c("blue","red"))+
      guides(colour=guide_legend(title=" "))+
      labs(y="")
    
  })
  output$plot7=renderPlot({
    x=Empresas_ativas%>%
      filter(`Seções da classificação de atividades` %in% input$cnae,
             `Faixas de pessoal ocupado assalariado` %in% input$porte)%>%
      group_by(Ano)%>%
      summarise(tx.sobrevivencia=sum(Sobreviventes,na.rm=T)/sum(`Empresas Ativas`,na.rm = T)*100)
    
    ggplot(x,aes(x=Ano))+
      geom_line(aes(y =tx.sobrevivencia),colour="orange") +
      geom_text(aes(y=tx.sobrevivencia,label=paste0(round(tx.sobrevivencia,2),"%")),vjust=-0.3, size=2.8)+
      scale_colour_manual(values = c("orange"))+
      ggtitle("Taxas de Sobrevivência das Empresas")+
      guides(colour=guide_legend(title=" "))+
      labs(y="")
  })
  output$plot8=renderPlot({
    
    x=Empresas_ativas%>%
      filter(`Seções da classificação de atividades` %in% input$cnae,
             `Faixas de pessoal ocupado assalariado` %in% input$porte)%>%
      group_by(Ano)%>%
      summarise(saldo=Entradas-Saídas)
    ggplot(x,aes(x=Ano))+
      geom_line(aes(y =saldo),colour="orange") +
      geom_text(aes(y=saldo,label=saldo),vjust=-0.3, size=2.8)+
      geom_hline(yintercept = 0)+
      scale_colour_manual(values = c("orange"))+
      ggtitle("Saldo Empresas=Entradas-Saidas")+
      guides(colour=guide_legend(title=" "))+
      labs(y="")
  })
  output$box_3 <- renderValueBox({
    x=Empresas_ativas%>%
      filter(`Seções da classificação de atividades` %in% input$cnae,
             `Faixas de pessoal ocupado assalariado` %in% input$porte,
             Ano %in% input$ano2)%>%
      summarise(saldo=paste0(round(sum(Nascimentos,na.rm = T)/sum(`Empresas Ativas`,na.rm=T)*100,2),"%"))
    valueBox(x[[1]],"Percentual de Empresas Nascentes",
             color = "green")
  })
  output$box_4 <- renderValueBox({
    x=Salario%>%filter(`Seções da classificação de atividades` %in% input$cnae,
                       `Faixas de pessoal ocupado assalariado` %in% input$porte,
                       ano %in% input$ano2)%>%
      mutate(salario.medio=ifelse(is.na(Total),NA,Total))%>%select(salario.medio)
    valueBox(round(x[[1]],2),"Número médio de Salário Mínimos do Assalariado",
             color = "green")
  })
  output$plot9=renderPlot({
    x=idademedia1%>%
      filter(`Seções da classificação de atividades`%in% input$cnae)%>%
      group_by(ano)%>%
      summarise(idade.media=`Média de idade das empresas (anos) (2)`)
    
    ggplot(x,aes(x=ano))+
      geom_line(aes(y =idade.media),colour="orange") +
      geom_text(aes(y=idade.media,label=idade.media),vjust=-0.3, size=2.8)+
      scale_colour_manual(values = c("orange"))+
      ggtitle("Idade Média das Empresas")+
      guides(colour=guide_legend(title=" "))+
      labs(y="")
  })
  output$plot10=renderPlot({
    x=idademedia1%>%
      filter(`Seções da classificação de atividades`%in% input$cnae,
             ano %in% input$ano2)%>%
      summarise(Sócios=round(socio/Total*100,2),
                Assalariados=round(Assalariado/Total*100,2))%>%pivot_longer(cols = names(.))
    ggplot(x,aes(x="",y=value,fill=name))+
      geom_bar(stat="identity",width=1)+
      coord_polar("y", start=0)+
      geom_text(aes(y=value,label=paste0(value,"%")), position = position_stack(vjust = .5))+
      scale_fill_manual(values=c("blue",'#E69F00'))+
      ggtitle("Proporção de Sócios e Assalariados")+
      theme_void()+theme(legend.text=element_text(size=12))+
      guides(fill=guide_legend(title=" "))
  })
  output$plot11=renderPlot({
    x=idademedia2%>%filter(ano %in% input$ano2)%>%
      mutate(prop=round(100*`Número de empresas`/sum(`Número de empresas`),2))
    
    ggplot(x,aes(x=reorder(`Faixas de pessoal ocupado total`, -prop),
                 y=prop,
                 fill=`Faixas de pessoal ocupado total`))+ geom_bar(stat = "identity") +
      coord_flip()+
      xlab("")+ylab("Porcentagem")+
      geom_text(aes(label=paste0(prop,"%")), vjust=1.6,
                position = position_dodge(0.9), size=3)+
      scale_fill_manual(values=c("#0047e6",'#E69F00',"#3472ff","#ffd067",
                                 "#002f9a","#9a6a00","#00184e","#ffc034",
                                 "#81a7ff","#cd8d00"),name="")+
      theme_minimal()+
      ggtitle("Porcentagem de empresas segundo Faixa de pessoal ocupado")+
      theme(legend.position = "none")
  })
  output$plot12=renderPlot({
    x=idademedia2%>%filter(ano %in% input$ano2)%>%
      mutate(idade.média=`Média de idade das empresas (anos) (2)`)
    
    ggplot(x,aes(x=reorder(`Faixas de pessoal ocupado total`, -idade.média),
                 y=idade.média,
                 fill=`Faixas de pessoal ocupado total`))+ geom_bar(stat = "identity") +
      xlab("")+ylab("Porcentagem")+
      geom_text(aes(label=paste0(idade.média," anos")), vjust=1.6,
                position = position_dodge(0.9), size=3)+
      scale_fill_manual(values=c("#0047e6",'#E69F00',"#3472ff","#ffd067",
                                 "#002f9a","#9a6a00","#00184e","#ffc034",
                                 "#81a7ff","#cd8d00"),name="")+
      theme_minimal()+
      ggtitle("Idade média segundo Faixa de pessoal ocupado")+
      theme(legend.position = "none")
    
  })
  
  output$plot13=renderPlot({
    ggplot(gazelas,aes(x=ano))+
      geom_line(aes(y =`Empresas gazelas`),colour="orange") +
      geom_text(aes(y=`Empresas gazelas`,label=`Empresas gazelas`),vjust=-0.3, size=2.8)+
      scale_colour_manual(values = c("orange"))+
      ggtitle("Número de Empresas Gazelas")+
      guides(colour=guide_legend(title=" "))+
      labs(y="")
    
  })
  output$plot14=renderPlot({
    
    x=Pessoal%>%filter(`Seções da classificação de atividades` %in% input$cnae,
                       `Faixas de pessoal ocupado assalariado` %in% input$porte)%>%
      group_by(Ano)%>%
      summarise(saldo=Entradas-Saídas)
    ggplot(x,aes(x=Ano))+
      geom_line(aes(y =saldo),colour="orange") +
      geom_text(aes(y=saldo,label=saldo),vjust=-0.3, size=2.8)+
      scale_colour_manual(values = c("orange"))+
      ggtitle("Saldo Pessoal Assalariado=Entradas-Saidas")+
      guides(colour=guide_legend(title=" "))+
      labs(y="")+
      scale_y_continuous(labels = unit_format(unit = "mil", scale = 1e-3))
    
  })
  output$plot15=renderPlot({
    
    
    x=Pessoal%>%
      filter(`Seções da classificação de atividades` %in% input$cnae,
             `Faixas de pessoal ocupado assalariado` %in% input$porte)%>%
      group_by(Ano)%>%
      summarise(tx.entrada=sum(Entradas,na.rm=T)/sum(`ocupado assalariado`,na.rm = T)*100,
                tx.saida=sum(Saídas,na.rm=T)/sum(`ocupado assalariado`,na.rm = T)*100)
    
    ggplot(x,aes(x=Ano))+
      geom_line(aes(y = tx.entrada,colour = "Taxa de Entrada")) +
      geom_text(aes(y=tx.entrada,label=paste0(round(tx.entrada,2),"%")),vjust=-0.3, size=2.8)+
      geom_line(aes(y = tx.saida,colour ="Taxa de Saída"))+
      geom_text(aes(y=tx.saida,label=paste0(round(tx.saida,2),"%")),vjust=-0.3, size=2.8)+
      ggtitle("Taxas de Entrada e Saída de Pessoal Assalariado")+
      scale_colour_manual(values = c("blue","red"))+
      guides(colour=guide_legend(title=" "))+
      labs(y="")
    
  })
  output$plot16=renderPlot({
    x=Receita%>%filter(CNAE1 %in% input$cnae1,
                       X20 %in% input$uf3)%>%
      group_by(ano)%>%summarise(n=sum(n))
    ggplot(x,aes(ano,n))+geom_line(aes(y =n),colour="orange") +labs(y=" ")+
      ggtitle("Número de Estabelecimentos Sede")
    
  })
  output$image1=renderImage({
    list(src="CapitalSocial.png",contentType="CapitalSocial/png")
  },deleteFile = F)
  
  output$image2=renderImage({
    list(src="Porte Empresa.png",contentType="Porte Empresa/png")
  },deleteFile = F)
  output$table5=renderDT({
    Rank
  })
}

shinyApp(ui, server)




