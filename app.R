


# import libraries -----
library(shiny)
library(shinyjs)
library(bslib)
library(plotly)
library(ggplot2)
library(ggcorrplot)
library(tidyr)
library(dplyr)
library(DT)
#library(gridExtra)



# Define UI  ----
ui <- page_sidebar(
  useShinyjs(),  # Set up shinyjs
  
  title = "Repax Burger (|)",  #set title
  
  sidebar = sidebar(
    selectInput("mode", "modalità: ", 
                c("panoramica serie",
                  "dati grezzi", 
                  "istruzioni",
                  "intervalli",
                  "correlazioni", 
                  "regressione", 
                  "previsione", 
                  "prezzo ottimo")),
    
    selectInput("opzioniPiatti", 'trova prezzo per piatto', choices = NULL),    
    sliderInput("costo", "costo unitario del piatto:",
                min = 0, max = 5, step=0.1, value = 1),    
    
    
    selectInput("cormode", "tipo di correlazione ", c("pearson", "spearman")),
    
    selectInput("dtab", "vedi ", c("distribuzione", "conf-intervals")),
    sliderInput("PVinput", "confidence interval (IC) level:",
                min = 0.001, max = 0.5, step=0.001, value = 0.05),
    
    sliderInput("ninput", "Number of numeric input variables:",
                min = 1, max = 3, step=1, value = 1),
    
    uiOutput("inputOptionN"),
    
    sliderInput("dinput", "Number of binary input variables:",
                min = 0, max = 10, step=1, value = 0),
    
    uiOutput("inputOptionD"),
    uiOutput("inputOptionN1"),
    uiOutput("inputOptionD1"),
    
    checkboxInput("filtriP", "filtro piatti", FALSE),
    checkboxInput("filtriA", "filtro anni", FALSE),
    checkboxInput("filtriG", "filtro giorni", FALSE),
    checkboxInput("selectall", "seleziona tutti", FALSE),
    checkboxGroupInput("filtriPiatti","filtra dataset per piatti",c()),
    checkboxGroupInput("filtriAnni","filtra dataset per anni",c()),
    checkboxGroupInput("filtriGiorni","filtra dataset per giorni feriali",c()),
    
    checkboxInput("contorni", "solo contorni", FALSE),
    checkboxInput("panini", "solo panini", FALSE)
    
  ),
  
  card(id='card_serie', 
       card_header(class = "bg-dark",
                   "manuale d'uso per REPAX BURGER"),
       
       card_body(
         fill=F,
         card_title("A cosa serve?"),
         markdown("
         La funzione di quest'app è quella di osservare e analizzare rapidamente i
         dati della paninoteca dell'oratorio Regina Pacis, al fine di: 
         <br>-prevedere le quantità vendute, in totale e giorno per giorno, di ogni piatto
         <br>-fissare i prezzi dei panini
         <br>-capire quali panini rendono di più, e quali non rendono affatto
         (anche considerando il costo degli ingredienti e il profitto finale per piatto)
         <br><br>
         Il menù a tendina 'modalità' consente di passare per le diverse funzionalità.
         Le opzioni 'panoramica serie' e 'dati grezzi' servono per guardare rapidamente
         il dati raccolti in Repax dal 2021 al 2023 (in una settimana di ottobre dal lunedì al venerdì).
         L'opzione 'intervalli' è forse la più utile, perché consente di stimare il range
         delle vendite/quantità vendute di **un piatto** in **una sera** (la variabile Q del dataset) in modo molto affidabile.
         Le tre opzioni 'regressione', 'previsione' e 'prezzo ottimo' sono collegate dal filo conduttore
         della regressione lineare, che è un modello utile per prevedere Q
         in una sera dato il prezzo, il piatto e il giorno, e correggere anche i dati distorti
         da situazioni eccezionali (come il covid nel 2021, che ha portato vendite inferiori agli standard).
         <br>La modalità 'prezzo ottimo' utilizza il modello definito in 'regressione' per poi stimare i ricavi
         e i profitti complessivi di una settimana.
         <br>La modalità 'correlazioni' può servire a decidere quali piatti si vendono bene insieme (come le birre e le salamelle),
         e quali invece si fanno concorrenza a vicenda (come i nuggets e i panzerotti).
           ")
       ),       
       
       
       
       card_body(
         fill=F,
         card_title("Info Utili"),
         markdown("
         Altri dati su questo lavoro si possono trovare su [GitHub](https://github.com/Fede-Rausa/RepaxDatiPaninoteca)
         <br>
         La matematica e la logica di 'prezzo ottimo' è spiegata in questo [paper](https://github.com/Fede-Rausa/RepaxDatiPaninoteca/blob/main/PaniniProfitti.pdf)
         <br>
         L'app importa e analizza il seguente [dataset csv](https://raw.githubusercontent.com/Fede-Rausa/RepaxDatiPaninoteca/main/datiVendite.csv)
           che si può anche aprire su Excel, GS, Rstudio e Python.")
       ),
       
       
       card_body(
         fill=F,
         card_title("Dati Grezzi"),
         markdown("Si tratta semplicemente del dataset che viene elaborato dall'app.
         <br>Un dataset è una tabella avente sulle colonne le variabili (come il prezzo dei panini) 
         e sulle righe le osservazioni (ad esempio la quantità venduta di patatine fuori dal menu il giovedì sera del 2023).
         <br>Alcune variabili sono dummy, ovvero binarie 0-1, e indicano se un certo evento è accaduto (1) oppure no (0).
         Ad esempio 'mar' è 1 il martedì, 0 altrimenti.
         <br>Le dummy si possono dividere in 4 gruppi:
         <br>- dummy dei giorni (lun-ven)
         <br>- dummy degli anni (2021-2023)
         <br>- dummy dei piatti
         <br>- le dummy menu e contorno, che dicono se il piatto aveva lo sconto menu e se era un contorno 
         (ad esempio le piadine alla nutella sono un contorno ma non sempre sono nel menu)
         
         <br> Poi ci sono le variabili continue (numeri qualsiasi, in questo caso sempre positivi):
         <br>- Q: vendite/quantità venduta (variabile y da prevedere nella regressione)
         <br>- P: prezzo (var indipendente)
         <br>- ggn: giorno, da 1 a 5 (da lun a ven) (var indipendente)
         <br>- annon: anno, da 1 a 3 (2021-2023)  (var indipendente)
           ")
       ),
       
       card_body(
         fill=F,
         card_title("Panoramica Serie"),
         markdown("Consente di osservare l'evoluzione temporale
         delle quantità vendute di ciascun piatto, su tre anni.
         <br>
         La variabile tempo non è proporzionale con il tempo
         relamente trascorso, serve solo a visualizzare facilmente la serie.
         <br> tale variabile è il risultato di questa formula:"),
         withMathJax("$$tempo = ggn + annon*7$$"),
         markdown("
         dove ggn e annon sono descritte sopra. annon viene moltiplicato per 7 
         perché si assume che il numero massimo di serate di paninoteca corrisponde alla durata della settimana (negli anni può cambiare, ma resta sotto tale soglia).
         <br>
         Per controllare le date, i nomi dei piatti, il prezzo di vendita,
         se sono contorni scontati nel menu o meno,
         passarci sopra col mouse.
         <br>
         <br>Per rimuovere o aggiungere una serie, cliccare una volta sul rispettivo nome a lato. 
         <br>Per selezionare o deselezionare tutte le serie tranne una, fare doppio click sul suo nome a lato.
         <br>Con gli strumenti in alto puoi anche zoomare o scaricare il grafico.
           ")
       ),
       
       card_body(
         fill=F,
         card_title("Intervalli"),
         markdown("
    Questa funzione può sembrare molto tecnica e noiosa, ma è la più utile se si ha la pazienza per capirla.
    Presenta un istogramma di frequenza, che misura quanto è probabile che la variabile Q assuma un certo valore.
    <br> puoi scegliere quali valori di Q osservare, utilizzando i filtri (per piatti, giorni, anni) sulla sidebar.
    In sintesi, presenta la distribuzione marginale di probabilità della variabile Q.
    Se non sai (come è facile che sia) cosa sia una distribuzione di probabilità, cerca di capirlo su Internet.
    <br>Oltre all'istogramma si presentano:
    <br>- le statistiche descrittive di Q (media, deviazione standard e i tre quantili/percentili
    (i valori di Q minimi tra i più alti, rispettivamente, del 25%/50%/75% delle osservazioni di Q))
    <br>- gli intervalli di confidenza (ovvero i range entro cui cade più probabilmente Q) basati su tre diverse funzioni
    di densità di probabilità (la densità kernel è molto larga, la poisson molto stretta, la normale media)
    
    <br>Una 'funzione di densità' è una cosa complicata, associata alla distribuzione di probabilità. Ogni distribuzione ha la sua densità.
    In modo poco ortodosso, si può interpretare come la funzione che, dato un valore di Q, e date la media e la deviazione standard di Q, dice quanto è probabile che Q assuma quel valore.
    Comunque puoi cercare di capire su Internet anche questo.
    <br>La ragione per cui si sono scelte le densità kernel, Poisson e Normale sono le seguenti:
    <br>- la densità kernel non è associata a nessuna distribuzione di probabilità, e può essere utilizzata sempre.
    <br>- la densità Normale, per il Teorema del Limite Centrale di Gauss, può descrivere qualsiasi fenomeno in natura.
    <br>- la densità Poisson, definita per valori numerici interi positivi (come i volumi di vendita), è la più adatta a descrivere l'andamento di Q, e il grafico lo conferma.
           
    <br> Infine, è possibile allargare o ridurre gli intervalli di confidenza (delle tre densità), 
    riducendo o aumentando il 'livello di confidenza', che si può in modo poco ortodosso interpretare come la (massima) 
    probabilità che Q cada fuori dal range. Di default la probabilità che Q cada nel range è del 95%, e quindi il livello di confidenza è del 5%.       
             ")
       ),
       
       card_body(
         fill=F,
         card_title("Correlazioni"),
         markdown(" 
             Correlazione delle quantità vendute dei diversi piatti.<br> 
             E' utile se si vuole capire quali piatti si fanno concorrenza 
             a vicenda (beni sostituti) e quali invece si vendono insieme (beni complementi). 
             <br>La correlazione è un valore che va da -1 
             (dipendenza negativa, ovvero trade-off: al crescere di una variabile decresce l'altra) 
             a 1 (dipendenza positiva, ovvero crescono insieme), 
             e in valore assoluto (modulo) da 0 (totale indipendenza) 
             a 1 (massima dipendenza).
             <br>Una differenza significativa tra la correlazione di Pearson 
             (solo lineare) e la correlazione di Spearman 
             (in genere più alta in modulo) 
             può significare che esiste una relazione non lineare 
             (quindi poliniomiale, esponenziale o logaritmica) 
             tra le variabili correlate. 
             Attenzione: le correlazioni NA sono state sostituite da 0 
             (si assumono indipendenti ma in realtà non si sono 
             mai visti insieme).
             ")
       ),
       
       card_body(
         fill=F,
         card_title("Regressione"),
         markdown("Un modello di regressione lineare è una funzione del tipo:"),
         
         withMathJax('$$y = b_0 + b_1*x_1 + b_2*x_2 + ... + b_k*x_k$$'),
         
         
         markdown("Dove nel nostro caso y è Q (vogliamo prevedere la quantità venduta)
             , le diverse x sono le altre variabili (dummy o continue, tra cui il prezzo P),
             e i b sono parametri stimati dal modello che azzerano la **media** degli errori.
             <br> la regressione lineare può diventare polinomiale se si aggiungono
             uno o più regressori elevati a una certa potenza (continui, in quanto le dummy (0 o 1) non cambiano con le potenze), come ad esempio il modello:"),
         withMathJax('$$y = b_0 + b_1*x_1 + b_2*x_1^2 + b_3*x_2$$'),
         
         markdown("
             Sostanzialmente, in questa modalità noi scegliamo quali regressori usare
             per costruire la regressione. 
             <br> Come verificare se il modello funziona bene? Vi sono vari modi:
             <br>- controlla il coefficiente del regressore P (prezzo): se è positivo, significa che il tuo
             modello ritiene che al crescere del prezzo crescano le vendite, cosa del tutto falsa.
             <br>- controlla il MAE (mean absolute error): se sembra alto, non va bene.
             <br>- controlla i p-value dei test t sui coefficienti della regressione:
             questi p-value rappresentano (in modo poco ortodosso) la (massima) probabilità che il coefficiente b associato
             sia pari a 0, e quindi sono tanto più alti quanto peggiore è la scelta di quella variabile come regressore 
             (valori accettabili dei p-value sui test t sono minori di 0.1)
             <br>- controlla la mediana (median) dei residui (errori fatti dal modello su tutte le osservazioni di Q):
             se la mediana non è vicina a 0 si può fare di meglio.
             
             <br> Ho verificato che il mio modello funziona male. Come costruisco una regressione migliore? Vi sono 2 modi:
             <br>1 - puoi filtrare il dataset con dati diversi (usando i filtri nella sidebar)
             <br>2 - puoi cambiare/aggiungere regressori (buona scelta sono P, le dummy dei giorni e year2021)
             ")
       ),
       
       card_body(
         fill=F,
         card_title("Previsione"),
         markdown("Consente semplicemente di calcolare le previsioni di Q con il modello di regressione scelto.
             Fa anche un bel grafico in cui si vede quanto è buono l'adattamento ai dati della retta di regressione. (variando solo P sulle ascisse)"
         )
       ), 
       
       card_body(
         fill=F,
         card_title("Esempio su come fare una buona regressione"),
         markdown("
    Poniamo ad esempio di voler prevedere la quantità venduta di salamelle.
    Se usiamo tutti i dati, e cerchiamo di prevederle in funzione del solo prezzo, avremo un pessimo modello,
    con un coefficiente del prezzo positivo. In questo caso possiamo utilizzare due tipi di soluzioni:
    
    <br>- soluzione easy: vado nei filtri, e rimuovo tutti i piatti diversi dalle salamelle. Il modello con il solo prezzo è buono.
    
    <br>-soluzione pro: vado nei filtri, e rimuovo tutti i piatti tranne salamella, cotto, vegeta e speck (sono prodotti che, almeno in teoria, dovrebbero essere simili dal punto di vista dei clienti). Poi aggiungo come regressori le seguenti dummy: 
    il piatto salamella (che viene venduto generalmente di più), quattro dummy sui giorni (da mar a ven), la dummy year2021 (che è stato un anno più basso).
    Il modello che ne esce è fantastico.
             ")
       ),   
       
       card_body(
         fill=F,
         card_title("Prezzo Ottimo"),
         markdown("
                 
             
             Ultima ma prima per importanza, questa modalità ci consente di stimare
             il bilancio complessivo di un certo piatto. 
             <br>A fronte del modello di regressione stimato, e del costo unitario fissato dall'utente (il costo in € 
             che devi sostenere per cucinare una singola porzione di quel piatto), si utilizza un ottimizzatore numerico
             per massimizzare il profitto complessivo su tutti i giorni presenti nel dataset (si possono rimuovere alcuni giorni utilizzando i soliti filtri sulla sidebar).
             L'ottimizzatore numerico cerca di fissare un prezzo ottimo nel range da 0.1€ a 10€.
             <br>Di solito, se la soluzione trovata è circa ~ 9.99€
             significa che il modello ha stimato un coefficiente di regressione b per P (prezzo) positivo (per cui prevede che al crescere del prezzo aumentano le vendite).
             <br>è quindi ragionevole, in questo caso, modificare il modello di regressione o filtrare il dataset, finché il coefficente di P non diventa negativo.
             <br>
             ATTENZIONE: a causa di un bug per recepire le modifiche sui filtri bisogna aprire il foglio 'previsione' prima di usare l'ottimizzatore.
             "
         )
       )    
       
  ), 
  
  
  textOutput("introreg"),
  
  textOutput("formula"),
  
  DT::dataTableOutput("tavola"),
  
  textOutput("corintro"),
  tags$head(tags$style("#corintro{ font-size: 10px; font-style: italic; }")),
  
  plotlyOutput(outputId = "serie", height="85%"),
  
  plotOutput(outputId = "cormat"),
  plotOutput(outputId = "Qhist"),
  
  
  textOutput("ttest"),
  tags$head(tags$style("#ttest{ font-size: 10px; font-style: italic; }")),
  tabPanel("pvalues dei test t", verbatimTextOutput("pvt")),
  
  textOutput("titolo"),
  
  tabPanel("prevedo", verbatimTextOutput("previsione")),
  
  tabPanel("distribution", verbatimTextOutput("DIST")),
  
  tabPanel("myprofit", verbatimTextOutput("profitto")),
  
  plotOutput(outputId = "scenario")
  
)



# define server ------

server <- function(input, output, session) {
  
  # funzioni utili ----
  catToDummy = function(cat){
    unici = unique(cat)
    mat = matrix(numeric(length(cat)*length(unici)), ncol=length(unici))
    
    for (i in 1:ncol(mat)){
      mat[,i] = ifelse(cat == unici[i],1,0)
    }
    colnames(mat) = unici
    return(mat)
  }
  
  #scrivi la formula di regressione
  exprFormula = function(reg, output, xnames){
    #testo della funzione
    #frm = formula(reg)
    #frm = Reduce(paste, deparse(frm))
    segni = ifelse(coef(reg)>0, " + ","")
    coeftext = paste(segni, as.character(round(coef(reg), 3)) , sep='')
    coeftext = paste(coeftext, c(1,xnames), sep=' * ')
    intext = paste(coeftext, collapse = "")
    frm = paste(output, "=", intext)
    return(frm)
  }
  
  #prevedi una sequenza data una continua in un range e le altre fissate
  iperpred = function(reg, varinput, varcostanti, valcostanti, min=0, max=10, step=0.1){
    varia = seq(min,max,step)
    df3 = matrix(rep(valcostanti, length(varia)), ncol=length(valcostanti),byrow=T)
    df3 = as.data.frame(cbind(df3, varia))
    colnames(df3) = c(varcostanti, varinput)
    fit = predict(reg, df3)
    return(list(y = fit, x = varia))
  }
  
  #grafica iperpred
  multiregplot = function(df,reg,output, input, costNames,costValues,min,max,step){
    res = iperpred(reg, input,costNames,costValues, min = min, max = max, step=step)
    plot(df[[input]], df[[output]], xlab = input, ylab=output, 
         ylim = c(min(min(res$y),min(df[[output]])), 
                  max(max(res$y), max(df[[output]]))) , 
         xlim = c(min(min(res$x),min(df[[input]])), 
                  max(max(res$x), max(df[[input]]))), 
         col = "blue")
    lines(res$x,res$y, col='red')
  }
  
  
  #scrivi la formula di regressione
  exprFormula = function(reg, output, xnames){
    #testo della funzione
    #frm = formula(reg)
    #frm = Reduce(paste, deparse(frm))
    segni = ifelse(coef(reg)>0, "+","")
    coeftext = paste(segni, as.character(round(coef(reg), 3)) , sep='')
    coeftext = paste(coeftext, c(1,xnames), sep='*')
    intext = paste(coeftext, collapse = "")
    frm = paste(output, "=", intext)
    return(frm)
  }
  
  #fai una singola previsione, dato un modello e un vettore di input compatibile
  minipred = function(reg, varcostanti, valcostanti){
    df3 = as.data.frame(rbind(numeric(length(valcostanti)) , valcostanti))
    colnames(df3) = varcostanti
    fit = predict(reg, df3)[2]
    return(fit)
  }
  
  
  #prevedi una sequenza data una continua in un range e le altre fissate
  iperpred = function(reg, varinput, varcostanti, valcostanti, min=0, max=10, step=0.1){
    varia = seq(min,max,step)
    if (length(varcostanti)>0){
      df3 = matrix(rep(valcostanti, length(varia)), ncol=length(valcostanti),byrow=T)
      df3 = as.data.frame(cbind(df3, varia))
      colnames(df3) = c(varcostanti, varinput)
    }else{
      df3 = data.frame(varia)
      colnames(df3) = varinput
    }
    fit = predict(reg, df3)
    return(list(y = fit, x = varia))
  }
  
  minipred = function(reg, varcostanti, valcostanti){
    df3 = as.data.frame(rbind(numeric(length(valcostanti)) , valcostanti))
    colnames(df3) = varcostanti
    fit = predict(reg, df3)[2]
    residui = summary(reg$residuals)
    mae = mean(abs(reg$residuals))
    names(fit) = "stima quantità venduta in una sera"
    names(mae) = "MAE: mean absolute error"
    return(c(fit, mae))
  }
  
  #grafica iperpred
  multiregplot = function(df,reg,output, input, costNames,costValues,min,max,step){
    res = iperpred(reg, input,costNames,costValues, min = min, max = max, step=step)
    plot(df[[input]], df[[output]], xlab = input, ylab=output, 
         ylim = c(min(min(res$y),min(df[[output]])), 
                  max(max(res$y), max(df[[output]]))) , 
         xlim = c(min(min(res$x),min(df[[input]])), 
                  max(max(res$x), max(df[[input]]))), 
         col = "blue")
    lines(res$x,res$y, col='red')
  }
  
  
  
  
  #crea un modello di regressione polinomiale multivariata con o senza dummy
  hypeReg = function(df, output, rdummy, rcontinui, gradi){
    
    df3 = df[,c(output, rcontinui, rdummy)]
    
    xnames = c() #per la formula visibile
    inames = c() #per la formula invisibile
    
    for (i in 1:length(rcontinui)){
      for (j in 1:gradi[i]){
        if (j>1){
          xnames=c(xnames,paste(rcontinui[i],"^", as.character(j), sep=""))
          inames=c(inames, paste("I(", rcontinui[i],"^", as.character(j),")"))
        }
        else{
          xnames=c(xnames,rcontinui[i])
          inames=c(inames,rcontinui[i])
        }
      }
    }
    
    xnames = c(xnames, rdummy)
    inames = c(inames, rdummy)
    
    reg = lm(reformulate(inames, output), data = df3)
    
    return(list(reg=reg,xnames=xnames))
  }
  
  
  
  
  
  # funzioni server ------  
  
  
  aggiornaDF = function(){
    #applica filtri
    selezione1 = input$filtriPiatti
    selezione2 = input$filtriAnni
    selezione3 = input$filtriGiorni
    dfFilter = filter(dati, prod %in% selezione1, 
                      anno %in% selezione2,
                      gg %in% selezione3)
    output$tavola = DT::renderDataTable({dfFilter})
  }
  
  
  creaModello = function(){
    #applica filtri
    selezione1 = input$filtriPiatti
    selezione2 = input$filtriAnni
    selezione3 = input$filtriGiorni
    dfreg = filter(dati, prod %in% selezione1, 
                   anno %in% selezione2,
                   gg %in% selezione3)
    
    #raccogli dummy
    catv = c("gg", "anno", "prod")
    dummyv = c("menu", "contorno")
    for (dum in catv){
      newmat = catToDummy(dfreg[[dum]])
      dummyv = c(dummyv, colnames(newmat))
      dfreg = cbind(dfreg, newmat)
    }
    
    #raccogli nomi e gradi continue (rimuovendo scelte duplicate)
    NR = input$ninput
    continui = numeric(NR)
    polinomi = numeric(NR)
    for (i in 1:NR){
      nuovo = input[[paste0("regressore",i)]]
      if (!(nuovo %in% continui)){
        continui[i] = nuovo
        polinomi[i] = input[[paste0("grado",i)]]      
      }else{
        continui[i] = NA
      }
    }
    escludi = !is.na(continui)
    continui = continui[escludi]
    polinomi = polinomi[escludi]
    
    #raccogli nomi dummy (rimuovendo scelte duplicate)
    ND = input$dinput
    if (ND>0){
      dummy = numeric(ND)
      for (i in 1:ND){
        nuovo = input[[paste0("dummy",i)]]
        if (!(nuovo %in% dummy)){
          dummy[i] = nuovo
        }else{
          dummy[i] = NA
        }
      }
      dummy = dummy[!is.na(dummy)]
    }else{
      dummy = c()
    }
    
    model = hypeReg(dfreg, "Q", dummy, continui, polinomi)  
    
    return(list(model=model, df=dfreg, continui= continui, dummy=dummy))
  }
  
  
  
  aggiornaReg = function(){
    
    scatola = creaModello()
    model = scatola$model
    continui = scatola$continui
    dummy = scatola$dummy
    dfreg = scatola$df
    
    #pvalues
    #pvt0 = "p-value dei t test sui coefficienti"
    pvt = summary(model$reg)$coefficients[,c(1,4)]
    colnames(pvt) = c("stima coeff", "p-value (on t-test)")
    #colnames(pvt) = c("stima coeff", pvt0)
    
    mae = mean(abs(model$reg$residuals))
    names(mae)="MAE"
    sumres = list(c(summary(model$reg$residuals), mae))
    names(sumres) = "summary dei residui"
    outprint = list(pvt, sumres, regression_info = summary(model$reg)) #summary(model$reg) #list(pvt0, pvt)
    
    #formula
    frm = exprFormula(model$reg, "Q", model$xnames)
    
    #mostra output
    output$formula = renderText({frm})
    output$pvt = renderPrint({outprint})
    
    #parte previsiva
    
    if (length(continui)>0){
      valC = numeric(length(continui))
      for (i in 1:length(continui)){
        valC[i] = input[[paste0("valoreC",i)]]
      }}else{
        valC = c()
      }
    
    if (length(dummy)>0){
      valD = numeric(length(dummy))
      for (i in 1:length(dummy)){
        valD[i] = input[[paste0("valoreD",i)]]
      }}else{
        valD = c()
      }
    
    
    
    varinput = continui[1]
    varcostanti = c(continui[-1], dummy)
    valcostanti = c(valC[-1], valD)
    prediction = minipred(model$reg, c(continui,dummy), c(valC,valD))
    
    output$previsione = renderPrint({prediction})
    
    output$scenario = renderPlot({
      multiregplot(dfreg, model$reg, "Q", 
                   varinput, varcostanti, valcostanti, 
                   min=0,max=6,step=0.1)
    })
    
  }  
  
  
  optimPrice = function(){
    #calcolo prezzo ottimo
    piatto = input$opzioniPiatti
    costo = input$costo
    
    scatola = creaModello()
    model = scatola$model
    dfreg = scatola$df
    continui = scatola$continui
    dummy = scatola$dummy
    
    #raccogli valori dummy e continui da previsione
    if (length(continui)>0){
      valC = numeric(length(continui))
      for (i in 1:length(continui)){
        valC[i] = input[[paste0("valoreC",i)]]
      }}else{
        valC = c()
      }
    
    if (length(dummy)>0){
      valD = numeric(length(dummy))
      for (i in 1:length(dummy)){
        valD[i] = input[[paste0("valoreD",i)]]
      }}else{
        valD = c()
      }  
    
    
    
    outprint0 = ottimizza1(model$reg, dfreg, costo, piatto, 
                           continui, dummy, valC, valD)
    output$profitto = renderPrint({outprint0})
  }
  
  #optimize one price
  
  ottimizza1 = function(reg, dati, costo, panino, 
                        continui=NULL, dummy=NULL, 
                        valcontinui=NULL, valdummy=NULL){
    
    id = which(dati[,panino]==1)
    gg = unique(dati$gg)
    ggn = unique(dati$ggn)
    datiPanino = numeric(ncol(dati)*length(gg))%>%
      matrix(ncol=ncol(dati)) %>% 
      as.data.frame()
    colnames(datiPanino) = colnames(dati)
    datiPanino$annon = 4
    
    #dummy piatti
    datiPanino[,panino] = 1
    
    mybool = c('menu', 'contorno')
    for (dum in mybool){
      if (dum %in% dummy){
        id = which(dummy ==dum)
        if (valdummy[id]==1){
          datiPanino[,dum]=1
        }}}
    
    #dummy giorni
    datiPanino$ggn = ggn
    for (i in 1:length(gg)){
      datiPanino[i,gg[i]] = 1
    }
    #dummy degli anni e degli altri piatti sono tutti nulli
    
    
    daOttimizzare = function(prezzo){
      datiPanino[,'P'] = prezzo
      Qpanino = predict(reg, datiPanino)
      costi = Qpanino*costo
      ricavi = Qpanino*prezzo
      profitti = ricavi-costi
      profitto = sum(profitti)
      return(profitto)
    }
    
    otti = optimize(daOttimizzare, maximum=T, lower=0.05, upper=10)
    bestP = otti$maximum
    maxProfitto = otti$objective
    
    
    prezzo = bestP
    datiPanino[,'P'] = prezzo
    Qpanino = predict(reg, datiPanino)
    costi = Qpanino*costo
    ricavi = Qpanino*prezzo
    profitti = ricavi-costi
    profitto = sum(profitti)
    
    debug = list(e0 = otti)
    
    mat = matrix(numeric((nrow(datiPanino)+1)*6),
                 ncol=6) %>%as.data.frame()
    colnames(mat) = c('prezzo1','costo1', 'Qvenduta', 'ricavi', 'costi', 'profitti')
    rownames(mat) =c(gg, 'total')
    mat$prezzo1 = prezzo
    mat$costo1 = costo
    mat$Qvenduta = c(Qpanino, sum(Qpanino))
    mat$ricavi = c(ricavi, sum(ricavi))
    mat$costi = c(costi, sum(costi))
    mat$profitti = c(profitti, sum(profitti))
    
    if (coef(reg)['P']>0){
      alert = matrix(c('il coefficiente del prezzo è positivo.  :(', 
                       'La previsione del prezzo ottimo può risultare vicina a 10 (il massimo)',
                       'si consiglia di aggiustare meglio il modello di regressione',
                       'ad esempio includendo le dummy dei giorni nei regressori',
                       'oppure filtrando il dataset per i piatti più simili a quello studiato'), nrow=5)
    }else{
      alert = matrix(c('il coefficiente del prezzo è negativo  :)', 
                       'per cui la stima è verosimile (se inferiore a 9.9)'),
                     nrow=2)
    }  
    
    return(list(alert = alert, prezzo_ottimo = prezzo, profitto=profitto, 
                vendite_stimate = mat,
                regressori_utilizzati = datiPanino[,-1]))
  }
  
  
  
  
  
  # azioni server --------  
  
  #scrivi i testi
  output$introreg = renderText({"Modello di regressione lineare per prevedere Q (quantità venduta nel dataset filtrato)"})
  output$corintro = renderText({
    "Correlazione delle quantità vendute dei diversi piatti. E' utile se si vuole capire quali piatti si fanno concorrenza a vicenda (beni sostituti) e quali invece si vendono insieme (beni complementi).
 La correlazione è un valore che va da -1 (dipendenza negativa, ovvero trade-off: al crescere di una variabile decresce l'altra) a 1 (dipendenza positiva, ovvero crescono insieme),
e in valore assoluto (modulo) da 0 (totale indipendenza) a 1 (massima dipendenza)
Una differenza significativa tra la correlazione di Pearson (solo lineare) e la correlazione di Spearman (in genere più alta in modulo) può significare che esiste una relazione non lineare (quindi poliniomiale, esponenziale o logaritmica) tra le variabili correlate. 
 Attenzione: le correlazioni NA sono state sostituite da 0 (si assumono indipendenti ma in realtà non si sono mai visti insieme).
"})
  
  output$ttest = renderText({"Valori dei p-value dei test T sull'ipotesi nulla che i coefficienti delle variabili input siano pari a 0. 
Se il pvalue (del coefficiente) di una variabile è basso allora quella variabile input va tenuta, altrimenti va levata dal modello (in quanto non sono sufficientemente correlate e non aiuta a prevedere la variabile indipendente).
Attenzione all'overfitting!! Se il numero di variabili è alto, le previsioni si adattano facilmente ai dati, ma ciò fa sembrare il modello migliore di quello che è."})
  
  
  
  
  
  #decidi se far comparire o meno gli elementi ui
  observeEvent(input$mode, {
    if (input$mode == "panoramica serie"){
      show("contorni")
      show("panini")
      show("serie")
      
      hide('card_serie')
      hide('profitto')
      hide('costo')
      hide('opzioniPiatti')
      hide('PVinput')
      hide('dtab')
      hide('DIST')
      hide("Qhist")   
      hide("previsione")
      hide("scenario")
      hide('selectall')
      hide("filtriP")
      hide("filtriPiatti")
      hide("filtriA")
      hide("filtriAnni")
      hide("filtriG")
      hide("filtriGiorni")
      hide("corintro")
      hide("cormat")
      hide("cormode")
      hide("tavola")
      hide("ninput")
      hide("inputOptionN")
      hide("dinput")
      hide("inputOptionD")
      hide("formula")
      hide("pvt")
      hide("ttest")
      hide("introreg")
      hide("inputOptionN1")
      hide("inputOptionD1")
    }
    
    if (input$mode == "correlazioni"){
      hide("contorni")
      hide("panini")
      hide("serie")
      hide("ninput")
      hide("inputOptionN")
      hide("dinput")
      hide("inputOptionD")
      hide("tavola")
      hide("formula")
      hide("pvt")
      hide("ttest")
      hide("introreg")
      hide("inputOptionN1")
      hide("inputOptionD1")
      hide("previsione")
      hide("scenario")
      hide("Qhist")
      hide('DIST')
      hide('dtab')
      hide('PVinput')
      hide('opzioniPiatti')
      hide('profitto')
      hide('costo')
      hide('card_serie')
      
      show("filtriA")
      show("filtriP")
      show("filtriG")
      show("corintro")
      show("cormat")
      show("cormode")
    }
    
    if (input$mode == "dati grezzi"){
      hide("contorni")
      hide("panini")
      hide("serie")
      hide("cormat")
      hide("corIntro")
      hide("cormode")
      hide("ninput")
      hide("inputOptionN")
      hide("dinput")
      hide("inputOptionD")
      hide("formula")
      hide("pvt")
      hide("ttest")
      hide("introreg")
      hide("inputOptionN1")
      hide("inputOptionD1")
      hide("previsione")
      hide("scenario")
      hide("Qhist")   
      hide('DIST')
      hide('dtab')
      hide('PVinput')
      hide('opzioniPiatti')
      hide('profitto')
      hide('costo')
      hide('card_serie')
      
      show("filtriA")
      show("filtriP")
      show("filtriG")
      show("tavola")
    }
    
    if (input$mode == "regressione"){
      hide("contorni")
      hide("panini")
      hide("serie")
      hide("cormat")
      hide("corintro")
      hide("cormode")
      hide("tavola")
      hide("inputOptionN1")
      hide("inputOptionD1")
      hide("previsione")
      hide("scenario")
      hide("Qhist")  
      hide('DIST')
      hide('dtab')
      hide('PVinput')
      hide('opzioniPiatti')
      hide('profitto')
      hide('costo')
      hide('card_serie')
      
      show("formula")
      show("pvt")
      show("ttest")
      show("introreg")
      show("filtriA")
      show("filtriP")
      show("filtriG")
      show("ninput")
      show("inputOptionN")
      show("dinput")
      show("inputOptionD")
    }
    
    if (input$mode == "previsione"){
      hide("contorni")
      hide("panini")
      hide("serie")
      hide("cormat")
      hide("corintro")
      hide("cormode")
      hide("tavola")
      hide("ninput")
      hide("inputOptionN")
      hide("dinput")
      hide("inputOptionD")
      hide("pvt")
      hide("ttest")
      hide("Qhist")     
      hide('DIST')
      hide('dtab')
      hide('PVinput')
      hide('opzioniPiatti')
      hide('profitto')
      hide('costo')
      hide('card_serie')
      
      show("formula")
      show("introreg")
      show("previsione")
      show("scenario")
      show("inputOptionN1")
      show("inputOptionD1")
      show("filtriA")
      show("filtriP")
      show("filtriG")
    }
    
    
    
    if (input$mode == "intervalli"){
      hide("contorni")
      hide("panini")
      hide("serie")
      hide("cormat")
      hide("corintro")
      hide("cormode")
      hide("tavola")
      hide("ninput")
      hide("inputOptionN")
      hide("dinput")
      hide("inputOptionD")
      hide("pvt")
      hide("ttest")
      hide("formula")
      hide("introreg")
      hide("previsione")
      hide("scenario")
      hide("inputOptionN1")
      hide("inputOptionD1")
      hide('opzioniPiatti')
      hide('profitto')
      hide('costo')
      hide('card_serie')
      
      show('PVinput')
      show('dtab')
      show('DIST')
      show("Qhist")
      show("filtriA")
      show("filtriP")
      show("filtriG")
    }        
    
    if (input$mode == "prezzo ottimo"){
      hide("contorni")
      hide("panini")
      hide("serie")
      hide("cormat")
      hide("corintro")
      hide("cormode")
      hide("tavola")
      hide("ninput")
      hide("inputOptionN")
      hide("dinput")
      hide("inputOptionD")
      hide("pvt")
      hide("ttest")
      hide("formula")
      hide("introreg")
      hide("previsione")
      hide("scenario")
      hide("inputOptionN1")
      hide("inputOptionD1")
      hide('PVinput')
      hide('dtab')
      hide('DIST')
      hide("Qhist")
      hide('card_serie')
      
      show('profitto')
      show('costo')
      show('opzioniPiatti')
      show("filtriA")
      show("filtriP")
      show("filtriG")
    }           
    
    
    if (input$mode == "istruzioni"){
      hide("contorni")
      hide("panini")
      hide("serie")
      hide("cormat")
      hide("corintro")
      hide("cormode")
      hide("tavola")
      hide("ninput")
      hide("inputOptionN")
      hide("dinput")
      hide("inputOptionD")
      hide("pvt")
      hide("ttest")
      hide("formula")
      hide("introreg")
      hide("previsione")
      hide("scenario")
      hide("inputOptionN1")
      hide("inputOptionD1")
      hide('PVinput')
      hide('dtab')
      hide('DIST')
      hide("Qhist")
      hide('profitto')
      hide('costo')
      hide('opzioniPiatti')
      hide("filtriA")
      hide("filtriP")
      hide("filtriG")  
      hide('selectall')
      
      show('card_serie')
    }              
    
    
    
    
    updateSelectInput(inputId='opzioniPiatti', choices=input$filtriPiatti)
    
    optimPrice() #calcola il prezzo ottimo
    
    aggiornaDF() #aggiorna dati grezzi
    
  })
  
  observeEvent(input$selectall,{
    unici = unique(dati$prod0)
    if (input$selectall){
      updateCheckboxGroupInput(session, "filtriPiatti", choices=unici, selected=unici)
    }else{
      updateCheckboxGroupInput(session, "filtriPiatti", choices=unici)
    }
  })
  
  
  observeEvent(input$filtriPiatti,{
    #fissa opzioni piatti in prezzo ottimo
    updateSelectInput(inputId='opzioniPiatti', choices=input$filtriPiatti)
    aggiornaDF() #aggiorna dati grezzi
  }) 
  
  
  observeEvent(input$opzioniPiatti,{
    #trova nuovo prezzo ottimo
    optimPrice()
  }) 
  
  observeEvent(input$costo,{
    #trova nuovo prezzo ottimo
    optimPrice()
  })  
  
  
  observeEvent(input$filtriAnni,{
    aggiornaDF() #aggiorna dati grezzi
  })   
  
  observeEvent(input$filtriGiorni,{
    aggiornaDF() #aggiorna dati grezzi
  })  
  
  observe({
    aggiornaReg() #crea le pagine previsione e regressione      
  })
  
  
  
  #sistema i checkbox
  observeEvent(input$panini, {
    if (input$panini){
      updateCheckboxInput(session, "contorni" , value = FALSE)
    }
  })
  observeEvent(input$contorni, {
    if (input$contorni){
      updateCheckboxInput(session, "panini" , value = FALSE)
    }
  })
  
  
  
  #importa e correggi i dati
  link = "https://raw.githubusercontent.com/Fede-Rausa/RepaxDatiPaninoteca/main/datiVendite.csv"   
  dati = read.csv(link ,sep=";")
  dati$P = as.numeric(gsub(",",".", dati$P))
  dati = mutate(dati, prod0 = prod, 
                prod = paste(prod, ifelse(menu==1," M",""),sep=''),
                annon = anno - 20,
                anno = paste0("year",anno + 2000))
  
  #nomi continue
  numv = c("P", "annon", "ggn")
  #nomi categoriche
  catv = c("gg", "anno", "prod0")
  dummyv = c("menu", "contorno")
  for (dum in catv){
    dummyv = c(dummyv, unique(dati[[dum]]))
  }
  
  #scegli regressori continui
  output$inputOptionN = renderUI({
    ninput = input$ninput
    opzioni = numv
    
    lapply(1:ninput, 
           function(i){
             fluidRow(
               column(6, selectInput(inputId = paste0("regressore", i),
                                     label = paste("variabile ", i),
                                     choices = opzioni)),
               column(6, sliderInput(inputId = paste0("grado", i),
                                     label = "input di grado",
                                     min = 1, max = 5, step=1, value = 1))
             )})})
  
  #scegli regressori binari
  output$inputOptionD = renderUI({
    
    selezione1 = input$filtriPiatti
    selezione2 = input$filtriAnni
    selezione3 = input$filtriGiorni
    dati = filter(dati, 
                  prod %in% selezione1, 
                  anno %in% selezione2, 
                  gg %in% selezione3)
    
    catv = c("gg", "anno", "prod0")
    dummyv = c("menu", "contorno")
    for (dum in catv){
      dummyv = c(dummyv, unique(dati[[dum]]))
    }
    
    ninput = input$dinput
    opzioni = dummyv
    if (ninput>0){    
      lapply(1:ninput, 
             function(i){
               fluidRow(
                 column(12, selectInput(inputId = paste0("dummy", i),
                                        label = paste("dummy ", i),
                                        choices = opzioni)),
               )})
    }
  })
  
  
  #fissa regressori continui
  output$inputOptionN1 = renderUI({
    ninput = input$ninput
    continui = numeric(ninput)
    for (i in 1:ninput){
      if (!(input[[paste0("regressore",i)]]%in%continui)){
        continui[i] = input[[paste0("regressore",i)]]
      }else{
        continui[i] = NA
      }
    }
    continui = continui[!is.na(continui)]
    
    if (length(continui)>0){
      lapply(1:length(continui), 
             function(i){
               fluidRow(
                 column(12, sliderInput(inputId = paste0("valoreC", i),
                                        label = continui[i],
                                        min = 1, max = 5, step=0.01, value = 1))
               )})      
    }
  })
  
  #fissa regressori dummy
  output$inputOptionD1 = renderUI({
    dinput = input$dinput
    dummy = numeric(dinput)
    for (i in 1:dinput){
      if (!(input[[paste0("dummy",i)]]%in%dummy)){
        dummy[i] = input[[paste0("dummy",i)]]
      }else{
        dummy[i] = NA
      }
    }
    dummy = dummy[!is.na(dummy)]
    
    if (length(dummy)>0){
      lapply(1:length(dummy), 
             function(i){
               fluidRow(
                 column(12, sliderInput(inputId = paste0("valoreD", i),
                                        label = dummy[i],
                                        min = 0, max = 1, step=1, value = 1))
               )})     
    }
  })
  
  
  
  
  
  
  #inizializza i filtri
  observe({
    updateCheckboxGroupInput(session, "filtriPiatti",
                             choices = unique(dati$prod),
                             selected = unique(dati$prod)
    )
    updateCheckboxGroupInput(session, "filtriAnni",
                             choices = unique(dati$anno),
                             selected = unique(dati$anno)
    )
    updateCheckboxGroupInput(session, "filtriGiorni",
                             choices = unique(dati$gg),
                             selected = unique(dati$gg)
    )
  })
  
  observeEvent(input$filtriP, {
    if (input$filtriP){
      show("filtriPiatti")
      show('selectall')
    }else{
      hide("filtriPiatti")
      hide('selectall')
    }
  })
  observeEvent(input$filtriA, {
    if (input$filtriA){
      show("filtriAnni")
    }else{
      hide("filtriAnni")
    }
  })
  observeEvent(input$filtriG, {
    if (input$filtriG){
      show("filtriGiorni")
    }else{
      hide("filtriGiorni")
    }
  })
  
  #fai il grafico
  output$serie = renderPlotly({
    id = dati$ggn + dati$annon * 7
    
    df = data.frame(time = id, q.venduta = dati$Q, punti=ifelse(dati$contorno==1,18,19),
                    day = dati$gg, year=(dati$anno), 
                    prod0 = dati$prod, prezzo = paste(as.character(dati$P),"€"),
                    prod = paste(dati$prod, ifelse(dati$menu==1,'M','') )  )
    
    #seleziona i dati del grafico
    if (input$contorni){
      df = df[which(dati$contorno==1),]
    }
    if (input$panini){
      df = df[which(dati$contorno==0),]
    }
    
    p <- ggplot(df,aes(price = prezzo, y = q.venduta , x = time,group=prod, col=prod,
                       giorno = day, anno = year)) + 
      geom_point(shape=df$punti) + geom_line() +
      theme_minimal()+ ggtitle("serie storica dei piatti venduti") +
      xlab("tempo") + ylab("q. venduta")
    
    ggplotly(p)
  })
  
  
  
  
  output$cormat = renderPlot({
    
    observeEvent(input$cormode,{})
    
    selezione1 = input$filtriPiatti
    selezione2 = input$filtriAnni
    selezione3 = input$filtriGiorni
    
    cormat = dati %>%
      mutate(ggyear = paste(gg, anno, sep='-'))%>% 
      #mutate(prod = paste(prod, ifelse(menu==1," M",""),sep=''))%>%
      filter(prod %in% selezione1, anno %in% selezione2, gg %in% selezione3) %>%
      select(ggyear, prod, Q)%>% 
      pivot_wider(names_from = prod, values_from = Q) %>%
      select(-ggyear) %>% 
      as.data.frame()%>%
      cor(use="pairwise.complete.obs", method=input$cormode)
    
    if (input$cormode=="spearman"){
      titolo = "correlazioni lineari e non tra le quantità vendute"
    }else{
      titolo = "correlazioni lineari tra le quantità vendute"
    }
    
    cormat[is.na(cormat)] = 0  
    ggcorrplot(cormat, method ="square", hc.order = T, title=titolo)  # type='lower',
  })
  
  
  output$Qhist = renderPlot({
    
    selezione1 = input$filtriPiatti
    selezione2 = input$filtriAnni
    selezione3 = input$filtriGiorni
    
    qvet = dati %>%
      filter(prod %in% selezione1, 
             anno %in% selezione2, 
             gg %in% selezione3) 
    
    qvet = qvet$Q
    N = length(qvet)
    
    hist(qvet, prob=T, 
         xlab='Q venduta in una sera di un piatto',
         main='frequenze quantità venduta (da filtri)',
         sub = paste0('distribuzione su ', N, ' osservazioni'))
    lines(density(qvet), col='blue', lwd=2, lty='dashed')
    ord = order(qvet)
    lines(qvet[ord], dpois(qvet, mean(qvet))[ord], 
          col='darkred', lwd=2, lty='dashed')
    lines(qvet[ord], dnorm(qvet, mean(qvet), sd(qvet))[ord], 
          col='purple', lwd=2, lty='dashed')
    abline(v=mean(qvet), col='red', lwd=2)
    abline(v=quantile(qvet, 0.5), col='orange', lwd=2)
    abline(v=quantile(qvet, 0.25), col='green', lwd=2, lty='dotted')
    abline(v=quantile(qvet, 0.75), col='green', lwd=2, lty='dotted')
    
    outprint = matrix(numeric(3*2), ncol=2)
    colnames(outprint) = c('inf-IC', 'sup-IC')
    rownames(outprint) = c('kernel', 'poisson', 'normale')
    
    IClevel = input$PVinput
    Z = qnorm(1-IClevel/2)#1.96
    med = mean(qvet)    
    
    dens = density(qvet)
    outprint[1,] = quantile(dens$x, c(IClevel/2, 1-IClevel/2))
    
    errP = Z * sqrt(med/N)
    ICpois = c(med- errP, med+ errP)
    outprint[2,] = ICpois
    
    errN = Z * sd(qvet)/sqrt(N)
    ICnorm = c(med- errN, med+ errN)
    outprint[3,] = ICnorm
    
    outprint = round(outprint,2)
    
    outprint2 = round(c(summary(qvet), sd(qvet)),2)
    names(outprint2) = c(names(summary(0)), 'dev std')
    
    if (input$dtab == 'distribuzione'){
      output$DIST = renderPrint({outprint2})
    }else{
      output$DIST = renderPrint({outprint})
    }
    
    legend("topright", 
           c('media','mediana (quantile 50%)','quantili 25% e 75%', 
             'densità kernel', 'densità poisson', 'densità normale'), 
           col=c('red', 'orange','green','blue','darkred', 'purple'), lwd=2)
  })
  
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)