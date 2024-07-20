# RepaxDatiPaninoteca
I dati delle vendite della paninoteca di ottobre nella parrochia Regina Pacis (repax) di Saronno.

I dati sono stati raccolti su tre anni, dal 2021 al 2023, nei giorni feriali di una settimana di ottobre.
Nel 2021 e nel 2022 la paninoteca è stata aperta dal martedì al venerdì, nel 2023 anche il lunedì, per cui si hanno
un totale di 13 giorni osservati.
I piatti venduti si dividono in due categorie: panini e contorni.
A ciascun piatto si associa, per ogni anno e giorno, la quantità venduta e il prezzo.
Per alcuni contorni, in alcuni anni, si distinguono due osservazioni, relative agli ordini in menu e fuori menu, in quanto si osservano differenze di prezzo e quantità venduta.

Le analisi dei dati sono automatizzate al seguente sito, programmato in R e Shiny:

[repaxBurger](https://fede-rausa.github.io/shinyRepax/)

Il sito è un'app Shiny di R, il cui codice è interamente nel file app.R (compilabile in locale con R o online tramite il compiler [shinylive](https://shinylive.io/r/examples/).

I dati sono stati raccolti con google fogli:

[datiPanini](https://docs.google.com/spreadsheets/d/1qeAc79YXXi5OGO2PGMZ_PmsMeU8R87O1duK5RucsWp4/edit?usp=sharing)

Code chunk per importare il dataset in uno script di R:

dati = read.csv("https://raw.githubusercontent.com/Fede-Rausa/RepaxDatiPaninoteca/main/datiVendite.csv", 
                sep=";")
                
dati$P = as.numeric(gsub(",",".", dati$P))
