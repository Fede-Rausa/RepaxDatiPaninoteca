# RepaxDatiPaninoteca
I dati delle vendite della paninoteca di ottobre nella parrochia Regina Pacis (repax) di Saronno.

I dati sono stati raccolti su tre anni, dal 2021 al 2023, nei giorni feriali di una settimana di ottobre.
Nel 2021 e nel 2022 la paninoteca è stata aperta dal martedì al venerdì, nel 2023 anche il lunedì, per cui si hanno
un totale di 13 giorni osservati.
I piatti venduti si dividono in due categorie: panini e contorni.
A ciascun piatto si associa, per ogni anno e giorno, la quantità venduta e il prezzo.
Per alcuni contorni, in alcuni anni, si distinguono due osservazioni, relative agli ordini in menu e fuori menu, in quanto si osservano differenze di prezzo e quantità venduta.

Le analisi dei dati sono automatizzate al seguente sito, programmato in R e Shiny:

[repaxBurger](https://shinylive.io/r/app/#h=0&code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAHQgBsBLAIwCcoWBPACgGcALBhA4BKWo1btuTHuNERatAMQACACJwAZoLjKAqgEllGoi2VpUy0nyillAEzYB3HmeUCepIgHM2MZQFpA-1oAVwZlAB5-ZVQoLzgAfR4GOzgmdi4FCGVlFQBBdEsGUjodIODsopKdAF5lajAAYQYoImUAZQEhAEIG3Cyc5NT00zqhtIyBnOUZFLgWfQhUENJMyunlQWXSfTtlOoamQR4+2g2cuigmODp9+rAAORCYa9MiDWUjiB5EU-XpmCCO4ARn6-xyMCgAA87gBWAAMYPOygAblA6CFasoAMzws7TOQ5YRg-GoOhEUgAeRW2y4RBpK12dwadgYHgACuTSA05HJaDx5ij5pFohoQhACKQGCQuFsVrhlPTSNthMoQPildsACSsjlckXKFgUVIsTkUrjq-4wqJGKDFPhiuhaxx2qUQLz4nJfFw2gUARy4gIgXChxOUkKhIbDpXdVgwSructIWu9ygA1MpgXz-u5Vp7plCkedWHAoABrFx1b1FjbEW4HMCKADssLyeVUACE-simCYTcywI4BORu+coZcmAOAOquwReIrwSxtCBwKG2eYhVBSkjKWXZIM8UR4fMQu3ZBsACTZnh8sEhDXzhOUAF85M+sipGkabDpOoIOGYhTlPyXQcAUqBcGEdxhAqAosEKowzIK8zCGAz4ALpAA)

I dati sono stati raccolti con google fogli:

[datiPanini](https://docs.google.com/spreadsheets/d/1qeAc79YXXi5OGO2PGMZ_PmsMeU8R87O1duK5RucsWp4/edit?usp=sharing)

Code chunk per importare il dataset in uno script di R:

dati = read.csv("https://raw.githubusercontent.com/Fede-Rausa/RepaxDatiPaninoteca/main/datiVendite.csv", 
                sep=";")
                
dati$P = as.numeric(gsub(",",".", dati$P))
