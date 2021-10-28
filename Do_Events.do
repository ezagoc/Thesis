/// Event Studies: Tesis

ssc install coefplot, replace

cd "C:\Users\lalo-\OneDrive\Documentos\Tesis\Bases_de_Datos\Limpias"

/// Google Trends: Violencia de genero

clear all
import delimited trendsgen_limpia2

drop if sem == 5

gen omit2 = sem < 18

forvalues i = 6/17{
    gen sem`i' = sem == `i'
}

/// Efectos fijos por estado

forvalues i = 1/5{
    gen est`i' = clavest == `i'
}

help substr
/// Efectos fjos por dia de la semana

forvalues i = 1/7{
    gen dia`i' = diasem == `i'
}

/// Efectos fijos por mes1
drop mes
gen mes = substr(date,7,1)
destring(mes), replace

forvalues i = 2/4{
    gen mes`i' = mes == `i'
}

/// Efectos fijos por dia de la semana

help coefplot
save EventStudy_Trends, replace

for varlist sem*: gen treat1X= treat*X

areg trend_reesc treat1sem6 treat1sem7 treat1sem8 treat1sem9 treat1sem10 treat1sem11 treat1sem12 treat1sem13 treat1sem14 treat1sem15 treat1sem16 treat1sem17 treat est1 est2 est3 est4 sem7 sem8 sem9 sem10 sem11 sem12 sem13 sem14 sem15 sem16 sem17 mes2 mes4 omit2, absorb(diasem) robust

coefplot, vertical omitted ///
drop(_cons treat est1 est2 est3 est4 dia1 dia2 dia3 dia4 dia5 dia6 sem7 sem8 sem9 sem10 sem11 sem12 sem13 sem14 sem15 sem16 sem17 mes2 mes4) ///
order(omit2 treat1sem6 treat1sem7 treat1sem8 treat1sem9 treat1sem10 treat1sem11 treat1sem12 treat1sem13 treat1sem14 treat1sem15 treat1sem16 treat1sem17) ///
coeflabels(omit2 = "-4" treat1sem6="-3" ///
treat1sem7="-2" treat1sem8="-1" treat1sem9="0" treat1sem10="1" treat1sem11="2" treat1sem12="3"treat1sem13="4" treat1sem14="5" treat1sem15="6"treat1sem16="7" treat1sem17="8", labsize(small)) ///
xtitle("Semanas diferidas", height(5)) ytitle("Coeficientes", height(5)) ///
title("Query: Violencia de Género") yline(0, lc(black)) ///
ciopts(lc(edkblue) recast(. rcap)) ///
xline(8, lc(black)) xline(5, lc(black))

save EventStudy_Trends, replace

/// LLamadas al 911: 

clear all
import delimited 911_limpia1

drop if year == 2021

gen omit2 = sem < 100

forvalues i = 1/26{
    gen sem`i' = sem == `i'
}

forvalues i = 1/7{
    gen dia`i' = diasem == `i'
}

forvalues i = 1/6{
    gen mes`i' = mes == `i'
}

for varlist sem*: gen treat1X= treat*X

areg total treat1sem1 treat1sem2 treat1sem3 treat1sem4 treat1sem5 treat1sem7 treat1sem8 treat1sem9 treat1sem11 treat1sem12 treat1sem13 treat1sem14 treat1sem15 treat1sem16 treat1sem17 treat1sem18 treat1sem19 treat1sem20 treat1sem21 treat1sem22 treat1sem23 treat1sem24 treat1sem25 treat1sem26 treat sem1 sem2 sem3 sem4 sem5 sem6 sem7 sem8 sem9 sem11 sem12 sem13 sem14 sem15 sem16 sem17 sem18 sem19 sem20 sem21 sem22 sem23 sem24 sem25 sem26 mes1 mes2 mes3 mes4 mes5 omit2, absorb(diasem) robust

coefplot, vertical omitted ///
drop(_cons treat1sem1 sem1 sem2 sem3 sem4 sem5 sem6 sem7 sem8 sem9 sem11 sem12 sem13 sem14 sem15 sem16 sem17 sem18 sem19 sem20 sem21 sem22 sem23 sem24 sem25 sem26 mes1 mes2 mes3 mes4 mes5 treat) ///
order(treat1sem1 treat1sem2 treat1sem3 treat1sem4 treat1sem5 treat1sem7 treat1sem8 treat1sem9 omit2 treat1sem11 treat1sem12 treat1sem13 treat1sem14 treat1sem15 treat1sem16 treat1sem17 treat1sem18 treat1sem19 treat1sem20 treat1sem21 treat1sem22 treat1sem23 treat1sem24 treat1sem25 treat1sem26) ///
coeflabels(omit2 = "0" treat1sem1="-9" treat1sem2="-8" treat1sem3="-7" treat1sem4="-6" treat1sem5="-5" treat1sem6="-4" ///
treat1sem7="-3" treat1sem8="-2" treat1sem9="-1" treat1sem11="1" treat1sem12="2" treat1sem13="3" treat1sem14="4" treat1sem15="5" treat1sem16="6" treat1sem17="7" treat1sem18="8" treat1sem19="9" treat1sem20="10" treat1sem21="11" treat1sem22="12" treat1sem23="13" treat1sem24="14" treat1sem25="15" treat1sem26="16", labsize(small)) ///
xtitle("Semanas diferidas del aislamiento", height(5)) ytitle("Coeficientes", height(5)) ///
title("LLamadas al 911") yline(0, lc(black)) ///
ciopts(lc(edkblue) recast(. rcap)) xline(8, lc(red)) ///
xline(11, lc(black)) 

