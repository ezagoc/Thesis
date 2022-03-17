/// Tesis: 
/// Limpiando la base completa de delitos
/// Autor: Eduardo Zago

cd "C:\Users\lalo-\OneDrive\Documentos\Tesis\Bases_de_Datos"

import delimited "C:\Users\lalo-\OneDrive\Documentos\Tesis\Bases_de_Datos\Municipal_Delitos_2015_2020_ago2020.csv"

save Delitos_Inicio, replace

// Cleaning the data set:

// 1.0) Renaming the months for the reshape

foreach var of varlist enero-diciembre{
     local j = `j' + 1
	 rename `var' mes_`j'
}

// 1.1) Assigning numbers to the relevant felonies involving gender violence

gen crimen = 1 if subtipodedelito == "Feminicidio"
replace crimen = 2 if subtipodedelito == "Abuso sexual"
replace crimen = 3 if subtipodedelito == "Acoso sexual"
replace crimen = 4 if subtipodedelito == "Hostigamiento sexual"
replace crimen = 5 if subtipodedelito == "Violación simple"
replace crimen = 6 if subtipodedelito == "Violación equiparada"
replace crimen = 7 if subtipodedelito == "Violencia familiar"
replace crimen = 8 if subtipodedelito == "Violencia de género en todas sus modalidades distinta a la violencia familiar"

// Dropping all felonies that are not relevant:

drop if crimen == .

// 1.2) Summarizing the data by State and Crime

collapse (sum) mes_1-mes_12, by (entidad crimen año)

// 1.3) Reshape WIDE----> LONG

reshape long mes_, i(entidad crimen año) j(crimenes_tot)
rename mes_ crim_tot
rename crimenes_tot mes

// 1.4) Recoding the State variable:

encode entidad, gen(clavEst)

br