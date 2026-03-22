graf4a <- graf_4_dta
graf4a$podil <- round(graf4a$pocet_zamestnancu/graf4a$pocet_zamestnancu_agg*100,1)
graf4a <- graf4a[,c("kategorie_2014_cz","rok","pocet_zamestnancu","podil")]
names(graf4a) <- c("Kategorie","Rok","Zaměstnanců","Podíl (%)")
writexl::write_xlsx(graf4a,"graf4a.xlsx")

graf5b <- graf_6_dt[,c("kategorie_2014_cz","rok","wage_to_general","max_change","max_change_kap","min_change","min_change_kap")]
graf5b$wage_to_general <- graf5b$wage_to_general*100
graf5b$max_change <- graf5b$max_change*100
graf5b$min_change <- graf5b$min_change*100
names(graf5b) <- c("Kategorie","Rok","Poměr platů státních úředníku a prům. mzdy (v %)",
                   "Největší nárůst (%)","Největší nárůst (Kapitola)",
                   "Největší pokles (%)","Největší pokles (Kapitola)")
writexl::write_xlsx(graf5b,"graf5b.xlsx")
