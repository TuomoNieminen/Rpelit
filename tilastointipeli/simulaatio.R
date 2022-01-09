# Tässä tutkitaan simulaatiolla tilastointipelin kestoa
# erilaisilla valinnoilla 
# Tuomo Nieminen 12/2021

library(dplyr, quiet = TRUE)

heita_noppaa <- function(nopan_silmaluku = 6, heittoja = 1) {
  sample(1:nopan_silmaluku, size = heittoja, replace = TRUE)
}

tilastoi <- function(tulokset, silmaluku) {
  tulokset[silmaluku] <- tulokset[silmaluku] + 1
  tulokset
}

pelaa_peli <- function( maksimi_lukumaara = 10, nopan_silmaluku = 6) {
  tulokset <- numeric(nopan_silmaluku)
  names(tulokset) <- 1:nopan_silmaluku
  kierroksia <- 0
  while(all(tulokset < maksimi_lukumaara)) {
    silmaluku <- heita_noppaa(nopan_silmaluku, heittoja = 1)
    tulokset <- tilastoi(tulokset, silmaluku)
    kierroksia <- kierroksia + 1
  }
  
  return(list(kierroksia = kierroksia, tulokset = tulokset))
}

pelaa_peleja <- function(maksimi_lukumaara = 10, peleja = 1000) {
  kierroksia <- numeric(peleja)
  
  for(i in 1:peleja)
    kierroksia[i] <- pelaa_peli( maksimi_lukumaara = maksimi_lukumaara)$kierroksia
  
  kierroksia
}

tulokset_max10 <- pelaa_peleja(maksimi_lukumaara =10)
tulokset_max9 <- pelaa_peleja(maksimi_lukumaara =9)
tulokset_max8 <- pelaa_peleja(maksimi_lukumaara = 8)
tulokset_max7 <- pelaa_peleja(maksimi_lukumaara = 7)
tulokset_max6 <- pelaa_peleja(maksimi_lukumaara = 6)
tulokset_max5 <- pelaa_peleja(maksimi_lukumaara = 5)


tulokset_max6 %>% quantile(p = c(0.05, 0.5, 0.95))
tulokset_max5 %>% quantile(p = c(0.05, 0.5, 0.95))
