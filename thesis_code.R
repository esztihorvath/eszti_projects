#######Adatok betöltése és adattisztítás#######

A = read.csv("C:/Users/dell/Desktop/szakdoga/kerdoivek/experiment_result_A.csv", header = TRUE, sep = ';')
B = read.csv("C:/Users/dell/Desktop/szakdoga/kerdoivek/experiment_result_B.csv", header = TRUE, sep = ';')
names(A) <- c("Ido", "Nem", "Kor", "Iskolai_vegzettseg", "Boldog_egyetem", "Szeret_egyetem", "Sikeresseg_egyetem", "Megmerettetes_egyetem", "Boldog_munka", "Szeret_munka", "Sikeresseg_munka", "Megmerettetes_munka", "kredit", "Legtobb_kredit", "szazalek", "Kredittullepok_szazaleka", "Kozossegi_media_idotoltes", "Atlaghoz_kepest", "Felesleges_szazalek", "Felesleges_helyett")
names(B) <- c("Ido", "Nem", "Kor", "Iskolai_vegzettseg", "Boldog_egyetem", "Szeret_egyetem", "Sikeresseg_egyetem", "Megmerettetes_egyetem", "Boldog_munka", "Szeret_munka", "Sikeresseg_munka", "Megmerettetes_munka", "kredit", "Legtobb_kredit", "szazalek", "Kredittullepok_szazaleka", "Kozossegi_media_idotoltes", "Atlaghoz_kepest", "Felesleges_szazalek", "Felesleges_helyett")

#A táblázat: az egyetemen tanulók és dolgozók válaszainak összevonása(2*4 oszlopból 1*4)
for(i in 1:length(A$Boldog_egyetem)){
    if (is.na(A$Boldog_egyetem[i])){
    A$Boldog_egyetem[i] <- A$Boldog_munka[i]
    A$Szeret_egyetem[i] <- A$Szeret_munka[i]
    A$Sikeresseg_egyetem[i] <- A$Sikeresseg_munka[i]
    A$Megmerettetes_egyetem[i] <- A$Megmerettetes_munka[i]
    }
}
colnames(A)[colnames(A) %in% c("Boldog_egyetem", "Szeret_egyetem", "Sikeresseg_egyetem", "Megmerettetes_egyetem")] <- c("Boldog", "Szeret", "Sikeresseg", "Megmerettetes")
A[, c("Boldog_munka", "Szeret_munka", "Sikeresseg_munka", "Megmerettetes_munka")] <- c(NULL, NULL, NULL, NULL)
colnames(A)
dim(A)

#B táblázat: az egyetemen tanulók és dolgozók válaszainak összevonása
for(i in 1:length(B$Megmerettetes_egyetem)){
  if (is.na(B$Megmerettetes_egyetem[i])){
    B$Megmerettetes_egyetem[i] <- B$Megmerettetes_munka[i]
    B$Sikeresseg_egyetem[i] <- B$Sikeresseg_munka[i]
    B$Szeret_egyetem[i] <- B$Szeret_munka[i]
    B$Boldog_egyetem[i] <- B$Boldog_munka[i]
  }
}
colnames(B)[colnames(B) %in% c("Megmerettetes_egyetem", "Sikeresseg_egyetem", "Szeret_egyetem", "Boldog_egyetem")] <- c("Megmerettetes", "Sikeresseg", "Szeret", "Boldog")
B[, c("Megmerettetes_munka", "Sikeresseg_munka", "Szeret_munka", "Boldog_munka")] <- c(NULL, NULL, NULL, NULL)
colnames(B)
dim(B)

#A közösségi média felületen töltött idõ egységesen(percben)
perc <- function(x){
  if(is.na(as.numeric(as.character(x))))
     {x <- 0}
  else {as.numeric(x)}
}

A$Kozossegi_media_idotoltes_perc <- 0
for (i in 1:length(A$Kozossegi_media_idotoltes)){
  A$Kozossegi_media_idotoltes_perc[i] <- as.numeric(substr(A$Kozossegi_media_idotoltes[i], 1, 2))*60 + perc(x = substr(A$Kozossegi_media_idotoltes[i], 7,9))
}
mean(A$Kozossegi_media_idotoltes_perc)

B$Kozossegi_media_idotoltes_perc <- 0
for (i in 1:length(B$Kozossegi_media_idotoltes)){
  B$Kozossegi_media_idotoltes_perc[i] <- as.numeric(substr(B$Kozossegi_media_idotoltes[i], 1, 2))*60 + perc(x = substr(B$Kozossegi_media_idotoltes[i], 7,9))
}
mean(B$Kozossegi_media_idotoltes_perc)

###########Kitöltõk tulajdonságai#############

for(i in 1:length(A$Nem)){
  A$index[i] <- i
}
for(i in 1:length(B$Nem)){
  B$index[i] <- i
}

for(i in 1:length(A$Nem)){
  if (A$Iskolai_vegzettseg[i] == "Középiskolát végeztem, jelenleg dolgozom"){A$Iskolai_vegzettseg_index[i] = "Dolgozó(középisk.)"}
  else if (A$Iskolai_vegzettseg[i] == "Folyamatban lévõ egyetemei tanulmányok(BSc/BA)"){A$Iskolai_vegzettseg_index[i] = "Egyetem-BSc"}
  else if (A$Iskolai_vegzettseg[i] == "Folyamatban lévõ egyetemei tanulmányok(MSc/MA)"){A$Iskolai_vegzettseg_index[i] = "Egyetem-MSc"}
  else {A$Iskolai_vegzettseg_index[i] = "Dolgozó(egyetem)"}
}
for(i in 1:length(B$Nem)){
  if (B$Iskolai_vegzettseg[i] == "Középiskolát végeztem, jelenleg dolgozom"){B$Iskolai_vegzettseg_index[i] = "Dolgozó(középisk.)"}
  else if (B$Iskolai_vegzettseg[i] == "Folyamatban lévõ egyetemei tanulmányok(BSc/BA)"){B$Iskolai_vegzettseg_index[i] = "Egyetem-BSc"}
  else if (B$Iskolai_vegzettseg[i] == "Folyamatban lévõ egyetemei tanulmányok(MSc/MA)"){B$Iskolai_vegzettseg_index[i] = "Egyetem-MSc"}
  else {B$Iskolai_vegzettseg_index[i] = "Dolgozó(egyetem)"}
}

library(ggplot2)
jpeg(filename="C:/Users/dell/Desktop/szakdoga/diagramok_kepek/iskolai_vegzettseg_eloszlasa.jpg")
require(gridExtra)
plot1 = ggplot(A) + aes(Iskolai_vegzettseg_index) +
  geom_bar(stat="count", width = 1, colour = "black", fill = "lightcoral") +
  geom_text(stat = "count", aes(label=..count..), vjust=-1) +
  xlab("") +
  ylab("") +
  ggtitle("'A' kitöltõk") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot2 = ggplot(B) + aes(Iskolai_vegzettseg_index) +
  geom_bar(stat="count", width = 1, colour = "black", fill = "lightcoral") +
  geom_text(stat = "count", aes(label=..count..), vjust=-1) +
  xlab("") +
  ylab("") +
  ggtitle("'B' kitöltõk") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(plot1, plot2, ncol=2)
dev.off()

#Korcsoportok létrehozása: 18-21, 22-25, 26-30, 30 felett

jpeg(filename="C:/Users/dell/Desktop/szakdoga/diagramok_kepek/korok_eloszlasa.jpg")
par(mfrow=c(1,2))
plot(A$index, A$Kor,xlab = "'A' kitöltõinek indexe", ylab = "Kor", type = "p", main = "", ylim = c(15,60))
plot(B$index, B$Kor,xlab = "'B' kitöltõinek indexe", ylab = "Kor", type = "p", main = "", ylim = c(15,60))
dev.off()

A$Kor_csoport <- 0
for (i in 1:length(A$Kor)){
  if (A$Kor[i] > 17 && A$Kor[i] < 22) {A$Kor_csoport[i] = "18-21"}
  else if (A$Kor[i] > 21 && A$Kor[i] < 26) {A$Kor_csoport[i] = "22-25"}
  else if (A$Kor[i] > 25 && A$Kor[i] < 31) {A$Kor_csoport[i] = "26-30"}
  else {A$Kor_csoport[i] = "30 <"}
}

B$Kor_csoport <- 0
for (i in 1:length(B$Kor)){
  if (B$Kor[i] > 17 && B$Kor[i] < 22) {B$Kor_csoport[i] = "18-21"}
  else if (B$Kor[i] > 21 && B$Kor[i] < 26) {B$Kor_csoport[i] = "22-25"}
  else if (B$Kor[i] > 25 && B$Kor[i] < 31) {B$Kor_csoport[i] = "26-30"}
  else {B$Kor_csoport[i] = "30 <"}
}

###Kontingenciatáblák###

# Altáblázatok

Nem_Vegzettseg_A <- table(A$Nem, A$Iskolai_vegzettseg)
Nem_Vegzettseg_B <- table(B$Nem, B$Iskolai_vegzettseg)

Kor_Vegzettseg_A <- table(A$Kor_csoport, A$Iskolai_vegzettseg)
Kor_Vegzettseg_B <- table(B$Kor_csoport, B$Iskolai_vegzettseg)

Nem_kor_A <- table(A$Nem, A$Kor_csoport)
Nem_kor_B <- table(B$Nem, B$Kor_csoport)

# Kontingencia táblák A-t és B-t összehasonlítva

Kontingencia_Kor <- matrix(c(rowSums(Kor_Vegzettseg_A), rowSums(Kor_Vegzettseg_B)), nrow = 2, ncol = 4, byrow = T)
colnames(Kontingencia_Kor) <- c("18-21", "22-25", "26-30", "30 <")
rownames(Kontingencia_Kor) <- c("A", "B")
chisq.test(Kontingencia_Kor)

Kontingencia_Vegzettseg <- matrix(c(colSums(Kor_Vegzettseg_A), colSums(Kor_Vegzettseg_B)), nrow = 2, ncol = 4, byrow = T)
colnames(Kontingencia_Vegzettseg) <- c("Dolgozó(egyetem)", "Egyetem(BSc)", "Egyetem(MSc)", "Dolgozó(középiskola)")
rownames(Kontingencia_Vegzettseg) <- c("A", "B")
chisq.test(Kontingencia_Vegzettseg)

Kontingencia_Nem <- matrix(c(rowSums(Nem_kor_A), rowSums(Nem_kor_B)), nrow = 2, ncol = 2, byrow = T)
colnames(Kontingencia_Nem) <- c("Férfi", "Nõ")
rownames(Kontingencia_Nem) <- c("A", "B")
chisq.test(Kontingencia_Nem)

############1.rész - Kérdések sorrendje############

####t-testek####

kerdesek <- colnames(A[5:8])
# Összesített
p_ertek_ossz <- matrix(0, nrow = 1, ncol = 4)
rownames(p_ertek_ossz) <- "Összes"
colnames(p_ertek_ossz) <- kerdesek
A_atlag_ossz <- matrix(0, nrow = 1, ncol = 4)
rownames(A_atlag_ossz) <- "Összes"
colnames(A_atlag_ossz) <- paste0(kerdesek, "_A")
B_atlag_ossz <- matrix(0, nrow = 1, ncol = 4)
rownames(B_atlag_ossz) <- "Összes"
colnames(B_atlag_ossz) <- paste0(kerdesek, "_B")
for(i in 1:length(kerdesek)){
   if (var.test(A[,kerdesek[i]], B[,kerdesek[i]])$p.value <= 0.05){
    variance <- F }
   else{variance <- T}
   p_ertek_ossz[1,i] <- t.test(A[,kerdesek[i]], B[, kerdesek[i]], var.equal = variance)$p.value
   A_atlag_ossz[1,i] <- mean(A[,kerdesek[i]])
   B_atlag_ossz[1,i] <- mean(B[,kerdesek[i]])
}
p_ertek_ossz

# Korcsoportonként
kor <- sort(unique(A$Kor_csoport)) # "18-21", "22-25", "26-30", "30 <"

p_ertek_kor <- matrix(0, nrow = 4, ncol = 4)
A_atlag_kor <- matrix(0, nrow = 4, ncol = 4)
B_atlag_kor <- matrix(0, nrow = 4, ncol = 4)
rownames(p_ertek_kor) <- kor
colnames(p_ertek_kor) <- kerdesek
rownames(A_atlag_kor) <- kor
colnames(A_atlag_kor) <- paste0(kerdesek, "_A")
rownames(B_atlag_kor) <- kor
colnames(B_atlag_kor) <- paste0(kerdesek, "_B")
for(j in 1:length(kerdesek)){
  for(i in 1:length(kor)){
    if (var.test(A[A$Kor_csoport == kor[i],kerdesek[j]],  B[B$Kor_csoport == kor[i],kerdesek[j]])$p.value <= 0.05){
      variance <- F}
    else {variance <- T}
    
    p_ertek_kor[i,j] <- t.test(A[A$Kor_csoport == kor[i],kerdesek[j]],  B[B$Kor_csoport == kor[i],kerdesek[j]], var.test = variance)$p.value
    A_atlag_kor[i,j] <- mean(A[A$Kor_csoport == kor[i], kerdesek[j]])
    B_atlag_kor[i,j] <- mean(B[B$Kor_csoport == kor[i], kerdesek[j]])
    
}
}
p_ertek_kor

# Nemenként
nem <- sort(unique(A$Nem))# "Férfi", "Nõ"
p_ertek_nem <- matrix(0, nrow = 2, ncol = 4)
colnames(p_ertek_nem) <- kerdesek
rownames(p_ertek_nem) <- c("Férfi", "Nõ")
A_atlag_nem <- matrix(0, nrow = 2, ncol = 4)
colnames(A_atlag_nem) <- paste0(kerdesek, "_A")
rownames(A_atlag_nem) <- c("Férfi", "Nõ")
B_atlag_nem <- matrix(0, nrow = 2, ncol = 4)
colnames(B_atlag_nem) <- paste0(kerdesek, "_B")
rownames(B_atlag_nem) <- c("Férfi", "Nõ")
for(j in 1:length(kerdesek)){
  for(i in 1:length(nem)){
    if (var.test(A[A$Nem == nem[i],kerdesek[j]],  B[B$Nem == nem[i],kerdesek[j]])$p.value <= 0.05){
      variance <- F}
    else {variance <- T}
    
    p_ertek_nem[i,j] <- t.test(A[A$Nem == nem[i],kerdesek[j]],  B[B$Nem == nem[i],kerdesek[j]], var.test = variance)$p.value
    A_atlag_nem[i,j] <- mean(A[A$Nem == nem[i], kerdesek[j]])
    B_atlag_nem[i,j] <- mean(B[B$Nem == nem[i], kerdesek[j]])
    
  }
}

p_ertek_nem

# Végzettség szerint
iskolai_vegzettseg <- sort(unique(A$Iskolai_vegzettseg))# "Dolgozó(egyetem)", "Egyetem(Bsc)", "Egyetem(Msc)", "Dolgozó(középiskola)"
p_ertek_vegzettseg <- matrix(0, nrow = 4, ncol = 4)
colnames(p_ertek_vegzettseg) <- kerdesek
rownames(p_ertek_vegzettseg) <- c("Dolgozó(egyetem)", "Egyetem(Bsc)", "Egyetem(Msc)", "Dolgozó(középiskola)")
A_atlag_vegzettseg <- matrix(0, nrow = 4, ncol = 4)
colnames(A_atlag_vegzettseg) <- kerdesek
rownames(A_atlag_vegzettseg) <- c("Dolgozó(egyetem)", "Egyetem(Bsc)", "Egyetem(Msc)", "Dolgozó(középiskola)")
B_atlag_vegzettseg <- matrix(0, nrow = 4, ncol = 4)
colnames(B_atlag_vegzettseg) <- kerdesek
rownames(B_atlag_vegzettseg) <- c("Dolgozó(egyetem)", "Egyetem(Bsc)", "Egyetem(Msc)", "Dolgozó(középiskola)")
for(j in 1:length(kerdesek)){
  for(i in 1:length(iskolai_vegzettseg)){
    if (var.test(A[A$Iskolai_vegzettseg == iskolai_vegzettseg[i],kerdesek[j]],  B[B$Iskolai_vegzettseg == iskolai_vegzettseg[i],kerdesek[j]])$p.value <= 0.05){
      variance <- F}
    else {variance <- T}
    
    p_ertek_vegzettseg[i,j] <- t.test(A[A$Iskolai_vegzettseg == iskolai_vegzettseg[i],kerdesek[j]],  B[B$Iskolai_vegzettseg == iskolai_vegzettseg[i],kerdesek[j]], var.test = variance)$p.value
    A_atlag_vegzettseg[i,j] <- mean(A[A$Iskolai_vegzettseg == iskolai_vegzettseg[i], kerdesek[j]])
    B_atlag_vegzettseg[i,j] <- mean(B[B$Iskolai_vegzettseg == iskolai_vegzettseg[i], kerdesek[j]])
  }
}
p_ertek_vegzettseg

# p-értékek egy táblázatban
p_ertek <- rbind(p_ertek_ossz, p_ertek_kor)
p_ertek <- rbind(p_ertek, p_ertek_nem)
p_ertek <- rbind(p_ertek, p_ertek_vegzettseg)
round(p_ertek, digits = 3)

A_atlag <- rbind(A_atlag_ossz, A_atlag_kor)
A_atlag <- rbind(A_atlag, A_atlag_nem)
A_atlag <- round(rbind(A_atlag, A_atlag_vegzettseg), digits = 2)

B_atlag <- rbind(B_atlag_ossz, B_atlag_kor)
B_atlag <- rbind(B_atlag, B_atlag_nem)
B_atlag <- round(rbind(B_atlag, B_atlag_vegzettseg), digits = 2)


atlag <- cbind(A_atlag, B_atlag)
atlag_2 <- atlag[,c(1,5,2,6,3,7,4,8)]

###Chi-négyzet próbák###

k = kerdesek
#A: Boldog-Megmérettetés; Boldog-Sikeres; Boldog-Szeret
boldog_megmerettetes_A <- matrix(0, nrow = 5, ncol = 5)
boldog_sikeres_A <- matrix(0, nrow = 5, ncol = 5)
boldog_szeret_A <- matrix(0, nrow = 5, ncol = 5)
szeret_sikeres_A <- matrix(0, nrow = 5, ncol = 5)
szeret_megmerettetes_A <- matrix(0, nrow = 5, ncol = 5)
sikeres_megmerettetes_A <- matrix(0, nrow = 5, ncol = 5)

for(i in c(1:5)){
  for(j in c(1:5)){
    boldog_megmerettetes_A[i,j] = nrow(A[A$Boldog == i & A$Megmerettetes == j, ])
    boldog_szeret_A[i,j] = nrow(A[A$Boldog == i & A$Szeret == j, ])
    boldog_sikeres_A[i,j] = nrow(A[A$Boldog == i & A$Sikeresseg == j, ])
    szeret_sikeres_A[i,j] = nrow(A[A$Szeret == i & A$Sikeresseg == j, ])
    szeret_megmerettetes_A[i,j] = nrow(A[A$Szeret == i & A$Megmerettetes == j, ])
    sikeres_megmerettetes_A[i,j] = nrow(A[A$Szeret == i & A$Megmerettetes == j, ])
  }
}
rownames(boldog_megmerettetes_A) <- lapply(c(1:5), function(x) paste("Boldog",x))
colnames(boldog_megmerettetes_A) <- lapply(c(1:5), function(x) paste("Megmer",x))

rownames(boldog_szeret_A) <- lapply(c(1:5), function(x) paste("Boldog",x))
colnames(boldog_szeret_A) <- lapply(c(1:5), function(x) paste("Szeret",x))

rownames(boldog_sikeres_A) <- lapply(c(1:5), function(x) paste("Boldog",x))
colnames(boldog_sikeres_A) <- lapply(c(1:5), function(x) paste("Sikeres",x))

rownames(szeret_sikeres_A) <- lapply(c(1:5), function(x) paste("Szeret",x))
colnames(szeret_sikeres_A) <- lapply(c(1:5), function(x) paste("Sikeres",x))

rownames(szeret_megmerettetes_A) <- lapply(c(1:5), function(x) paste("Szeret",x))
colnames(szeret_megmerettetes_A) <- lapply(c(1:5), function(x) paste("Megmer",x))

rownames(sikeres_megmerettetes_A) <- lapply(c(1:5), function(x) paste("Sikeres",x))
colnames(sikeres_megmerettetes_A) <- lapply(c(1:5), function(x) paste("Megmer",x))


A1 <- chisq.test(boldog_szeret_A)$p.value
A2 <- chisq.test(boldog_sikeres_A)$p.value
A3 <- chisq.test(boldog_megmerettetes_A)$p.value
A4 <- chisq.test(szeret_sikeres_A)$p.value
A5 <- chisq.test(szeret_megmerettetes_A)$p.value
A6 <- chisq.test(sikeres_megmerettetes_A)$p.value

#B: Boldog-Megmérettetés; Boldog-Sikeres; Boldog-Szeret

boldog_megmerettetes_B <- matrix(0, nrow = 5, ncol = 5)
boldog_szeret_B <- matrix(0, nrow = 5, ncol = 5)
boldog_sikeres_B <- matrix(0, nrow = 5, ncol = 5)
szeret_sikeres_B <- matrix(0, nrow = 5, ncol = 5)
szeret_megmerettetes_B <- matrix(0, nrow = 5, ncol = 5)
sikeres_megmerettetes_B <- matrix(0, nrow = 5, ncol = 5)

for(i in c(1:5)){
  for(j in c(1:5)){
    boldog_megmerettetes_B[i,j] = nrow(B[B$Boldog == i & B$Megmerettetes == j, ])
    boldog_szeret_B[i,j] = nrow(B[B$Boldog == i & B$Szeret == j, ])
    boldog_sikeres_B[i,j] = nrow(B[B$Boldog == i & B$Sikeresseg == j, ])
    szeret_sikeres_B[i,j] = nrow(B[B$Szeret == i & B$Sikeresseg == j, ])
    szeret_megmerettetes_B[i,j] = nrow(B[B$Szeret == i & B$Megmerettetes == j, ])
    sikeres_megmerettetes_B[i,j] = nrow(B[B$Szeret == i & B$Megmerettetes == j, ])
  }
}
rownames(boldog_megmerettetes_B) <- lapply(c(1:5), function(x) paste("Boldog",x))
colnames(boldog_megmerettetes_B) <- lapply(c(1:5), function(x) paste("Megmer",x))

rownames(boldog_szeret_B) <- lapply(c(1:5), function(x) paste("Boldog",x))
colnames(boldog_szeret_B) <- lapply(c(1:5), function(x) paste("Szeret",x))

rownames(boldog_sikeres_B) <- lapply(c(1:5), function(x) paste("Boldog",x))
colnames(boldog_sikeres_B) <- lapply(c(1:5), function(x) paste("Sikeres",x))

rownames(szeret_sikeres_B) <- lapply(c(1:5), function(x) paste("Szeret",x))
colnames(szeret_sikeres_B) <- lapply(c(1:5), function(x) paste("Sikeres",x))

rownames(szeret_megmerettetes_B) <- lapply(c(1:5), function(x) paste("Szeret",x))
colnames(szeret_megmerettetes_B) <- lapply(c(1:5), function(x) paste("Megmer",x))

rownames(sikeres_megmerettetes_B) <- lapply(c(1:5), function(x) paste("Sikeres",x))
colnames(sikeres_megmerettetes_B) <- lapply(c(1:5), function(x) paste("Megmer",x))

B1 <- chisq.test(boldog_szeret_B)$p.value
B2 <- chisq.test(boldog_sikeres_B)$p.value
B3 <- chisq.test(boldog_megmerettetes_B)$p.value
B4 <- chisq.test(szeret_sikeres_B)$p.value
B5 <- chisq.test(szeret_megmerettetes_B)$p.value
B6 <- chisq.test(sikeres_megmerettetes_B)$p.value


chiteszt <- round(matrix(c(A1,A2,A3,A4,A5,A6,B1,B2,B3,B4,B5,B6), nrow = 2, ncol = 6, byrow = T), digits = 3)
rownames(chiteszt) <- c('A', 'B')
colnames(chiteszt) <- c("B-Sz", "B-S", "B-M", "Sz-S", "Sz-M", "S-M")

##########korrelációs próba##############

library(psych)

A_corr <- A[,c(5,6,7,8,1,2,3,4,9,10,11,12,13,14,15,16,17,18,19,20)]
B_corr <- B[,c(8,7,6,5,1,2,3,4,9,10,11,12,13,14,15,16,17,18,19,20)]
 
n1 <- nrow(A)
n2 <- nrow(B)
korrelacio_teszt_r1 <- 0
korrelacio_teszt_r2 <- 0
korrelacio_teszt_p <- 0

for(i in 1:3){
  for(j in (i+1):4){
    r1 = cor.test(A_corr[,i], A_corr[,j], method = "p")$estimate
    r2 = cor.test(B_corr[,i], B_corr[,j], method = "p")$estimate
    if(i == 1){
      korrelacio_teszt_r1[j-i] = r1
      korrelacio_teszt_r2[j-i] = r2
      korrelacio_teszt_p[j-i] = paired.r(r1,r2,NULL,n1,n2)$p}
    if(i == 2 || i == 3) {
      korrelacio_teszt_r1[i+j-1] = r1
      korrelacio_teszt_r2[i+j-1] = r2
      korrelacio_teszt_p[i+j-1] = paired.r(r1,r2,NULL,n1,n2)$p}
    
  }
}

corrteszt <- matrix(0, nrow = 6, ncol = 3)
corrteszt[,1] <- t(t(round(korrelacio_teszt_r1, digits = 3)))
corrteszt[,2] <- t(t(round(korrelacio_teszt_r2, digits = 3)))
corrteszt[,3] <- t(t(round(korrelacio_teszt_p, digits = 3)))
rownames(corrteszt) <- c("Boldog-Szeret", "Boldog-Sikeresség", "Boldog-Megmérettetés", "Szeret-Sikeresség", "Szeret-Megmérettetés", "Sikeresség-Megmérettetés")
colnames(corrteszt) <- c("r1-A", "r2-B", "Korr.teszt-p")

#############2. rész - "tippelõs" #############

var.test(A$Legtobb_kredit, B$Legtobb_kredit)
t.test(A$Legtobb_kredit, B$Legtobb_kredit, var.equal = FALSE) # szignifikáns eltérés van a két kérdõív között

var.test(A$Kredittullepok_szazaleka, B$Kredittullepok_szazaleka)
t.test(A$Kredittullepok_szazaleka, B$Kredittullepok_szazaleka, var.equal = FALSE) # szignifikáns eltérés van a két kérdõív között

anova_valtozok <- c("Nem", "Kor_csoport", "Iskolai_vegzettseg")
egyszempontos_anova_kredit <- matrix(0, nrow = 2, ncol = 3)
egyszempontos_anova_szazalek <- matrix(0,nrow = 2, ncol = 3)
colnames(egyszempontos_anova_kredit) <- anova_valtozok
rownames(egyszempontos_anova_kredit) <- c("A", "B")
colnames(egyszempontos_anova_szazalek) <- anova_valtozok
rownames(egyszempontos_anova_szazalek) <- c("A", "B")
for(i in 1:3){
  egyszempontos_anova_kredit[1,i] <- round(summary(aov(A$Legtobb_kredit ~ A[,anova_valtozok[i]], data = A))[[1]][["Pr(>F)"]][1], digits = 3)
  egyszempontos_anova_kredit[2,i] <- round(summary(aov(B$Legtobb_kredit ~ B[,anova_valtozok[i]], data = B))[[1]][["Pr(>F)"]][1], digits = 3)
  egyszempontos_anova_szazalek[1,i] <- round(summary(aov(A$Kredittullepok_szazaleka ~ A[,anova_valtozok[i]], data = A))[[1]][["Pr(>F)"]][1], digits = 3)
  egyszempontos_anova_szazalek[2,i] <- round(summary(aov(B$Kredittullepok_szazaleka ~ B[,anova_valtozok[i]], data = B))[[1]][["Pr(>F)"]][1], digits = 3)
}

tulajdonsagok <- c("A", "B", "H.index", "A", "B", "H.index")
horgony_nem <- matrix(0, nrow = 2, ncol = 6)
rownames(horgony_nem) <- nem
colnames(horgony_nem) <- tulajdonsagok
for(i in 1:length(nem)){
    horgony_nem[i,1] <- mean(A[A$Nem == nem[i],][,"Legtobb_kredit"])
    horgony_nem[i,2] <- mean(B[B$Nem == nem[i],][,"Legtobb_kredit"])
    horgony_nem[i,3] <- ((as.numeric(horgony_nem[i,1])-as.numeric(horgony_nem[i,2]))/(75-52))*100
    horgony_nem[i,4] <- mean(A[A$Nem == nem[i],][,"Kredittullepok_szazaleka"])
    horgony_nem[i,5] <- mean(B[B$Nem == nem[i],][,"Kredittullepok_szazaleka"])
    horgony_nem[i,6] <- ((as.numeric(horgony_nem[i,4])-as.numeric(horgony_nem[i,5]))/(60-20))*100
}

B_szazalek_kor_anova <- summary(aov(B$Kredittullepok_szazaleka ~ B[,"Kor_csoport"], data = B))

B_szazalek_vegz_anova <- summary(aov(B$Kredittullepok_szazaleka ~ B[,"Iskolai_vegzettseg"], data = B))

horgony_kor <- matrix(0, nrow = 4, ncol = 6)
rownames(horgony_kor) <- kor
colnames(horgony_kor) <- tulajdonsagok
for(i in 1:length(kor)){
  horgony_kor[i,1] <- mean(A[A$Kor_csoport == kor[i],][,"Legtobb_kredit"])
  horgony_kor[i,2] <- mean(B[B$Kor_csoport == kor[i],][,"Legtobb_kredit"])
  horgony_kor[i,3] <- ((as.numeric(horgony_kor[i,1])-as.numeric(horgony_kor[i,2]))/(75-52))*100
  horgony_kor[i,4] <- mean(A[A$Kor_csoport == kor[i],][,"Kredittullepok_szazaleka"])
  horgony_kor[i,5] <- mean(B[B$Kor_csoport == kor[i],][,"Kredittullepok_szazaleka"])
  horgony_kor[i,6] <- ((as.numeric(horgony_kor[i,4])-as.numeric(horgony_kor[i,5]))/(60-20))*100
}

horgony_vegzettseg <- matrix(0, nrow = 4, ncol = 6)
rownames(horgony_vegzettseg) <- iskolai_vegzettseg
colnames(horgony_vegzettseg) <- tulajdonsagok
for(i in 1:length(iskolai_vegzettseg)){
  horgony_vegzettseg[i,1] <- mean(A[A$Iskolai_vegzettseg == iskolai_vegzettseg[i],][,"Legtobb_kredit"])
  horgony_vegzettseg[i,2] <- mean(B[B$Iskolai_vegzettseg == iskolai_vegzettseg[i],][,"Legtobb_kredit"])
  horgony_vegzettseg[i,3] <- ((as.numeric(horgony_vegzettseg[i,1])-as.numeric(horgony_vegzettseg[i,2]))/(75-52))*100
  horgony_vegzettseg[i,4] <- mean(A[A$Iskolai_vegzettseg == iskolai_vegzettseg[i],][,"Kredittullepok_szazaleka"])
  horgony_vegzettseg[i,5] <- mean(B[B$Iskolai_vegzettseg == iskolai_vegzettseg[i],][,"Kredittullepok_szazaleka"])
  horgony_vegzettseg[i,6] <- ((as.numeric(horgony_vegzettseg[i,4])-as.numeric(horgony_vegzettseg[i,5]))/(60-20))*100
}

horgony <- rbind(horgony_nem, horgony_kor)
horgony <- round(rbind(horgony, horgony_vegzettseg), digits = 1)

#####Kétszempontos ANOVA#####

quantile(A$Legtobb_kredit, probs = c(0,17,34,51,68,85,100)/100)
befolyasolas_A <- 0
for(i in 1:nrow(A)){
  if(A$Legtobb_kredit[i] > 63 & A$Legtobb_kredit[i] <= 80){A$befolyasolas[i] = "nagyon"}
  else if((A$Legtobb_kredit[i] > 55 & A$Legtobb_kredit[i] <= 63) || (A$Legtobb_kredit[i] > 80 & A$Legtobb_kredit[i] <= 85)){A$befolyasolas[i] = "kicsit"}
  else {A$befolyasolas[i] = "nincs"}
}
nrow(A[A$befolyasolas == "nagyon",])
nrow(A[A$befolyasolas == "kicsit",])
nrow(A[A$befolyasolas == "nincs",])
quantile(B$Legtobb_kredit, probs = c(0,17,34,51,68,85,100)/100)
befolyasolas_B <- 0
for(i in 1:nrow(B)){
  if(B$Legtobb_kredit[i] > 46 & B$Legtobb_kredit[i] <= 57){B$befolyasolas[i] = "nagyon"}
  else if((B$Legtobb_kredit[i] > 44 & B$Legtobb_kredit[i] <= 46) || (B$Legtobb_kredit[i] > 57 & B$Legtobb_kredit[i] <= 61)){B$befolyasolas[i] = "kicsit"}
  else {B$befolyasolas[i] = "nincs"}
}
nrow(B[B$befolyasolas == "nagyon",])
nrow(B[B$befolyasolas == "kicsit",])
nrow(B[B$befolyasolas == "nincs",])
colnames(B)
tabla<-0
for(i in 1:nrow(A)){
  A$tabla[i] <- "A"
}

colnames(B)
for(i in 1:nrow(B)){
  B$tabla[i] <- "B"
}
B2<-B[,c(1,2,3,4,8,7,6,5,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
colnames(B2)
C <- rbind(A,B2)
dim(C)

ketszempontos_anova_kszazalek <- summary(aov(Kredittullepok_szazaleka ~ tabla * befolyasolas, data = C))

#boxplotok 

colnames(C)
jpeg(filename="C:/Users/dell/Desktop/szakdoga/diagramok_kepek/boxplot_tabla_kredit.jpg")
par(mar =c(5.1, 4.1, 4.1, 2.1))
boxplot(C$Legtobb_kredit ~ C$tabla, xlab = "Kérdõívek", ylab = "Legtöbb kredit", main = "Kérdõívek szerinti legtöbb kredit tippre vonatkozó boxplot ábra" )
dev.off()
median(A$Legtobb_kredit)
median(B$Legtobb_kredit)

jpeg(filename="C:/Users/dell/Desktop/szakdoga/diagramok_kepek/boxplot_tabla_szazalek.jpg")
par(mar =c(5.1, 4.1, 4.1, 2.1))
boxplot(C$Kredittullepok_szazaleka ~ C$tabla, xlab = "Kérdõívek", ylab = "Kredittúllépõk (%)", main = "Kérdõívek szerinti kredittúllépési tippre vonatkozó boxplot ábra")
dev.off()
median(A$Kredittullepok_szazaleka)
median(B$Kredittullepok_szazaleka)

jpeg(filename="C:/Users/dell/Desktop/szakdoga/diagramok_kepek/boxplot_Bnem_kredit.jpg")
par(mar =c(5.1, 4.1, 4.1, 2.1))
boxplot(B$Legtobb_kredit ~ B$Nem, xlab = "Nem", ylab = "Legtöbb kredit", main = "Nem szerinti legtöbb kredit tippre vonatkozó \nboxplot ábra - B kérdõív")
dev.off()
median(B[B$Nem == "Nõ",][,"Legtobb_kredit"])
median(B[B$Nem == "Férfi",][,"Legtobb_kredit"])


for(i in 1:length(B$Nem)){
  if (B$Iskolai_vegzettseg[i] == "Középiskolát végeztem, jelenleg dolgozom"){B$Iskolai_vegzettseg_index_2[i] = "Dolgozó-ki."}
  else if (B$Iskolai_vegzettseg[i] == "Folyamatban lévõ egyetemei tanulmányok(BSc/BA)"){B$Iskolai_vegzettseg_index_2[i] = "Egyetem-BSc"}
  else if (B$Iskolai_vegzettseg[i] == "Folyamatban lévõ egyetemei tanulmányok(MSc/MA)"){B$Iskolai_vegzettseg_index_2[i] = "Egyetem-MSc"}
  else {B$Iskolai_vegzettseg_index_2[i] = "Dolgozó-e."}
}

jpeg(filename="C:/Users/dell/Desktop/szakdoga/diagramok_kepek/boxplot_Bisk_szazalek.jpg")
par(mar =c(5.1, 4.1, 4.1, 2.1))
boxplot(B$Kredittullepok_szazaleka ~ B$Iskolai_vegzettseg_index_2, xlab = "Iskolai státusz", ylab = "Kredittúllépõk (%)", main = "Iskolai státusz szerinti kredittúllépési tippre vonatkozó \nboxplot ábra - B kérdõív")
dev.off()
median(B[B$Kor_csoport == "18-21",][,"Kredittullepok_szazaleka"])
median(B[B$Kor_csoport == "22-25",][,"Kredittullepok_szazaleka"])
median(B[B$Kor_csoport == "26-30",][,"Kredittullepok_szazaleka"])
median(B[B$Kor_csoport == "30 <",][,"Kredittullepok_szazaleka"])

jpeg(filename="C:/Users/dell/Desktop/szakdoga/diagramok_kepek/boxplot_Akor_szazalek.jpg")
par(mar =c(5.1, 4.1, 4.1, 2.1))
boxplot(A$Kredittullepok_szazaleka ~ A$Kor_csoport, xlab = "Korcsoport", ylab = "Kredittúllépõk (%)", main = "Korcsoport szerinti kredittúllépési tippre vonatkozó \nboxplot ábra - A kérdõív")
dev.off()
median(A[A$Kor_csoport == "18-21",][,"Kredittullepok_szazaleka"])
median(A[A$Kor_csoport == "22-25",][,"Kredittullepok_szazaleka"])
median(A[A$Kor_csoport == "26-30",][,"Kredittullepok_szazaleka"])
median(A[A$Kor_csoport == "30 <",][,"Kredittullepok_szazaleka"])

jpeg(filename="C:/Users/dell/Desktop/szakdoga/diagramok_kepek/boxplot_Bkor_szazalek.jpg")
par(mar =c(5.1, 4.1, 4.1, 2.1))
boxplot(B$Kredittullepok_szazaleka ~ B$Kor_csoport, xlab = "Korcsoport", ylab = "Kredittúllépõk (%)", main = "Korcsoport szerinti kredittúllépési tippre vonatkozó \nboxplot ábra - B kérdõív")
dev.off()
median(B[B$Iskolai_vegzettseg_index_2 == "Dolgozó-e.",][,"Kredittullepok_szazaleka"])
median(B[B$Iskolai_vegzettseg_index_2 == "Dolgozó-ki.",][,"Kredittullepok_szazaleka"])
median(B[B$Iskolai_vegzettseg_index_2 == "Egyetem-BSc",][,"Kredittullepok_szazaleka"])
median(B[B$Iskolai_vegzettseg_index_2 == "Egyetem-MSc",][,"Kredittullepok_szazaleka"])

#############3. rész - képek #############

var.test(A$Kozossegi_media_idotoltes_perc, B$Kozossegi_media_idotoltes_perc)
t.test(A$Kozossegi_media_idotoltes_perc, B$Kozossegi_media_idotoltes_perc, var.equal = T) #nincs szignifikáns eltérés, csak 5 perccel nagyobb az átlaga A-nak

A_kevesebb <- A[A$Atlaghoz_kepest == "Kevesebb",]
nrow(A_kevesebb)
A_tobb <- A[A$Atlaghoz_kepest == "Több",]
nrow(A_tobb)

B_kevesebb <- B[B$Atlaghoz_kepest == "Kevesebb",]
nrow(B_kevesebb)
B_tobb <- B[B$Atlaghoz_kepest == "Több",]
nrow(B_tobb)
#Ez csak érdekes, hogy sokkal többen gondolják, hogy átlagnál kevesebb idõt töltenek el így, minthogy általágnál többet.
var.test(A$Felesleges_szazalek, B$Felesleges_szazalek)
t.test(A$Felesleges_szazalek, B$Felesleges_szazalek, var.equal = T) # itt viszont szignifikáns különbség van! 

ketszempontos_anova_media1 <- summary(aov(Kozossegi_media_idotoltes_perc ~ Nem * Kor_csoport, data = C))
ketszempontos_anova_media2 <- summary(aov(Kozossegi_media_idotoltes_perc ~ Iskolai_vegzettseg * Kor_csoport, data = C))

media_nem <- matrix(0, nrow = 1, ncol = 2)
colnames(media_nem) <- nem
for (i in 1:length(nem)){
  media_nem[i] <- mean(C[C$Nem == nem[i],][, "Kozossegi_media_idotoltes_perc"])
}

media_kor <- matrix(0, nrow = 1, ncol = 4)
colnames(media_kor) <- kor
for (i in 1:length(kor)){
  media_kor[i] <- mean(C[C$Kor_csoport == kor[i],][, "Kozossegi_media_idotoltes_perc"])
}

media_vegzettseg <- matrix(0, nrow = 1, ncol = 4)
colnames(media_vegzettseg) <- iskolai_vegzettseg
for (i in 1:length(kor)){
  media_vegzettseg[i] <- mean(C[C$Iskolai_vegzettseg == iskolai_vegzettseg[i],][, "Kozossegi_media_idotoltes_perc"])
}

media <- cbind(media_nem, media_kor)
media <- round(cbind(media, media_vegzettseg), digits = 0)

media_nem_A_B <- matrix(0, nrow = 2, ncol = 2)
colnames(media_nem_A_B) <- nem
rownames(media_nem_A_B) <- c("A", "B")
for (i in 1:length(nem)){
  media_nem_A_B[1,i] <- mean(A[A$Nem == nem[i],][, "Kozossegi_media_idotoltes_perc"])
  media_nem_A_B[2,i] <- mean(B[B$Nem == nem[i],][, "Kozossegi_media_idotoltes_perc"])
}

media_kor_A_B <- matrix(0, nrow = 2, ncol = 4)
colnames(media_kor_A_B) <- kor
rownames(media_kor_A_B) <- c("A", "B")
for (i in 1:length(kor)){
  media_kor_A_B[1,i] <- mean(A[A$Kor_csoport == kor[i],][, "Kozossegi_media_idotoltes_perc"])
  media_kor_A_B[2,i] <- mean(B[B$Kor_csoport == kor[i],][, "Kozossegi_media_idotoltes_perc"])
}

media_vegzettseg_A_B <- matrix(0, nrow = 2, ncol = 4)
colnames(media_vegzettseg_A_B) <- iskolai_vegzettseg
rownames(media_vegzettseg_A_B) <- c("A", "B")
for (i in 1:length(kor)){
  media_vegzettseg_A_B[1,i] <- mean(A[A$Iskolai_vegzettseg == iskolai_vegzettseg[i],][, "Kozossegi_media_idotoltes_perc"])
  media_vegzettseg_A_B[2,i] <- mean(B[B$Iskolai_vegzettseg == iskolai_vegzettseg[i],][, "Kozossegi_media_idotoltes_perc"])
}

media_A_B <- cbind(media_nem_A_B, media_kor_A_B)
media_A_B <- round(cbind(media_A_B, media_vegzettseg_A_B), digits = 0)

#Edzés és hasonló szavak elõfordulása a két kérdõívben
edzes <- c("edzes", "edzés", "sport","mozgas", "mozgás")

A_sport <- sapply(edzes, grepl, A$Felesleges_helyett, ignore.case=TRUE)
sport_elofordulas_A <- 0

for(i in 1:length(A_sport[,1])){
  for(j in 1:length(edzes)){
    if(A_sport[i,j] == TRUE){sport_elofordulas_A = sport_elofordulas_A + 1}
  }
}
sport_elofordulas_A

B_sport <- sapply(edzes, grepl, B$Felesleges_helyett, ignore.case=TRUE)
sport_elofordulas_B <- 0

for(i in 1:length(B_sport[,1])){
  for(j in 1:length(edzes)){
    if(B_sport[i,j] == TRUE){sport_elofordulas_B = sport_elofordulas_B + 1}
  }
}
sport_elofordulas_B

# Takarítás stb elõfordulása a kérdõívekben
hazimunka <- c("hazimunka", "házimunka", "takaritas","takarítás", "háztartás", "haztartas", "rend")
A_takaritas <- sapply(hazimunka, grepl, A$Felesleges_helyett, ignore.case=TRUE)
takaritas_elofordulas_A <- 0

for(i in 1:length(A_takaritas[,1])){
  for(j in 1:length(hazimunka)){
    if(A_takaritas[i,j] == TRUE){takaritas_elofordulas_A = takaritas_elofordulas_A + 1}
  }
}
takaritas_elofordulas_A

B_takaritas <- sapply(hazimunka, grepl, B$Felesleges_helyett, ignore.case=TRUE)
takaritas_elofordulas_B <- 0

for(i in 1:length(B_takaritas[,1])){
  for(j in 1:length(hazimunka)){
    if(B_takaritas[i,j] == TRUE){takaritas_elofordulas_B = takaritas_elofordulas_B + 1}
  }
}
takaritas_elofordulas_B

#Család stb szavak elõfordulása

csalad <- c("csalad", "család", "barat", "barát", "közösség", "kozosseg", "szeretteim")

A_csalad <- sapply(csalad, grepl, A$Felesleges_helyett, ignore.case=TRUE)
csalad_elofordulas_A <- 0

for(i in 1:length(A_csalad[,1])){
  for(j in 1:length(csalad)){
    if(A_csalad[i,j] == TRUE){csalad_elofordulas_A = csalad_elofordulas_A + 1}
  }
}
csalad_elofordulas_A

B_csalad <- sapply(csalad, grepl, B$Felesleges_helyett, ignore.case=TRUE)
csalad_elofordulas_B <- 0

for(i in 1:length(B_csalad[,1])){
  for(j in 1:length(csalad)){
    if(B_csalad[i,j] == TRUE){csalad_elofordulas_B = csalad_elofordulas_B + 1}
  }
}
csalad_elofordulas_B

#Pihenés, természet stb szavak elõfordulása

relax <- c("alvas", "alvás", "pihenes", "pihenés", "relax", "természet", "termeszet", "kirandulas", "kirándulás", "szabadban")
 
A_relax <- sapply(relax, grepl, A$Felesleges_helyett, ignore.case=TRUE)
relax_elofordulas_A <- 0

for(i in 1:length(A_relax[,1])){
  for(j in 1:length(relax)){
    if(A_relax[i,j] == TRUE){relax_elofordulas_A = relax_elofordulas_A + 1}
  }
}
relax_elofordulas_A

B_relax <- sapply(relax, grepl, B$Felesleges_helyett, ignore.case=TRUE)
relax_elofordulas_B <- 0

for(i in 1:length(B_relax[,1])){
  for(j in 1:length(relax)){
    if(B_relax[i,j] == TRUE){relax_elofordulas_B = relax_elofordulas_B + 1}
  }
}
relax_elofordulas_B

tanulas <- c("tanulas", "tanulás", "szakdoga", "vizsga")
A_tanulas <- sapply(tanulas, grepl, A$Felesleges_helyett, ignore.case=TRUE)
tanulas_elofordulas_A <- 0

for(i in 1:length(A_tanulas[,1])){
  for(j in 1:length(tanulas)){
    if(A_tanulas[i,j] == TRUE){tanulas_elofordulas_A = tanulas_elofordulas_A + 1}
  }
}
tanulas_elofordulas_A

B_tanulas <- sapply(tanulas, grepl, B$Felesleges_helyett, ignore.case=TRUE)
tanulas_elofordulas_B <- 0

for(i in 1:length(B_tanulas[,1])){
  for(j in 1:length(tanulas)){
    if(B_tanulas[i,j] == TRUE){tanulas_elofordulas_B = tanulas_elofordulas_B + 1}
  }
}
tanulas_elofordulas_B

#összegzõ táblázat
kont_sport <- matrix(c(sport_elofordulas_A, n1-sport_elofordulas_A, sport_elofordulas_B, n2-sport_elofordulas_B), nrow = 2, ncol = 2, byrow = T)
chisq.test(kont_sport)$p.value
kont_takaritas <- matrix(c(takaritas_elofordulas_A, n1-takaritas_elofordulas_A, takaritas_elofordulas_B, n2-takaritas_elofordulas_B), nrow = 2, ncol = 2, byrow = T)
chisq.test(kont_takaritas)
kont_csalad <- matrix(c(csalad_elofordulas_A, n1-csalad_elofordulas_A, csalad_elofordulas_B, n2-csalad_elofordulas_B), nrow = 2, ncol = 2, byrow = T)
chisq.test(kont_csalad)
kont_relax <- matrix(c(relax_elofordulas_A, n1-relax_elofordulas_A, relax_elofordulas_B, n2-relax_elofordulas_B), nrow = 2, ncol = 2, byrow = T)
chisq.test(kont_relax)
kont_szavak <- matrix(c(chisq.test(kont_sport)$p.value, chisq.test(kont_takaritas)$p.value, chisq.test(kont_csalad)$p.value, chisq.test(kont_relax)$p.value), nrow = 1, ncol = 4, byrow = T)

