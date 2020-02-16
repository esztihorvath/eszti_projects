#c)

# Kedvez?/?sszes alapon ?tlagolunk minden ?vre.

P = 0 * atmin[, , 1]
for (i in 1:dim(atmin)[3]) {
  for (j in 1:dim(atmin)[1]){
    for (k in 1:dim(atmin)[2]){
      P[j, k] <- atmin[j, k, i] / sum(atmin[j, , i]) / dim(atmin)[3] + P[j, k]
    }
  }
}

P

# Kieg?sz?tett?k a P m?trixunkat a k?vetkez? form?ra: P = (Q,R,0,I) [(0,I)-vel eg?sz?tj?k ki].

P <- rbind(P,
        cbind(matrix(0, nrow = dim(atmin)[2] - dim(atmin)[1], ncol = dim(atmin)[1]), diag(c(1, 1))))

# Elnevezz?k az ?j sorainkat eszt?tikai c?lb?l.

rownames(P)[dim(atmin)[1]:dim(atmin)[2]] <- colnames(P)[dim(atmin)[1]:dim(atmin)[2]]

P

# L?trehozzuk a Q ?s R r?szm?trixokat
Q = P[1:dim(atmin)[1], 1:dim(atmin)[1]]
R = P[1:dim(atmin)[1], (dim(atmin)[1]+1):dim(atmin)[2]]

#d)

# K?sz?tett?nk egy m?trixot amelyben kisz?m?tottuk kateg?ri?nk?nt, 
# a cs?d bek?vetkez?s?nek ar?ny?t ?venk?nt.
# Ezeket az ?rt?keket mintak?nt haszn?lva hajtottuk v?gre az 
# intervallumbecsl?st.

C <- matrix(0, ncol = dim(atmin)[3], nrow = dim(atmin)[1])
rownames(C) <- rownames(P)[1:dim(atmin)[1]]
colnames(C) <- dimnames(atmin)[[3]]

for (i in 1:dim(atmin)[3]) {
  for (j in 1:dim(atmin)[1]) {
    C[j, i] <- atmin[j, 'D', i]/sum(atmin[j, , i]) 
  }
}

D <- matrix(0, ncol = 2, nrow = dim(Q)[1])

for (i in 1:dim(Q)[1]){
D[i, ] <- quantile(C[i, ], c(0.025, 0.975), type = 6)
}

rownames(D) <- rownames(Q)
colnames(D) <- c('Als? hat?r', 'Fels? hat?r')
D

#e)

# 'AAA'-b?l 'D'-be val? ?tmenet val?sz?n?s?ge (1 l?p?s alatt)
P['AAA', 'D']

# Egy ?v alatt p=0 val?sz?n?s?ggel megy cs?dbe 
# egy v?llalat 'AAA' min?s?t?sb?l.

#f)

# Az f m?trix megadja, hogy egyes kateg?ri?kb?l indulva 
# milyen val?sz?n?s?ggel jutunk el az egyes elnyel? pontokba.
f <- solve(diag(rep(1, dim(atmin)[1])) - Q) %*% R
f['C', 'Lejar']

# 74,37% val?sz?n?s?ggel fizeti vissza 
# a hitel?t egy 'C' besorol?s? v?llalat.

#g)

# Beolvassuk a megadott t?bl?t.
osztaly <- matrix(c(20, 10, 6,
                    18, 8,  6.5,
                    15, 6,  7,
                    12, 4,  7.5,
                    10, 3,  8,
                    9,  2,  9,
                    8,  1,  10), nrow = dim(atmin)[1], byrow = T)

# Elnevezz?k az ?j sorainkat eszt?tikai c?lb?l.

rownames(osztaly) <- rownames(Q)
colnames(osztaly) <- c('?tlagos hitel?sszeg (M Ft)',
                       '?tlagos lej?rati id? (?v)', 
                       '?tlagos kamat (%)')



db_kat <- 0 # Megsz?moljuk h?nyan vannak a 2018-as ?v elej?n kateg?ri?nk?nt.
veszteseg_kat <- 0 # Kateg?ri?nk?nt mennyit bukunk.
for (i in 1:dim(atmin)[1]) {
  db_kat[i] <- sum(atmin[, i, '2018']) 
  veszteseg_kat[i]<- db_kat[i] * P[i, 'D'] * # 2019-es cs?dbemen?sek v?rhat? ?rt?ke kateg?ri?nk?nt.
                     (osztaly[i, 1] * ((100 + osztaly[i, 3]) / 100) ^ osztaly[i, 2]) # Bukott t?ke.
}

# ?sszes bukott t?ke.
ossz <- 0
for (i in 1:dim(atmin)[1] ) {
  ossz <- ossz + veszteseg_kat[i] 
}
ossz
# 43761,33M forint vesztes?get gener?lnak az ?gyfelek cs?djei a 2018-as ?vben.
# Ez nagy ?sszegnek t?nik nekem, de ?gy tapasztalat n?lk?l egy?ltal?n nem tudtam mit v?rjak eredm?ny?l.










