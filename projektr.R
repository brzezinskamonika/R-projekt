#importujemy pobraną tablicę wielowymiarową z naszymi produktami, zmieniamy znak separujący i wprowadzamy polskie znaki: 

ceny <- read.table("CENY_2917_CTAB_20200123153137.csv", sep=";",dec=",",fileEncoding = "UTF-8")

#pozbywamy się niepotrzebnej kolumny 
ceny <- ceny[,-1]

#TABELE Z CENAMI RYZU W POSZCZEGOLNYCH LATACH
ryz06 <- subset(ceny,select=c(V2,V3,V143,V283,V423,V563,V703,V843,V983,V1123,V1263,V1403,V1543))
ryz07 <- subset(ceny,select=c(V2,V4,V144,V284,V424,V564,V704,V844,V984,V1124,V1264,V1404,V1544))
ryz08 <- subset(ceny,select=c(V2,V5,V145,V285,V425,V565,V705,V845,V985,V1125,V1265,V1405,V1545))
ryz09 <- subset(ceny,select=c(V2,V6,V146,V286,V426,V566,V706,V846,V986,V1126,V1266,V1406,V1546))
ryz10 <- subset(ceny,select=c(V2,V7,V147,V287,V427,V567,V707,V847,V987,V1127,V1267,V1407,V1547))
ryz11 <- subset(ceny,select=c(V2,V8,V148,V288,V428,V568,V708,V848,V988,V1128,V1268,V1408,V1548))
ryz12 <- subset(ceny,select=c(V2,V9,V149,V289,V429,V569,V709,V849,V989,V1129,V1269,V1409,V1549))
ryz13 <- subset(ceny,select=c(V2,V10,V150,V290,V430,V570,V710,V850,V990,V1130,V1270,V1410,V1550))
ryz14 <- subset(ceny,select=c(V2,V11,V151,V291,V431,V571,V711,V851,V991,V1131,V1271,V1411,V1551))
ryz15 <- subset(ceny,select=c(V2,V12,V152,V292,V432,V572,V712,V852,V992,V1132,V1272,V1412,V1552))
ryz16 <- subset(ceny,select=c(V2,V13,V153,V293,V433,V573,V713,V853,V993,V1133,V1273,V1413,V1553))
ryz17 <- subset(ceny,select=c(V2,V14,V154,V294,V434,V574,V714,V854,V994,V1134,V1274,V1414,V1554))
ryz18 <- subset(ceny,select=c(V2,V15,V155,V295,V435,V575,V715,V855,V995,V1135,V1275,V1415,V1555))
ryz19 <- subset(ceny,select=c(V2,V16,V156,V296,V436,V576,V716,V856,V996,V1136,V1276,V1416,V1556))

#srednia cena w polsce w 2006
r6 <- mean(c(2.59,2.56,2.57,2.60,2.58,2.62,2.63,2.65,2.69,2.69,2.71,2.72))
#2007
r7 <- mean(c(2.73,	2.78, 2.79,	2.83,	2.85,	2.86,	2.88,	2.89,	2.91,	2.97,	3.09,	3.15))
#i tak dalej... 
r8 <- mean(c(3.26,	3.33,	3.38,	3.46,	3.69,	3.97,	4.21,	4.38,	4.48,	4.55,	4.58,	4.60))
r9 <- mean(c(4.58,	4.60,	4.62,	4.71,	4.74,	4.72,	4.72,	4.70,	4.66,	4.56,	4.48,	4.41))
r10 <- mean(c(4.41,	4.36,	4.35,	4.30,	4.27,	4.25,	4.15,	4.13,	4.07,	4.02,	3.95,	3.97))
r11 <- mean(c(4.01,	4.04,	4.10,	4.11,	4.09,	4.12	,4.11,	4.09,	4.10,	4.12,	4.14,	4.18))
r12 <- mean(c(4.19,	4.26,	4.31,	4.30,	4.29,	4.35,	4.32,	4.27,	4.22,	4.23,	4.23,	4.24))
r13 <- mean(c(4.27,	4.27,	4.25,	4.25,	4.19,	4.16,	4.12,	4.08,	4.08,	4.07,	4.03,	4.01))
r14 <- mean(c(4.03,	4.03,	4.02,	4.00,	3.95,	3.91,	3.88,	3.86,	3.82,	3.85,	3.82,	3.81))
r15 <- mean(c(3.80,	3.80,	3.79,	3.77,	3.79,	3.80,	3.78,	3.80,	3.79,	3.78,	3.79,	3.76))
r16 <- mean(c(3.80,	3.81,	3.78,	3.74,	3.81,	3.79,	3.81,	3.78,	3.82,	3.80,	3.86,	3.80))
r17 <- mean(c(3.86,	3.88,	3.86,	3.84,	3.85,	3.84,	3.86,	3.83,	3.84,	3.82,	3.86,	3.83))
r18 <- mean(c(3.83,	3.81,	3.83,	3.83,	3.83,	3.81,	3.77,	3.77,	3.85,	3.81,	3.79,	3.80))
r19 <- mean(c(3.82,	3.85,	3.87,	3.92,	3.97,	3.99,	4.03,	4.01,	4.02,	4.03,	4.01,	3.99))

#srednia cena ryzu z wszystkich lat 
sredniaryz=(r6+r7+r8+r9+r10+r11+r12+r13+r14+r15+r16+r17+r18+r19)/14

daneryz <- c(r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19)

#odchylenie standardowe
sd(daneryz)

#wariacja 
var(daneryz)

#pozostale wartosci 
summary(daneryz)

