library(nortest)

data_hasil <- read.csv("data/hasil_survei.csv")

data_hasil$TRANSFORM_SKTOT <- log((53 - data_hasil$SKOR_TOTAL))

hasil <- shapiro.test(data_hasil$TRANSFORM_SKTOT)

print(hasil)

print(head(data_hasil))


hist(data_hasil$TRANSFORM_SKTOT,
     main = "histogram SKOR_TOTAL",
     xlab = "Nilai SKOR_TOTAL",
     ylab = "Frekuensi",
     col = "blue")

curve(dnorm(x, mean = mean(data_hasil$TRANSFORM_SKTOT), sd = sd(data_hasil$TRANSFORM_SKTOT)), add = T, col = "black")

legend("topright",
       legend = "Distribusi Normal",
       col = "black",
       lwd = 2)


