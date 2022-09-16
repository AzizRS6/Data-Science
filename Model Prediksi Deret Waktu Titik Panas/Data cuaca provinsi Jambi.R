library(readr) # membaca file excel/csv/txt/json
library(readxl) # membaca file excel/csv/txt/json
library(dplyr) # memanipulasi data
library(ggpubr) # menghitung uji korelasi
library(ggcorrplot) # visualisasi uji korelasi
library(ggplot2) # membuat diagram
library(plotly) # memperindah diagram
library(gridExtra) # menyatukan sebanyak n visualisasi kedalam 1 frame

#soal nomor 1 

#input data
data = read_csv('D:/.TUGAS KAMPUS/Semester 5/BIg Data/dataset/tugas 3/data iklim nasa.csv')
head(data)

#membuat dataframe dari data tersebut
data = data.frame(
  tahun = as.numeric(data$YEAR),
  bulan = as.numeric(data$MO),
  hari = as.numeric(data$DOY),
  curah_hujan = as.numeric(data$PRECTOTCORR),
  radiasi_matahari = as.numeric(data$ALLSKY_SFC_UV_INDEX),
  suhu = as.numeric(data$T2M_RANGE),
  kecepatan_angin = as.numeric(data$WS10M_RANGE),
  kelembaban = as.numeric(data$RH2M)
)
head(data)

#integrasi data menjadi data bulanan dan harian
#data bulanan
tgl_bulan = seq(from = as.Date("2001-01-01"), to = as.Date("2020-12-31"), by = 'month')
data_bulanan = aggregate(data,
                        by = list(data$tahun, data$bulan), FUN = median)
data_bulanan = subset(data_bulanan, select = -c(Group.1,Group.2, hari))
data_bulanan = data_bulanan[order(data_bulanan$tahun),]
data_bulanan$tanggal = tgl_bulan
data_bulanan = data_bulanan %>% relocate(tanggal, .before = tahun)
data_bulanan = subset(data_bulanan, select = -c(tahun,bulan))
data_bulanan

#data harian
tgl_hari = seq(from = as.Date("2001-01-01"), to = as.Date("2020-12-31"), by = 'day')
data_harian = subset(data, select = -c(tahun,bulan,hari))
data_harian$tanggal = tgl_hari
data_harian = data_harian %>% relocate(tanggal, .before = curah_hujan)

#visualisasi time series data bulanan dan harian 
#data bulanan

visual_bulanan = ggplot(data = data_bulanan, aes(x = tanggal)) +
  geom_line(aes(y = radiasi_matahari,color = "radiasi_matahari"), size=1)+
  geom_line(aes(y = curah_hujan, color = "curah_hujan"), size=1)+
  geom_line(aes(y = suhu, color = "suhu"), size=1)+
  geom_line(aes(y = kelembaban, color = "kelembaban"), size=1)+
  geom_line(aes(y = kecepatan_angin, color = "kecepatan_angin"), size=1)+
  scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))+
  scale_colour_manual("",values = c("radiasi_matahari"="Blue", "curah_hujan"="green", "suhu"="Red", "kelembaban"="Yellow","kecepatan_angin"="Orange"))+
  labs(title="Data Cuaca Provinsi Jambi bulanan", y="nilai")+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5))

visual_bulanan

#data harian
visual_harian = ggplot(data = data_harian, aes(x = tanggal)) +
  geom_line(aes(y = radiasi_matahari,color = "radiasi_matahari"), size=1)+
  geom_line(aes(y = curah_hujan, color = "curah_hujan"), size=1)+
  geom_line(aes(y = suhu, color = "suhu"), size=1)+
  geom_line(aes(y = kelembaban, color = "kelembaban"), size=1)+
  geom_line(aes(y = kecepatan_angin, color = "kecepatan_angin"), size=1)+
  scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))+
  scale_colour_manual("",values = c("radiasi_matahari"="Blue", "curah_hujan"="green", "suhu"="Red", "kelembaban"="Yellow","kecepatan_angin"="Orange"))+
  labs(title="Data Cuaca Provinsi Jambi Harian", y="nilai")+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5))

visual_harian

#menggabungkan dua plot

perbandingan <- grid.arrange(visual_bulanan, visual_harian, ncol = 2)
plot(perbandingan)

