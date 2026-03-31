# 4.2 INPUT DATA
library(readxl)
library(survival)
library(randomForestSRC)
library(survcomp)
library(dplyr)
library(ggplot2)
data <- read_excel("D:/TA/File/Skripsi selesai/datafix - Copy.xlsx")

# 4.3 ANALISIS DESKRIPTIF
# ROA
summary(data$ROA)
# CR
summary(data$CR)
# DAR
summary(data$DAR)
# SGR
summary(data$SGR)
# ITR
summary(data$ITR)


# 4.4 Identifikasi Data Tersensor dan Tidak Tersensor
table(data$d,data$T)
tapply(data$d,data$T)

# Hitung jumlah individu per tahun (T) dan status sensor (d)
data_summary <- data %>%
  group_by(T, d) %>%
  summarise(Count = n(), .groups = 'drop')
head(data_summary)

ggplot(data_summary, aes(x = 0, xend = T, 
                         y = reorder(Count, T), 
                         color = factor(d))) +
  geom_segment(size = 1.2) +
  geom_point(aes(x = T), size = 3) +
  
  # Ganti judul legenda
  labs(title = "Waktu Survival dengan Data Tersensor", 
       x = "Waktu Pengamatan", y = "Jumlah Individu", 
       color = "Status Tersensor") +  # Judul legend untuk color
  
  scale_color_manual(values = c("lightblue", "orange"), 
                     labels = c("Tersensor", "Tidak Tersensor")) +
  
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

data_summary2 <- data %>%
  group_by(d) %>%
  summarise(Count = n(), .groups = 'drop')
data_summary2
# Menghitung persentase
data_summary2$Percentage <- round((data_summary2$Count / sum(data_summary2$Count)) * 100, 1)
data_summary2$Label <- paste0(data_summary2$Percentage, "%")

# Membuat pie chart dengan persentase
ggplot(data_summary2, aes(x = "", y = Count, fill = factor(d))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribusi Status Sensor Data", fill = "Status") +
  scale_fill_manual(values = c("lightblue", "orange"), labels = c("Tersensor", "Tidak Tersensor")) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# SPLIT DATA
set.seed(123)
train_index <- sample(1:nrow(data), size = 0.8 * nrow(data))
train_data <- data[train_index, ]
dim(train_data)
test_data <- data[-train_index, ]
dim(test_data)

# TUNING PARAMETER
# Membuat grid 
grid <- expand.grid(
  ntree = c(100, 500, 1000),  
  nodesize = c(5, 10, 20),
  nsplit = c(1, 2, 3)                   
)

# Membuat data frame untuk menyimpan hasil c-index
c_index_results <- data.frame(
  ntree = numeric(0),
  nodesize = numeric(0),
  nsplit = numeric(0),
  c_index = numeric(0)
)

# Menetapkan nilai mtry tetap 2
mtry_value <- 2

# Melakukan loop untuk tunning parameter ntree, nodesize, dan nsplit
for (i in 1:nrow(grid)) {
  param <- grid[i, ]
  
  # Membuat model RSF dengan parameter yang sedang diuji
  rsf_model <- rfsrc(Surv(T, d) ~ ROA + CR + DAR + SGR + ITR,
                     data = train_data,
                     ntree = param$ntree,
                     nodesize = param$nodesize,
                     mtry = mtry_value,  
                     nsplit = param$nsplit,
                     importance = TRUE,
                     samptype = "swr")
  
  # Evaluasi model
  predictions <- predict(rsf_model, newdata = test_data)$predicted
  c_index <- concordance.index(predictions, test_data$T, test_data$d)$c.index
  
  # Menyimpan hasil c-index
  c_index_results <- rbind(c_index_results, data.frame(
    ntree = param$ntree,
    nodesize = param$nodesize,
    nsplit = param$nsplit,
    c_index = c_index
  ))
}
# Tentukan kombinasi terbaik berdasarkan c-index
best_model <- c_index_results[which.max(c_index_results$c_index), ]
print(best_model)


# 4.6 RSF Training
rsf_model <- rfsrc(Surv(T, d) ~ ROA + CR + DAR + SGR + ITR,
                   data = train_data, 
                   ntree = best_model$ntree, 
                   mtry = 2,
                   nodesize = best_model$nodesize,
                   nsplit = best_model$nsplit, 
                   importance = TRUE,
                   samptype = "swr")
rsf_model
plot(rsf_model)

# 4.7 VIMP
# Hitung VIMP dengan metode permutasi
vimp_values <- vimp(rsf_model, importance = "permute")
vimp <- rsf_model$importance
print(vimp)

# 4.8 RSF Testing
rsf_predictions <- predict(rsf_model, newdata = test_data)
rsf_predictions

# 4.9 C-INDEX
actual_time <- test_data$T          
actual_status <- test_data$d        
c_index_result <- concordance.index(rsf_predictions$predicted, actual_time, actual_status)
print(c_index_result$c.index)


rsf_model$chf
rsf_predictions$chf
summary(rsf_model$chf)
summary(rsf_predictions$chf)

boxplot(rsf_predictions$chf, main="Boxplot CHF di Berbagai Waktu", 
        xlab="Waktu", ylab="CHF", col=c("yellow","orange","red"))


boxplot(rsf_predictions$predicted ~ test_data$d, 
        main="Perbandingan CHF Berdasarkan Status Distress",
        xlab="0=Cencored Data
1=Uncencored Data",
        ylab="CHF", col=c("lightblue", "orange"))




#==============================================================================
# Skor Risiko
# Mengambil skor risiko dari model
skor_risiko <- rsf_predictions$predicted
summary(skor_risiko)
# Menentukan threshold persentil (90% dan 10%)
threshold_90 <- quantile(skor_risiko, 0.90)
threshold_10 <- quantile(skor_risiko, 0.10)
threshold_90
threshold_10

# Mengelompokkan perusahaan berdasarkan skor risiko
risiko_tinggi <- which(skor_risiko >= threshold_90)
risiko_rendah <- which(skor_risiko <= threshold_10)

# Menambahkan skor risiko pada data perusahaan yang berisiko tinggi dan rendah
perusahaan_tinggi_risiko <- test_data[risiko_tinggi, ]
perusahaan_tinggi_risiko$skor_risiko <- skor_risiko[risiko_tinggi]

perusahaan_rendah_risiko <- test_data[risiko_rendah, ]
perusahaan_rendah_risiko$skor_risiko <- skor_risiko[risiko_rendah]
print(perusahaan_tinggi_risiko)
print(perusahaan_rendah_risiko)

library(writexl)

# Simpan data ke Excel
write_xlsx(perusahaan_tinggi_risiko, "perusahaan_tinggi_risiko.xlsx")
write_xlsx(perusahaan_rendah_risiko, "perusahaan_rendah_risiko.xlsx")


# Menghitung rata-rata DAR dan ROA untuk perusahaan berisiko tinggi
mean_DAR_tinggi <- mean(perusahaan_tinggi_risiko$DAR)
mean_ROA_tinggi <- mean(perusahaan_tinggi_risiko$ROA)

# Menghitung rata-rata DAR dan ROA untuk perusahaan berisiko rendah
mean_DAR_rendah <- mean(perusahaan_rendah_risiko$DAR)
mean_ROA_rendah <- mean(perusahaan_rendah_risiko$ROA)

# Menampilkan hasil analisis
cat("Perusahaan Berisiko Tinggi (Skor Risiko >= Persentil 90%):\n")
cat("Rata-rata DAR:", mean_DAR_tinggi, "\n")
cat("Rata-rata ROA:", mean_ROA_tinggi, "\n")

cat("\nPerusahaan Berisiko Rendah (Skor Risiko <= Persentil 10%):\n")
cat("Rata-rata DAR:", mean_DAR_rendah, "\n")
cat("Rata-rata ROA:", mean_ROA_rendah, "\n")

# Load library
library(ggplot2)

# Menambahkan kategori risiko pada dataset
test_data$risk_category <- ifelse(test_data$Kode %in% perusahaan_tinggi_risiko$Kode, "High Risk",
                                  ifelse(test_data$Kode %in% perusahaan_rendah_risiko$Kode, "Low Risk", "Moderate"))

# Membuat scatter plot
ggplot(test_data, aes(x = ROA, y = DAR, color = risk_category)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("High Risk" = "red", "Low Risk" = "green", "Moderate" = "blue")) +
  labs(title = "Scatter Plot ROA vs DAR",
       x = "Return on Assets (ROA)",
       y = "Debt to Asset Ratio (DAR)",
       color = "Kategori Risiko") +
  theme_minimal()

