---
title: "Tugas Besar Analisis Data Statistika"
author: "Kelompok 8"
date: "2025-11-21"
output: html_document
---

```{r}
# --- Memuat Library ---
library(ggplot2)
library(dplyr)

# --- 1. Membaca Data ---
data <- read.csv("C:/Users/yazid/Downloads/dataset bersih.csv")

# Memeriksa nama kolom (biasanya spasi diubah menjadi titik oleh R)
# Kita asumsikan kolom menjadi 'IPK' dan 'Keterlibatan.Organisasi'
colnames(data)

# --- 2. Analisis Statistik (Independent t-test) ---
# Uji ini menghitung selang kepercayaan 95% untuk perbedaan dua rata-rata
t_test_result <- t.test(IPK ~ Keterlibatan.Organisasi, data = data)

# Menampilkan hasil statistik lengkap
print(t_test_result)

# Menampilkan khusus Selang Kepercayaan (Confidence Interval)
cat("\nSelang Kepercayaan 95% untuk perbedaan rata-rata:\n")
print(t_test_result$conf.int)

# --- 3. Persiapan Data untuk Visualisasi ---
# Menghitung Rata-rata dan Batas Error (Margin of Error) untuk setiap grup
plot_data <- data %>%
  group_by(Keterlibatan.Organisasi) %>%
  summarise(
    Mean_IPK = mean(IPK),
    SD = sd(IPK),
    N = n(),
    SE = SD / sqrt(N),
    # Menghitung batas atas dan bawah CI 95% untuk mean grup
    CI_Lower = Mean_IPK - qt(0.975, df = N - 1) * SE,
    CI_Upper = Mean_IPK + qt(0.975, df = N - 1) * SE
  )

print(plot_data)

# --- 4. Visualisasi (Plot Selang Kepercayaan) ---
plot1 <- ggplot(plot_data, aes(x = Keterlibatan.Organisasi, y = Mean_IPK)) +
  # Garis Error Bar (Selang Kepercayaan)
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                width = 0.15, size = 1, color = "black") + 
  # Titik Rata-rata (Mean) - Dibuat besar agar jelas
  geom_point(size = 5, color = "#0073C2") + 
  # Label Angka di dekat titik
  geom_text(aes(label = round(Mean_IPK, 2)), 
            hjust = -0.5, vjust = -0.5, fontface = "bold") +
  # Kustomisasi Tema agar bersih (seperti jurnal)
  theme_classic() +
  labs(
    title = "  Perbandingan Rata-Rata IPK Berdasarkan Keaktifan Organisasi ",
    #subtitle = "Titik = #Rata-rata, Garis = Rentang Kepercayaan 95%",
    x = "",
    y = "Indeks Prestasi Kumulatif (IPK)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black")
  )

# Tampilkan Plot 1
print(plot1)
```
