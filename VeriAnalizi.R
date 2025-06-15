Sys.setlocale("LC_ALL", "en_US.UTF-8")
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis) # Isı haritası için renk paleti
library(ggrepel)
library(gganimate)
library(gifski)
library(glue) # Etiketleri düzeltmek için eklendi
library(car)
library(nortest)
library(emmeans)
library(rstatix) # Games-Howell testi için


# ------------------- Veri Yükleme ------------------- #
rawdata <- read_excel("C:/Users/msi/Desktop/Mort.xlsx")

str(rawdata)
head(rawdata)
anyNA(rawdata)  # Eksik değerlerin kontrolü
# which(is.na(rawdata))
# cat("NA özet:")
# colSums(is.na(rawdata)) 

# ------------------- Veri Önişleme ------------------- #
# Veriyi uzun formata çevirelim
long_data <- rawdata %>%
	pivot_longer(
		cols = `1999`:`2019`,
		names_to = "Year",
		values_to = "Death"
	)  
# Veri setindeki Death değişkeni her 100.000 insan için ölüm oranını veriyor.

# Değişkenleri faktör atayalım, levels görmek için
long_data <- long_data %>%
	mutate(Origin = as.factor(Origin),
		`Cause of death` = as.factor(`Cause of death`),
		Year = as.factor(Year))

# Faktör seviyelerinin Türkçe adlandırılması
long_data <- long_data %>%
	mutate(Origin = fct_recode(Origin,
														 "Amerikan Yerlisi/Alaska Yerlisi ve Hispanik/Latino" = "American Indian or Alaska Native and Hispanic or Latino",
														 "Amerikan Yerlisi/Alaska Yerlisi ancak Hispanik/Latino Değil" = "American Indian or Alaska Native but not Hispanic or Latino",
														 "Asyalı/Pasifik Adalı ve Hispanik/Latino" = "Asian or Pacific Islander and Hispanic or Latino",
														 "Asyalı/Pasifik Adalı ancak Hispanik/Latino Değil" = "Asian or Pacific Islander but not Hispanic or Latino",
														 "Siyah/Afro-Amerikan ve Hispanik/Latino" = "Black or African American and Hispanic or Latino",
														 "Siyah/Afro-Amerikan ancak Hispanik/Latino Değil" = "Black or African American but not Hispanic or Latino",
														 "Beyaz ve Hispanik/Latino" = "White and Hispanic or Latino",
														 "Beyaz ancak Hispanik/Latino Değil" = "White but not Hispanic or Latino"
		),`Cause of death` = fct_recode(`Cause of death`,
																		"Tüm Nedenler" = "All causes",
																		"Alzheimer" = "Alzheimer's disease",
																		"Serebrovasküler Hastalıklar" = "Cerebrovascular diseases",
																		"Kronik Karaciğer H. ve Siroz" = "Chronic liver disease and cirrhosis",
																		"Kronik Solunum Yolu Hastalıkları" = "Chronic lower respiratory diseases",
																		"Kolon, Rektum ve Anüs Kanserleri" = "Colon, rectum, and anus",
																		"Diyabet" = "Diabetes mellitus",
																		"Kalp Hastalıkları" = "Diseases of heart",
																		"Cinayet" = "Homicide",
																		"HIV" = "Human immunodeficiency virus (HIV) disease",
																		"Grip ve Zatürre" = "Influenza and pneumonia",
																		"İskemik Kalp Hastalığı" = "Ischemic heart disease",
																		"Kötü Huylu Tümörler" = "Malignant neoplasms",
																		"Trafik Kazaları" = "Motor vehicle-related injuries",
																		"Böbrek Hastalıkları" = "Nephritis, nephrotic syndrome and nephrosis",
																		"Zehirlenme" = "Poisoning",
																		"İntihar" = "Suicide",
																		"Trakea, Bronş ve Akciğer Kanserleri" = "Trachea, bronchus, and lung",
																		"Kasıtsız Yaralanmalar" = "Unintentional injuries"))


# ------------------- Keşifsel Veri ------------------- #
dim(long_data)
str(long_data)
head(long_data)

levels(long_data$Origin)
levels(long_data$`Cause of death`)
levels(long_data$Year)

# table(long_data$Origin)
# table(long_data$Culture)
# table(long_data$`Cause of death`)
# head(table(long_data$Death), 10)


# long_data$Death[long_data$`Cause of death`== "Human immunodeficiency virus (HIV) disease"]         # HIV'den ölenler
# mean(long_data$Death[long_data$`Cause of death`== "Human immunodeficiency virus (HIV) disease"])   # HIV'den ölenlerin sayısının ortalaması
tapply(long_data$Death, long_data$`Cause of death`, summary) # Her bir ölüm sebebinin özet/tanımlayıcı istatistiği


# Origin bazında toplam değerler (dplyr ile)
origin_totals <- long_data %>%
	group_by(Origin) %>%
	summarise(Total_Death = sum(Death)) %>%
	arrange(desc(Total_Death)) # Sonuçları büyükten küçüğe sıralamak için arrange(dasc())
origin_totals

# # tapply ile (vektör, grup, fonksiyon) 
# origin_totals_tapply <- tapply(long_data$Death, long_data$Origin, sum)
# origin_totals_tapply


# Ölüm nedeni bazında toplam değerler
cause_totals <- long_data %>%
	group_by(`Cause of death`) %>%
	summarise(Total_Death = sum(Death)) %>%
	arrange(desc(Total_Death))
cause_totals


# # Ölüm nedenlerinin özet istatistikleri
# cause_summary <- long_data %>%
# 	group_by(`Cause of death`) %>%
# 	summarise(
# 		Ortalama  = mean(Death),
# 		Medyan    = median(Death),
# 		Std_Sapma = sd(Death),
# 		Min_Değer = min(Death),
# 		Max_Değer = max(Death),)  %>%  arrange(desc(Ortalama))
# cause_summary



# ------------------- Veri Görselleştirme ------------------- #

# 1999 ve 2019 Yıllarındaki ölüm oranlarının kıyaslanması #
kıyas_data <- long_data %>%
	filter(`Cause of death` != "Tüm Nedenler") %>%
	filter(Year %in% c("1999", "2019"))

plot1 <- ggplot(kıyas_data, aes(x = reorder(`Cause of death`, Death), y = Death)) +
	geom_col(fill = "steelblue") +
	coord_flip() +
	facet_wrap(~ Year) +
	labs( title = "Ölüm Nedenleri Oranları: 1999 ve 2019 Karşılaştırması",
		x = "Ölüm Nedeni",  y = "Oran (100.000 Kişide)") + theme_minimal()
plot1


# 1999 ve 2019 Değişimini Gösteren Dambıl Grafiği 
dambıl_data <-  kıyas_data %>% # 1999 ve 2019 yıllarını filtrelenip geniş formata getirilmiş hali 
	group_by(`Cause of death`, Year) %>%
	summarise(Avg_Death = mean(Death, na.rm = TRUE), .groups = 'drop') %>%
	pivot_wider(names_from = Year, values_from = Avg_Death, names_prefix = "year_")

plot2 <- ggplot(dambıl_data, aes(y = reorder(`Cause of death`, year_1999))) +
	geom_segment(aes(x = year_1999, xend = year_2019, yend = `Cause of death`), 
							 color = "grey", linewidth = 1.5, alpha = 0.7) +
	geom_point(aes(x = year_1999, color = "1999"), size = 4) +
	geom_point(aes(x = year_2019, color = "2019"), size = 4) +
	scale_color_manual(name = "Yıl", values = c("1999" = "#f8766d", "2019" = "#00bfc4")) +
	labs(title = "Ölüm Oranlarındaki Değişim (1999 ve 2019)",
		x = "Ölüm Oranı (100.000 Kişide)", y = "Ölüm Nedeni") + theme_minimal()
plot2


# Tüm hastalık kategorilerinin kendi içindeki seyrini Heatmap ile görselleştirme
heatmap_data <- long_data %>%
	filter(`Cause of death` != "") %>%
	group_by(Year, `Cause of death`) %>%
	summarise(Avg_Death = mean(Death, na.rm = TRUE), .groups = 'drop') %>%
	group_by(`Cause of death`) %>%    # Her bir hastalık için Avg_Death değerini 0-1 arasına ölçeklendireceğiz.
	mutate( Scaled_Death = (Avg_Death - min(Avg_Death)) / (max(Avg_Death) - min(Avg_Death))) %>%
	ungroup() # Gruplamayı kaldırmak için

plot3 <- ggplot(heatmap_data, aes(x = Year, y = `Cause of death`, fill = Scaled_Death)) +
	geom_tile(color = "white", linewidth = 0.15) +
	scale_fill_viridis(option = "H", name = "Göreceli Oran\n(0 = Min, 1 = Max)") +
	labs(title = "Hastalıkların Kendi İçindeki Yıllık Değişimi",
		x = "Yıl", y = "Ölüm Nedeni") + theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
plot3


# Tüm hastalıkların kökenlerdeki seyrini Heatmap ile görselleştirme
heatmap_origin_rate <- long_data %>%
	filter(`Cause of death` == "Tüm Nedenler")

plot3.1 <- ggplot(heatmap_origin_rate, aes(x = Year, y = Origin, fill = Death)) +
	geom_tile(color = "white", linewidth = 0.15) + 
	scale_fill_viridis(option = "H", name = "Kırmızı = Yüksek Ölüm Oranı \nLacivert = Düşük Ölüm Oranı") +
	labs(title = "Kökenlerin Tüm Hastalıklardaki Toplamının Yıllık Değişimi",
			 x = "Yıl", y = "Ölüm Nedeni") + theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

plot3.1   # Hangi köken ne kadar dezavantajlı?


# Tüm hastalıkların kökenlere göre kendi içindeki seyri
heatmap_data_scaled <- long_data %>%
	filter(`Cause of death` == "Tüm Nedenler") %>%
	group_by(Origin) %>% mutate(Scaled_Death = (Death - min(Death)) / (max(Death) - min(Death))) %>%
	ungroup() # Gruplamayı kaldırır

plot3.2 <- ggplot(heatmap_data_scaled, aes(x = Year, y = Origin, fill = Scaled_Death)) +
	geom_tile(color = "white", linewidth = 0.2) +
	scale_fill_viridis(option = "H", name = "Göreceli Oran\n(0=En Düşük, 1=En Yüksek)") +
	labs(title = "Köken Gruplarının Kendi İçindeki Yıllık Değişimi",
		   subtitle = "Her satır, bir kökenin kendi 'Tüm Nedenler' ölüm oranı verilerine göre renklendirilmiştir",
		   x = "Yıl",  y = "Köken (Origin)"	) +  theme_minimal() +
	theme( axis.text.x = element_text(angle = 45, hjust = 1),  legend.position = "bottom")

plot3.2   # Genel olarak kökenlere göre ölüm oranları azalmış mı?



# Kalp hastalıklarının origine göre yıllar içindeki seyrini görselleştirelim 
kalp_data <- long_data %>%
	filter(`Cause of death` == "Kalp Hastalıkları")

plot4 <- ggplot(kalp_data, aes(x = as.numeric(as.character(Year)), y = Death, color = Origin)) +
	geom_line(linewidth = 0.7) + geom_point(size = 2.5, alpha = 0.8) +
	labs( title = "Kalp Hastalıkları Ölüm Oranlarındaki Trendler (1999:2019)",
		subtitle = "Farklı Kökenlere Göre Karşılaştırma",
		x = "Yıl",  y = " Ölüm Oranı (100.000 Kişide)",
		color = "Köken") + theme_minimal()+
	theme( legend.position = "bottom",
		plot.title = element_text(face = "bold"))
plot4


# Zehirlenmenin origine göre yıllar içindeki seyrini görselleştirelim 
poisoning_data <- long_data %>%
	filter(`Cause of death` == "Zehirlenme")

plot4.1 <- ggplot(poisoning_data, aes(x = as.numeric(as.character(Year)), y = Death, color = Origin)) +
	geom_line(linewidth = 0.7) + geom_point(size = 2.5, alpha = 0.8) +
	labs( title = "Zehirlenme Ölüm Oranlarındaki Trendler (1999:2019)",
				subtitle = "Farklı Kökenlere Göre Karşılaştırma",
				x = "Yıl",  y = " Ölüm Oranı (100.000 Kişide)",
				color = "Köken") + theme_minimal() +
	theme( legend.position = "bottom", plot.title = element_text(face = "bold"))
plot4.1


# # Alzheimer'ın kökenlere göre yıllar içindeki seyri 
# poisoning_data <- long_data %>%
# 	filter(`Cause of death` == "Alzheimer")
# 
# plot6 <- ggplot(poisoning_data, aes(x = as.numeric(as.character(Year)), y = Death, color = Origin)) +
# 	geom_line(linewidth = 0.7) + geom_point(size = 2.5, alpha = 0.8) +
# 	labs( title = "Alzheimer Ölüm Oranlarındaki Trendler (1999:2019)",
# 				subtitle = "Farklı Kökenlere Göre Karşılaştırma",
# 				x = "Yıl",  y = " Ölüm Oranı (100.000 Kişide)",
# 				color = "Köken") + theme_minimal() +
# 	theme( legend.position = "bottom", plot.title = element_text(face = "bold"))
# plot6


# Toplam ölüm oranlarının kökenlere göre yıllar içindeki seyri
all_causes_data <- long_data %>%
	filter(`Cause of death` == "Tüm Nedenler")

plot4.2 <- ggplot(all_causes_data, aes(x = as.numeric(as.character(Year)), y = Death, color = Origin)) +
	geom_line(linewidth = 0.7) + geom_point(size = 2.5, alpha = 0.8) +
	labs( title = "Toplam Ölüm Oranlarının Kökenlere göre Trendi (1999:2019)",
				subtitle = "Farklı Kökenlere Göre Karşılaştırma",
				x = "Yıl",  y = " Ölüm Oranı (100.000 Kişide)",
				color = "Köken") + theme_minimal() +
	theme( legend.position = "bottom", plot.title = element_text(face = "bold"))
plot4.2



# ------------------- Hareketli Veri Görselleştirme ------------------- # 


#  Avantajlı ve Dezavantajlı Grupların Yığılmış ve Hareketli Sıralama Grafiği 

groups_to_stack <- c("Siyah/Afro-Amerikan ancak Hispanik/Latino Değil",
										 "Asyalı/Pasifik Adalı ve Hispanik/Latino")

# Veriyi bu iki grup için hazırlayalım ve sıralama için toplamları hesaplama:
stacked_bcr_data <- long_data %>%
	filter(Origin %in% groups_to_stack) %>%                     # İki grubu da alıyor
	filter(`Cause of death` != "Tüm Nedenler") %>%              # Hastalıkları ayrı ayrı görmek için tüm nedenler seviyesini çıkarıyor
	pivot_wider(names_from = Origin, values_from = Death) %>%  	# Veriyi, sütunların köken grupları olacak şekilde geniş formata çeviriyor
	rename(death_siyah = `Siyah/Afro-Amerikan ancak Hispanik/Latino Değil`, 	# R için geçerli sütun adları atıyor
		     death_asyali = `Asyalı/Pasifik Adalı ve Hispanik/Latino`) %>%
	replace_na(list(death_siyah = 0, death_asyali = 0)) %>%    	# Eksik değerleri (bir grup için veri olmayan yıllar) 0 ile dolduruyor
	mutate(Total_Death = death_siyah + death_asyali) %>%        # Sıralama için toplam ölüm oranını hesaplayalım
	group_by(Year) %>%                                          # Her yıl içinde toplam orana göre sıralama yapalım
	mutate(rank = min_rank(-Total_Death) * 1) %>%
	filter(rank <= 10) %>%                                    	# Sadece ilk 10 hastalık
	ungroup() %>%                                              	
	pivot_longer( cols = c(death_siyah, death_asyali),          # ggplot ile yığılmış grafik çizmek için veriyi tekrar uzun formata çeviriyor
		names_to = "Origin",	values_to = "Death"	) %>%
	mutate(Origin = case_when(                                  # Orijinal köken isimlerini geri yüklüyor
		Origin == "death_siyah" ~ "Siyah/Afro-Amerikan ancak Hispanik/Latino Değil",
		Origin == "death_asyali" ~ "Asyalı/Pasifik Adalı ve Hispanik/Latino",
		TRUE ~ Origin         )) %>%                             	# Yığılma sırasını kontrol etmek için faktör seviyelerini belirliyor
	mutate(Origin = fct_relevel(Origin, "Asyalı/Pasifik Adalı ve Hispanik/Latino"))

stacked_bcr_data$Year <- as.numeric(as.character(stacked_bcr_data$Year)) # Yıl'ı animasyon için sayısal yapıyor

#  Yığılmış (stacked) ve hareketli grafik
anim_stacked_race <- ggplot(stacked_bcr_data, 	aes(x = rank, y = Death, fill = Origin)) + 
# Temel grafik katmanını kurar: x ekseninde sıralama, y'de ölüm oranı, renk dolgusu kökene göre.
	
	
	#--- GRAFİK ÖĞELERİ (GEOM'LAR) ---
	
	geom_col(position = "stack", width = 0.7, alpha = 0.9) +   # Yığılmış çubukları çizer. 'position="stack"' iki grubun değerini üst üste ekler. 'width' çubukları inceltir.
	
	geom_text(   # Etiket 1: Az olan grubun (Asyalı...) değerini kendi barının içine yazar.
		data = . %>% filter(Origin == "Asyalı/Pasifik Adalı ve Hispanik/Latino"),
		aes(label = glue('{round(Death, 0)}')),   # Etiket olarak yuvarlanmış ölüm oranını alır.
		position = position_stack(vjust = 0.5),   # Etiketi, yığılmış çubuğun KENDİ segmentinin dikey olarak ortasına yerleştirir.
		color = "white", fontface = "bold", size = 4) +
	
	geom_text(    # Etiket 2: Toplam değeri ("Siyah..." grubunun verisi üzerinden) barın sonuna yazar.
		data = . %>% filter(Origin == "Siyah/Afro-Amerikan ancak Hispanik/Latino Değil"),
		aes(y = Total_Death, label = glue(' {round(Total_Death, 0)}')),   # Y konumu olarak toplam değeri alır ve etiketi çubuğun sağına yerleştirir.
		hjust = 0, size = 5) +                                            # Metni yatay olarak sola hizalar (sayı çubuğun dışında başlar).
		
	
	geom_text(    # Etiket 3: Hastalık adını ("Siyah..." grubunun verisi üzerinden) barın köküne yazar.
		data = . %>% filter(Origin == "Siyah/Afro-Amerikan ancak Hispanik/Latino Değil"),
		aes(group = `Cause of death`, y = 0, label = `Cause of death`),   # Y konumunu 0 olarak belirleyerek metni en sola (köküne) yerleştirir.
		hjust = 1, vjust = 0.2, size = 5, color = "black") +              # Metni sağa hizalayarak çubuğun içinde kalmasını sağlar.
	
	#--- EKSEN ve TEMA AYARLARI ---
	
	coord_flip(clip = "off", expand = FALSE) + # Grafiğin eksenlerini değiştirir (x-y), böylece çubuklar yatay olur.
	scale_x_reverse() +      # X eksenini (artık dikey olan sıralamayı) ters çevirir, böylece 1. sıra en üstte olur.
	
	theme_minimal(base_size = 14) + # Sade, arka planı olmayan bir tema seçer.
	theme(
		panel.grid = element_blank(),  # Arka plandaki kılavuz çizgilerini kaldırır.
		axis.title = element_blank(),  # Eksen başlıklarını ("rank", "Death") kaldırır.
		axis.text.x = element_blank(), # Yatay eksendeki sayı etiketlerini (oranlar) kaldırır.
		axis.text.y = element_blank(), # Dikey eksendeki sayı etiketlerini (sıralama) kaldırır.
		plot.title = element_text(size = 22, face = "bold", hjust = 0.5),    # Ana başlığın stilini ayarlar.
		plot.subtitle = element_text(size = 18, hjust = 0.5),                # Alt başlığın stilini ayarlar.
		plot.caption = element_text(size = 12, hjust = 1, color = "grey40"), # En alttaki kaynak yazısının stilini ayarlar.
		legend.position = "bottom",                                          # Renk lejantını grafiğin altına yerleştirir.
		plot.margin = margin(1, 1, 1, 9, "cm")) + # Grafiğin kenar boşluklarını ayarlar (sol boşluk isimlerin sığması için fazla).
	
	#--- ANİMASYON KATMANI ---
	
	labs(
		title = "İki Köken Grubunun Hastalık Sıralaması",
		subtitle = "Yıl: {closest_state}",                                             # Alt başlığı dinamik yapar; o anki 'state' olan yılı gösterir.
		caption = "Çubuklar, iki grubun toplam ölüm oranını gösterir.",
		fill = "Köken Grubu:") +                                                       # Renk lejantının başlığını değiştirir.
	transition_states(Year, transition_length = 2, state_length = 2, wrap = FALSE) + # Animasyonun ana motoru: Yıl'a göre durumlar arası geçiş yapar.
	ease_aes('cubic-in-out') +                                                       # Geçişlerin daha yumuşak ve estetik olmasını sağlar.
	view_follow(fixed_x = TRUE)             # Dikey eksenin (sıralama) sabit kalmasını, yatay eksenin (oranlar) ise verilere göre değişmesini sağlar.

# Animasyonu oluşturalım ve kaydedelim
animate(anim_stacked_race, nframes = 250, fps = 10, width = 1200, height = 800, renderer = gifski_renderer())

# anim_stacked_race,                      # Canlandırılacak ggplot animasyon nesnesi.
# nframes = 250,                          # Animasyonun toplam kare sayısı (süresini etkiler).
# fps = 10,                               # Saniyedeki kare sayısı (hızını etkiler).
# width = 1200,                           # Çıktı olarak alınacak GIF'in piksel cinsinden genişliği.
# height = 800,                           # Çıktı olarak alınacak GIF'in piksel cinsinden yüksekliği.
# renderer = gifski_renderer()            # Animasyonu yüksek kaliteli bir GIF formatında oluşturacak motor.

# animasyon1 <- animate(anim_stacked_race, nframes = 250, fps = 10, width = 1200, height = 800, renderer = gifski_renderer())
# 
# # Dışa aktar (örnek: GIF olarak)
# anim_save("animasyon1.gif", animasyon1)



#  HIV Trendlerinin Canlandırılması 

hiv_data_anim <- long_data %>%
	filter(`Cause of death` == "HIV") 
# Yıl değişkenini animasyonun doğru çalışması için sayısal formata çevirelim.
hiv_data_anim$Year <- as.numeric(as.character(hiv_data_anim$Year))


# Hareketli Grafiği Oluşturma
anim_line_hiv <- ggplot(hiv_data_anim, aes(x = Year, y = Death, color = Origin)) + # Temel grafik katmanını kurar: x=Yıl, y=Ölüm Oranı, renk=Köken.
	
	#--- GRAFİK ÖĞELERİ (GEOM'LAR) ---
	
	geom_line(linewidth = 1.5) + # Her bir köken grubu için trendi gösteren çizgileri çizer.
	geom_point(size = 3) + # Çizgilerin üzerindeki her bir yıla ait veri noktalarını ekler.
	geom_text(aes(label = round(Death, 1)), vjust = -1.5, fontface = "bold") + # Her noktanın üzerine, yuvarlanmış sayısal değerini yazar.
	
	#--- EKSEN ve TEMA AYARLARI ---
	
	labs( # Grafiğin başlıklarını ve etiketlerini belirler.
		title = "HIV Hastalığı Ölüm Oranlarındaki Trend (1999-2019)",
		subtitle = "Yıl: {frame_along}", # Alt başlığı dinamik yapar; o anki yılı gösterir.
		x = "Yıl",
		y = "Ölüm Oranı (100.000 Kişide)",
		color = "Köken" # Renk lejantının başlığını ayarlar.
	) +
	theme_minimal(base_size = 14) + # Sade ve modern bir tema seçer.
	theme(
		legend.position = "top", # Lejantı grafiğin üstüne konumlandırır.
		plot.title = element_text(face = "bold", size = 20), # Ana başlığın stilini ayarlar.
		plot.subtitle = element_text(size = 16) # Alt başlığın stilini ayarlar.
	) +
	
	#--- ANİMASYON KATMANI ---
	
	transition_reveal(Year) # Animasyonun ana motoru: Grafiğin Yıl değişkeni boyunca "çizilerek" ortaya çıkmasını sağlar.

animate(
	anim_line_hiv,                 # Canlandırılacak ggplot animasyon nesnesi.
	nframes = 200,                 # Animasyonun toplam kare sayısı (süresini etkiler).
	fps = 10,                      # Saniyedeki kare sayısı (hızını etkiler).
	width = 1000,                  # Çıktı olarak alınacak GIF'in piksel cinsinden genişliği.
	height = 750,                  # Çıktı olarak alınacak GIF'in piksel cinsinden yüksekliği.
	renderer = gifski_renderer())  


# Animasyonu Oluşturma ve Kaydetme
animasyon2 <- animate(
	anim_line_hiv,                 # Canlandırılacak ggplot animasyon nesnesi.
	nframes = 200,                 # Animasyonun toplam kare sayısı (süresini etkiler).
	fps = 10,                      # Saniyedeki kare sayısı (hızını etkiler).
	width = 1000,                  # Çıktı olarak alınacak GIF'in piksel cinsinden genişliği.
	height = 750,                  # Çıktı olarak alınacak GIF'in piksel cinsinden yüksekliği.
	renderer = gifski_renderer())  # Animasyonu yüksek kaliteli bir GIF formatında oluşturacak motor.

# Dışa aktar (örnek: GIF olarak)
anim_save("animasyon2.gif", animasyon2)





#  Alzheimer Trendlerinin Canlandırılması 

alzheimer_data_anim <- long_data %>%
	filter(`Cause of death` == "Alzheimer")
alzheimer_data_anim$Year <- as.numeric(as.character(alzheimer_data_anim$Year))

# Hareketli Grafiği Oluşturma
anim_line_alzheimer <- ggplot(alzheimer_data_anim, aes(x = Year, y = Death, color = Origin)) + 

	#--- GRAFİK ÖĞELERİ (GEOM'LAR) ---
	geom_line(linewidth = 1.5) + 
	geom_point(size = 3) + 
	geom_text(aes(label = round(Death, 1)), vjust = -1.5, fontface = "bold") + 
	
	#--- EKSEN ve TEMA AYARLARI ---
	labs( 
		title = "Alzheimer Hastalığı Ölüm Oranlarındaki Trend (1999-2019)",
		subtitle = "Yıl: {frame_along}",
		x = "Yıl",	y = "Ölüm Oranı (100.000 Kişide)",
		color = "Köken" ) +
	theme_minimal(base_size = 14) + 
	theme(
		legend.position = "top", 
		plot.title = element_text(face = "bold", size = 20), 
		plot.subtitle = element_text(size = 16) ) +
	
	#--- ANİMASYON KATMANI ---
	transition_reveal(Year) 

animate(anim_line_alzheimer, nframes = 200, fps = 10, width = 1000, height = 750, renderer = gifski_renderer()) 


# Animasyonu Oluşturma ve Kaydetme
animasyon3 <- animate(anim_line_alzheimer, nframes = 200, fps = 10, width = 1000, height = 750, renderer = gifski_renderer()) 

anim_save("animasyon3.gif", animasyon3)





# # 1. En avantajlı ve en dezavantajlı grupları belirleyelim.
# #    "Tüm Nedenler" ölüm oranlarının ortalamasına göre sıralıyoruz.
# group_ranking <- long_data %>%
# 	filter(`Cause of death` == "Tüm Nedenler") %>%
# 	group_by(Origin) %>%
# 	summarise(Avg_Death_Rate = mean(Death, na.rm = TRUE)) %>%
# 	arrange(Avg_Death_Rate)
# 
# # En düşük ve en yüksek oranlara sahip grupları seçelim
# most_advantaged_group <- group_ranking$Origin[1]
# least_advantaged_group <- group_ranking$Origin[nrow(group_ranking)]
# 
# cat("En Avantajlı Grup (En Düşük Ortalama Ölüm Oranı):", as.character(most_advantaged_group), "\n")
# cat("En Dezavantajlı Grup (En Yüksek Ortalama Ölüm Oranı):", as.character(least_advantaged_group), "\n")
# 
# #  Veriyi bu iki grup ve iki ana hastalık (Kalp Hast. ve Kanser) için hazırlayalım.
# scatter_data <- long_data %>%
# 	filter(Origin %in% c(most_advantaged_group, least_advantaged_group)) %>%
# 	filter(`Cause of death` %in% c("Kalp Hastalıkları", "Kötü Huylu Tümörler")) %>%
# 	# Veriyi geniş formata çevirerek hastalıkları sütun yapalım
# 	pivot_wider(
# 		id_cols = c(Origin, Year),
# 		names_from = `Cause of death`,
# 		values_from = Death)
# 
# # Yıl'ı animasyon için sayısal yapalım
# scatter_data$Year <- as.numeric(as.character(scatter_data$Year))
# 
# 
# # Animasyonu oluşturalım
# anim_scatter <- ggplot(scatter_data, aes(x = `Kalp Hastalıkları`, y = `Kötü Huylu Tümörler`, color = Origin)) +
# 	geom_point(size = 8, alpha = 0.7) +
# 	# Yıl etiketlerini noktaların içine ekleyelim
# 	geom_text(aes(label = Year), color = "purple", fontface = "bold", size = 3) +
# 	labs(
# 		title = "İki Ana Hastalıkta Köken Gruplarının Evrimi (1999-2019)",
# 		subtitle = "Noktalar, grupların yıllar içindeki yolculuğunu gösteriyor.",
# 		x = "Kalp Hastalıkları Ölüm Oranı",
# 		y = "Kötü Huylu Tümörler Ölüm Oranı",
# 		color = "Köken Grubu"
# 	) +
# 	theme_minimal(base_size = 14) +
# 	theme(
# 		legend.position = "bottom",
# 		plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
# 		plot.subtitle = element_text(size = 14, hjust = 0.5)
# 	) +
# 	
# 	# --- gganimate katmanı ---
# 	transition_time(Year) +
# 	# Noktaların arkasında bir iz bırakarak hareketin yönünü gösterelim
# 	shadow_wake(wake_length = 0.1, alpha = 0.3)
# 
# animate(anim_scatter, nframes = 200, fps = 10, width = 1000, height = 800, renderer = gifski_renderer())



#  Kökenlerin Yarışı (Bar Chart Race) ---

# 1. Veriyi hazırlayalım: "All causes" için filtreleyip her yıl kökenleri sıralayalım.
origin_race_data <- long_data %>%
	filter(`Cause of death` == "Tüm Nedenler") %>%
	group_by(Year) %>%
	mutate(
		# Her yıl içinde köken gruplarını ölüm oranlarına göre sıralıyoruz.
		rank = min_rank(-Death) * 1,
		Value_lbl = paste0(" ", round(Death))
	) %>%
	# Bu animasyonda tüm grupları (8 grup) göstereceğiz.
	filter(rank <= 8)

# Yıl'ı animasyon için sayısal yapalım
origin_race_data$Year <- as.numeric(as.character(origin_race_data$Year))


# 2. Animasyonu oluşturalım
anim_origin_race <- ggplot(origin_race_data, aes(rank, group = Origin, fill = as.factor(Origin))) +
	geom_tile(aes(y = Death/2, height = Death, width = 0.9), alpha = 0.8) +
	geom_text(aes(y = 0, label = paste(Origin, " ")), vjust = 0.2, hjust = 1, size = 5) +
	geom_text(aes(y = Death, label = Value_lbl, hjust = 0), size = 5) +
	
	coord_flip(clip = "off", expand = FALSE) +
	scale_y_continuous(labels = scales::comma) +
	scale_x_reverse() +
	
	guides(color = FALSE, fill = FALSE) +
	
	theme_minimal() +
	theme(
		panel.grid = element_blank(),
		axis.text.y = element_blank(),
		axis.text.x = element_blank(),
		axis.title = element_blank(),
		plot.margin = margin(1, 4, 1, 6, "cm"),
		plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
		plot.subtitle = element_text(size = 18, hjust = 0.5),
		plot.caption = element_text(size = 12, hjust = 1, color = "grey40")
	) +
	
	# --- gganimate katmanı ---
	labs(
		title = "Köken Gruplarının Genel Ölüm Oranı Sıralaması",
		subtitle = "Yıl: {as.integer(frame_time)}",
		caption = "Veri Kaynağı: CDC (100.000 kişideki yaşa göre ayarlanmış ölüm oranı)"
	) +
	transition_time(Year) +
	ease_aes('cubic-in-out')

# Animasyonu render edip görüntüleyelim
# Bu işlem biraz zaman alabilir.
animasyon4 <- animate(anim_origin_race, nframes = 200, fps = 10, width = 1200, height = 900, renderer = gifski_renderer())

anim_save("animasyon4.gif", animasyon4)








# --- Tek Köken Grubu İçin Hastalık Sıralaması  ---

# 1. Veriyi hazırlayalım:
bcr_single_origin_data <- long_data %>%
	filter(Origin == "Beyaz ancak Hispanik/Latino Değil") %>% # Sadece ilgili köken grubunu seçer.
	filter(`Cause of death` != "Tüm Nedenler") %>%            # Genel kategori olan "Tüm Nedenler"i analizden çıkarır.
	group_by(Year) %>%                      # İşlemleri her bir yıl için ayrı ayrı yapmak üzere gruplar.
	mutate(
		rank = min_rank(-Death) * 1,          # Her yıl içinde hastalıkları ölüm oranına göre büyükten küçüğe sıralar (1. en yüksek).
		Value_lbl = paste0(" ", round(Death)) # Çubukların yanına yazılacak sayısal etiketleri oluşturur.
	) %>%
	filter(rank <= 10) %>%                  # Her yıl için sadece ilk 10'a giren hastalıkları tutar.
	ungroup()                               # Gruplamayı kaldırır.

bcr_single_origin_data$Year <- as.numeric(as.character(bcr_single_origin_data$Year)) # Yıl değişkenini sayısal formata çevirme

# 2. Animasyonu oluşturalım:
anim_single_origin_race <- ggplot(bcr_single_origin_data, aes(rank, group = `Cause of death`, fill = as.factor(`Cause of death`))) + # Temel grafik katmanını kurar.
	
	#--- GRAFİK ÖĞELERİ (GEOM'LAR) ---
	
	geom_tile(aes(y = Death/2, height = Death, width = 0.9), alpha = 0.8) +    # Her bir hastalık için çubukları (tile) çizer.
	geom_text(aes(y = 0, label = paste(`Cause of death`, " ")), vjust = 0.2, hjust = 1, size = 5) + # Hastalık isimlerini çubukların köküne yazar.
	geom_text(aes(y = Death, label = Value_lbl, hjust = 0), size = 5) +        # Ölüm oranı değerlerini çubukların ucuna yazar.
	
	#--- EKSEN ve TEMA AYARLARI ---
	
	coord_flip(clip = "off", expand = FALSE) +    # Eksenleri değiştirerek yatay çubuk grafiği oluşturur.
	scale_x_reverse() +                           # Sıralamayı ters çevirerek 1. sıranın en üstte olmasını sağlar.
	guides(color = FALSE, fill = FALSE) +         # Renk lejantını gizler, çünkü her çubuk zaten etiketlidir.
	theme_minimal() +                             # Sade bir tema kullanır.
	theme(
		panel.grid = element_blank(),           # Arka plan kılavuz çizgilerini kaldırır.
		axis.text.y = element_blank(),          # Dikey eksen etiketlerini (sıralama) kaldırır.
		axis.text.x = element_blank(),          # Yatay eksen etiketlerini (oranlar) kaldırır.
		axis.title = element_blank(),           # Eksen başlıklarını kaldırır.
		plot.margin = margin(1, 4, 1, 6, "cm"), # Grafik kenar boşluklarını ayarlar, solda isimler için yer bırakır.
		plot.title = element_text(size = 22, face = "bold", hjust = 0.5),   # Ana başlığın stilini ayarlar.
		plot.subtitle = element_text(size = 18, hjust = 0.5),               # Alt başlığın stilini ayarlar.
		plot.caption = element_text(size = 12, hjust = 1, color = "grey40") # Kaynak bilgisinin stilini ayarlar.
	) +
	
	#--- ANİMASYON KATMANI ---
	
	labs(
		title = "Beyaz (Hispanik Değil) Grubunda En Ölümcül Hastalıklar",
		subtitle = "Yıl: {as.integer(frame_time)}", # Alt başlığa dinamik olarak o anki yılı yazar.
		caption = "Veri Kaynağı: CDC (100.000 kişideki yaşa göre ayarlanmış ölüm oranı)"
	) +
	transition_time(Year) +    # Animasyonun ana motoru: Yıl'a göre durumlar arası geçiş yapar.
	ease_aes('cubic-in-out')   # Geçişlerin daha yumuşak ve estetik olmasını sağlar.

# 3. Animasyonu Oluşturma ve Kaydetme
animasyon5 <- animate(
	anim_single_origin_race,        # Canlandırılacak ggplot animasyon nesnesi.
	nframes = 200,                  # Animasyonun toplam kare sayısı.
	fps = 10,                       # Saniyedeki kare sayısı (hız).
	width = 1200,                   # Çıktı GIF'inin genişliği (piksel).
	height = 900,                   # Çıktı GIF'inin yüksekliği (piksel).
	renderer = gifski_renderer())   # Yüksek kaliteli GIF oluşturma motoru.

anim_save("animasyon5.gif", animasyon5)








# ------------------- ANOVA ------------------- # 

# Neden ANOVA?
# ANOVA, bir sürekli (sayısal) değişkenin ortalamasının, iki veya daha fazla kategorik grup arasında farklı olup olmadığını test etmek için kullanılır.
# 	
# Sürekli Değişken: Death
# Kategorik Değişkeniniz (Gruplar): Origin (8 farklı köken grubu).
# ANOVA, bu 8 farklı köken grubunun ortalama ölüm oranlarının birbirinden istatistiksel olarak anlamlı derecede farklı olup olmadığını söyleyecektir.


# Analiz Adımları:
# 1. Hipotezleri Kurma:
#    H0: Farklı köken gruplarının ortalama ölüm oranları arasında istatistiksel olarak anlamlı bir fark yoktur.
#    H1: En az bir köken grubunun ortalama ölüm oranı diğerlerinden farklıdır.
# 2. ANOVA Testini Uygulama:
# 	 * R gibi bir istatistik programında Death bağımlı değişken, Origin ise bağımsız değişken olacak şekilde bir ANOVA modeli kurarsınız.
#    * Testin sonucunda bir p-değeri elde edersiniz. Genellikle p < 0.05 ise, H0 hipotezini reddedersiniz. Bu, köken grupları arasında anlamlı bir fark olduğu anlamına gelir.
# 3. İlişkinin Kaynağını Bulma (Post-Hoc Testler):
# 	 ANOVA size sadece gruplar arasında bir fark olduğunu söyler, ancak bu farkın hangi gruplar arasında olduğunu söylemez.
#    İşte bu noktada ikinci sorunuzun cevabı olan Post-Hoc testleri devreye girer. En yaygın kullanılanı Tukey's HSD (Honestly Significant Difference) testidir.
#    Bu test, tüm köken gruplarını ikili olarak karşılaştırır (örneğin, "Beyaz ve Hispanik/Latino" ile "Siyah/Afro-Amerikan ancak Hispanik/Latino Değil") ve 
#    hangi ikililer arasındaki farkın istatistiksel olarak anlamlı olduğunu size gösterir.


# --- 1. VERİ HAZIRLIĞI ---
anova_input_data <- long_data %>%
	filter(!(`Cause of death` %in% c("Tüm Nedenler", "Kasıtsız Yaralanmalar", "Kötü Huylu Tümörler")))


# --- 2. "ALTIN STANDART" ANOVA FONKSİYONUNU TANIMLAMA ---
# Bu fonksiyon, kendisine verilen tek bir hastalığın adını alarak tüm analiz adımlarını uygular.

run_single_disease_anova <- function(disease_name, data) {
	
	# Adım A: Veriyi sadece o hastalık için filtrele
	disease_data <- data %>% filter(`Cause of death` == disease_name)
	
	# Adım B: Varyans homojenliğini Levene Testi ile kontrol et
	levene_p <- leveneTest(Death ~ Origin, data = disease_data)$`Pr(>F)`[1]
	
	# Adım C: Levene sonucuna göre uygun ANOVA testini yap
	if (levene_p >= 0.05) {
		# Varyanslar homojen ise, standart ANOVA
		anova_test <- aov(Death ~ Origin, data = disease_data)
		anova_p <- summary(anova_test)[[1]]$`Pr(>F)`[1]
		test_type <- "Standart ANOVA"
	} else {
		# Varyanslar homojen değilse, Welch's ANOVA
		anova_test <- oneway.test(Death ~ Origin, data = disease_data, var.equal = FALSE)
		anova_p <- anova_test$p.value
		test_type <- "Welch's ANOVA"
	}
	
	# Adım D: ANOVA anlamlıysa, uygun Post-Hoc testini yap
	significant_pairs <- "Genel fark yok (p >= 0.05)" # Varsayılan değer
	
	if (anova_p < 0.05) {
		if (levene_p >= 0.05) {
			# Varyanslar homojen ise Tukey HSD
			post_hoc_res <- tukey_hsd(disease_data, Death ~ Origin)
		} else {
			# Varyanslar homojen değilse Games-Howell
			post_hoc_res <- games_howell_test(disease_data, Death ~ Origin)
		}
		
		# Anlamlı çiftleri bul ve tek bir metin olarak birleştir
		significant_pairs_df <- post_hoc_res %>% filter(p.adj < 0.05)
		if (nrow(significant_pairs_df) > 0) {
			significant_pairs <- paste(significant_pairs_df$group1, "-", significant_pairs_df$group2, collapse = "; ")
		} else {
			significant_pairs <- "Anlamlı ikili fark bulunamadı"
		}
	}
	
	# Adım E: Tüm sonuçları tek bir satırlık veri çerçevesi (tibble) olarak döndür
	tibble(
		hastalik_adi = disease_name,
		kullanilan_test = test_type,
		levene_p_degeri = round(levene_p, 4),
		anova_p_degeri = round(anova_p, 4),
		anlamli_farklar = significant_pairs
	)
}

# --- 3. OTOMASYON: FONKSİYONU TÜM HASTALIKLARA UYGULAMA ---

# Analiz edilecek tüm hastalıkların listesini alalım
disease_list <- unique(anova_input_data$`Cause of death`)

# 'purrr::map_dfr' fonksiyonu ile her hastalık için analizi çalıştırıp sonuçları birleştirelim.
# Bu tek satır, tüm işi yapar!
final_results_table <- map_dfr(disease_list, ~run_single_disease_anova(disease_name = .x, data = anova_input_data))


# --- 4. NİHAİ ÖZET TABLOSUNU GÖRÜNTÜLEME ---
cat("--- Tüm Hastalıklar İçin Otomatik ANOVA Sonuçları Tablosu ---")
print(final_results_table, width = Inf)
