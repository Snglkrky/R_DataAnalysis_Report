# R_DataAnalysis_Report
Hastalıkların Kökenle İlişkisi: Veri Analizi ve Görselleştirme Projesi

Bu depo, çeşitli hastalıklar ve belirli popülasyon kökenleri arasındaki olası ilişkileri keşfetmek ve görselleştirmek amacıyla yürütülen bir veri bilimi projesini içermektedir. Proje, veri manipülasyonu, istatistiksel analiz ve etkili veri görselleştirme tekniklerini kullanarak karmaşık kalıpları anlaşılır bir şekilde sunmayı hedeflemektedir.

## Proje Amacı

* Belirli hastalıkların prevalansının veya risk faktörlerinin, farklı coğrafi/etnik köken grupları arasında nasıl değiştiğini analiz etmek.
* İstatistiksel yöntemlerle gözlemlenen ilişkilerin anlamlılığını değerlendirmek.
* Bulguları etkileşimli ve bilgilendirici görselleştirmeler aracılığıyla sunmak.

## Veri Gizliliği ve Etik Yaklaşım

**ÖNEMLİ:** Bu projede kullanılan veri seti, **[https://www.cdc.gov/nchs/hus/topics/heart-disease-deaths.htm#definitions]** adresinden alınan SlctMort veri setinin analiz için kullanılabilir formata getirilmesinden türetilmiştir. Kişisel olarak tanımlanabilir hiçbir hasta bilgisi (PHI) veya bireysel hassas veri **kullanılmamıştır**. Analizler, veri gizliliği ve etik prensiplere tam uyum sağlayacak şekilde tasarlanmıştır. Bu proje, sadece metodolojiyi ve genel eğilimleri keşfetmeyi amaçlamaktadır; herhangi bir bireysel teşhis veya tedavi tavsiyesi içermez.

## İçerik

Bu depo aşağıdaki ana dosyaları içermektedir:

* `analiz_kodu.R`: Veri temizleme, manipülasyonu ve istatistiksel analiz adımlarını içeren R script dosyası.
* `rapor.Rmd`: Analizin tüm adımlarını, kullanılan yöntemleri, sonuçları ve görselleri içeren dinamik bir R Markdown raporudur.
* `rapor.html`: `rapor.Rmd` dosyasının derlenmiş HTML çıktısıdır. Projenin bulgularını tarayıcı üzerinden kolayca görüntülemenizi sağlar.
* `SlctMort/`: https://www.cdc.gov/nchs/hus/topics/heart-disease-deaths.htm#definitions adresinden alınan kökenlere göre hastalık ölüm oranlarının veri seti
 * `Mort/`: SlctMort veri setinin excel üzerinde bir kaç değişiklik ile analize hazır hale getirilmesiyle kullanılan asıl veri seti.


## Analiz Yöntemleri

Proje kapsamında kullanılan başlıca yöntemler:

* **Veri Hazırlığı:** Veri temizleme, eksik değer yönetimi, kategori kodlaması.
* **Keşifsel Veri Analizi (EDA):** Popülasyon grupları ve hastalıklar arasındaki ilk ilişkileri anlamak için dağılım analizleri ve temel istatistikler.
* **İstatistiksel Testler:** ANOVA ile İlişkilerin istatistiksel anlamlılığını değerlendirmek.
* **Veri Görselleştirme:** `ggplot2` gibi kütüphaneler kullanarak çubuk grafikler, yoğunluk grafikleri, ısı haritaları ve interaktif görsellerle bulguların sunumu.

## Kullanılan Teknolojiler ve Kütüphaneler

* **R**
* **RStudio**
* **R Markdown**
* **Kütüphaneler:** `tidyverse` (data wrangling ve görselleştirme için), `ggplot2`, `dplyr`, `tidyr`, `readr`, `plotly` (interaktif görseller için), `ggiraph` (görseller için), vb.

## Raporu Görüntüleme ve Projeyi Çalıştırma

* **HTML Raporunu Görüntüleme:** `rapor.html` dosyasını doğrudan bu depodan veya klonladıktan sonra yerel olarak web tarayıcınızda açarak analizin tüm detaylarını görüntüleyebilirsiniz.
* **R Markdown'ı Çalıştırma:** R ve RStudio kurulu ise, `rapor.Rmd` dosyasını RStudio'da açıp `Knit` butonuna tıklayarak raporu yeniden derleyebilirsiniz. İndirdiğiniz "Mort" veri seti için kendi dosya uzantınızı girmeyi unutmayın!!
* **R Kodunu İnceleme:** `analiz_kodu.R` dosyasını inceleyerek analizin adım adım nasıl yapıldığını görebilirsiniz. İndirdiğiniz "Mort" veri seti için kendi dosya uzantınızı girmeyi unutmayın!!

## Author

Songül Karakaya
(www.linkedin.com/in/songül-karakaya-a03257322)

---
