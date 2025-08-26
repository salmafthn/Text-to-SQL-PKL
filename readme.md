# Text to SQL with Mistral & Streamlit

Aplikasi ini memungkinkan pengguna untuk mengetikkan pertanyaan dalam Bahasa Natural (Bahasa Indonesia) dan mengubahnya secara otomatis menjadi query SQL menggunakan LLM (Mistral), lalu menjalankannya langsung ke database MySQL yang terisolasi dalam lingkungan Docker.

-----

## Fitur

  * Konversi pertanyaan dalam Bahasa Indonesia menjadi SQL.
  * Menjalankan query secara langsung dari antarmuka web.
  * Menampilkan hasil query dalam bentuk tabel dan bisa diunduh sebagai file CSV.
  * Pencegahan eksekusi query berbahaya (`DROP`, `DELETE`, `TRUNCATE`, `ALTER`, `UPDATE`, `INSERT`).
  * Mendeteksi maksud dari pertanyaan untuk merespons dengan lebih akurat.

-----

## Requirements

Untuk menjalankan aplikasi ini, Anda hanya perlu menginstal **Docker Desktop** di komputer Anda. Semua dependensi lainnya (MySQL, Ollama, Python, library, dll.) sudah terpaket di dalam Docker.

-----

## Model AI yang Digunakan

  * **LSTM** untuk Klasifikasi Maksud (Intent Classification) dari pertanyaan.
  * **Mistral 7B** (via [Ollama](https://ollama.com/)) untuk Generasi Query SQL berdasarkan skema database yang diberikan.

-----

## Instalasi

Aplikasi ini dikemas dalam kontainer Docker untuk memastikan portabilitas. Ikuti langkah-langkah di bawah ini untuk memulai.

1.  **Clone Repositori:**
    Buka terminal dan jalankan perintah berikut untuk mengunduh kode proyek:

    ```bash
    git clone <[repo-url](https://github.com/Ikram-sabila/PKL_Text2SQL_LabSI)>
    cd <PKL_Text2SQL_LabSI>
    ```

2.  **Jalankan Docker Compose:**
    Jalankan perintah ini. Proses ini akan membangun image aplikasi, menginisialisasi database, dan menjalankan semua layanan.

    ```bash
    docker-compose up --build -d
    ```

3.  **Unduh Model Ollama (Cukup sekali saja):**
    Buka terminal kedua dan jalankan perintah ini untuk mengunduh model Mistral ke dalam kontainer Ollama.

    ```bash
    docker exec -it ollama_server ollama pull mistral
    ```

-----

## Konfigurasi

Semua konfigurasi database sudah diotomatisasi di dalam Docker.

  - Database `skripsi` dan tabelnya dibuat secara otomatis dari file `.sql`.
  - Kredensial database (`root` dengan password `123`) sudah didefinisikan di dalam file `.env` dan `docker-compose.yml`.

-----

## Contoh Pertanyaan

Berikut adalah beberapa contoh pertanyaan yang dapat Anda coba untuk menguji fungsionalitas aplikasi.

### Mudah (Pencarian Langsung)

  * Tampilkan semua data dari tabel Dosen.
  * Siapa nama mahasiswa dengan NIM 195150707111021?
  * Apa program studi dari mahasiswa bernama NANDA RISKA DEWI?

### Menengah (Menggabungkan Dua Tabel)

  * Tampilkan judul skripsi yang ditulis oleh mahasiswa bernama SALMIN RASIDIN THAHIR.
  * Siapa nama dosen pembimbing 1 untuk skripsi berjudul "Analisis Faktor-Faktor yang Memengaruhi Kepuasan dan Loyalitas Pengguna Chatbot MITA Bank Mandiri"?
  * Sebutkan semua nama mahasiswa yang dibimbing oleh dosen Herman Tolle.

### Sulit (Menggabungkan Banyak Informasi & Logika)

  * Siapa nama dosen pembimbing 1 dan nama dosen pembimbing 2 untuk mahasiswa bernama M. ALFIAN NOOR?
  * Tampilkan nama mahasiswa yang dibimbing oleh dosen AGHNIYA FAZA dari program studi Sistem Informasi.
  * Sebutkan semua judul skripsi dengan minat "Data Science" yang diajukan setelah tanggal 1 Januari 2025.

-----

## Keamanan

Aplikasi ini **tidak mengeksekusi query destruktif** seperti:

  - `DROP`
  - `DELETE`
  - `TRUNCATE`
  - `ALTER`
  - `UPDATE`
  - `INSERT`

-----

## Export Data

Setelah query dijalankan, Anda dapat mengunduh hasilnya dalam format `.csv` langsung dari antarmuka Streamlit.

-----

### Proyek ini dikembangkan oleh:

  - **Muhammad Ikram Sabila Rasyad** – ([https://github.com/Ikram-sabila](https://github.com/Ikram-sabila))
  - **Ananda Annisa Sungkar** – ([https://github.com/annisasungkar](https://github.com/annisasungkar))
  - **Salma Adzra Fathina** – ([https://github.com/salmafthn](https://github.com/salmafthn))