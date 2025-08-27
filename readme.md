# Text to SQL with Mistral & Streamlit

Aplikasi ini memungkinkan pengguna untuk mengetikkan pertanyaan dalam Bahasa Natural (Bahasa Indonesia) dan mengubahnya secara otomatis menjadi query SQL menggunakan LLM (Mistral), lalu menjalankannya langsung ke database MySQL yang terisolasi dalam lingkungan Docker.

-----

## Requirements

Untuk menjalankan aplikasi ini, Anda hanya perlu menginstal **Docker Desktop** di komputer Anda. Semua dependensi lainnya (MySQL, Ollama, Python, library, dll.) sudah terpaket di dalam Docker.

-----

## Petunjuk Instalasi

Aplikasi ini dikemas dalam kontainer Docker untuk memastikan portabilitas

1.  **Clone Repository:**
    Clone repository ini kemudian pindah ke direktori repository tersebut.
    https://github.com/salmafthn/Text-to-SQL-PKL


2.  **Jalankan Docker Compose:**
    Jalankan perintah ini. Proses ini akan membangun image aplikasi, menginisialisasi database, dan menjalankan semua layanan.

    ```bash
    docker-compose up --build
    ```

3.  **Unduh Model Ollama (cukup sekali saja)):**
    Buka terminal kedua dan jalankan perintah ini untuk mengunduh model Mistral ke dalam kontainer Ollama. **Anda boleh menjalankannya saat sedang membangun image aplikasi**

    ```bash
    docker exec -it ollama_server ollama pull mistral
    ```

#### Konfigurasi

Semua konfigurasi database sudah diotomatisasi di dalam Docker.

  - Database `skripsi` dan tabelnya dibuat secara otomatis dari file `.sql`.
  - Kredensial database (`root` dengan password `123`) sudah didefinisikan di dalam file `.env` dan `docker-compose.yml`.

-----

## Contoh Pertanyaan

Berikut adalah beberapa contoh pertanyaan yang dapat Anda coba.

### Mudah (Direct Search)

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

## Ekspor Data

Setelah query dijalankan, Anda dapat mengunduh hasilnya dalam format `.csv` langsung dari antarmuka Streamlit.

-----

## Developer

Proyek ini dikembangkan oleh:

  - **Muhammad Ikram Sabila Rasyad** – ([https://github.com/Ikram-sabila](https://github.com/Ikram-sabila))
  - **Ananda Annisa Sungkar** – ([https://github.com/annisasungkar](https://github.com/annisasungkar))
  - **Salma Adzra Fathina** – ([https://github.com/salmafthn](https://github.com/salmafthn))