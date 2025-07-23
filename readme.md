# 💬 Text to SQL with Mistral & Streamlit

Aplikasi ini memungkinkan pengguna untuk mengetikkan pertanyaan dalam Bahasa Natural (Natural Language) dan mengubahnya secara otomatis menjadi query SQL menggunakan LLM (Mistral), lalu menjalankannya langsung ke database MySQL.

---

## 🚀 Fitur

* Mengubah pertanyaan dalam Bahasa Indonesia atau Inggris menjadi SQL
* Menampilkan dan mengeksekusi SQL langsung dari Streamlit
* Menampilkan hasil query dalam bentuk tabel dan bisa diunduh sebagai CSV
* Deteksi otomatis tabel relevan berdasarkan pertanyaan
* Pencegahan eksekusi query berbahaya (`DROP`, `DELETE`, dsb.)

---

## 📦 Requirements

* Python 3.8+
* MySQL Server
* [Ollama](https://ollama.com) dengan model `mistral` yang sudah diinstal

---

## 🧠 Model AI yang Digunakan

* **Mistral 7B** (via [Ollama](https://ollama.com/))
* LLM ini digunakan untuk menerjemahkan teks natural menjadi SQL berdasarkan struktur skema database

---

## 🛠️ Instalasi

1. **Clone repo dan masuk ke folder proyek:**

   ```bash
   git clone <[repo-url](https://github.com/Ikram-sabila/PKL_Text2SQL_LabSI)>
   cd <PKL_Text2SQL_LabSI>
   ```

2. **Install dependencies:**

   ```bash
   pip install -r requirements.txt
   ```

3. **Instal dan jalankan Ollama:**

   * Download dan install [Ollama](https://ollama.com/)

   * Jalankan model mistral:

     ```bash
     ollama run mistral
     ```

   * Pastikan Ollama berjalan di `http://localhost:11434`

4. **Jalankan aplikasi:**

   ```bash
   streamlit run main.py
   ```

---

## 🗄️ Konfigurasi Database

Ubah konfigurasi koneksi database di bagian ini (dalam file `main.py`):

```python
pymysql.connect(
    host="localhost",
    user="root",
    password="",
    database="",
    ...
)
```

Pastikan database dan tabel sudah tersedia, dan file `extract_mysql.py` berisi dictionary `tabel_info` yang mendeskripsikan skema tabel seperti:

```python
tabel_info = {
    "warga": ["id", "nama", "alamat", "tanggal_lahir"],
    "pemeriksaan": ["id", "warga_id", "berat", "tinggi", "tanggal"],
    ...
}
```

---

## 💡 Contoh Pertanyaan

* "Tampilkan semua warga yang pernah diperiksa tahun 2024"
* "Berapa rata-rata berat badan bayi di setiap desa?"
* "Siapa saja warga yang belum pernah diperiksa?"

---

## ⚠️ Keamanan

Aplikasi ini **tidak mengeksekusi query destruktif** seperti:

* `DROP`
* `DELETE`
* `TRUNCATE`
* `ALTER`
* `UPDATE`
* `INSERT`

---

## 📤 Ekspor Data

Setelah query dijalankan, kamu dapat mengunduh hasilnya dalam format `.csv` langsung dari antarmuka Streamlit.

---

## 🧑‍💻 Developer

Dikembangkan menggunakan:

* Python
* Streamlit
* LLM (Mistral via Ollama)
* MySQL

---

### ✨ Dibuat oleh

Proyek ini dikerjakan oleh:

- 🧑‍💻 **Muhammad Ikram Sabila Rasyad** – ([https://github.com/Ikram-sabila](https://github.com/Ikram-sabila))
- 👩‍💻 **Ananda Annisa Sungkar** – ([https://github.com/annisasungkar](https://github.com/annisasungkar))
- 👨‍💻 **Salma Adzra Fathina** – ([https://github.com/salmafthn](https://github.com/salmafthn))
