import pymysql.cursors
import streamlit as st
import requests
import extract_mysql
import json
import pandas as pd
from sqlalchemy import create_engine
import pymysql
import re
from predict_intent import predict_intent
from dotenv import load_dotenv
import os

load_dotenv()

username_db = os.getenv("USERNAME_DB")
password_db = os.getenv("PASSWORD_DB")
port_db = os.getenv("PORT_DB")
name_db = os.getenv("DATABASE_NAME")

# ==== CACHED CONNECTION ====
@st.cache_resource
def get_connection():
    return pymysql.connect(
        host="localhost",
        user=username_db,
        password=password_db,
        database=name_db,
        cursorclass=pymysql.cursors.DictCursor
    )

# ==== DETEKSI SQL BERBAHAYA ====
def is_dangerous_query(sql):
    dangerous = ['drop', 'delete', 'truncate', 'alter', 'update', 'insert']
    tokens = re.findall(r'\b\w+\b', sql.lower())
    return any(word in tokens for word in dangerous)

# ==== DETECT RELEVANT TABLES ====
def detect_relevant_tables(prompt, tabel_info):
    relevant = {}
    prompt_lower = prompt.lower()
    for table, columns in tabel_info.items():
        if table.lower() in prompt_lower:
            relevant[table] = columns
        else:
            for col in columns:
                if col.lower() in prompt_lower:
                    relevant[table] = columns
                    break
    return relevant

# ==== Model: Intent Detection ====
def intent_detect(text):
    result = predict_intent(text)
    return result

# ==== LLM: TEXT TO SQL ====
def text_to_sql(prompt):
    schema_string = json.dumps(extract_mysql.tabel_info, indent=2)


    url = "http://localhost:11434/api/generate"
    data = {
        "model": "mistral",
        "prompt": f"""
        You are an expert MySQL assistant. Given the database schema below:

        ### Database Schema:
        {schema_string}

        ### Instructions:
        1. Convert the user's question into a valid MySQL query.
        2. ONLY return the SQL code, nothing else.
        3. Ensure all table names (like `Persons`, `Mahasiswa`, `Dosen`) and column names are exactly as provided in the schema. Do not invent tables or columns.

        ### User Question:
        {prompt}

        ### SQL Query:
        """,
        "stream": False,
        "temperature": 0.0 # Set ke 0 untuk hasil yang lebih akurat dan konsisten
    }

    try:
        response = requests.post(url, json=data)
        response.raise_for_status()
        result = response.json()
        # Membersihkan output dari LLM yang kadang menyertakan markdown
        sql_query = result.get("response", "").strip()
        if "```sql" in sql_query:
            sql_query = sql_query.split("```sql")[1].split("```")[0].strip()
        return sql_query
    except Exception as e:
        return f"❌ Error saat menghubungi LLM: {str(e)}"

# ==== RUN SQL ====
def run_sql(sql):
    try:
        conn = get_connection()
        cursor = conn.cursor()
        cursor.execute(sql)
        results = cursor.fetchall()
        cursor.close()
        return pd.DataFrame(results)
    except Exception as e:
        return f"❌ Query Execution Error: {str(e)}"

# ==== STREAMLIT UI ====
st.set_page_config(page_title="Text to SQL with Mistral", page_icon="🤖")

st.sidebar.title("Database Schema")
for table, columns in extract_mysql.tabel_info.items():
    st.sidebar.markdown(f"### {table}")
    st.sidebar.write(", ".join(columns))

st.title("💬 Text to SQL Generator + Executor")
st.markdown("Ketik pertanyaan dalam bahasa natural, lalu lihat hasil SQL-nya dan jalankan langsung.")

# Inisialisasi session_state
if "question" not in st.session_state:
    st.session_state.question = ""
if "generated_sql" not in st.session_state:
    st.session_state.generated_sql = ""

# Text area input
question = st.text_area(
    "Pertanyaan (Natural Language):",
    value=st.session_state.question,
    height=150,
    key="question"
)

# ==== BUTTON CONVERT ====
if st.button("🔍 Convert to SQL"):
    if st.session_state.question.strip() == "":
        st.warning("Masukkan pertanyaan dulu ya.")
    else:
        with st.spinner("Menggunakan Mistral via Ollama..."):
            intent = intent_detect(st.session_state.question)
            if intent == "faq_general":
                st.error("Kami tidak dapat menjawab pertanyaan general")
            elif intent == "external_event_query":
                st.error("Kami tidak dapat menjawab pertanyaan external")
            else:
                sql_result = text_to_sql(st.session_state.question)
                st.session_state.generated_sql = sql_result
                st.code(sql_result, language="sql")

# ==== BUTTON EXECUTE ====
if st.session_state.generated_sql:
    if st.button("Execute SQL"):
        sql = st.session_state.generated_sql
        st.code(sql, language="sql")

        if is_dangerous_query(sql):
            st.error("⚠️ Query terdeteksi berbahaya dan dibatalkan.")
        else:
            with st.spinner("Mengeksekusi query..."):
                result = run_sql(sql)

            if isinstance(result, pd.DataFrame):
                st.success("✅ Query berhasil dijalankan!")
                st.dataframe(result)

                st.download_button(
                    "⬇️ Download CSV",
                    data=result.to_csv(index=False),
                    file_name="query_result.csv",
                    mime="text/csv"
                )
            else:
                st.error(result)