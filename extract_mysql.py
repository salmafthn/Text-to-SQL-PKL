from sqlalchemy import create_engine, inspect
from dotenv import load_dotenv
import os

load_dotenv()

username_db = os.getenv("USERNAME_DB")
password_db = os.getenv("PASSWORD_DB")
port_db = os.getenv("PORT_DB")
name_db = os.getenv("DATABASE_NAME")

print("✅ Database aktif:", name_db)
engine = create_engine("mysql+pymysql://root:@localhost:3306/skripsi")

inspector = inspect(engine)
tables = inspector.get_table_names()

tabel_info = {}

for table_name in tables:
    columns = inspector.get_columns(table_name)
    tabel_info[table_name] = {}
    for col in columns:
        tipe = str(col["type"]).split(" ")[0].replace('"utf8mb4_unicode_ci"', "").replace("COLLATE", "").strip()
        tabel_info[table_name][col["name"]]=tipe

## Jika ingin melihat full isi database di terminal
# import json
# print(json.dumps(tabel_info, indent=2))