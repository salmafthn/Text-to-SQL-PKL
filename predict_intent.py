import numpy as np
import string
import joblib
from tensorflow.keras.preprocessing.text import Tokenizer
from tensorflow.keras.preprocessing.sequence import pad_sequences
from keras.models import load_model
from tensorflow.keras.preprocessing.text import tokenizer_from_json
import json

max_len = 100

model = load_model("LSTM/intent.keras")
label_encoder = joblib.load("LSTM/label_encoder.pkl")
with open("LSTM/tokenizer.json", "r", encoding="utf-8") as f:
    tokenizer_json = f.read()
    tokenizer = tokenizer_from_json(tokenizer_json)

def predict_intent(text):
    text = text.lower()
    seq = tokenizer.texts_to_sequences([text])
    padded = pad_sequences(seq, maxlen=max_len, padding='post')
    pred = model.predict(padded)
    class_id = np.argmax(pred, axis=1)[0]
    intent_label = label_encoder.inverse_transform([class_id])[0]
    return intent_label