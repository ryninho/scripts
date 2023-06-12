import streamlit as st
import json

# Set the layout to wide
st.set_page_config(layout="wide")

# Load the JSON file
with open('sg_90k_part1.json') as f:
    data = json.load(f)

# Set up a session state for the index
if 'index' not in st.session_state:
    st.session_state.index = 0

# Define a function to display a conversation
def display_conversation(index):
    conversation = data[index]['conversations']
    for msg in conversation:
        st.markdown(f"**{msg['from']}**: {msg['value']}", unsafe_allow_html=True)

# Add next and previous buttons
col1, col2 = st.columns(2)
if col1.button('Previous') and st.session_state.index > 0:
    st.session_state.index -= 1

if col2.button('Next') and st.session_state.index < len(data) - 1:
    st.session_state.index += 1

# Display the current conversation
display_conversation(st.session_state.index)
