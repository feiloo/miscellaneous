import argparse
import os
import whisper
from transformers import AutoModelForCausalLM, AutoTokenizer

def transcribe_audio(video_file):
    # Load the Whisper model
    model = whisper.load_model("base")

    # Transcribe the audio from the video file
    result = model.transcribe(video_file)
    return result["text"]

def generate_summary_qwen(transcript, model, tokenizer):
    # Define the prompt and messages
    #prompt = f"Summarize the following text:\n{transcript}"
    prompt = f"Fasse den folgenden Text Stichwortartig zusammen:\n{transcript}"
    messages = [
        {"role": "system", "content": "You are a helpful assistant. You talk in German."},
        {"role": "user", "content": prompt}
    ]

    # Apply chat template
    text = tokenizer.apply_chat_template(
        messages,
        tokenize=False,
        add_generation_prompt=True
    )

    # Tokenize the input
    model_inputs = tokenizer([text], return_tensors="pt").to(model.device)

    # Generate the summary
    generated_ids = model.generate(
        **model_inputs,
        max_new_tokens=512
    )

    # Decode the generated tokens
    generated_ids = [
        output_ids[len(input_ids):] for input_ids, output_ids in zip(model_inputs.input_ids, generated_ids)
    ]

    response = tokenizer.batch_decode(generated_ids, skip_special_tokens=True)[0]
    return response

def main(files, action):
    # Load the Qwen model and tokenizer
    model_name = "Qwen/Qwen2.5-32B-Instruct"
    model = AutoModelForCausalLM.from_pretrained(
        model_name,
        torch_dtype="auto",
        device_map="auto"
    )
    tokenizer = AutoTokenizer.from_pretrained(model_name)

    for file in files:
        print(f"Processing {file}...")

        if action == "transcribe" or action == "both":
            try:
                transcript = transcribe_audio(file)
            except Exception as e:
                print(f"Error transcribing {file}: {e}")
                continue

        if action == 'transcribe':
            print(f"Transcript: {transcript}\n")


        if action == "summarize":
            # Assume the file is a transcript file
            if not os.path.isfile(file):
                print(f"Error: File {file} does not exist.")
                continue

            try:
                with open(file, 'r') as f:
                    transcript = f.read()
            except Exception as e:
                print(f"Error reading transcript file {file}: {e}")
                continue

        if action == "summarize" or action == "both":
            try:
                summary = generate_summary_qwen(transcript, model, tokenizer)
                print(f"Summary: {summary}\n")
            except Exception as e:
                print(f"Error summarizing {file}: {e}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Process video files to transcribe or summarize audio.")
    parser.add_argument('files', nargs='+', help='List of video files or transcript files')
    parser.add_argument('--action', choices=['transcribe', 'summarize', 'both'], default='both',
                        help='Action to perform: transcribe, summarize, or both (default: both)')

    args = parser.parse_args()

    main(args.files, args.action)
