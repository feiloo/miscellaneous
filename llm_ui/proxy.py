from fastapi import FastAPI, Request, Body
from fastapi.staticfiles import StaticFiles
import httpx
from fastapi.responses import Response
import json
import requests

# localhost:8081/v1/chat/completions -d

app = FastAPI()

# Serve static files (including index.html) from "static" directory at root
app.mount("/", StaticFiles(directory="static", html=True), name="static")

# Proxy all other requests to the backend service
@app.api_route("/{path:path}", methods=["GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS", "TRACE"])
async def proxy(request: Request, path: str):
    async with httpx.AsyncClient() as client:
        resp = await client.request(
            method=request.method,
            url=f"http://localhost:3000/{path}",
            headers={h: v for h, v in request.headers if h.lower() != "host"},
            data=await request.body(),
            params=request.query_params
        )
    return Response(resp.content, status_code=resp.status_code, headers=dict(resp.headers))

system_prompt = "You are an Scientist and Engineer. \nYou write concisely. \nYou dont use emoji."
user_message = 'hello'
example_messages = [
    {"role": "system", "content": system_prompt},
    {"role": "user", "content": user_message}
]


@app.post("/v1/chat/completions")
def get_completion(messages: list[dict]):
    url = "http://localhost:8081/v1/chat/completions"
    headers = {"Content-Type": "application/json"}
    '''
        [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_message}
        ],
    '''

    data = {
        "messages": messages,
        "stream": False,
        "cache_prompt": True,
        "samplers": "edkypmxt",
        "temperature": 0.6,
        "dynatemp_range": 0,
        "dynatemp_exponent": 1,
        "top_k": 20,
        "top_p": 0.95,
        "min_p": 0,
        "typical_p": 1,
        "xtc_probability": 0,
        "xtc_threshold": 0.1,
        "repeat_last_n": 64,
        "repeat_penalty": 1,
        "presence_penalty": 1,
        "frequency_penalty": 0,
        "dry_multiplier": 0,
        "dry_base": 1.75,
        "dry_allowed_length": 2,
        "dry_penalty_last_n": -1,
        "max_tokens": 32000,
        "timings_per_token": True
    }

    response = requests.post(url, headers=headers, data=json.dumps(data))
    return response.json()
