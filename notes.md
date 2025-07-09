# Notes:

### llamacpp openhands
to use llamacpp with All-Hands-AI/OpenHands locally:

under settings -> LLM -> advanced -> custom model, enter `openai/...`

since llamacpp-server is openai api compatible.

under settings -> LLM -> advanced -> base url, enter: `http://host.docker.internal:8080/v1`

host.docker.internal is the internal gateway of docker, to reach external services/ports as an alternative to `--net=host`

its specified by the openhands docker run command: `--add-host=host.docker.internal:host-gateway`
