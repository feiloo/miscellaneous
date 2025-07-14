# Notes:

### llamacpp openhands
to use llamacpp with All-Hands-AI/OpenHands locally:

under settings -> LLM -> advanced -> custom model, enter `openai/...`

since llamacpp-server is openai api compatible.

under settings -> LLM -> advanced -> base url, enter: `http://host.docker.internal:8080/v1`

host.docker.internal is the internal gateway of docker, to reach external services/ports as an alternative to `--net=host`

its specified by the openhands docker run command: `--add-host=host.docker.internal:host-gateway`

#### or run raw with only the sandbox dockerized

git clone openhands...
cd openhands...
export SANDBOX_RUNTIME_CONTAINER_IMAGE=ghcr.io/all-hands-ai/runtime:0.48-nikolaik
INSTALL_PLAYWRIGHT=0 RUNTIME=docker make build RUNTIME=docker

DEBUG=1 make run FRONTEND_PORT=3000 FRONTEND_HOST=0.0.0.0 BACKEND_HOST=0.0.0.0
