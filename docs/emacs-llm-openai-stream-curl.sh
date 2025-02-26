#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-02-26 13:47:15 (ywatanabe)"
# File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/curl/emacs-llm-openai-stream-curl.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_PATH="$0.log"
touch "$LOG_PATH"

curl -N https://api.openai.com/v1/chat/completions \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer $OPENAI_API_KEY" \
     -d '{
"engine": "gpt-4",
"messages": [
             {
             "role": "system",
             "content": "You are a helpful assistant."
             },
             {
             "role": "user",
             "content": "Hello!"
             }
             ],
"stream": true
}'

# data: {"id":"chatcmpl-B4xV6rdSGNVcnBlY6ghdGwUPO8IB3","object":"chat.completion.chunk","created":1740521732,"engine":"gpt-4-0613","service_tier":"default","system_fingerprint":null,"choices":[{"index":0,"delta":{"role":"assistant","content":"","refusal":null},"logprobs":null,"finish_reason":null}]}

# data: {"id":"chatcmpl-B4xV6rdSGNVcnBlY6ghdGwUPO8IB3","object":"chat.completion.chunk","created":1740521732,"engine":"gpt-4-0613","service_tier":"default","system_fingerprint":null,"choices":[{"index":0,"delta":{"content":"Hello"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"chatcmpl-B4xV6rdSGNVcnBlY6ghdGwUPO8IB3","object":"chat.completion.chunk","created":1740521732,"engine":"gpt-4-0613","service_tier":"default","system_fingerprint":null,"choices":[{"index":0,"delta":{"content":"!"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"chatcmpl-B4xV6rdSGNVcnBlY6ghdGwUPO8IB3","object":"chat.completion.chunk","created":1740521732,"engine":"gpt-4-0613","service_tier":"default","system_fingerprint":null,"choices":[{"index":0,"delta":{"content":" How"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"chatcmpl-B4xV6rdSGNVcnBlY6ghdGwUPO8IB3","object":"chat.completion.chunk","created":1740521732,"engine":"gpt-4-0613","service_tier":"default","system_fingerprint":null,"choices":[{"index":0,"delta":{"content":" can"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"chatcmpl-B4xV6rdSGNVcnBlY6ghdGwUPO8IB3","object":"chat.completion.chunk","created":1740521732,"engine":"gpt-4-0613","service_tier":"default","system_fingerprint":null,"choices":[{"index":0,"delta":{"content":" I"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"chatcmpl-B4xV6rdSGNVcnBlY6ghdGwUPO8IB3","object":"chat.completion.chunk","created":1740521732,"engine":"gpt-4-0613","service_tier":"default","system_fingerprint":null,"choices":[{"index":0,"delta":{"content":" assist"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"chatcmpl-B4xV6rdSGNVcnBlY6ghdGwUPO8IB3","object":"chat.completion.chunk","created":1740521732,"engine":"gpt-4-0613","service_tier":"default","system_fingerprint":null,"choices":[{"index":0,"delta":{"content":" you"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"chatcmpl-B4xV6rdSGNVcnBlY6ghdGwUPO8IB3","object":"chat.completion.chunk","created":1740521732,"engine":"gpt-4-0613","service_tier":"default","system_fingerprint":null,"choices":[{"index":0,"delta":{"content":" today"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"chatcmpl-B4xV6rdSGNVcnBlY6ghdGwUPO8IB3","object":"chat.completion.chunk","created":1740521732,"engine":"gpt-4-0613","service_tier":"default","system_fingerprint":null,"choices":[{"index":0,"delta":{"content":"?"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"chatcmpl-B4xV6rdSGNVcnBlY6ghdGwUPO8IB3","object":"chat.completion.chunk","created":1740521732,"engine":"gpt-4-0613","service_tier":"default","system_fingerprint":null,"choices":[{"index":0,"delta":{},"logprobs":null,"finish_reason":"stop"}]}

# data: [DONE]

# EOF