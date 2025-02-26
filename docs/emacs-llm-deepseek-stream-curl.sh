#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-02-26 13:47:15 (ywatanabe)"
# File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/curl/emacs-llm-deepseek-stream-curl.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_PATH="$0.log"
touch "$LOG_PATH"

curl https://api.deepseek.com/chat/completions \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer $DEEPSEEK_API_KEY" \
     -d '{
        "engine": "deepseek-chat",
        "messages": [
          {"role": "system", "content": "You are a helpful assistant."},
          {"role": "user", "content": "Hello!"}
        ],
        "stream": true
      }'

# data: {"id":"37db0079-44a0-4f95-adae-bbea58339a70","object":"chat.completion.chunk","created":1740537349,"engine":"deepseek-chat","system_fingerprint":"fp_3a5770e1b4_prod0225","choices":[{"index":0,"delta":{"role":"assistant","content":""},"logprobs":null,"finish_reason":null}]}

# data: {"id":"37db0079-44a0-4f95-adae-bbea58339a70","object":"chat.completion.chunk","created":1740537349,"engine":"deepseek-chat","system_fingerprint":"fp_3a5770e1b4_prod0225","choices":[{"index":0,"delta":{"content":"Hello"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"37db0079-44a0-4f95-adae-bbea58339a70","object":"chat.completion.chunk","created":1740537349,"engine":"deepseek-chat","system_fingerprint":"fp_3a5770e1b4_prod0225","choices":[{"index":0,"delta":{"content":"!"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"37db0079-44a0-4f95-adae-bbea58339a70","object":"chat.completion.chunk","created":1740537349,"engine":"deepseek-chat","system_fingerprint":"fp_3a5770e1b4_prod0225","choices":[{"index":0,"delta":{"content":" How"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"37db0079-44a0-4f95-adae-bbea58339a70","object":"chat.completion.chunk","created":1740537349,"engine":"deepseek-chat","system_fingerprint":"fp_3a5770e1b4_prod0225","choices":[{"index":0,"delta":{"content":" can"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"37db0079-44a0-4f95-adae-bbea58339a70","object":"chat.completion.chunk","created":1740537349,"engine":"deepseek-chat","system_fingerprint":"fp_3a5770e1b4_prod0225","choices":[{"index":0,"delta":{"content":" I"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"37db0079-44a0-4f95-adae-bbea58339a70","object":"chat.completion.chunk","created":1740537349,"engine":"deepseek-chat","system_fingerprint":"fp_3a5770e1b4_prod0225","choices":[{"index":0,"delta":{"content":" assist"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"37db0079-44a0-4f95-adae-bbea58339a70","object":"chat.completion.chunk","created":1740537349,"engine":"deepseek-chat","system_fingerprint":"fp_3a5770e1b4_prod0225","choices":[{"index":0,"delta":{"content":" you"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"37db0079-44a0-4f95-adae-bbea58339a70","object":"chat.completion.chunk","created":1740537349,"engine":"deepseek-chat","system_fingerprint":"fp_3a5770e1b4_prod0225","choices":[{"index":0,"delta":{"content":" today"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"37db0079-44a0-4f95-adae-bbea58339a70","object":"chat.completion.chunk","created":1740537349,"engine":"deepseek-chat","system_fingerprint":"fp_3a5770e1b4_prod0225","choices":[{"index":0,"delta":{"content":"?"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"37db0079-44a0-4f95-adae-bbea58339a70","object":"chat.completion.chunk","created":1740537349,"engine":"deepseek-chat","system_fingerprint":"fp_3a5770e1b4_prod0225","choices":[{"index":0,"delta":{"content":" 😊"},"logprobs":null,"finish_reason":null}]}

# data: {"id":"37db0079-44a0-4f95-adae-bbea58339a70","object":"chat.completion.chunk","created":1740537349,"engine":"deepseek-chat","system_fingerprint":"fp_3a5770e1b4_prod0225","choices":[{"index":0,"delta":{"content":""},"logprobs":null,"finish_reason":"stop"}],"usage":{"prompt_tokens":11,"completion_tokens":11,"total_tokens":22,"prompt_tokens_details":{"cached_tokens":0},"prompt_cache_hit_tokens":0,"prompt_cache_miss_tokens":11}}

# data: [DONE]

# (wsl) llemacs.el $
# (wsl) llemacs.el $

# EOF