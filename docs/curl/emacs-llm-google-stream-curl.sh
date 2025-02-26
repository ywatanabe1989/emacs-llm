#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-02-26 13:47:15 (ywatanabe)"
# File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/curl/emacs-llm-google-stream-curl.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_PATH="$0.log"
touch "$LOG_PATH"

curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:streamGenerateContent?alt=sse&key=${GOOGLE_API_KEY}" \
     -H 'Content-Type: application/json' \
     --no-buffer \
     -d '{ "contents":[{"parts":[{"text": "Write a cute story about cats."}]}]}'

# (wsl) llemacs.el $ curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:streamGenerateContent?alt=sse&key=${GOOGLE_API_KEY}" \
    #       >       -H 'Content-Type: application/json' \
    #       >       --no-buffer \
    #       >       -d '{ "contents":[{"parts":[{"text": "Write a cute story about cats."}]}]}'

# data: {"candidates": [{"content": {"parts": [{"text": "C"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": "lementine was a tiny tabby kitten with a white bib and eyes the color of melted butter"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": "scotch. She lived in a cozy little cottage with Mrs. Higgins, a woman whose"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " heart was as warm as the freshly baked bread that always seemed to be cooling on the windowsill.\n\nClementine, or \"Clemmy\" as Mrs. Higgins affectionately called"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " her, was a curious creature. Every day was a new adventure, filled with fascinating discoveries. A dust bunny under the rug became a ferocious beast, the sunbeam"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " on the kitchen floor a purrfect napping spot, and Mrs. Higgins' knitting yarn a delightfully bouncy, wriggly prey.\n\nOne crisp autumn afternoon, Clemmy was playing hide-and-seek amongst the fallen leaves in"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " Mrs. Higgins' garden. The crunchy leaves tickled her whiskers as she pounced and batted, chasing after imaginary squirrels. Suddenly, she saw something new: a bright red apple, fallen from the ancient apple tree. It was bigger than her"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " head!\n\nClemmy cautiously approached the apple, her tail twitching with curiosity. She sniffed it tentatively, then gave it a little boop with her nose. The apple rolled slightly, and Clemmy jumped back in surprise. This giant, red thing was clearly alive!\n\nShe decided to stalk it. Low"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " to the ground, she crept forward, her tiny paws making barely a sound in the leaves. When she was close enough, she launched herself onto the apple, clinging to its smooth, waxy surface. But the apple, being perfectly round, began to roll again!\n\nClemmy squeaked, her claws scr"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": "abbling for purchase. She was rolling! The apple was carrying her on an unexpected adventure! She tumbled through the leaves, giggling (or at least, the kitten equivalent of giggling), as the apple rolled faster and faster.\n\nFinally, the apple bumped gently against Mrs. Higgins' boots. Mrs. Higgins, who"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " had been watching the whole scene with a smile, chuckled.\n\n\"Well, look at you, Clemmy!\" she said, picking up the little kitten, apple and all. Clemmy purred loudly, rubbing her head against Mrs. Higgins' cheek. The adventure had been exciting, but being back in her human"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": "'s arms felt even better.\n\nMrs. Higgins carried Clemmy inside, placing the apple on the kitchen table. Clemmy, still feeling adventurous, decided the table was now Mount Everest. She carefully climbed to the top, puffing out her chest. From her vantage point, she surveyed her domain: the cozy"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " cottage, the warm fire crackling in the hearth, and Mrs. Higgins humming as she poured milk into a saucer.\n\nClemmy knew she was the luckiest kitten in the world. She had a warm home, a loving human, and endless adventures waiting for her, even if they only involved a very large"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": ", very red, and very rolly apple. And as she curled up on Mrs. Higgins' lap, purring contentedly, she dreamt of even bigger and bolder apple-rolling adventures to come.\n"}],"role": "model"},"finishReason": "STOP"}],"usageMetadata": {"promptTokenCount": 7,"candidatesTokenCount": 627,"totalTokenCount": 634,"promptTokensDetails": [{"modality": "TEXT","tokenCount": 7}],"candidatesTokensDetails": [{"modality": "TEXT","tokenCount": 627}]},"modelVersion": "gemini-2.0-flash"}

# (wsl) llemacs.el $
# (wsl) llemacs.el $

# EOF