#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-02-28 10:02:26 (ywatanabe)"
# File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/docs/emacs-llm-google-stream-curl.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_PATH="$0.log"
touch "$LOG_PATH"

curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:streamGenerateContent?alt=sse&key=${GOOGLE_API_KEY}" \
     -H 'Content-Type: application/json' \
     --no-buffer \
     -d '{ "contents":[{"parts":[{"text": "Write a cute story about cats."}]}]}'

# (.env-home) (wsl) mngs_repo $ curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:streamGenerateContent?alt=sse&key=${GOOGLE_API_KEY}" \
#             >      -H 'Content-Type: application/json' \
#             >      --no-buffer \
#             >      -d '{ "contents":[{"parts":[{"text": "Write a cute story about cats."}]}]}'
# data: {"candidates": [{"content": {"parts": [{"text": "Wh"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": "iskerton"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " was a tiny ball of fluff, a calico kitten with eyes like melted caramel"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": ". Her favorite pastime was batting at dust bunnies that danced in the sunbeams on"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}


# data: {"candidates": [{"content": {"parts": [{"text": " Mrs. Willowbrook's porch. She wasn't allowed inside, not yet. Mrs. Willowbrook had warned, \"You're still too much of"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " a whirlwind, Whiskerton! Wait until you're a proper lady.\"\n\nBeing a proper lady involved not pouncing on Mrs. Willowbrook's knitting"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " needles and definitely not unravelling her yarn into multi-colored chaos. Whiskerton tried, she really did. She'd practice sitting perfectly still, tail curled neatly around her paws. But then a particularly juicy dust bunny would float"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " past, and her self-control would vanish in a flurry of kitten enthusiasm.\n\nOne afternoon, a rumble started in the sky, followed by fat, lazy raindrops. Whiskerton shivered under the porch, her fur getting damp."}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " Usually, she loved watching the rain from under the eaves, but today it felt cold and lonely.\n\nSuddenly, a warm, furry shape squeezed under the porch beside her. It was Marmalade, the biggest, fluffiest ginger cat in the neighborhood, legendary for his bravery and his booming purr. Marmalade was"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " practically a celebrity in the cat world. Whiskerton had always admired him from afar.\n\n\"Cold, little one?\" Marmalade rumbled, his voice a comforting vibration.\n\nWhiskerton nodded miserably, pushing closer. Marmalade didn't seem to mind. He wrapped his long, fluffy tail around her,"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " creating a cozy haven.\n\n\"Mrs. Willowbrook thinks I'm not a proper lady,\" she confided, her voice a tiny squeak. \"She says I can't come inside until I'm more behaved.\"\n\nMarmalade chuckled, a deep, rumbling sound that shook his whole body. \"Proper"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " lady, you say? Well, I’ve seen Mrs. Willowbrook tripping over her own slippers more than once. Seems to me, even 'proper ladies' have their clumsy moments.\"\n\nHe nuzzled Whiskerton with his head. \"Being a cat is about being yourself. Chase the dust bunnies."}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " Unravel the yarn if it's calling to you. Just do it with a little charm.\"\n\nAs if on cue, a particularly tempting strand of red yarn snaked its way from the porch. Whiskerton’s eyes widened. She glanced at Marmalade, then at the yarn.\n\nMarmalade wink"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": "ed. \"A little charm, remember?\"\n\nWhiskerton took a deep breath and, instead of pouncing, she gently, carefully, nudged the yarn with her paw. She batted it playfully, making it dance just out of her reach. She purred, a tiny, contented sound.\n\nMrs. Willowbrook opened"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " the door, a concerned look on her face. \"Oh, you poor dears! Come inside before you catch a chill!\"\n\nWhiskerton looked up at Mrs. Willowbrook, then back at Marmalade. She knew she hadn't been perfectly behaved, but she had tried.\n\nMrs. Willowbrook smiled"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": ", her eyes crinkling at the corners. \"And you, Marmalade, you big softy. Always looking after the little ones.\"\n\nShe scooped Whiskerton into her arms, warm and dry, and ushered them both inside. As Whiskerton snuggled into Mrs. Willowbrook's lap"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": ", Marmalade sat regally beside them, purring louder than ever. \n\nMaybe, just maybe, being a proper lady wasn't about being perfect, but about being loved, and about having a big, fluffy friend to help you along the way. And that, Whiskerton thought, was the best"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 7,"totalTokenCount": 7},"modelVersion": "gemini-2.0-flash"}

# data: {"candidates": [{"content": {"parts": [{"text": " kind of proper lady of all.\n"}],"role": "model"},"finishReason": "STOP"}],"usageMetadata": {"promptTokenCount": 7,"candidatesTokenCount": 782,"totalTokenCount": 789,"promptTokensDetails": [{"modality": "TEXT","tokenCount": 7}],"candidatesTokensDetails": [{"modality": "TEXT","tokenCount": 782}]},"modelVersion": "gemini-2.0-flash"}

# (.env-home) (wsl) mngs_repo $
# (.env-home) (wsl) mngs_repo $

# EOF