<!-- ---
!-- Timestamp: 2025-03-01 16:20:28
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/README.md
!-- --- -->

# Emacs LLM

[![Build Status](https://github.com/ywatanabe1989/emacs-llm/workflows/tests/badge.svg)](https://github.com/ywatanabe1989/emacs-llm/actions)

An Emacs interface for LLMs including OpenAI, Google, Anthropic and DeepSeek engines.

## Features

- Stream responses from multiple LLM providers
- Markdown rendering for AI responses
- Conversation history management
- Code block navigation and extraction
- Provider and engine switching
- Template system for prompts

## Installation

Clone this repository to your preferred location:

```bash
git clone https://github.com/your-username/emacs-llm.git ~/.emacs.d/lisp/emacs-llm
```

Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-llm")
(require 'emacs-llm)
(--el-setup-key-bindings)
```

## Configuration

Set up API keys in your environment variables or in your Emacs configuration:

```elisp
;; API Keys (set one or more)
(setq --el-api-key-openai "your-openai-api-key")
(setq --el-api-key-anthropic "your-anthropic-api-key")
(setq --el-api-key-google "your-google-api-key")
(setq --el-api-key-deepseek "your-deepseek-api-key")

;; Default provider and engine
(setq --el-default-provider "google")
(setq --el-default-engine "gemini-2.0-flash")
```

## Usage

- `C-c l o` - Run LLM on region or prompt
- `C-c l s` - Switch provider and engine
- `C-c l h` - Show conversation history
- `C-c l c` - Copy last LLM response
- `C-c l q` - Quick menu for emacs-llm

## Key Bindings

All key bindings are under the `C-c l` prefix by default.

## History Management
Conversation history is automatically saved and managed:
- History is stored in JSON format
- Auto-backup when history file exceeds 10MB
- View history with `el-history-show` command
- Copy previous responses with `--el-copy-last-response`

## Templates
Templates for common prompt types are stored in the `templates/` directory.

## Supported Providers
- OpenAI (GPT-4o, GPT-3.5-Turbo, etc.)
- Google (Gemini Pro, Gemini Flash, etc.)
- Anthropic (Claude 3, etc.)
- DeepSeek (DeepSeek Chat, DeepSeek Coder, etc.)

(define-key global-map
            (kbd "C-M-g")
            'genai-on-region)

<!-- EOF -->