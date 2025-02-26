#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-02-26 13:47:14 (ywatanabe)"
# File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/curl/emacs-llm-anthropic-stream-curl.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_PATH="$0.log"
touch "$LOG_PATH"

curl https://api.anthropic.com/v1/messages \
     --header "x-api-key: $ANTHROPIC_API_KEY" \
     --header "anthropic-version: 2023-06-01" \
     --header "anthropic-beta: output-128k-2025-02-19" \
     --header "content-type: application/json" \
     --data \
     '{
    "engine": "claude-3-7-sonnet-20250219",
    "max_tokens": 128000,
    "stream": true,
    "thinking": {
        "type": "enabled",
        "budget_tokens": 32000
    },
    "messages": [
        {
            "role": "user",
            "content": "Generate a comprehensive analysis of..."
        }
    ]
}'

# event: message_start
# data: {"type":"message_start","message":{"id":"msg_01FWVuF9t1o5peo9Zz2B1seh","type":"message","role":"assistant","engine":"claude-3-7-sonnet-20250219","content":[],"stop_reason":null,"stop_sequence":null,"usage":{"input_tokens":41,"cache_creation_input_tokens":0,"cache_read_input_tokens":0,"output_tokens":6}}          }

# event: content_block_start
# data: {"type":"content_block_start","index":0,"content_block":{"type":"thinking","thinking":"","signature":""} }

# event: ping
# data: {"type": "ping"}

# event: content_block_delta
# data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"It looks like the user"}       }

# event: content_block_delta
# data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" has begun to provide an"}     }

# event: content_block_delta
# data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" instruction, but the message"}           }

# event: content_block_delta
# data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" was cut off. I"}            }

# event: content_block_delta
# data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" only see \"Generate a comprehensive"}  }

# event: content_block_delta
# data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" analysis of...\" "}    }

# event: content_block_delta
# data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"\n\nIn this situation, I shoul"}               }

# event: content_block_delta
# data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"d ask the user to complete their request"}            }

# event: content_block_delta
# data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":", rather than attempt to guess"}           }

# event: content_block_delta
# data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" what they want me to analyze. I'll"}            }

# event: content_block_delta
# data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" politely ask them to provide"}  }

# event: content_block_delta
# data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" the complete instruction so I can properly"}            }

# event: content_block_delta
# data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" assist them."}            }

# event: content_block_delta
# data: {"type":"content_block_delta","index":0,"delta":{"type":"signature_delta","signature":"EuYBCkQYAiJAk7JxeiLUwvEG+ieVefBBoien7A7BpUQB64WbblMdoI+Kk6pPciBbKpMWfsuaXo0E5/IikaaWgTrXii7wlDhunRIM9GWKw9yeT6PbBIJrGgwnmdTJHy9fzgTHttEiMLDOicGD2LyT9mzjlkksOfrqG//Bm1/yY5fyvW7O81rxq4inHDt+OUVW/B0aq22RTipQdde0yYh8Op7aDP6K+Vrscx7mU1PGA0cj2Mn6vt4XguR6MJT1bfe2yuNizU7ip+tr7nCGBaw1DXKDTQO6hu0z82eOxIZgzhVhuBdvX/U8KWQ="}              }

# event: content_block_stop
# data: {"type":"content_block_stop","index":0    }

# event: content_block_start
# data: {"type":"content_block_start","index":1,"content_block":{"type":"text","text":""}              }

# event: content_block_delta
# data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"I notice your message was cut off after"} }

# event: content_block_delta
# data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":" \"Generate a comprehensive analysis of...\" \n\nCoul"}            }

# event: content_block_delta
# data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"d you please complete your request"}              }

# event: content_block_delta
# data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"? I'd be happy to provide"}    }

# event: content_block_delta
# data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":" a comprehensive analysis once I"}          }

# event: content_block_delta
# data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":" know what topic or subject you'd like"}            }

# event: content_block_delta
# data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":" me to examine."}   }

# event: content_block_stop
# data: {"type":"content_block_stop","index":1       }

# event: message_delta
# data: {"type":"message_delta","delta":{"stop_reason":"end_turn","stop_sequence":null},"usage":{"output_tokens":134}  }

# event: message_stop
# data: {"type":"message_stop"         }

# (wsl) llemacs.el $
# (wsl) llemacs.el $

# EOF