#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-03-01 07:44:55 (ywatanabe)"
# File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/docs/emacs-llm-anthropic-stream-curl.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_PATH="$0.log"
touch "$LOG_PATH"

# this is a working example
curl https://api.anthropic.com/v1/messages \
     --header "x-api-key: $ANTHROPIC_API_KEY" \
     --header "anthropic-version: 2023-06-01" \
     --header "content-type: application/json" \
     --data \
     '{
"model": "claude-3-7-sonnet-20250219",
"max_tokens": 64000,
"stream": true,
    "thinking": {
        "type": "enabled",
        "budget_tokens": 32000
    },
"messages": [
{
"role": "user",
"content": "hi"
},
{
"role": "assistant",
"content": "hello"
},
{
"role": "user",
"content": "what is your name? could you tell me your characteristics?"
}
]
}'

# (.env-home) (wsl) mngs_repo $ curl https://api.anthropic.com/v1/messages \
    #     }>      --header "x-api-key: $ANTHROPIC_API_KEY" \
    #             >      --header "anthropic-version: 2023-06-01" \
    #             >      --header "content-type: application/json" \
    #             >      --data \
    #             >      '{
# > "model": "claude-3-7-sonnet-20250219",
# > "max_tokens": 64000,
# > "stream": true,
# >     "thinking": {
# >         "type": "enabled",
# >         "budget_tokens": 32000
# >     },
# > "messages": [
# > {
# > "role": "user",
# > "content": "hi"
# > },
# > {
# > "role": "assistant",
# > "content": "hello"
# > },
# > {
# > "role": "user",
# > "content": "what is your name? could you tell me your characteristics?"
# > }
# > ]
# > }'
#      event: message_start
#      data: {"type":"message_start","message":{"id":"msg_01QNTGNkVjq4Cq8H3BMEZjwo","type":"message","role":"assistant","model":"claude-3-7-sonnet-20250219","content":[],"stop_reason":null,"stop_sequence":null,"usage":{"input_tokens":55,"cache_creation_input_tokens":0,"cache_read_input_tokens":0,"output_tokens":12}}     }

#      event: content_block_start
#      data: {"type":"content_block_start","index":0,"content_block":{"type":"thinking","thinking":"","signature":""}              }

#      event: ping
#      data: {"type": "ping"}

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"I'm an AI assistant created by Anthropic to"}     }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" be helpful, harmless, and honest. I don"}              }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"'t have a personal name beyond \"Claude\" or"}           }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" \"Assistant.\" Regarding my characteristics:\n\n1. I'm"}}

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" designed to be helpful by providing accurate information, answ"}           }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"ering questions, and assisting with various tasks within my capabilities.\n\n2"}}

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":". I aim to be harmless by avoiding generating"}           }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" harmful, illegal, unethical or deceptive content."}    }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"\n\n3. I strive to be honest by"}  }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" acknowledging the limitations of my knowledge, being transparent about my uncertainty"} }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":", and not pretending to be human.\n\n4. I"}            }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"'m conversational and aim to be friendly and respect"}}

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"ful.\n\n5. I have knowledge cutoffs"}         }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":", meaning I don't have information about events after my"}   }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" training data cutoff.\n\n6. I don't have the"}          }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" ability to browse the internet, access files unless they"}}

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"'re shared in our conversation, or run code"}         }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" unless specifically enabled.\n\n7. I can't"}         }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" remember details from previous conversations we may have had.\n\n8"}               }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":". I aim to be balanced and fair in my responses."}            }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"\n\nI should provide this information in a straightforward way to"}             }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" the user."}               }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":0,"delta":{"type":"signature_delta","signature":"ErUBCkYIARgCIkDokeDpo6mfn23S/t4IBxCgub1HD+G18/xQA1/5goNJMoUudKTJJi+RtlNyI/HorVbvM2W9meAArUke7AzV3XQHEgxLyEgxeym6YqCQxw0aDLzYRGOKwiPeuES/YCIwL9TTLBtIhe7paMVMZu7HtU5p6Wy3nwvmi5Wi0ySZwUQiDlDSOneyLYF44YvA+SpdKh0NTMDXQLM1dl2+A2XkJLNC1LHEp+cCYT9foABWlw=="}            }

#      event: content_block_stop
#      data: {"type":"content_block_stop","index":0           }

#      event: content_block_start
#      data: {"type":"content_block_start","index":1,"content_block":{"type":"text","text":""}  }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"Hi there! My name is Claude. I'm an AI assistant"} }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":" created by Anthropic.\n\nAs for my characteristics:"}      }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"\n- I'm designed to be helpful, harmless, and honest"}            }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"\n- I aim to be conversational and friendly in"}           }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":" my interactions\n- I can assist with a wide range of tasks like"}          }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":" answering questions, creative writing, summarizing information, and more"}            }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"\n- I have knowledge limitations - my training"}              }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":" data has a cutoff date, so I may"}            }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":" not know about very recent events\n- I st"}  }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"rive to be balanced and fair in my responses"}       }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"\n- I'm always learning and improving through feedback"} }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"\n\nIs there something specific I can help you with today"} }

#      event: content_block_delta
#      data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"?"}      }

#      event: content_block_stop
#      data: {"type":"content_block_stop","index":1        }

#      event: message_delta
#      data: {"type":"message_delta","delta":{"stop_reason":"end_turn","stop_sequence":null},"usage":{"output_tokens":390} }

#      event: message_stop
#      data: {"type":"message_stop"           }

#      (.env-home) (wsl) mngs_repo $

# EOF