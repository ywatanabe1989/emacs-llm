<!-- ---
!-- Timestamp: 2025-02-26 14:52:51
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/TODO.md
!-- --- -->

Transfer from genai.el

- [x] History management
  - [x] Routing based on size; 10 MB maximum for a history file; backup with timestamp
  - [x] Use ./history directory
  - [x] Show history with parameter n to show interactions
- [ ] Show user input to the buffer as well as template name
- [ ] Scroll
- [ ] Template Handling
- [ ] Copy Blocks
- [ ] Tests
- [ ] On Dired


1. Create a proper history management system with:
   - 10MB maximum file size limit 
   - Automatic backup with timestamps
   - Storage in ./history directory

2. Implement show history functionality with:
   - Parameter for number of interactions to display
   - Display of user input and template name in buffer

3. Implement scrolling mechanism for navigating responses

4. Add template handling system 

5. Implement code block copying functionality

6. Add test framework

7. Include functionality for working with Dired mode

<!-- EOF -->