(defun begin ()
  "doc..."
  (push-mark) ;set mark at current position
  (goto-char (point-min)))
(begin)

(string-lessp "a" "ab")
(string< "a" "ab")
(string= "a" "a")
(string-equal "a" "a")

(defun mark-whole ()
  "doc..."
  (interactive)
  (push-mark)
  (push-mark (point-max) nil t)
  (goto-char (point-min)))
(mark-whole)

(defun append-buffer (buffer start end)
  "doc..."
  (interactive
   (list (read-buffer "Append to buffer: "                    ; read the name of the buffer.
                      (other-buffer (current-buffer) t))      ; Most recently BUFFER that is not 'current-buffer.'
         (region-beginning) (region-end)))
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (let* ((append-to (get-buffer-create buffer))           ; find a named buffer or create one.
             (windows (get-buffer-window-list append-to t t)) ; return list of all window displaying the specified buffer.
             point)
        (set-buffer append-to)                                ; make buffer current for editing operations.
        (setq point (point))
        (barf-if-buffer-read-only)                            ; signal an error if the current buffer is read only.
        (insert-buffer-substring oldbuf start end)
        (dolist (window windows)
          (when (= (window-point window) point)
            (set-window-point window (point))))))))

(defun copy-buffer (buffer start end)
  "doc..."
  (interactive "BCopy to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (with-current-buffer (get-buffer-create buffer) ;execute body with the specified buffer.
      (barf-if-buffer-read-only)
      (erase-buffer)
      (save-excursion
        (insert-buffer-substring oldbuf start end)))))

(defun insert-buffer (buffer)
  "doc..."
  (interactive "*bInsert buffer: ")      ; * warning read only buffer / b must be an existing buffer
  (or (bufferp buffer)                   ;if buffer is actually a real buffer, not the name of the buffer.
      (setq buffer (get-buffer buffer))) ;if it's the name, get buffer by name.
  (let (start end newmark)
    (save-excursion
      (save-excursion
        (set-buffer buffer)
        (setq start (point-min) end (point-max)))
      (insert-buffer-substring buffer start end)
      (setq newmark (point)))
    (push-mark newmark)))

; or can take any number of arguments. It returns the value of the first arg that is not nil.

(defun begin-full-buffer (&optional arg)
  "doc..."
  (interactive "P")                                     ; the arg is a raw arg not a numeric number. It is a prefix arg.
  (or (consp arg)
      (and transient-mark-mode mark-active)
      (push-mark))
  (let ((size (- (point-max) (point-min))))
    (goto-char (if (and arg (not (consp arg)))
                   (+ (point-min)
                      (if(size > 10000)
                          (* (prefix-numeric-value arg) ;convert raw arg to numeric arg
                             (/ size 10))
                          (/ (+ 10 (* size (prefix-numeric-value arg)))
                           10)))
                 (point-min))))
  (if arg (forwark-line1)))

(save-restriction
  body...) ;recover narrowing after the execution of the body.

(defun what-line ()
  "doc..."
  (interactive)
  (save-restriction
    (widen) ;undoes any narrowing in the buffer
    (save-excursion
      (beginning-of-line)
      (message "Line %d"
               (+1 (count-lines 1 (point)))))))

(car '(1 2 3 4))          ; the head of the list ; return the first element as a list.
(cdr '(1 2 3 4))          ; the rest of the list
(nthcdr 2 '(1 2 3 4))     ; (cdr (cdr '(1 2 3 4)))
(cons 1 '(2 3 4 5))       ; construct a list
(length '(1 2 3 4))

(setq numbers '(1 2 3 4))
(setcar numbers 0)        ;replace 1 by 0
numbers

(setcdr numbers '(5 6 7)) ; replace the rest of the list by (5 6 7)
numbers

(defun zap-to-char (arg char)
  "doc..."
  (interactive "p\ncZap to char: ")                                  ; p -> processed prefix || \nc -> prompt + wait for a char
  (if (char-table-p translation-table-for-input)                     ; char-table-p -> caracter belongs to char table 
      (setq char (or (aref translation-table-for-input char) char))) ; aref -> extract an element from an array
  (kill-region (point)(progn                                         ; each of its args are evaluated. The last arg return the final value.
                        (search-forward (char-to-string char)        ; ensure that computer treats that caracter as a string
                                        nil nil arg)                 ; bound the search; what it should do if the search failed; repeat count
                        (point))))                                   ; return the found caracter point

; Macro like when and unless enable you to define new control constructs and other language features.

(defun kill-region (beg end)
  "doc..."
  (interactive (list (point) (mark)))
  (unless (and beg end)                                     ; like if without then part
    (error "The mark is not set now, so there is no region"))
  (condition-case nil                                       ; first argument -> if nil, information about the error signal is not stored for use by another function.
      (let ((string (filter-buffer-substring beg end t)))   ; second argument -> when all goes well. Extract the specified string.
        (when string                                        ; check is string exists => like if without else clause. It's a macro.
          (if (eq last-command 'kill-region)                ; if previous command was another call to kill-region, then the new text is appended to the old text.
              (kill-append string (< end beg) yank-handler) ; concat the new string with the old.
            (kill-new string nil yank-handler)))            ; else kill-new is called. This attach a new element to the kill-new function.
        (when (or string (eq last-command 'kill-region))    ; if string defined or last command was kill-region so ...
          (setq this-command 'kill-region'))                ; set the current command to be kill-region
        nil)
    ((buffer-read-only text-read-only)                      ; third argument -> manage errors : if this condition are met (read only buffer/text), execute the body.
     (copy-region-as-kill beg end)
     (setq this-command 'kill-region)
     (if kill-read-only-ok
         (progn (message "Read only text copied to kill ring") nil)
       (barf-if-buffer-read-only)
       (signal 'text-read-only (list (current-buffer)))))))

;condition-case is also a macro and seems strongly to if : a then & else parts.

(defun copy-region-as-kill (beg end)
  "doc..."
  (interactive "r") ;"r" the function is interactive so, the 2 args must refer to the begining and end of th region.
  (if(eq last-command 'kill-region) ; eq tests is the two args are the same Object (but with different names)
      (kill-append (filter-buffer-substring beg end) (< beg end))
    (kill-new (filter-buffer-substring beg end)))
  (if transient-mark-mode 
      (setq desactivate-mark t))
  nil)

(defun kill-append (string before-p &optional yank-handler) ; yank-handler tells the function how to deal with properties added to text.
  "doc..." ; before-p if actually a predicate like (< end beg)
  (let * ((cur (car kill-ring)))
       (kill-new (if before-p (concat string cur) (concat cur string)) ; if before-p we prepend the string to last inserted elt. Otherwise, we append it.
                 (or (= (length cur) 0)
                     (equal yank-handler
                            (get-text-property 0 'yank-handler cur)))
                 yank-handler)))

(defun kill-new (string &optional replace yank-handler)
  "doc..."
  if( (> (length string) 0)
      (if yank-handler
          (put-text-property 0 (length string)
                             'yank-handler yank-handler string))
      (if yank-handler
          (signal 'args-out-of-range
                  (list string "yank-handler specified for empty string"))))
  (if (fboundp 'menu-bar-update-yank-menu)
      (menu-bar-update-yank-menu string (and replace (car kill-ring))))
  (if (and replace kill-ring)
      (setcar kill-ring string)
    (push string kill-ring)
    (if (> (length kill-ring) kill-ring-max)
        (setcdr (nthcdr (1 - kill-ring-max) kill-ring) nil)))
      (setq kill-region-yank-pointer kill-ring)
      (if interprogram-cut-function
          (funcall interprogram-cut-function string (not replace))))
