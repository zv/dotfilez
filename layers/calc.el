
;;; Mode settings stored by Calc on Sat Nov 14 15:10:44 2015
(setq calc-language 'big)
(setq calc-group-digits t)
;;; End of mode settings
(define-key calc-mode-map next-buffer-key 'next-buffer)

(defmath nradix (n base)
  "This function convertions the number `n' to the radix specified by `base'"
  (let ((calc-number-radix base))
    (math-format-radix n)))
