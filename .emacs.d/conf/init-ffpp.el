;; FreeFem++ Mode

;; enable cc-mode
(autoload 'awk-mode "cc-mode" nil t)

(autoload 'ff++-mode "ff++-mode" "Major mode for editing FreeFem++ code." t)
(add-to-list 'auto-mode-alist '("\\.edp$" . ff++-mode))
