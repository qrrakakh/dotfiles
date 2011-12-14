(require 'ecb)

(defun ecb-toggle ()
    (interactive)
      (if ecb-minor-mode
                (ecb-deactivate)
            (ecb-activate)))
(global-set-key [f2] 'ecb-toggle)

(custom-set-variables
 '(ecb-options-version "2.32"))
