;; Stolen from http://oremacs.com/2015/01/17/setting-up-ediff/
;; This is its own file to facilitate requiring it for things that are
;; being byte-compiled.
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(provide 'ethan-custom)
