;;; ethan-java.el: customizations for Java

;;; java-mode-indent-annotations
(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)
;;; end java-mode-indent-annotations

(provide 'ethan-java)
