(defun egc-java-mode-fix-inner-class-offsets ()
  "Mess with the c-offsets-alist to make inner classes indent correctly.

Without this, two indents are added in anonymous inner
classes. This means indentation becomes:

    Callable<Rule> task = new Callable<Rule>() {
            public Rule call() {
                Rule rule = Rule.get(id);
                return rule;
            }
        };

instead of the desired:

    Callable<Rule> task = new Callable<Rule>() {
        public Rule call() {
            Rule rule = Rule.get(id);
            return rule;
        }
    };

This might need to be different on a per-project basis, but
hopefully I won't be writing enough Java that it matters.

See also http://www.mail-archive.com/jde@sunsite.auc.dk/msg01159.html and
http://ftp.heanet.ie/disk1/www.gnu.org/software/emacs/manual/html_node/ccmode/Anonymous-Class-Symbol.html"
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0)))

(add-hook 'java-mode-hook 'egc-java-mode-fix-inner-class-offsets)

(provide 'ethan-java)
