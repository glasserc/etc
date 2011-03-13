I'm currently messing around with keeping my .emacs.d using elhome_, which is itself kept in el-get. Accordingly, most of my configs are loaded automatically from elhome/startup or elhome/settings. init.el is basically a shim to load el-get, and with el-get to load elhome.

.. _elhome: https://github.com/dabrahams/elhome/

Originally, however, I copied the structure from `emacs-starter-kit`_, so there's still a certain amount of vestigal stuff from there. Specifically, the lisp config is inherited from emacs-starter-kit, and so is the elpa-to-submit. I copied the idea of having subject-specific files named (for example) ethan-defuns.el, and many of the functions defined there were copied from emacs-starter-kit. Some files like this are still loaded manually; when possible, they've been moved to elhome/site-lisp.

.. _emacs-starter-kit: https://github.com/technomancy/emacs-starter-kit/

I also "manually" track an upstream git repo or two myself. These are in elhome/site-lisp/upstream.
