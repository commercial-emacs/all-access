|build-status|

=================================================
 Emacs All Access
=================================================

.. COMMENTARY (see Makefile)

Install
=======
Clone and ``make install``.

Frequently Asked Questions
==========================

... How can I integrate this with ``use-package``?
    ::

        M-x customize-option RET use-package-ensure-function RET
        Custom: aa-use-package-ensure-function

.. _Getting started: http://melpa.org/#/getting-started
.. _Issue 2944: https://github.com/melpa/melpa/issues/2944
.. _Advising Functions: https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
.. _reimplementing their service: https://github.com/commercial-emacs/shmelpa
.. _quelpa: https://github.com/quelpa/quelpa
.. _package-build: https://github.com/melpa/package-build
.. _politza/pdf-tools: https://github.com/politza/pdf-tools
.. _uninteresting comments: https://raw.githubusercontent.com/commercial-emacs/all-access/dev/elpas.txt
.. _MELPA: https://github.com/melpa/melpa#recipe-format
.. _ELPA: https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/README

.. |build-status|
   image:: https://github.com/commercial-emacs/all-access/workflows/CI/badge.svg?branch=github
   :target: https://github.com/commercial-emacs/all-access/actions
   :alt: Build Status

.. |---| unicode:: U+02014 .. em dash
   :trim:
