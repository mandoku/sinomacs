Easymacs
========

Easymacs is a configuration for Emacs designed for teaching markup and programming, particularly for the digital humanities.  Emacs provides a number of essential tools in a single, open-source, cross-platform learning environment which can be installed easily (even in a classroom where one does not have administrative rights).  In one package, it provides:

  * A schema-aware, validating XML editor (nxml-mode)
  * A platform-independent command line for running programs (eshell)
  * An interactive tool for learning about regular expressions (re-builder with pcre2el)
  * A fully-featured development environment for programming in almost any language.

Emacs has a reputation for complexity, archaism and idiosyncrasy, so it might seem an inappropriate application to introduce to students from a non-technical background.  But Emacs has most of the features of a modern desktop application; it is just that not all of these are turned on by default.  Emacs can be configured to be a surprisingly user-friendly application.

Easymacs is designed to turn Emacs into as familiar an application as possible, so that students can start using it right away.  Conventional keystrokes and familiar terminology are used for basic file manipulations.  More advanced Emacs functionality is assigned to function keys.  Some of these choices override standard defaults, but the full power of Emacs is still available to the student who wants to delve deeper.

Easymacs is similar in principle to projects like [Emacs Starter Kit](http://xgarrido.github.io/emacs-starter-kit/starter-kit.html), but it supposes a narrower and less technically oriented audience.

Easymacs comes with an installer that modifies the user's .emacs configuration file to run itself. It automatically installs a number of third-party packages, including adaptive-wrap, undo-tree, company, magit, auctex, pcre2el and re-builder.

For a list of keybindings, see the file easymacs-help.txt, which is the page shown when Easymacs starts up.
