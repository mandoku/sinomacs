Easymacs Mandoku
================

Easymacs is a configuration for Emacs designed for teaching markup and
programming, particularly for the digital humanities.  Easymacs
Mandoku is a fork of of
[Easymacs](https://github.com/pjheslin/easymacs) by Peter Heslin,
which adds [Mandoku](http:/www.mandoku.org) to the existing
configuration. Emacs provides a number of essential tools in a single,
open-source, cross-platform learning environment which can be
installed easily (even in a classroom where one does not have
administrative rights).  In one package, it provides:

  * A schema-aware, validating XML editor (nxml-mode)
  * A platform-independent command line for running programs (eshell)
  * An interactive tool for learning about regular expressions (re-builder with pcre2el)
  * A fully-featured development environment for programming in almost any language.

Emacs has a reputation for complexity, archaism and idiosyncrasy, so it might seem an inappropriate application to introduce to students from a non-technical background.  But Emacs has most of the features of a modern desktop application; it is just that not all of these are turned on by default.  Emacs can be configured to be a surprisingly user-friendly application.

Easymacs is designed to turn Emacs into as familiar an application as possible, so that students can start using it right away.  Conventional keystrokes and familiar terminology are used for basic file manipulations.  More advanced Emacs functionality is assigned to function keys.  Some of these choices override standard defaults, but the full power of Emacs is still available to the student who wants to delve deeper.

Easymacs is similar in principle to projects like [Emacs Starter Kit](http://xgarrido.github.io/emacs-starter-kit/starter-kit.html), but it supposes a narrower and less technically oriented audience.

Easymacs comes with an installer that modifies the user's .emacs configuration file to run itself. It automatically installs a number of third-party packages, including adaptive-wrap, undo-tree, company, magit, auctex, pcre2el and re-builder.

For a list of keybindings, see the file easymacs-help.txt, which is the page shown when Easymacs starts up.

Installation
------------


  1. Install Emacs, version 24.4 or greater:

	* Windows: [Download Emacs](https://ftp.gnu.org/gnu/emacs/windows/).  The filename will look something like emacs-24.5-bin-i686-mingw32.zip.  Unzip the file in a convenient place and start Emacs by going to the bin folder and double-clicking on emacs.exe.  You may want to set up a short-cut.
	
	* Mac: [Download Emacs](https://emacsformacosx.com).  Install and run the application as normal.
	
	* Linux: Install via your package manager.
	
  2. Get the Easymacs zip file: Use the green download button above or [get the Easymacs Mandoku archive from ](http://www.mandoku.org/data/easymacs-mandoku.zip).  Unzip it in a convenient location.
  
  3. Run Emacs and click on the menu option File->Open File.  Navigate to the folder in which you just unzipped Easymacs and open the file called INSTALL.el.  Follow the directions on screen, which will tell you to select the menu item Emacs-Lisp->Evaluate Buffer.
  
  4. Easymacs is now installed.  You may want to install some auxiliary programmes such as [Hunspell](https://hunspell.github.io) for spell-checking.  qFor Macs, [Homebrew](http://brew.sh) is the best way to do this.
