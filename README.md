## My emacs configuration
Author : Jaseem Abid <jaseemabid@gmail.com>

Emacs configuration files rebuild for Emacs 24. Feel free to use my config files
if they are of any interest to you. As of now it is a mess. Use at your own
risk. Will clean up someday when I get time.

### Instructions

```
	# backup your existing emacs config
	cd ~/
	mv .emacs .emacs_
	mv .emacs.d .emacs.d_

	# Clone the repo to ~/.emacs.d
	git clone https://github.com/jaseemabid/emacs.d.git .emacs.d
	cd .emacs.d
	# Update the submodules. You need this for the awesome solarized-dark theme
	git submodule update --init

	# Fire up your emacs and Install all packages in elpa_list
	M-x list-packages # Shows all the packages
	# Press 'i' to select (coffeescript-mode, js2-mode etc)
	# Press 'x' to install
	# Restart emacs

```

### TODO

* Load Yasnippets, centered mode etc *after* elpa packages load
* Check these packages
	* http://julien.danjou.info/projects/emacs-packages
* Fix ERB
* Fix gnus
