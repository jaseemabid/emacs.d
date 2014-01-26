## My emacs configuration
Author : Jaseem Abid <jaseemabid@gmail.com>

Emacs configuration files rebuild for Emacs 24.

### Screenshot

![Screenshot](./screens/Emacs, Jan 16 2014.png)

### Instructions

Backup your existing emacs config as this will replace it.

```sh
    # Backup existing files
    cd ~/
	mv .emacs .emacs.backup
	mv .emacs.d .emacs.d.backup

	# Clone the repo to ~/.emacs.d
	git clone https://github.com/jaseemabid/emacs.d.git .emacs.d

	# Fire up your emacs
	# All packages in elpa-list.el will be  installed automatically
	# Restart emacs
```
