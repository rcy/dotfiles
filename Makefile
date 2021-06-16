packages=					\
	bash					\
	emacs					\
	git					\
	mail					\
	make					\
	nix					\
	ratpoison

stow:
	stow -S $(packages)
