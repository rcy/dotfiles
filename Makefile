packages=					\
	bash					\
	emacs					\
	git					\
	mail					\
	ratpoison

stow:
	stow -v -S $(packages)

