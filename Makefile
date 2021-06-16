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

unstow:
	stow -D $(packages)

reorigin:
	git remote remove origin https
	git remote add origin git@github.com:rcy/dotfiles.git
	git fetch
	git branch -u origin/main main
