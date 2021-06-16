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

# update the remote from an initial https bootstrap
remote:
	git remote set-url origin git@github.com:rcy/dotfiles.git
	git fetch
	git branch -u origin/main main
