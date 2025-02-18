packages=					\
	bash					\
	doom \
	emacs					\
	git					\
	mail					\
	make					\
	nix					\
	ratpoison				\
	ruby			                \
	tmux

stow: pre
	stow --verbose -S $(packages)

.PHONY: pre
pre:
	mkdir -p ~/.emacs.d

unstow:
	stow --verbose -D $(packages)

cron:
	-crontab -l > crontab.orig
	cat crontab | crontab -
	crontab -l | diff - crontab.orig && rm crontab.orig

# update the remote from an initial https bootstrap
remote:
	git remote set-url origin git@github.com:rcy/dotfiles.git
	git fetch
	git branch -u origin/main main
