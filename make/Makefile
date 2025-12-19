.PHONY: dotfiles
stow:
	@$(MAKE) -C dotfiles stow

pull:
	cd dotfiles && git pull

build:
	home-manager build

switch:
	home-manager switch

install-nix:
	sudo install -d -m755 -o $$(id -u) -g $$(id -g) /nix
	curl -L https://nixos.org/nix/install | sh

install-home-manager:
	nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
	nix-channel --update
	nix-shell '<home-manager>' -A install

.ssh/id_ed25519.pub:
	ssh-keygen -t ed25519

ssh: .ssh/id_ed25519.pub
	cat $<
	@echo https://github.com/settings/ssh/new
	@echo https://gitlab.com/-/user_settings/ssh_keys

.ssh/allowed_signers: .ssh/id_ed25519.pub
	echo "$$(whoami) $$(cat $<)" >> $@
	chmod 600 $@

wifi: wifi-list wifi-connect
wifi-list:
	nmcli device wifi list
wifi-connect:
	nmcli device wifi connect $(SSID) --ask
wifi-up:
	nmcli device connect wlp2s0
wifi-down:
	nmcli device disconnect wlp2s0

mail:
	mbsync -a
	notmuch new

headphones:
	cat /proc/asound/card*/codec*

restart-pulse-audio:
	-pulseaudio --kill
	pulseaudio --start

trackpoint:
	xinput --set-prop 'TPPS/2 Elan TrackPoint' 'libinput Accel Profile Enabled' 0, 1

downloads: DIR=Downloads.$(shell date +%Y-%m)
downloads:
	rm Downloads
	mkdir -p $(DIR)
	ln -s $(DIR) Downloads

xorg-caps:
	setxkbmap -option caps:ctrl_modifier

# run to make sure all packages are installed
emacs-package-install:
	emacs --batch --load .emacs --eval '(package-install-selected-packages)'

mlbv-tor-vpn:
	-rm .config/mlbv/cookies
	-rm .config/mlbv/session
	nordvpn connect us8945
	mlbv -t tor

# https://github.com/tailscale/tailscale/issues/8223#issuecomment-1871678581
dns:
	sudo systemctl restart tailscaled systemd-resolved
