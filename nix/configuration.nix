# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 50;
  boot.loader.efi.canTouchEfiVariables = true;
  #boot.loader.efi.efiSysMountPoint = "/boot";
  #boot.loader.grub.enable = true;
  #boot.loader.grub.device = "nodev";
  #boot.loader.grub.efiSupport = true;
  #boot.loader.grub.useOSProber = false;
  #boot.kernelModules = [ "vboxdrv" ];
  boot.supportedFilesystems = [ "ntfs" ];

  networking.hostName = "hp-ben"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "de_DE.UTF-8";
    LC_IDENTIFICATION = "de_DE.UTF-8";
    LC_MEASUREMENT = "de_DE.UTF-8";
    LC_MONETARY = "de_DE.UTF-8";
    LC_NAME = "de_DE.UTF-8";
    LC_NUMERIC = "de_DE.UTF-8";
    LC_PAPER = "de_DE.UTF-8";
    LC_TELEPHONE = "de_DE.UTF-8";
    LC_TIME = "de_DE.UTF-8";
  };

  # Configure keymap in X11
  services.xserver = {
    layout = "de";
    xkbVariant = "";
    enable = true;
    autorun = true;
    displayManager.gdm = {
      enable = true;
    };
    displayManager.defaultSession = "none+i3";
    windowManager.i3 = {
     enable = true;
    };
  };

  # Configure console keymap
  console.keyMap = "de";

  # Define user account for ben
  users.users.ben = {
    isNormalUser = true;
    description = "Benjamin Schichtholz";
    extraGroups = [ "networkmanager" "wheel" "audio" "vboxusers" "wireshark" "scanner" "lp" "da" "davfs2"];
    uid = 1000;
    shell = pkgs.zsh;
    packages = with pkgs; [];
  };
  users.extraGroups.audio.members = ["ben"];

  security.pam.services.gdm.enableGnomeKeyring = true;
  security.sudo.extraRules= [
    {  users = [ "ben" ];
      commands = [
        { command = "ALL" ;
          options= [ "NOPASSWD" ];
        }
      ];
    }
  ];
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # tools
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    emacs
    wget
    git
    gcc
    fd
    bash
    zsh
    rofi
    exa
    bat
    zip
    unzip
    (python3.withPackages(ps: with ps; [python-lsp-server autopep8 pyflakes pytest nose]))
    alacritty
    vagrant
    ffmpeg
    rtkit
    dbus
    bluez
    htop
    networkmanagerapplet
    xss-lock
    jmtpfs
    imagemagick
    ripgrep

    gcr
    gnumake
    cmake
    glslang
    irony-server
    pipenv
    isort

    i3
    pferd
    pass
    acpilight
    ispell
    libsecret
    libclang
    nixfmt
    rtags

    appimage-run
    vlc
    zathura
    librewolf
    betterbird
    arandr
    flameshot
    anki
    discord
    gparted
    chromium
    spotify
    signal-desktop-beta
    autorandr
    xsane
    morgen
    simplenote
    nextcloud-client
    element-desktop
    cinnamon.nemo
    gnome.gnome-contacts
    gnome.simple-scan
    gnome.eog
    gnome.cheese
    baobab
    libcanberra-gtk3
    gsettings-desktop-schemas
    drawio
    pdfarranger
    whatsapp-for-linux
  ];

  # Environment Variables
  environment.variables = {
    EDITOR = "/run/current-system/sw/bin/vim";
    MONITOR = "HDMI-1";
  };

  environment.etc = {
    "xdg/gtk-2.0/gtkrc".text = ''
      gtk-theme-name = "Adwaita-dark"
      gtk-icon-theme-name = "Adwaita"
    '';
    "xdg/gtk-3.0/settings.ini".text = ''
      [Settings]
      gtk-theme-name = Adwaita-dark
      gtk-application-prefer-dark-theme = true
      gtk-icon-theme-name = Adwaita
    '';
  };

  # Fonts
  fonts.fonts = with pkgs; [
    noto-fonts
    ibm-plex
    fira
    fira-code
    fira-code-symbols
    liberation_ttf
    roboto
    material-icons
    material-symbols
    dejavu_fonts
    powerline-fonts
    (nerdfonts.override { fonts = [ "DroidSansMono" "NerdFontsSymbolsOnly"]; })
  ];

  # xdg.mime.defaultApplications = {
  #   "application/pdf" = "zathura.desktop";
  #   "image/png" = [
  #     "eog.desktop"
  #   ];
  # };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.zsh.enable = true;
  programs.adb.enable = true;
  services.gnome.gnome-keyring.enable = true;
  services.nginx = {
    enable = true;
      virtualHosts."192.168.0.92" = {
        locations."/" = {
                proxyPass = "http://127.0.0.1:4000";
        };
      };
  };
  services.davfs2 = {
    enable = false;
    extraConfig = ''
    use_locks 0
    '';
  };

  programs.wireshark.enable = true;

  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true; # run sudo nixos-rebuild switch --install-bootloader to start ssh service
  # services.emacs = {
  #   enable = true;
  # };

  services.xserver.libinput = {
    enable = true;
    mouse = {
      accelProfile = "flat";
    };
    touchpad = {
      naturalScrolling = true;
    };
  };

  services.xserver.desktopManager.gnome.extraGSettingsOverrides = ''
    [org/gnome/desktop/peripherals/touchpad]
    natural-scroll=true
  '';

  services.printing.enable = true;
  services.system-config-printer.enable = true;
  services.avahi.enable = true;
  services.avahi.nssmdns = true;
  # for a WiFi printer
  services.avahi.openFirewall = true;
  services.udev.extraRules = ''
    KERNEL=="vboxdrv",   OWNER="root", GROUP="vboxusers",      MODE="0777", TAG+="systemd"
    KERNEL=="vboxdrvu",   OWNER="root", GROUP="vboxusers",      MODE="0777", TAG+="systemd"
  '';
  services.printing.drivers = [ pkgs.hplip ];
  services.udisks2.enable = true;
  services.teamviewer.enable = true;

  # Enable VirtuelBox
  virtualisation.virtualbox.host.enable = true;
  # virtualisation.docker.enable = true;
  # virtualisation.virtualbox.host.enableExtensionPack = true;
  # users.extraGroups.vboxusers.members = ["ben"];
  # virtualisation.virtualbox.guest.enable = false;
  # virtualisation.libvirtd.enable = true;

  qt = {
    enable = true;
    platformTheme = "gnome";
    style = "adwaita-dark";
  };

  # services.picom.enable = true;
  # services.picom.inactiveOpacity = 0.3;

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.bluetooth.enable = true;
  hardware.pulseaudio.extraConfig = "load-module module-combine-sink";
  services.blueman.enable = true;
  hardware.bluetooth.package = pkgs.bluez;
  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.hplipWithPlugin ];

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 161 162 5900 49710 9100 8080];
  networking.firewall.allowedUDPPorts = [ 161 162 49710 9100 8080];
  # Or disable the firewall altogether.
  networking.firewall.enable = true;
  # networking.extraHosts = ''
  #   10.0.0.1 sdnbw
  # '';

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
