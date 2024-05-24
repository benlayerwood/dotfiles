# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  essentials = with pkgs; [
    wget
    git
    gcc
    fd
    bash
    zsh
    alacritty
  ];

  system-tools = with pkgs; [
    eza
    bat
    zip
    unzip
    dig
    vagrant
    ffmpeg
    rtkit
    dbus
    bluez
    htop
    jmtpfs
    imagemagick
    ripgrep
    btrfs-progs
    pciutils
  ];

  desktop-tools = with pkgs; [
    i3
    rofi
    xss-lock
    networkmanagerapplet
    libcanberra-gtk3
    gsettings-desktop-schemas
    gsettings-qt
    libsForQt5.qt5ct
    glib
    polybarFull
  ];

  tools = with pkgs; [
    gcr
    isort
    pferd
    pass
    acpilight
    ispell
    libsecret
    libclang
    rtags
    gvfs
    cifs-utils
    nfs-utils
    tree-sitter
    tree-sitter-grammars.tree-sitter-typst
  ];

  programming = with pkgs; [
    pipenv
    (python3.withPackages(ps: with ps; [ numpy scipy pandas matplotlib jupyter]))
    libcanberra-gtk3
    gsettings-desktop-schemas
    gsettings-qt
    libsForQt5.qt5ct
    glib
    gnumake
    cmake
    glslang
    irony-server
    nixfmt
    go
    gopls
    golangci-lint
    golangci-lint-langserver
    typst
    typstfmt
    typst-lsp
  ];

  desktop-apps = with pkgs; [
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
    ungoogled-chromium
    firefox
    spotify
    autorandr
    xsane
    morgen
    nextcloud-client
    element-desktop
    cinnamon.nemo
    baobab
    drawio
    pdfarranger
    element-desktop
  ];

  gnome-apps = with pkgs.gnome; [
    gnome-contacts
    simple-scan
    eog
    cheese
  ];

  emacs-pkgs = with pkgs.emacsPackages; [
    pytest
    treesit-auto
  ];
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 50;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" "btrfs" "vfat"];
  boot.kernelModules = [ "coretemp" ];
  boot.kernelPackages = pkgs.linuxPackages_latest;

  powerManagement.cpuFreqGovernor = "ondemand";
  powerManagement.powertop.enable = true;

  networking.hostName = "hp-ben"; # Define your hostname.

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

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages =
    essentials ++ system-tools ++ desktop-tools ++ tools ++ programming ++ desktop-apps ++ gnome-apps ++ emacs-pkgs;

  # Environment Variables
  environment.variables = {
    EDITOR = "/home/ben/.nix-profile/bin/nvim";
    MONITOR = "HDMI-1";
    QT_QPA_PLATFORMTHEME = "qt5ct";
  };

  environment.etc = {
    "xdg/gtk-2.0/gtkrc".text = ''
      [Settings]
      gtk-theme-name=Orchis-Dark
      gtk-icon-theme-name=Tela
      gtk-modules=canberra-gtk-module
    '';
    "xdg/gtk-3.0/settings.ini".text = ''
      [Settings]
      gtk-theme-name=Orchis-Dark
      gtk-icon-theme-name=Tela
      gtk-modules=canberra-gtk-module
    '';
  };

  # Fonts
  fonts.packages = with pkgs; [
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
    jetbrains-mono
    (nerdfonts.override { fonts = [ "DroidSansMono" "NerdFontsSymbolsOnly"]; })
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.zsh.enable = true;
  programs.adb.enable = true;
  programs.java.enable = true;

  programs.neovim = {
   enable = true;
   defaultEditor = true;
  };

  services.gnome.gnome-keyring.enable = true;
  services.nginx = {
    enable = false;
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

  services.emacs.enable = true;

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = false; # run sudo nixos-rebuild switch --install-bootloader to start ssh service
    settings.X11Forwarding = true;
  };

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

  services.syncthing = {
    enable = true;
    user = "ben";
    dataDir = "/home/ben/";
    configDir = "/home/ben/.config/syncthing";
  };
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
  services.teamviewer.enable = false;

  virtualisation.docker.enable = true;
  users.extraGroups.docker.members = ["ben"];
  virtualisation.libvirtd.enable = true;

  virtualisation.lxc.enable = false;

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

  qt.enable = false;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 161 162 5900 49710 9100 8080];
  networking.firewall.allowedUDPPorts = [ 161 162 49710 9100 8080];
  # Or disable the firewall altogether.
  networking.firewall.enable = true;
  networking.extraHosts = ''
    10.0.0.1 sdnbw
    193.196.36.102 bwcloud
  '';
  # networking.bridges."lxcbr0" = {
  #   interfaces = [ "enp1s0" ];
  # };

  nixpkgs.config.permittedInsecurePackages = [
    "electron-25.9.0"
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}

