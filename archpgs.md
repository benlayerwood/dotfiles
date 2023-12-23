# Arch Packages

## Check if packages exist
```
grep -v -E "(^$|#|\`\`\`)" archpgs.md | xargs paru -Si | grep error
```

## Essential applications
    vim
    wget
    git
    gcc
    fd
    bash
    zsh
    emacs-nativecomp
    alacritty

## System tools
    eza
    bat
    zip
    unzip
    bind
    vagrant
    ffmpeg
    rtkit
    bluez
    htop
    imagemagick
    ripgrep
    btrfs-progs

## Desktop-tools
    i3-wm
    xorg-server
    lightdm
    lightdm-slick-greeter
    gnome-keyring
    libsecret
    rofi
    xss-lock
    networkmanager
    gsettings-desktop-schemas

## Other tools
    gcr
    ispell
    gvfs
    cifs-utils
    nfs-utils
    wireshark-qt

## Programming
    python
    python-pipenv
    python-jupyter-core
    python-jupyter-client
    python-isort
    jdk-openjdk
    libcanberra
    gsettings-desktop-schemas
    gsettings-qt
    qt5ct
    make
    cmake
    glslang

## Desktop apps
    vlc
    zathura
    arandr
    flameshot
    gparted
    firefox
    autorandr
    xsane
    nextcloud-client
    element-desktop
    nemo
    baobab
    pdfarranger
    element-desktop

## Gnome apps
    simple-scan
    eog
    cheese

## Paru packages
    # jmtpfs
    # pferd
    # nixfmt
    # librewolf-bin
    # betterbird-bin
    # anki
    # discord
    # ungoogled-chromium-bin
    # spotify
    # drawio-desktop

## Fonts
    ttf-ibmplex-mono-nerd
    noto-fonts-emoji

## Graphics
    mesa
