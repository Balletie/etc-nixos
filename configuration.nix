# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./mount-disks.nix
      ./xserver.nix
    ];

  # Use the systemd-boot efi boot loader.
  # Disable timeout, press space to show menu.
  boot.loader.systemd-boot.enable = true;
  boot.loader.timeout = null;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_3_18;

  networking.hostName = "samsara"; # Define your hostname.
  networking.networkmanager.enable = true;
  networking.networkmanager.packages = [ pkgs.networkmanagerapplet ];
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  nixpkgs.config = import ./nixpkgs/config.nix;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    git
    grml-zsh-config
    texlive.combined.scheme-full
    vim
    wget
    ## For correct mime-types. See https://github.com/NixOS/nixpkgs/issues/13134
    # shared_mime_info
  ];

  # Fonts
  fonts = {
    fontconfig.ultimate.enable = false;
    fontconfig.defaultFonts = {
      sansSerif = [ "Bitstream Vera Sans"      "EmojiOne Color" ];
      serif     = [ "Bitstream Vera Serif"     "EmojiOne Color" ];
      monospace = [ "Bitstream Vera Sans Mono" "EmojiOne Color" ];
    };
    fonts = with pkgs; [
      ttf_bitstream_vera
      emojione
      noto-fonts
      noto-fonts-emoji
      google-fonts

      # Icon font
      font-awesome-ttf

      # Mono-space fonts
      fira-mono
      opensans-ttf
      dina-font
      dina-font-pcf
      gohufont
      iosevka
      unifont
    ];
  };

  # Enable ACPI daemon.
  services.acpid = {
    enable = true;
    lidEventCommands = ''
      case "$3" in
          close)
              logger 'Lid closed, going to sleep'
              systemctl suspend
              ;;
          open)
              logger 'Lid opened'
              ;;
          *)
              logger 'Unhandled lid event action: $3'
              ;;
      esac
    '';
  };

  # Enable udisks2
  services.udisks2.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable AccountsService, for lightdm.
  # services.accounts-daemon.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Configure ZSH
  programs.zsh.enable = true;
  # Enable GRML zsh config
  environment.etc."zshrc" = {
    source = "${pkgs.grml-zsh-config}/etc/zsh/zshrc";
  };
  environment.etc."zsh/keephack" = {
    source = "${pkgs.grml-zsh-config}/etc/zsh/keephack";
  };

  # Set vim as editor.
  environment.variables.EDITOR = "vim";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.skip = {
    description = "Skip Lentz";
    home = "/home/skip";
    createHome = true;
    extraGroups = [ "wheel" ];
    shell = "/run/current-system/sw/bin/zsh";
    isNormalUser = true;
    uid = 1000;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

}
