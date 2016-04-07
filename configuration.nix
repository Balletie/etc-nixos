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

  # Use the gummiboot efi boot loader. Disable timeout, press space to show menu.
  boot.loader.gummiboot.enable = true;
  boot.loader.gummiboot.timeout = null;
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

  # For the broadcom driver :(
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    git
    grml-zsh-config
    texLiveFull
    vim
    wget
    ## For correct mime-types. See https://github.com/NixOS/nixpkgs/issues/13134
    # shared_mime_info
  ];

  # Fonts
  fonts = {
    fonts = with pkgs; [
      dina-font
      dina-font-pcf
      gohufont
      iosevka
      unifont
    ];
  };

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
