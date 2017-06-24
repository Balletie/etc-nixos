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
  boot.kernelParams = [ "vt.global_cursor_default=0" "vga=current" ];

  # Fix hang in kernel space by disabling hybrid graphics.
  hardware.amdHybridGraphics.disable = true;

  networking.hostName = "samsara"; # Define your hostname.
  networking.networkmanager.enable = true;
  networking.networkmanager.packages = [ pkgs.networkmanagerapplet ];

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  nixpkgs.config = import ./nixpkgs/config.nix;

  nix.useSandbox = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    git
    grml-zsh-config
    imagemagick
    texlive.combined.scheme-full
    vim
    wget
    ## For correct mime-types. See https://github.com/NixOS/nixpkgs/issues/13134
    # shared_mime_info
  ];

  services.emacs.enable = true;
  # Following line enables daemon service.
  services.emacs.install = true;
  # skipsEmacs is defined in nixpkgs/config.nix.
  services.emacs.package = pkgs.skipsEmacs;

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
      roboto

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

  services.journald.extraConfig = "SystemMaxUse=50M";
  # Enable udisks2
  services.udisks2.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable Keyboard / LCD backlight keys, media keys, volume keys with beep, eject key.
  services.hardware.pommed.enable = true;
  services.hardware.pommed.configFile = pkgs.substituteAll {
    src = ./pommed.conf;
    pommed_beep_file = "${pkgs.pommed_light.out}/share/pommed/click.wav";
  };

  # Fix suspend / resume while lid stays open.
  systemd.services.root_suspend = {
    description = "Disable Resume While Lid Open";
    before = [ "sleep.target" ];
    wantedBy = [ "sleep.target" ];
    script = ''
      # If lid is open and we're going to suspend, disable resume on
      # lid open.
      if grep -qE "^state: *open$" /proc/acpi/button/lid/LID0/state
      then
        # Disable resuming when lid is open.
        if grep -qE '^LID0.*enabled' /proc/acpi/wakeup
        then
          echo "Lid is open when going to sleep: disable resume on lid open."
          echo LID0 > /proc/acpi/wakeup
        fi
      else
        echo "Lid is closed, do nothing."
      fi

      exit 0
    '';
  };

  systemd.services.root_resume = {
    description = "Restore Resume On Lid Open";
    after = [ "suspend.target" ];
    wantedBy = [ "suspend.target" ];
    script = ''
      # Enable resuming on lid open again.
      if grep -qE '^LID0.*disabled' /proc/acpi/wakeup
      then
        echo "Lid open wakeup is disabled: enable again."
        echo LID0 > /proc/acpi/wakeup
      fi

      exit 0
    '';
  };

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
  # Set texmf home.
  environment.variables.TEXMFHOME = "$HOME/.local/share/texmf";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.skip = {
    description = "Skip Lentz";
    initialPassword = "foobar";
    home = "/home/skip";
    createHome = true;
    extraGroups = [ "wheel" "networkmanager" ];
    shell = "/run/current-system/sw/bin/zsh";
    isNormalUser = true;
    uid = 1000;
  };

  # Enable coredumps.
  systemd.coredump.enable = true;
  security.pam.loginLimits = [ { domain = "@wheel"; type = "-"; item = "core"; value = "unlimited"; } ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

}
