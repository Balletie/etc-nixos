{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # PCManFM and related (gvfs for mounting, mime-types and application menu)
    # I use pkgs.xfce.gvfs, because pkgs.gvfs has samba as dependency.
    pcmanfm
    xfce.gvfs
    shared_mime_info
    lxmenu-data

    redshift

    # GTK themes, icon themes and cursors.
    gtk2
    gtk_engines
    gtk-engine-murrine
    zuki-themes
    vanilla-dmz
    gnome3.adwaita-icon-theme
    elementary-icon-theme
    moka-icon-theme
    faba-icon-theme
    faba-mono-icons

    # LightDM and light-locker for login screen and locking on suspend.
    lightdm
    lightlocker

    # Patched TWMN defined in ./nixpkgs/config.nix
    twmn
    rxvt_unicode-with-plugins
  ];

  environment.variables.GIO_EXTRA_MODULES = [ "${pkgs.xfce.gvfs}/lib/gio/modules" ];

  environment.pathsToLink = [
    "/share/xfce4"
    "/share/themes"
    "/share/mime"
    "/share/desktop-directories"
    "/share/gtksourceview-2.0"
  ];

  # See https://github.com/NixOS/nixpkgs/issues/16327.
  services.gnome3.at-spi2-core.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    autorun = true;
    enable = true;
    exportConfiguration = true;
    layout = "us";
    updateDbusEnvironment = true;

    displayManager = {
      lightdm.enable = true;
      lightdm.background = "#1d1f21";
      lightdm.greeters.gtk = {
        theme.package = pkgs.zuki-themes;
        theme.name = "Zukitre";
      };
      sessionCommands = ''
        # Set GTK_PATH so that GTK+ can find the theme engines.
        export GTK_PATH="${config.system.path}/lib/gtk-2.0:${config.system.path}/lib/gtk-3.0"
        # Set GTK_DATA_PREFIX so that GTK+ can find the Xfce themes.
        export GTK_DATA_PREFIX=${config.system.path}
        # SVG loader for pixbuf
        export GDK_PIXBUF_MODULE_FILE=$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)

        # Set XDG menu prefix
        export XDG_MENU_PREFIX="lxde-"
      '';
      session = [ {
        name = "custom";
        manage = "desktop";
        start = ''
          # TWMN for notifications
          ${pkgs.twmn.out}/bin/twmnd &

          # Lockscreen, e.g. when I suspend.
          ${pkgs.lightlocker.out}/bin/light-locker --lock-on-suspend &

          # NetworkManager applet
          ${pkgs.networkmanagerapplet.out}/bin/nm-applet &

          # rxvt-unicode in daemon mode. Faster startup for terminals.
          ${pkgs.rxvt_unicode-with-plugins.out}/bin/urxvtd -q -f -o &

          # Redshift, duh.
          ${pkgs.redshift.out}/bin/redshift-gtk -l 51.913799:4.468502 -t 6500:2500 &
        '';
      } {
        name = "exwm";
        manage = "window";
        start = let exwm_load = ./exwm-config.el; in ''
          ${pkgs.skipsEmacs}/bin/emacs -l ${exwm_load} &
          waitPID=$!
        '';
      } ];
    };

    desktopManager.xterm.enable = false;
    # I do this by hand above.
    # windowManager.exwm.enable = true;
    # windowManager.exwm.enableDefaultConfig = true;

    # Touchpad configuration
    libinput = {
      enable = true;
      dev = "/dev/input/event*";
      tapping = false;
    };

    videoDrivers = [ "ati" "intel" "modesetting" ];
    deviceSection = ''Option "TearFree" "on"'';
    # Set compose key to right alt, disable caps lock to be a control key.
    xkbOptions = "terminate:ctrl_alt_bksp, compose:ralt, ctrl:nocaps";
  };
}
