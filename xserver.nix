{ config, pkgs, ... }:

{
  # I need this, for my eyes.
  # services.redshift = {
  #   enable = true;
  #   latitude = "52.37";
  #   longitude = "4.9" ;
  # };

  environment.systemPackages = with pkgs; [
    shared_mime_info
    gtk
    xfce.gtk_xfce_engine
    xfce.xfconf
    xfce.xfce4volumed
    xfce.xfce4_power_manager

    # Use pkgs.xfce.gvfs, because pkgs.gvfs has samba as dependency
    xfce.gvfs
    pcmanfm
    redshift
    dmenu
    vanilla-dmz
    lightlocker
    elementary-icon-theme

    # Icons from haiku OS, defined in ./nixpkgs/config.nix
    haiku-icon-theme

    # Haiku-ish GTK theme, needs murrine engine
    murrina-haikuish
    gtk-engine-murrine

    # Patched TWMN defined in ./nixpkgs/config.nix
    twmn
    tint2
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

  # Enable the X11 windowing system.
  services.xserver = {
    autorun = true;
    enable = true;
    exportConfiguration = true;
    layout = "us";

    displayManager = {
      lightdm.enable = true;
      lightdm.background = "#FCAF3E";
      sessionCommands = ''
        # Set GTK_PATH so that GTK+ can find the theme engines.
        export GTK_PATH="${config.system.path}/lib/gtk-2.0:${config.system.path}/lib/gtk-3.0"
        # Set GTK_DATA_PREFIX so that GTK+ can find the Xfce themes.
        export GTK_DATA_PREFIX=${config.system.path}
        # SVG loader for pixbuf
        export GDK_PIXBUF_MODULE_FILE=$(echo ${pkgs.librsvg}/lib/gdk-pixbuf-2.0/*/loaders.cache)
      '';
    };

    desktopManager.session = [ {
      name = "custom";
      start = ''
        ## My own "desktop environment"

        # Start caching dmenu_run entries in advance
        ${pkgs.dmenu}/bin/dmenu_path &

        # Desktop background and desktop files
        ${pkgs.pcmanfm}/bin/pcmanfm --desktop &

        # Panel
        ${pkgs.tint2}/bin/tint2 &

        # TWMN for notifications
        ${pkgs.twmn}/bin/twmnd &

        # Lockscreen, e.g. when I suspend.
        ${pkgs.lightlocker}/bin/light-locker &

        # Brightness keys, automatically starts xfce4-notifyd if no notification daemon is running.
        ${pkgs.xfce.xfce4_power_manager}/bin/xfce4-power-manager &

        # Volume keys, automatically starts xfce4-notifyd if no notification daemon is running.
        ${pkgs.xfce.xfce4volumed}/bin/xfce4-volumed &

        # NetworkManager applet
        ${pkgs.networkmanagerapplet}/bin/nm-applet &

        # rxvt-unicode in daemon mode. Faster startup for terminals.
        ${pkgs.rxvt_unicode-with-plugins}/bin/urxvtd -q -f -o &

        # Redshift, duh.
        ${pkgs.redshift}/bin/redshift-gtk -l 51.913799:4.468502 -t 6500:2500 &
      '';
    } ];
    desktopManager.xterm.enable = false;
    # desktopManager.xfce.enable = true;
    # desktopManager.default = "xfce";

    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.default = "xmonad";

    # Touchpad configuration
    libinput = {
      enable = true;
      dev = "/dev/input/event*";
      tapping = false;
    };

    videoDrivers = [ "ati" "intel" "modesetting" ];
    # Set compose key to right alt, disable caps lock to be a control key.
    xkbOptions = "terminate:ctrl_alt_bksp, compose:ralt, ctrl:nocaps";
  };
}
