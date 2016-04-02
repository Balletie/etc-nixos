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
    xfce.xfce4panel
    xfce.xfce4mixer
    xfce.xfce4volumed
    xfce.xfce4notifyd
    xfce.xfce4screenshooter
    xfce.xfce4_power_manager
    xfce.exo
    xfce.garcon
    pcmanfm
    gvfs
    dmenu
    # vanilla-dmz
  ];

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
      '';
    };

    desktopManager.xterm.enable = false;
    # desktopManager.xfce.enable = true;
    # desktopManager.default = "xfce";

    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.default = "xmonad";

    # Touchpad configuration
    synaptics = {
      enable = true;
      dev = "/dev/input/event*";
      twoFingerScroll = true;
      accelFactor = "0.001";
      buttonsMap = [ 1 3 2 ];
      tapButtons = false;
    };

    videoDrivers = [ "ati" "intel" "modesetting" ];
    # Set compose key to right alt, disable caps lock to be a control key.
    xkbOptions = "terminate:ctrl_alt_bksp, compose:ralt, control:nocaps";
  };
}
