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

    # volnoti fork defined in ./nixpkgs/config.nix
    volnoti_fork
    compton
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
    enable = true;
    autorun = true;
    exportConfiguration = true;
    layout = "us";
    updateDbusEnvironment = true;

    displayManager = rec {
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
      session = [ rec {
        name = "custom";
        manage = "desktop";
        start =
        let
	  scaleSvg = img: pkgs.runCommand (baseNameOf (builtins.unsafeDiscardStringContext img)) { nativeBuildInputs = [ pkgs.librsvg ]; } "${pkgs.librsvg.out}/bin/rsvg-convert -a -w 200 -f svg ${img} > $out";
          xbindkeysrc = pkgs.substituteAll {
            src = ./xbindkeysrc;
	    vol_low = "${scaleSvg "${pkgs.gnome3.adwaita-icon-theme.out}/share/icons/Adwaita/scalable/status/audio-volume-low-symbolic.svg"}";
	    vol_medium = "${scaleSvg "${pkgs.gnome3.adwaita-icon-theme.out}/share/icons/Adwaita/scalable/status/audio-volume-medium-symbolic.svg"}";
	    vol_high = "${scaleSvg "${pkgs.gnome3.adwaita-icon-theme.out}/share/icons/Adwaita/scalable/status/audio-volume-high-symbolic.svg"}";
	    vol_mute = "${scaleSvg "${pkgs.gnome3.adwaita-icon-theme.out}/share/icons/Adwaita/scalable/status/audio-volume-muted-symbolic.svg"}";
	    brightness = "${scaleSvg "${pkgs.gnome3.adwaita-icon-theme.out}/share/icons/Adwaita/scalable/status/display-brightness-symbolic.svg"}";
	    kbd_brightness = "${scaleSvg "${pkgs.gnome3.adwaita-icon-theme.out}/share/icons/Adwaita/scalable/status/keyboard-brightness-symbolic.svg"}";
          };
        in
        ''
	  # Compton for showing transparent notifications.
	  ${pkgs.compton.out}/bin/compton -b --config /dev/null --backend glx --vsync opengl

          # TWMN for notifications
          ${pkgs.twmn.out}/bin/twmnd &

	  # Volnoti for showing notifications on volume/brightness change
	  ${pkgs.volnoti_fork.out}/bin/volnoti -r 6 -a 0.5 &

	  # XBindkeys for calling volnoti-show on keypresses.
	  ${pkgs.xbindkeys.out}/bin/xbindkeys -f ${xbindkeysrc} &

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
          ${pkgs.skipsEmacs}/bin/emacs -fs -l ${exwm_load} &
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
