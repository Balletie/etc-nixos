{
  # For the broadcom driver :(
  allowUnfree = true;
  allowBroken = true;
  packageOverrides = pkgs: rec {
    twmn = pkgs.stdenv.lib.overrideDerivation pkgs.twmn (oldAttrs: {
      patches = [
        ./twmn/0001-Add-fallback-title-option.patch
      ] ++ oldAttrs.patches or [];
    });

    murrina-haikuish = pkgs.stdenv.mkDerivation rec {
      package-name = "murrina-haikuish";
      version = "1.01";
      name = "${package-name}-${version}";
      src = pkgs.fetchurl {
        name = "${name}.tar.gz";
        url = "http://gnome-look.org/CONTENT/content-files/127322-Murrina%20Haikuish.tar.gz";
        sha256 = "11mbffnxy7vyd03mmnvcy27lmmsa4sgir372pxym4fz8nsfnja4s";
      };

      dontBuild = true;

      installPhase = ''
        # Set -f flag so that it fails silently if it does not exist.
        rm -f "gtk-2.0/gtkrc (copy)"
        install -dm 755 "$out/share/themes/Murrina-Haikuish/"
        cp -r . "$out/share/themes/Murrina-Haikuish/"
      '';
    };

    haiku-icon-theme = pkgs.stdenv.mkDerivation rec {
      package-name = "haiku-icon-theme";
      version = "0.7";
      name = "${package-name}-${version}";

      src = pkgs.fetchurl {
        name = "${name}.tar.bz2";
        url = "http://gnome-look.org/CONTENT/content-files/114723-Haiku-${version}.tar.bz2";
        sha256 = "08nq21qmrjqz09k1wzf5vkvi6gzb22ry8ri58q65bqwal0r8cq95";
      };

      dontBuild = true;

      installPhase = ''
        rm index.theme\~
        mv "scalable/apps/sound-juicer .svg" "scalable/apps/sound-juicer.svg"
        install -dm 755 "$out/share/icons/Haiku"
        cp -r ./* "$out/share/icons/Haiku"
      '';

      meta = with pkgs.stdenv.lib; {
        description = "Haiku-OS icon theme";
        homepage = "http://gnome-look.org/content/show.php/Haiku?content=114723";
        license = licenses.gpl2;

        platforms = platforms.all;
      };
    };
  };
}
