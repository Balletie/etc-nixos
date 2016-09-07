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

    haiku-gtk = pkgs.stdenv.mkDerivation rec {
      package-name = "haiku-gtk";
      version = "5-1-11";
      name = "${package-name}-${version}";
      src = pkgs.fetchurl {
        name = "${name}.tar.bz2";
        url = "https://dl.opendesktop.org/api/files/download/id/1460967677/106952-Haiku-${version}.tar.bz2";
        sha256 = "1lyvh3lvvbdh6352gkf5gzyjlmcpgr4c40rlk6c6jrjvhjcavy5c";
      };

      dontBuild = true;

      installPhase = ''
        install -dm 755 "$out/share/themes/Haiku/"
        cp -r . "$out/share/themes/Haiku/"
      '';
    };

    haiku-hand = pkgs.stdenv.mkDerivation rec {
      package-name = "haiku-hand";
      version = "0.5";
      name = "${package-name}-${version}";
      src = pkgs.fetchurl {
        name = "${name}.tar.bz2";
        url = "https://dl.opendesktop.org/api/files/download/id/1460735079/116169-HaikuHand-${version}.tar.bz2";
        sha256 = "0jr0ybb2kczbfxpxqxgm7267xzqijabll4rrrjk933x0q9i80awi";
      };

      dontBuild = true;

      installPhase = ''
        install -dm 755 "$out/share/icons/HaikuHand/"
        cp -r . "$out/share/icons/HaikuHand/"
      '';
    };

    murrina-haikuish = pkgs.stdenv.mkDerivation rec {
      package-name = "murrina-haikuish";
      version = "1.01";
      name = "${package-name}-${version}";
      src = pkgs.fetchurl {
        name = "${name}.tar.gz";
        url = "https://dl.opendesktop.org/api/files/download/id/1460969042/127322-Murrina%20Haikuish.tar.gz";
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
        url = "https://dl.opendesktop.org/api/files/download/id/1460759082/114723-Haiku-${version}.tar.bz2";
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
