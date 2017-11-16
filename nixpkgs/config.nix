{
  # For the broadcom driver :(
  allowUnfree = true;
  allowBroken = true;
  packageOverrides = pkgs: rec {

    pommed_light = pkgs.pommed_light.overrideAttrs (oldAttrs: {
      postPatch = oldAttrs.postPatch + ''
        substituteInPlace pommed/evdev.h --replace 0262 0252
      '';
    });

    twmn = pkgs.stdenv.lib.overrideDerivation pkgs.twmn (oldAttrs: {
      patches = [
        ./twmn/0001-Add-fallback-title-option.patch
      ] ++ oldAttrs.patches or [];
    });

    volnoti_fork = pkgs.stdenv.lib.overrideDerivation pkgs.volnoti (oldAttrs: rec {
      name = "volnoti-fork-${version}";
      version = "22-10-2015";

      src = pkgs.fetchFromGitHub {
        owner = "hcchu";
        repo = "volnoti";
        rev = "c5a94af7338d86ba015f11a2d0ce288ba5f5cbb6";
        sha256 = "14h8a2cdfhyjvz2p1s2n128nsqawhgnp32bbcfdxxis1wswqai2z";
      };

      patches = [ ./volnoti/fix_icon_bounds.patch ];
    });

    skipsEmacs = with pkgs.emacs25PackagesNg;
      pkgs.emacs25WithPackages ((with melpaPackages; [
        aggressive-indent
        auctex
        avy
        benchmark-init
        cmake-ide
        color-theme-sanityinc-tomorrow
        company
        dante
        direnv
        evil
        evil-goggles
        evil-magit
        flycheck
        haskell-mode
        helm
        helm-ag
        helm-google
        helm-projectile
        hlinum
        htmlize
        linum-off
        magit
        nix-mode
        nix-sandbox
        nixos-options
        popwin
        projectile
        pretty-sha-path # This prettifies nix-store paths.
        smart-mode-line
        use-package
        web-mode
      ]) ++ (with elpaPackages; [
        exwm
      ]) ++ (with orgPackages; [
        org-plus-contrib
      ]) ++ [ rtags ]);
  };
}
