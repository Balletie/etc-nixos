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

    skipsEmacs = with pkgs.emacs26PackagesNg;
      pkgs.emacs26WithPackages ((with melpaPackages; [
        # Emacs utility packages and libraries
        use-package
        benchmark-init
        f
        s

        auctex # LaTeX mode
        avy # Quick search on character
        popwin # Popup windows
        smart-mode-line # Configurable modeline
        hlinum
        linum-off
        htmlize
        magit # Git frontend
        color-theme-sanityinc-tomorrow
        diminish # Remove minor modes from modeline.
        projectile # Find projects, search in projects
        elfeed
        elfeed-goodies
        elfeed-org
        emms # Media player

        # Vim emulation
        evil
        evil-goggles # Visual highlighting when yankin/deleting/etc.
        evil-magit

        helm
        helm-ag
        helm-projectile

        # org-mode stuff
        org-bullets
	
        # Nix related stuff
        nix-mode
        # nix-update

        yasnippet
        yasnippet-snippets

        # Switches direnv environments.
        # When used together with Nix, provides seamless switching of environments when
        # switching buffers in Emacs.
        direnv

        # Programming language modes
        php-mode
        web-mode
        haskell-mode
        dante # Frontend to GHCi and integrates with flycheck & company.
        vue-mode
        # Code completion
        company
        company-jedi
        # Syntax checking
        flycheck
      ]) ++ (with elpaPackages; [
        # Emacs window manager
        exwm
      ]) ++ (with orgPackages; [
        org-plus-contrib
      ]) ++ [ rtags ]);
  };
}
