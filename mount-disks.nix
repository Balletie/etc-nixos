{ config, pkgs, ... }:

{
  # HFS+ kernel module. Probably loaded by default, but set here
  # explicitly to be sure.
  boot.kernelModules = [ "hfsplus" ];

  # Bring bindfs into scope, which will be used to correct file permissions
  # so that files and directories are accessible. This instead of using
  # chmod or chown which will break the OS X install (dual boot).
  system.fsPackages = [ pkgs.bindfs ];

  # Mount the OS X partition
  fileSystems."/mnt/osx_pre_mnt" =
    { device = "/dev/disk/by-uuid/f7273f9c-c88b-38a2-b60c-7af40a8b4e25";
      fsType = "hfsplus";
      options = "ro,uid=501,gid=20";
    };

  # Use bindfs on top of the other mount, to correct the UIDs and GIDs
  # to the ones used in this install (501 in OS X, 1000 in NixOS install).
  fileSystems."/run/media/skip/Macintosh\\040HD" =
    { device = "/mnt/osx_pre_mnt";
      # FIXME: Hack to workaround "command not found" bug.
      fsType = "fuse./run/current-system/sw/bin/bindfs";
      options = "ro,map=501/1000:@20/@10";
    };
}
