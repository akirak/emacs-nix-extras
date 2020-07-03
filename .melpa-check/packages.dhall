let Schema = ./schema.dhall

let Package = Schema.Package

let TestDriver = Schema.TestDriver

in  [ Package::{
      , pname = "nix-browse"
      , version = "0.1"
      , emacsVersion = "26.1"
      , files = [ "nix-bookmark.el", "nix-browse.el" ]
      , dependencies = [] : List Text
      , testDrivers = [ TestDriver.buttercup ]
      , testExcludes = [ "**/test-helper?(s).el" ]
      , mainFile = Some "nix-browse.el"
      , recipe =
          ''
          (nix-browse :fetcher github :repo "akirak/emacs-nix-extras" :files ("nix-bookmark.el" "nix-browse.el"))
          ''
      }
    , Package::{
      , pname = "nixut"
      , version = "0.1"
      , emacsVersion = "25.1"
      , files = [ "nixut.el" ]
      , dependencies = [] : List Text
      , testDrivers = [ TestDriver.buttercup ]
      , testExcludes = [ "**/test-helper?(s).el" ]
      , recipe =
          ''
          (nixut :fetcher github :repo "akirak/emacs-nix-extras" :files ("nixut.el"))
          ''
      }
    , Package::{
      , pname = "nemo"
      , version = "0.1"
      , emacsVersion = "26.1"
      , files = [ "nemo.el" ]
      , dependencies = [] : List Text
      , testDrivers = [ TestDriver.buttercup ]
      , buttercupTests = [ "nemo-tests.el" ]
      , recipe =
          ''
          (nemo :fetcher github :repo "akirak/emacs-nix-extras" :files ("nemo.el"))
          ''
      }
    ]
