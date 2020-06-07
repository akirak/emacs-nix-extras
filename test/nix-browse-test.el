;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'nix-browse)

(describe "nix-browse--check-http-url"
  (it "returns nil on existing URL"
    (expect (nix-browse--check-http-url "https://github.com/akirak/org-starter.git")
            :to-be nil))
  (it "throws an error on non-existing URL"
    (expect (nix-browse--check-http-url "https://github.com/akirak/org-starter.el.git")
            :to-throw)))

(describe "nix-bookmark--filename-escape-url"
  (it "escapes a URL into a filename-safe string"
    (expect (nix-bookmark--filename-escape-url
             "https://github.com/akirak/org-starter.git")
            :to-equal "github_com__akirak__org-starter")
    (expect (nix-bookmark--filename-escape-url
             "https://github.com/akirak.akirak/org-starter.git")
            :to-equal "github_com__akirak_akirak__org-starter")))

(provide 'nix-browse-test)
