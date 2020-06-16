;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'nixut)

(describe "Internal functions"
  (describe "nixut-remove-prop"
    (it "removes a property"
      (expect (nixut-remove-prop :a '(1 2 3 :a 4 5 :b c))
              :to-equal '(1 2 3 5 :b c))
      (expect (nixut-remove-prop :a '(:a 4 5 :b c))
              :to-equal '(5 :b c))
      (expect (nixut-remove-prop :a '(1 2 3 :a 4))
              :to-equal '(1 2 3))
      (expect (nixut-remove-prop :a '(1 2 3 4 5))
              :to-equal '(1 2 3 4 5)))))

(describe "JSON functions"

  (describe "nixut-with-json-types"
    (it "sets json-object-type to alist"
      (expect (nixut-with-json-types json-object-type)
              :to-be 'alist))
    (it "sets json-array-type to list"
      (expect (nixut-with-json-types json-array-type)
              :to-be 'list)))

  (describe "nixut-json-read-from-string"
    (it "reads a string"
      (expect (nixut-json-read-from-string "\"hello\"")
              :to-equal "hello"))
    (it "reads a number"
      (expect (nixut-json-read-from-string "25")
              :to-equal 25))
    (it "reads a real number"
      (expect (nixut-json-read-from-string "25.2")
              :to-equal 25.2))
    (it "reads a sequence as a list"
      (expect (nixut-json-read-from-string "[\"hello\", 1]")
              :to-equal '("hello" 1)))
    (it "reads an object as an alist "
      (expect (nixut-json-read-from-string "{\"key\":\"hello\"}")
              :to-equal '((key . "hello")))))

  (describe "nixut-json-read-buffer"
    (it "reads a JSON buffer"
      (expect (with-temp-buffer
                (insert "{\"key\":\"hello\"}")
                (nixut-json-read-buffer))
              :to-equal '((key . "hello")))))

  (describe "nixut-read-json-file"
    (it "reads a JSON file"
      (expect (let ((filename (make-temp-file "nixut-test-sample" nil ".json")))
                (unwind-protect
                    (progn
                      (with-temp-file filename
                        (insert "{\"key\":\"hello\"}"))
                      (nixut-json-read-file filename))
                  (delete-file filename)))
              :to-equal '((key . "hello"))))))

(describe "Processes"

  (describe "nixut-format-command-line"
    (it "formats a command line"
      (expect (nixut-format-command-line "nix" '("--version"))
              :to-equal "nix --version"))
    (it "escapes space"
      (expect (nixut-format-command-line "echo" '("good morning"))
              :to-equal "echo good\\ morning")))

  (describe "nixut-sync-process"
    (it "runs processes"
      (expect (nixut-sync-process "true")
              :to-be nil)
      (expect (nixut-sync-process "echo" "1")
              :to-be nil))
    (it "accepts an output buffer"
      (expect (let ((buffer (generate-new-buffer "*output*")))
                (unwind-protect
                    (progn
                      (nixut-sync-process "echo"
                                          :destination buffer
                                          "-n" "hello")
                      (with-current-buffer buffer
                        (buffer-string)))
                  (kill-buffer buffer)))
              :to-equal "hello"))
    (it "throws an error on non-zero exit"
      (expect (nixut-sync-process "false")
              :to-throw)))

  (describe "nixut-read-process"
    (it "reads process output"
      (expect (nixut-read-process "echo" "-n" "hello")
              :to-equal "hello"))))

(describe "nixut-sha1-file"
  (it "returns the sha1 sum of a file"
    (let ((file (make-temp-file "nixut-test-sample" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "hello good bye"))
            (expect (string-prefix-p (nixut-sha1-file file)
                                     (nixut-read-process "sha1sum" file))))
        (delete-file file)))))

(describe "Nix"
  (describe "nixut-nix-installed-p"
    (describe "without argument"
      (xit "checks if nix is installed"
        (expect (nixut-nix-installed-p)
                :to-be-truthy))
      (it "nil if nix is not installed"
        (expect (let ((exec-path nil))
                  (not (nixut-nix-installed-p)))))
      (it "throws an error on unsupported systems"
        (expect (let ((system-type 'msdos))
                  (nixut-nix-installed-p))
                :to-throw)))
    (xdescribe "with version"
      (it "returns the current version"
        (expect (version< "2.0" (nixut-nix-installed-p "2.0"))
                :to-be-truthy))
      (it "throws an error"
        (expect (nixut-nix-installed-p "999.9")
                :to-throw)))))

(describe "Niv"
  (describe "nixut-niv-sources"
    (it "reads nix/sources.json"
      (expect (let ((tempdir (make-temp-file "nixut-test" 'dir)))
                (unwind-protect
                    (progn
                      (make-directory (expand-file-name "nix" tempdir))
                      (with-temp-buffer
                        (setq buffer-file-name
                              (expand-file-name "nix/sources.json" tempdir))
                        (insert "{\"hello\":\"OK\"}")
                        (save-buffer))
                      (nixut-niv-sources tempdir))
                  (delete-directory tempdir 'recursive)))
              :to-equal '((hello . "OK"))))))

(provide 'nixut-test)
