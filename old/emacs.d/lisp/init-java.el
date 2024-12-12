;; init-java.lisp

(use-package lsp-java
  :ensure t
  :config
  (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/snapshots/jdt-language-server-1.10.0-202203040350.tar.gz")
  (setenv "JAVA_HOME"  "/Library/Java/JavaVirtualMachines/openjdk.jdk/Contents/Home/")
  (setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/openjdk.jdk/Contents/Home/bin/java")
  (setq lsp-java-configuration-runtimes '[(:name "Open JDK 18"
                                                 :path "/Library/Java/JavaVirtualMachines/openjdk.jdk/Contents/Home"
                                                 :default t)])
  (add-hook 'java-mode-hook 'lsp))

(provide 'init-java)
