((nil . ((projectile-use-git-grep . t)
         (projectile-project-compilation-cmd . "npm run build")
         (eval . (setq projectile-project-test-cmd
                       (lambda ()
                         (projectile-run-compilation "npm run test"))))
         (projectile-project-run-cmd . "npm run main")
         (psc-ide-use-npm-bin . t))))
