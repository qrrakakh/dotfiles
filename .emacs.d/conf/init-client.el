; server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))
