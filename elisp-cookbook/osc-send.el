;;; elisp-cookbook/osc-send.el -*- lexical-binding: t; -*-

(setq my-client (osc-make-client "192.168.1.60" 8001))
(osc-send-message my-client "/action" 65793)
(osc-send-message my-client "/midiaction" 65792)
(osc-send-message my-client "/midiaction/_RS7d3c_f263afbe0356f5babe7cc0d783c3d0f64b05f574")
(osc-send-message my-client "/midiaction/123456789")
(osc-send-message my-client "/repltic")
(delete-process my-client)
