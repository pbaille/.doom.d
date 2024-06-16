;;; elisp-cookbook/osc-send.el -*- lexical-binding: t; -*-

(setq pb-client (osc-make-client "192.168.1.60" 8001))
(osc-send-message pb-client "/action" 65793)
(osc-send-message pb-client "/midiaction" 65792)
(osc-send-message pb-client "/midiaction/_RS7d3c_f263afbe0356f5babe7cc0d783c3d0f64b05f574")
(osc-send-message pb-client "/midiaction/123456789")
(osc-send-message pb-client "/repltic")
(delete-process pb-client)
