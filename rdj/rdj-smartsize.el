(defun rdj-small-font ()
  (if rdj-is-mac
      "-*-menlo-*-r-*-*-12-*-*-*-*-*-*-*"
    "-*-consolas-*-r-*-*-12-*-*-*-*-*-*-*"
    )
  )
(defun rdj-big-font   ()
  (if rdj-is-mac
      "-*-menlo-*-r-*-*-14-*-*-*-*-*-*-*"
    "-*-consolas-*-r-*-*-14-*-*-*-*-*-*-*"
    )
  )
(defun rdj-huge-font   ()
  (if rdj-is-mac
      "-*-menlo-*-r-*-*-15-*-*-*-*-*-*-*"
    "-*-consolas-*-r-*-*-15-*-*-*-*-*-*-*"
    )
  )

(defun rdj-change-font (fontspec) "Change the current and default frame font"
  (interactive)
  (set-frame-font fontspec)
  (aput 'default-frame-alist 'font fontspec)
)

(defun rdj-setup-frame (xpos ypos width height font)
  (rdj-change-font font)
  (set-frame-position (selected-frame) xpos ypos)
  (set-frame-size (selected-frame) width height)
)

(defun rdj-smartsize-frame-for (display-width display-height)
  (apply 'rdj-setup-frame (cond    ; +X  +Y   WxH  FONT
    ((= display-width 1680)   (list   1   0 184 52 (rdj-huge-font)))
    ((= display-width 1920)   (list 175  50 225 72 (rdj-small-font)))
    ((= display-width 2560)   (list 240  50 260 75 (rdj-big-font)))
    ((< display-width 1920)   (list   0   0 180 50 (rdj-small-font)))
    ((< display-height 1600)  (list 261  50 260 76 (rdj-big-font)))
    ((>= display-height 1920) (list 261 125 260 80 (rdj-big-font)))
  ))
)

(defun rdj-smartsize-frame () "Set frame size, position, font to something reasonable"
  (interactive)
  ;; In 23.1, x-display-pixel-width is the width of the current
  ;; screen, not the entire display including all screens. However,
  ;; set-frame-position takes bounds which are across all screens. So
  ;; moving the frame back to (0,0) first lets us query the leftmost
  ;; topmost screen's size. Which - at least at this point - is what I
  ;; care about. I'll learn to live with the unsightly flash.
  (rdj-setup-frame 0 0 180 50 (rdj-small-font))
  (rdj-smartsize-frame-for (x-display-pixel-width) (x-display-pixel-height))
)

(provide 'rdj-smartsize)
