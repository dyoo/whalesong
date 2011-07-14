#lang s-exp "../lang/js-impl/js-impl.rkt"

;; Loaded so we have access to image library stuff, as well as the world kernel
(require "../world/kernel.rkt"
	 "../image/image.rkt")


(require-js "private/jsworld/jsworld.js"
	    "private/jsworld.js"
	    "jsworld.js")


(provide big-bang
	 to-draw
         to-draw-page
         
         key=?
	 on-tick on-tick!
	 on-key on-key!
	 stop-when stop-when!

	 initial-effect

	 js-p
	 js-div
	 js-button
	 js-button!
	 js-input
	 js-img
	 js-text
	 js-select


	 empty-page
	 place-on-page

	 make-world-config
	 make-effect-type
	 effect-type?
	 effect?

	 #;make-render-effect-type
	 #;render-effect-type?

	 world-with-effects

	 #;make-render-effect
	 #;render-effect?
	 #;render-effect-dom-node
	 #;render-effect-effects)
