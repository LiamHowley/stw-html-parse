(in-package html.parse)


(defvar *end-conditional*)

(defvar *end-title*)

(defvar *end-style*)

(defvar *end-script*)

;;; NOTE: THIS IS INCOMPLETE AND UNFINISHED!!!
;;; How the multiple attribute slots such as data-*, aria-* and event-*
;;; are handled may also change.

(defclass html-document-node (xml-document-node)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; Boolean attributes and content model class mixins
;;; are wrapped in an eval-when so to be
;;; available for DEFINE-HTML-NODE macro.

;;; mixins - putting html nodes in their respective
;;; and often multiple categories

  (defclass structure-node ()
    ()
    (:metaclass singleton-class))

  (defclass metadata-content ()
    ((characters :initform 0 :reader characters))
    (:metaclass singleton-class)
    (:documentation "For use with metadata elements. CHARACTERS
facilitates checks on meta/link elements with charset/content-type 
attributes, which should be placed within the first 1024 bytes 
of the document."))

  (defclass flow-content ()
    ()
    (:metaclass singleton-class))

  (defclass sectioning-root ()
    ()
    (:metaclass singleton-class))

  (defclass heading-content ()
    ()
    (:metaclass singleton-class))

  (defclass phrasing-content ()
    ()
    (:metaclass singleton-class))

  (defclass embedded-content ()
    ()
    (:metaclass singleton-class))

  (defclass interactive-content ()
    ()
    (:metaclass singleton-class))

  (defclass palpable-content ()
    ()
    (:metaclass singleton-class))

  (defclass scripting-content ()
    ()
    (:metaclass singleton-class))



  ;; boolean and global attributes 

  (defclass-with-initargs global-event-attribute ()
    (onabort onautocomplete onautocompleteerror onblur oncancel oncanplay oncanplaythrough onchange onclick onclose oncontextmenu oncuechange ondblclick ondrag ondragend ondragenter ondragleave ondragover ondragstart ondrop ondurationchange onemptied onended onerror onfocus oninput oninvalid onkeydown onkeypress onkeyup onload onloadeddata onloadedmetadata onloadstart onmousedown onmouseenter onmouseleave onmousemove onmouseout onmouseover onmouseup onmousewheel onpause onplay onplaying onprogress onratechange onreset onresize onscroll onseeked onseeking onselect onshow onsort onstalled onsubmit onsuspend ontimeupdate ontoggle onvolumechange onwaiting))

  (defvar *boolean-attributes* `(allowfullscreen async autofocus autoplay checked controls default disabled formnovalidate hidden ismap itemscope loop-attribute multiple muted nomodule novalidate details-open playsinline readonly required reversed selected truespeed))

  (defvar *html-global-attributes*
    '((html-class :attribute "class" :initarg :class :type cons :accessor html-class)
      (data-* :initarg :data-* :type multiple-attributes :accessor data-*)
      (aria-* :initarg :aria-* :type multiple-attributes :accessor aria-*)
      (event-* :type global-event-attribute
	       :attribute "on*"
	       :reader event-*
	       :documentation "lazy loaded, as the large volume of global events would clobber performance. 
I.e. use only as required.")
      (dir :initarg :dir :expected-value ("ltr" "rtl" "auto") :accessor dir)
      (translate :initarg :translate :expected-value ("yes" "no") :accessor translate)
      accesskey autocapitalize contenteditable draggable hidden id lang spellcheck style tabindex title inputmode is itemid itemprop itemref itemscope itemtype slot role)))


(defmethod initialize-instance :after ((class metadata-content) &key)
  (setf (slot-value class 'characters) *char-index*))


(defmacro define-html-node (name supers slots &rest rest)
  "Wrapper on DEFINE-ELEMENT-NODE macro. "
  ;; add global slots
  (let ((slots
	  (loop
	    for slot in *html-global-attributes*
	    do (pushnew slot slots :test #'eq)
	    finally (return slots))))
       `(define-element-node ,name ,supers
	 ,(loop
	    for slot in slots
	    do (setf slot (ensure-list slot))
	    when (member (car slot) *boolean-attributes* :test #'eq)
	      do (push 'boolean (cdr slot)) and
	    do (push :type (cdr slot))
	    collect slot)
	 (:metaclass html-element-class)
	 ,@rest)))

(define-sgml-node !--[if (!--)
  ((closing-tag :initarg :closing-tag :initform "<![endif]-->"))
  (:documentation "conditional comments - IE specific"))


(define-html-node html ()
  (manifest version xmlns)
  (:permitted-content . `(head body)))


(define-html-node head (structure-node)
  ((profile :status :obsolete))
  (:permitted-parent . `(html)))


(define-html-node base (metadata-content leaf-node)
  (href target)
  (:permitted-parent . `(head)))


(define-html-node link (metadata-content leaf-node)
  ((link-type :attribute "type")
   (crossorigin :expected-value ("anonymous" "use-credentials"))
   (as :expected-value ("audio" "document" "embed" "fetch" "font" "image" "object" "script" "style" "track" "video" "worker"))
   (integrity :status :experimental)
   (prefetch :status :experimental)
   (referrerpolicy :status :experimental
		   :expected-value ("no-referrer" "no-referrer-when-downgrade" "origin" "origin-when-cross-origin" "unsafe-url"))
   (rel :expected-value ("alternate" "canonical" "author" "dns-prefetch" "help" "icon" "manifest" "modulepreload" "license" "next" "pingback" "preconnect" "prefetch" "preload" "prerender" "prev" "search" "shortcut" "stylesheet"))
   (charset :status :obsolete)
   (rev :status :obsolete)
   href hreflang media sizes imagesrcset color disabled))


(define-html-node meta (metadata-content leaf-node)
  ((http-equiv :expected-value ("content-security-policy" "content-type" "default-style" "x-ua-compatible" "refresh"))
   charset name property content))


(define-html-node title (metadata-content content-node)
  ()
  (:permitted-parent . `(head)))

(defmethod initialize-instance :before ((class title) &key)
  (setf (slot-value class 'closing-tag) *end-title*))


(define-html-node style (metadata-content content-node)
  ((style-type :attribute "type" :initarg :type)
   (scoped :status :deprecated)
   media nonce))

(defmethod initialize-instance :before ((class style) &key)
  (setf (slot-value class 'closing-tag) *end-style*))


;; document nodes

(define-html-node body (structure-node sectioning-root)
  ((alink :status :obsolete)
   (background :status :obsolete)
   (bgcolor :status :obsolete)
   (bottommargin :status :obsolete)
   (leftmargin :status :obsolete)
   (link :status :obsolete)
   (rightmargin :status :obsolete)
   (text :status :obsolete)
   (topmargin :status :obsolete)
   (vlink :status :obsolete)
   onafterprint onbeforeprint onbeforeunload onhashchange onmessage onoffline ononline onpagehide onpageshow onpopstate onstorage onunload)
  (:permitted-parent . `(html))
  (:implicit-aria . :document)
  (:permitted-aria . :none))

(define-html-node address (flow-content palpable-content) ())

(define-html-node article (flow-content sectioning-root palpable-content) ())

(define-html-node aside (flow-content sectioning-root palpable-content) ())

(define-html-node footer (flow-content palpable-content) ())

(define-html-node header (flow-content palpable-content) ())

(define-html-node hgroup (flow-content heading-content palpable-content)
  ()
  (:status . :deprecated))

(define-html-node h1 (flow-content heading-content palpable-content) ())

(define-html-node h2 (flow-content heading-content palpable-content) ())

(define-html-node h3 (flow-content heading-content palpable-content) ())

(define-html-node h4 (flow-content heading-content palpable-content) ())

(define-html-node h5 (flow-content heading-content palpable-content) ())

(define-html-node h6 (flow-content heading-content palpable-content) ())

(define-html-node nav (flow-content sectioning-root palpable-content) ())

(define-html-node section (flow-content sectioning-root palpable-content) ())

(define-html-node blockquote (flow-content sectioning-root palpable-content)
  (cite))

(define-html-node dd () ())

(define-html-node div (flow-content palpable-content) ())

(define-html-node dl (flow-content) ())

(define-html-node dt () ())

(define-html-node figcaption () ())

(define-html-node figure (flow-content sectioning-root palpable-content) ())

(define-html-node hr (flow-content leaf-node)
  (align))

(define-html-node li ()
  ((value :type integer)))

(define-html-node main (flow-content palpable-content)())

(define-html-node ol (flow-content palpable-content)
  ((start :type integer)
   reversed))

(define-html-node p (flow-content palpable-content) ())

(define-html-node pre (flow-content palpable-content) ())

(define-html-node ul (flow-content palpable-content) ())

(define-html-node a (flow-content phrasing-content interactive-content palpable-content)
  ((shape :expected-value ("circle" "default" "poly" "rect")
	  :status :deprecated)
   (charset :status deprecated)
   (coords :status deprecated)
   (name :status deprecated)
   (rev :status deprecated)
   href download hreflang media ping referrerpolicy rel target))

(define-html-node abbr (flow-content phrasing-content palpable-content) ())

(define-html-node b (flow-content phrasing-content palpable-content) ())

(define-html-node bdi (flow-content phrasing-content palpable-content) ())

(define-html-node bdo (flow-content phrasing-content palpable-content) ())

(define-html-node br (flow-content phrasing-content leaf-node) ())

(define-html-node cite (flow-content phrasing-content palpable-content) ())

(define-html-node code (flow-content phrasing-content palpable-content) ())

(define-html-node data (flow-content phrasing-content palpable-content)
  (value))

(define-html-node dfn (flow-content phrasing-content palpable-content) ())

(define-html-node em (flow-content phrasing-content palpable-content) ())

(define-html-node i (flow-content phrasing-content palpable-content) ())

(define-html-node kbd (flow-content phrasing-content palpable-content) ())

(define-html-node mark (flow-content phrasing-content palpable-content) ())

(define-html-node q (flow-content phrasing-content palpable-content)
  (cite))
(define-html-node rb () ())

(define-html-node rp () ())

(define-html-node rt () ())

(define-html-node rtc () ())

(define-html-node ruby (flow-content phrasing-content palpable-content) ())

(define-html-node s (flow-content phrasing-content) ())

(define-html-node samp (flow-content phrasing-content palpable-content) ())

(define-html-node small (flow-content phrasing-content) ())

(define-html-node span (flow-content phrasing-content) ())

(define-html-node strong (flow-content phrasing-content palpable-content) ())

(define-html-node sub (flow-content phrasing-content palpable-content) ())

(define-html-node sup (flow-content phrasing-content palpable-content) ())

(define-html-node html-time (flow-content phrasing-content palpable-content)
  (datetime)
  (:element . "TIME"))

(define-html-node u (flow-content phrasing-content palpable-content) ())

(define-html-node var (flow-content phrasing-content palpable-content) ())

(define-html-node wbr (flow-content phrasing-content leaf-node) ())

(define-html-node area (flow-content phrasing-content leaf-node)
  ((shape :expected-value ("circle" "default" "poly" "rect"))
   href alt coords download hreflang media ping referrerpolicy rel target))

(define-html-node audio (flow-content interactive-content palpable-content)
  ((loop-attribute :attribute "loop" :initarg :loop)
   (preload :expected-value ("none" "metadata" "auto"))
   (crossorigin :expected-value ("anonymous" "use-credentials"))
   src autoplay buffered controls muted))

(define-html-node img (flow-content phrasing-content embedded-content palpable-content leaf-node)
  ((crossorigin :expected-value ("anonymous" "use-credentials"))
   (width :type integer)
   (height :type integer)
   src align alt decoding ismap referrerpolicy sizes srcset usemap))

(define-html-node html-map (flow-content phrasing-content palpable-content)
  (name)
  (:element . "MAP"))

(define-html-node track (leaf-node)
  ((kind :expected-value ("subtitles" "captions" "descriptions" "chapters" "metadata"))
   src default label srclang))

(define-html-node video (flow-content phrasing-content embedded-content)
  ((loop-attribute :attribute "loop" :initarg :loop)
   (preload :expected-value ("none" "metadata" "auto"))
   (crossorigin :expected-value ("anonymous" "use-credentials"))
   (width :type integer)
   (height :type integer)
   src autoplay buffered controls muted poster))

(define-html-node embed (flow-content phrasing-content embedded-content interactive-content palpable-content leaf-node)
  ((embed-type :attribute "type" :initarg :type)
   (width :type integer)
   (height :type integer)
   src))

(define-html-node portal () ())

(define-html-node iframe (flow-content phrasing-content embedded-content interactive-content palpable-content)
  ((allow :initform nil :accessor allow :initarg :allow :documentation "allow accepts a list of origins that takes one or more of the following values: * 'self' 'src' 'none' or <origin> (i.e. https://example.com). E.g. <iframe allow=\"camera 'none'; microphone 'none'\"> See https://developer.mozilla.org/en-US/docs/Web/HTTP/Feature_Policy/Using_Feature_Policy#allowlist for more.")
   (allowtransparency :attribute "allowTransparency" :initform nil :reader allowtransparency :status :obsolete :documentation "obsolete - do not use")
   (scrolling :initform nil :reader scrolling :status :deprecated :documentation "deprecated - do not use")
   (longdesc :initform nil :reader longdesc :status :deprecated :documentation "deprecated - do not use")
   (align :initform nil :reader align :status :deprecated :documentation "deprecated - do not use")
   (frameborder :initform nil :reader frameborder :status :deprecated :documentation "deprecated - do not use")
   (marginheight :initform nil :reader marginheight :status :deprecated :documentation "deprecated - do not use")
   (marginwidth :initform nil :reader marginwidth :status :deprecated :documentation "deprecated - do not use")
   (loading :initform "lazy" :accessor loading)
   (sandbox :expected-value ("allow-forms" "allow-pointer-lock" "allow-popups" "allow-same-origin" "allow-scripts" "allow-top-navigation"))
   (width :type integer)
   (height :type integer)
   src name referrerpolicy sourcedoc))

(define-html-node object (flow-content phrasing-content embedded-content palpable-content interactive-content)
  ((object-type :attribute "type" :initarg :type)
   (width :type integer)
   (height :type integer)
   data form name usemap))

(define-html-node param (leaf-node)
  (name value))

(define-html-node picture (flow-content phrasing-content embedded-content)
  ())

(define-html-node source (leaf-node)
  ((source-type :attribute "type" :initarg :type)
   src media sizes srcset))

(define-html-node canvas (flow-content phrasing-content embedded-content palpable-content)
  ((width :type integer)
   (height :type integer)))

(define-html-node noscript (metadata-content flow-content phrasing-content) ())

(define-html-node script (metadata-content content-node flow-content phrasing-content)
  ((script-type :attribute "type" :initarg :type)
   (crossorigin :expected-value ("anonymous" "use-credentials"))
   src async charset defer integrity language referrerpolicy nomodule))

(defmethod initialize-instance :before ((class script) &key)
  (setf (slot-value class 'closing-tag) *end-script*))


(define-html-node del (phrasing-content flow-content)
  (cite datetime))

(define-html-node ins (phrasing-content flow-content)
  (cite datetime))

(define-html-node caption ()
  (align))

(define-html-node col (leaf-node)
  (align span))

(define-html-node colgroup ()
  (align span))

(define-html-node table (flow-content)
  (align summary))

(define-html-node tbody ()
  (align))

(define-html-node td ()
  ((colspan :type integer)
   (rowspan :type integer)
   align headers))

(define-html-node tfoot ()
  (align headers))

(define-html-node th ()
  ((scope :expected-value ("row" "col" "rowgroup" "colgroup"))
   (align :status :obsolete)
   (axis :status :obsolete)
   (html-char :attribute "char" :status :obsolete)
   (charoff :status :obsolete)
   (height :status :obsolete)
   (valign :status :obsolete)
   (width :status :obsolete)
   (bgcolor :status :non-standard)
   abbr headers
   (colspan :type integer)
   (rowspan :type integer)))

(define-html-node thead ()
  (align))

(define-html-node tr ()
  (align))

(define-html-node button (flow-content phrasing-content interactive-content palpable-content)
  (autofocus disabled form formaction formenctype formmethod formnovalidate formtarget name button-type value))


(define-html-node datalist (flow-content phrasing-content) ())

(define-html-node fieldset (flow-content sectioning-root palpable-content)
  (disabled form name))

(define-html-node form (flow-content palpable-content)
  ((form-method :attribute "method" :initarg :method :expected-value ("get" "post"))
   accept accept-charset action autocomplete enctype name novalidate target))

(define-html-node input (flow-content phrasing-content palpable-content leaf-node)
  ((input-type :attribute "type" :initform "text" :initarg :type :accessor input-type)
   (input-min :attribute "min" :initarg :min)
   (input-max :attribute "max" :initarg :max)
   (input-list :attribute "list" :initarg :list)
   (input-step :attribute "step" :initarg :step :type number)
   accept alt autocomplete autofocus capture checked dirname disabled form formaction formenctype formmethod formnovalidate formtarget height maxlength minlength multiple name pattern placeholder readonly required size usemap value width))

(define-html-node label (flow-content phrasing-content interactive-content palpable-content)
  (for form))

(define-html-node legend () ())

(define-html-node meter (flow-content phrasing-content palpable-content)
  ((meter-max :attribute "max" :initarg :max :type number)
   (meter-min :attribute "min" :initarg :min :type number)
   (low :type number)
   (high :type number)
   (optimum :type number)
   (value :type number)
   form))

(define-html-node optgroup ()
  (disabled label))

(define-html-node option ()
  (disabled label selected value))

(define-html-node output (flow-content phrasing-content palpable-content)
  (for form name))

(define-html-node progress (flow-content phrasing-content palpable-content)
  ((progress-max :attribute "max" :initarg :max :type number)
   (value :type number)
   form))

(define-html-node select (flow-content phrasing-content interactive-content)
  ((size :type integer)
   autocomplete autofocus disabled form multiple name required))

(define-html-node textarea (flow-content phrasing-content interactive-content)
  ((wrap :expected-value ("soft" "hard"))
   (cols :type integer)
   (rows :type integer)
   (minlength :type integer)
   (maxlength :type integer)
   autocomplete autofocus disabled form inputmode name placeholder readonly required))

(define-html-node details (flow-content sectioning-root interactive-content palpable-content)
  ((details-open :attribute "open" :initarg :open)))

(define-html-node dialogue (flow-content sectioning-root) ())

(define-html-node menu (flow-content palpable-content)
  ((menu-type :attribute "type" :initarg :type)))

(define-html-node summary () ())

(define-html-node slot (flow-content phrasing-content) ())

(define-html-node template (metadata-content) ())

(define-html-node tt (flow-content phrasing-content palpable-content)
  ()
  (:status . :deprecated))


;; obsolete elements

(define-html-node acronym () () (:status . :obsolete))

(define-html-node applet (flow-content phrasing-content embedded-content interactive-content palpable-content)
  (align alt code codebase)
  (:status . :obsolete))

(define-html-node basefont () () (:status . :obsolete))

(define-html-node bgsound ()
  ((loop-attribute :attribute "loop" :initarg :loop))
  (:status . :obsolete))

(define-html-node big () () (:status . :obsolete))

(define-html-node blink () () (:status . :obsolete))

(define-html-node center () () (:status . :obsolete))

(define-html-node command (leaf-node)
  ((command-type :attribute "type" :initarg :type)
   checked disabled icon radiogroup))

(define-html-node content () () (:status . :obsolete))

(define-html-node dir () () (:status . :obsolete))

(define-html-node font () () (:status . :obsolete))

(define-html-node frame () () (:status . :obsolete))

(define-html-node frameset () () (:status . :obsolete))

(define-html-node image () () (:status . :obsolete))

(define-html-node isindex () () (:status . :obsolete))

(define-html-node keygen ()
  (autofocus challenge disabled form keytype name)
  (:status . :obsolete))

(define-html-node listing () () (:status . :obsolete))

(define-html-node marquee ()
  ((loop-attribute :attribute "loop" :initarg :loop)
   truespeed)
  (:status . :obsolete))

(define-html-node menuitem (leaf-node) () (:status . :obsolete))

(define-html-node multicol () () (:status . :obsolete))

(define-html-node nextid () () (:status . :obsolete))

(define-html-node nobr () () (:status . :obsolete))

(define-html-node noembed () () (:status . :obsolete))

(define-html-node noframes () () (:status . :obsolete))

(define-html-node plaintext () () (:status . :obsolete))

(define-html-node html-shadow ()
  ()
  (:element . "SHADOW")
  (:status . :obsolete))

(define-html-node spacer () () (:status . :obsolete))

(define-html-node strike () () (:status . :obsolete))

(define-html-node xmp () () (:status . :obsolete))
