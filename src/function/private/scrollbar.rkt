#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
        ; racket/gui/base
         racket/class)
(provide (protect-out setscrollpos:vertical
                      setscrollpos:horizontal
                      getscrollpos:vertical
                      getscrollpos:horizontal
                      getscrollrange:vertical
                      getscrollinfo:vertical
                      (struct-out tagSCROLLINFO)))

;; Win32 type and structure declarations.

(define user-dll (and (eq? (system-type) 'windows)
                        (ffi-lib "User32.dll")))

(define-ffi-definer define-userapi user-dll
  #:default-make-fail make-not-available)

;; for functions that use the Windows stdcall ABI:
(define-syntax-rule (_wfun type ...)
  (_fun #:abi winapi type ...))

(define SB_VERT 1)

(define SB_HORZ 0)

(define _windows-pointer (_cpointer 'HWND))

(define-cstruct _RECT
  ([ left _long]
  [top _long]
  [right _long]
  [bottom _long]))

(define-cstruct _tagSCROLLINFO
  ([cbSize _uint]
   [fMask _uint]
   [nMin _int]
   [nMax _int]
   [nPage _int]
   [nPos _int]
   [nTrackPos _int]))

(define-userapi win32-SetScrollPos (_wfun _windows-pointer _int _int _bool -> _int)
  #:c-id SetScrollPos)

(define-userapi win32-getScrollPos (_wfun _windows-pointer _int -> _int)
  #:c-id GetScrollPos)

(define-userapi win32-getScrollinfo (_wfun _windows-pointer _int _tagSCROLLINFO-pointer -> _bool)
  #:c-id GetScrollInfo)

(define-userapi win32-getScrollRange (_wfun _windows-pointer _int (_box _int) (_box _int) -> _bool)
  #:c-id GetScrollRange)

(define-userapi win32-UpdateWindow (_wfun _windows-pointer -> _bool)
  #:c-id UpdateWindow)

(define-userapi win32-ScrollWindow (_wfun _windows-pointer _int _int _RECT-pointer/null _RECT-pointer/null -> _bool)
   #:c-id ScrollWindow)

(define(getscrollrange:vertical handle)
  (define min (box 0))
  (define max (box 0))
  (win32-getScrollRange handle SB_VERT min max)
  (values (unbox min) (unbox max)))

(define SIF_PAGE 1)

(define SIF_POS 2)

(define SIF_RANGE 4)

(define(getscrollinfo:vertical handle)
  (define infobox (make-tagSCROLLINFO (ctype-sizeof _tagSCROLLINFO) 7  0 0 0 0 0))
  (win32-getScrollinfo handle SB_VERT infobox)
  (values (tagSCROLLINFO-nPage infobox)
          (tagSCROLLINFO-nPos infobox)
          (tagSCROLLINFO-nMin infobox)
          (tagSCROLLINFO-nMax infobox)
          (tagSCROLLINFO-nTrackPos infobox)))

(define(getscrollreal-range:vertical handle)
  (define infobox (make-tagSCROLLINFO (ctype-sizeof _tagSCROLLINFO) 7  0 0 0 0 0))
  (win32-getScrollinfo handle SB_VERT infobox)
  (values (tagSCROLLINFO-nMin infobox)
          (-(tagSCROLLINFO-nMax infobox) (tagSCROLLINFO-nPage infobox) -1)))

(define (setscrollpos:vertical panel pos)
  (define handle (send panel get-handle))
  (define-values(min max)(getscrollreal-range:vertical handle))
  (define rpos (if(< pos min) min (if(> pos max) max pos)))
  (define pos1 (win32-getScrollPos handle SB_VERT))
  (win32-SetScrollPos handle SB_VERT rpos #t)
  (win32-ScrollWindow handle 0 (- pos1 rpos) #f #f))

(define (setscrollpos:horizontal panel pos)
  (win32-SetScrollPos (send panel get-handle) SB_HORZ pos #t))

(define (getscrollpos:vertical panel)
  (win32-getScrollPos (send panel get-handle) SB_VERT))

(define (getscrollpos:horizontal panel)
  (win32-getScrollPos (send panel get-handle) SB_HORZ))