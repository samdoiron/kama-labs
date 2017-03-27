(use posix)
(use utils)

(define (system-output command)
  (call-with-input-pipe command read-all))

(define (pwd)
  (system "pwd"))

(define (pwd)
  (system-output "pwd"))

(define escape-code
  (string (integer->char 27) #\[))
  
(define clear-formatting
  (conc escape-code ""))

;;; ANSI escape code is of the formm <ESC>[<n>m
;;; Here you give the (integer) n to use.
(define (style n)
  (conc escape-code
        (number->string n)
        "m"))

(define (color name)
  (cond ((equal? name 'reset) 0)
        ((equal? name 'black) 30)
        ((equal? name 'red) 31)
        ((equal? name 'green) 32)
        ((equal? name 'yellow) 33)
        ((equal? name 'blue) 34)
        ((equal? name 'magenta) 35)
        ((equal? name 'cyan) 36)
        ((equal? name 'white) 37)))

(define path
  '("/usr/local/bin"
    "/Users/samdoiron/.cargo/bin"
    "/usr/local/bin"
    "/usr/local/sbin"
    "/usr/bin"
    "/bin"
    "/usr/sbin"
    "/sbin"
    "/opt/X11/bin"
    "/usr/local/go/bin"
    "/Users/samdoiron/.rvm/bin"
    "/Users/samdoiron/.rvm/bin"
    "/Users/samdoiron/bin"
    "/Users/samdoiron/.cabal/bin"
    "/Users/samdoiron/bin/checker-278/bin"
    "/Users/samdoiron/.nimble/bin"
    "/Users/samdoiron/Code/Go/bin"
    "/Users/samdoiron/.fzf/bin"))

(define reset-style
  (style (color 'reset)))

(define (prompt)
  (conc (style (color 'green))
        (pwd)
        reset-style
        "$ "))

(define (empty? list)
  (= 0 (length list)))

(define (range-iter start stop prev)
  (if (= start stop) (reverse prev)
    (range-iter (+ start 1) stop (cons start prev))))

(define (range max)
  (range-iter 0 max (list)))

(define (split string token)
  (define (inner next chunk chunks)
    (cond ((empty? next)
          (cons (list->string chunk)
                chunks))
          ((equal? (car next) token)
            (inner (cdr next)
                   '()
                   (cons (list->string chunk) chunks)))
          (else (inner (cdr next)
                       (cons (car next) chunk)
                       chunks))))
    (inner (reverse (string->list string)) (list) (list)))

(define (lines string)
  (split string #\newline))

(define (ls)
  (lines (system-output "ls -1")))

(define (quit)
  (print "Goodbye")
  (exit))

(define (execute command)
  (cond ((equal? #!eof command) (quit))
        ((string=? "ls" command) (ls))))

(define (main-loop)
  (display (prompt))
  (let ((command (read-line)))
    (display (execute command)))
  (newline)
  (main-loop))

;(main-loop)