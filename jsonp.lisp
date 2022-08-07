
(defun return-> (result remainder)
    "structure parser's output"
    (cons result remainder))

(defun .< (x)
    "get the result part of parser's output"
    (car x))

(defun >. (x)
    "get the remainder part of parser's output"
    (cdr x))

(defun if-> (condition)
    (lambda (s)
        (if (and (> (length s) 0) (funcall condition (elt s 0)))
            (return-> (elt s 0) (subseq s 1))
            nil)))

(defun char-> (achar)
    (if-> (lambda (x) (equal x achar))))

(defun word-> (aword)
    (lambda (s)
        (labels ((_w (fns result remainder)
                    (if (null fns)
                        (return-> (nreverse result) remainder)
                        (let ((rlt (funcall (car fns) remainder)))
                            (if (null rlt)
                                nil
                                (_w (cdr fns) (push (.< rlt) result) (>. rlt)))))))
            (_w (map 'list #'char-> aword) nil s))))

(defun trans-> (parser trans)
    (lambda (s)
        (let ((rlt (funcall parser s)))
            (if (null rlt)
                nil
                (return-> (funcall trans (.< rlt)) (>. rlt))))))

(defun and-> (lp rp)
    (lambda (s)
        (let ((lr (funcall lp s)))
            (if (null lr)
                nil
                (let ((rr (funcall rp (>. lr))))
                    (if (null rr)
                        nil
                        (return-> (list (.< lr) (.< rr)) (>. rr))))))))

(defun all-> (&rest parsers) (reduce #'and-> parsers))

(defun or-> (lp rp)
    (lambda (s)
        (let ((rlt (funcall lp s)))
            (if (null rlt)
                (funcall rp s)
                rlt))))

(defun any-> (&rest parsers) (reduce #'or-> parsers))

(defun 1ormany-> (parser)
    (lambda (s)
        (labels ((_m (fn result remainder)
                    (let ((rlt (funcall fn remainder)))
                        (if (null rlt)
                            (if (null result)
                                nil
                                (return-> (nreverse result) remainder))
                            (_m fn (push (.< rlt) result) (>. rlt))))))
            (_m parser nil s))))

(defun 0ormany-> (parser)
    (lambda (s)
        (labels ((_m (fn result remainder)
                    (let ((rlt (funcall fn remainder)))
                        (if (null rlt)
                            (return-> (nreverse result) remainder)
                            (_m fn (push (.< rlt) result) (>. rlt))))))
            (_m parser nil s))))

(defun 0or1-> (parser)
    (lambda (s)
        (let ((rlt (funcall parser s)))
            (if (null rlt)
                (return-> nil s)
                rlt))))

(defun dr-> (lp rp)
    "discard the right parser's result"
    (lambda (s)
        (let ((lr (funcall lp s)))
            (if (null lr)
                nil
                (let ((rr (funcall rp (>. lr))))
                    (if (null rr)
                        nil
                        (return-> (.< lr) (>. rr))))))))

(defun dl-> (lp rp)
    "discard the left parser's result"
    (lambda (s)
        (let ((lr (funcall lp s)))
            (if (null lr)
                nil
                (funcall rp (>. lr))))))

(defun between-> (lp mp rp)
    (dr-> (dl-> lp mp) rp))

(defun list-> (parser separator)
    (trans->
        (and-> parser (0ormany-> (dl-> separator parser)))
        (lambda (rlt) (and (.< rlt) (cons (.< rlt) (cadr rlt))))))

(defun flatten (ls &key (keep-nil t))
    (mapcan #'(lambda (x)
                (if (atom x)
                    (if (and (not keep-nil) (null x))
                        nil
                        (list x))
                    (flatten x :keep-nil keep-nil)))
        ls))

(defvar _json nil)

(defvar =json (lambda (s) (funcall _json s)))

(defvar =null (trans-> (word-> "null") (lambda (_) :null)))

(defvar =true (trans-> (word-> "true") (lambda (_) :true)))

(defvar =false (trans-> (word-> "false") (lambda (_) :false)))

(defvar =digit (apply #'any-> (map 'list #'char-> "0123456789")))

(defvar =integer (and-> (0or1-> (char-> #\-)) (1ormany-> =digit)))

(defvar =number
    (trans->
        (all->
            (0or1-> (char-> #\-))
            (1ormany-> =digit)
            (0or1-> (and-> (char-> #\.) (1ormany-> =digit)))
            (0or1->
                (all-> (or-> (char-> #\e) (char-> #\E))
                    (0or1-> (or-> (char-> #\+) (char-> #\-)))
                    (1ormany-> =digit))))
        (lambda (x)
            (read-from-string (map 'string #'identity (flatten x :keep-nil nil))))))

(defvar =whitespace (any-> (char-> #\Space) (char-> #\Tab) (char-> #\Return) (char-> #\Newline)))

(defvar =spaces (0ormany-> =whitespace))

(defvar =unescaped (if-> (lambda (x) (not (or (char= x #\\) (char= x #\"))))))

(defvar =escaped
    (apply #'any-> 
        (mapcar
            (lambda (p) (trans-> (word-> (car p)) (lambda (_) (cdr p))))
            (list (cons "\\\"" #\") (cons "\\\\" #\\)
                (cons "\\/" #\/) (cons "\\b" #\backspace)
                (cons "\\f" #\formfeed) (cons "\\n" #\newline)
                (cons "\\r" #\return) (cons "\\t" #\tab)))))

(defvar =hex (apply #'any-> (map 'list #'char-> "abcdefABCDEF")))

(defvar =hexdigit (or-> =digit =hex))

(defvar =unicode
    (trans->
        (dl-> 
            (char-> #\\)
            (dl->
                (or-> (char-> #\u) (char-> #\U))
                (all-> =hexdigit =hexdigit =hexdigit =hexdigit)))
        (lambda (x)
            (code-char (parse-integer (map 'string #'identity (flatten x)) :radix 16)))))

(defvar =string
    (between-> 
        (char-> #\") 
        (trans->
            (0ormany-> (any-> =unescaped =escaped =unicode))
            (lambda (cl)
                (map 'string #'identity (flatten cl)))) 
        (char-> #\")))

(defvar =object
    (trans->
        (between->
            (dl-> =spaces (dr-> (char-> #\{) =spaces))
            (0or1-> 
                (list->
                (and->
                    (dr-> (dr-> =string =spaces) (dr-> (char-> #\:) =spaces))
                    (dr-> =json =spaces))
                (dr-> (char-> #\,) =spaces)))
            (dr-> (char-> #\}) =spaces))
        (lambda (ol) (mapcar (lambda (p) (cons (car p) (cadr p))) ol))))
        ;; (lambda (ol)
        ;;     (let ((ht (make-hash-table :test #'equal)))
        ;;         (mapc (lambda (x) (setf (gethash (car x) ht) (cadr x))) ol)
        ;;         ht))))

(defvar =array
    (trans->
        (between->
            (dr-> (char-> #\[) =spaces)
            (0or1-> (list-> (dr-> =json =spaces) (dr-> (char-> #\,) =spaces)))
            (dr-> (char-> #\]) =spaces))
        (lambda (al) (map 'vector #'identity al))))

(setf _json (any-> =null =true =false =number =string =object =array))



;; test

(defvar json (funcall =json "{\"basic\":{\"cmCode\":\"40010000008\",\"channelId\":\"1234\",\"protocolId\":\"40\",\"deviceName\":\"\",\"modelId\":\"lwm2mtest\",\"orgId\":1},\"relation\":{\"1234\":{\"cmCode\":\"40010000008\",\"metricName\":null,\"deviceTypeEname\":\"collection\",\"deviceInfo\":null,\"state\":null},\"/3/0/0\":{\"cmCode\":\"40010000008\",\"metricName\":\"o0r0\",\"deviceTypeEname\":\"dev1\",\"deviceInfo\":null,\"state\":null},\"/3/0/9\":{\"cmCode\":\"40010000008\",\"metricName\":\"o0r9\",\"deviceTypeEname\":\"dev1\",\"deviceInfo\":null,\"state\":null},\"/3303/1/5601\":{\"cmCode\":\"40010000008\",\"metricName\":false,\"deviceTypeEname\":true,\"deviceInfo\":null,\"state\":null},\"/3303/1/5700\":{\"cmCode\":\"40010000008\",\"metricName\":\"o1r5700\",\"deviceTypeEname\":\"dev2\",\"deviceInfo\":null,\"state\":null}},\"extend\":{\"lwm2mDataType\":\"{\\\"/3/0/0\\\":\\\"STRING\\\",\\\"/3/0/9\\\":\\\"INTEGER\\\",\\\"/3303/1/5601\\\":\\\"FLOAT\\\",\\\"/3303/1/5700\\\":\\\"FLOAT\\\"}\"},\"tags\":null,\"cmCodeChannelMap\":{}}"))


;; (defvar json (funcall =json "{ \"cmCode\":\"我\", \"channelId\":\"10.10.20.31:9933\"}"))

;; (defvar json 
;;     (funcall 
;;         =string
;;         ;; (char-> #\\)
;;         "\"\\u263aa☺b\""
        
;;         ))

(print json)


;; (print (funcall (1ormany-> =whitespace) " a"))


;; (print (funcall (char-> #\A) "Abc"))

;; (print (funcall (word-> "null") "nulla"))

;; (print (funcall =null "nullo"))

;; (print (funcall =true "truea"))

;; (print (funcall =false "false!"))

;; (print (funcall (and-> (char-> #\A) (char-> #\B)) "ABc"))

;; (print (funcall (or-> (char-> #\A) (char-> #\B)) "Bdc"))

;; (print (funcall (1ormany-> =digit) "123a"))

;; (print (funcall =number "-12.3e4a"))

;; (print (funcall =string "\"123.-=qwe\""))

;; (print (funcall (list-> (char-> #\a) (char-> #\,)) "a,a,a,a,x"))

;; (print (funcall (between-> (char-> #\A) (char-> #\B) (char-> #\A)) "ABAc"))

;; (print (funcall (dr-> (char-> #\[) (char-> #\a)) "[abc"))

;; (print (funcall (dl-> (char-> #\[) (char-> #\a)) "[abc"))

;; (print (funcall =array "[1,2,3]"))

;; (print (funcall =object "{\"a\":1,\"b\":2}"))

;; (maphash (lambda (k v) (format t "~%~A : ~A" k v)) (.< (funcall =object "{\"a\":1,\"b\":2}")))





