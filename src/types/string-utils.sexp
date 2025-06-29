;; ============================================================================
;; STRING UTILITY FUNCTIONS IN PURE HOTT
;; ============================================================================
;; Implements string-prefix?, string-suffix? and other utilities

(import types types)
(import types string)

;; Check if s1 is a prefix of s2
(type string-prefix? (-> String String Bool))
(define string-prefix?
  (fn (prefix str)
    (String-elim prefix
      ;; empty string is prefix of any string
      true
      ;; prefix = cons(ch1, rest1)
      (fn (ch1 rest1 rec1)
        (String-elim str
          ;; non-empty prefix of empty string
          false
          ;; both non-empty: check first char and recurse
          (fn (ch2 rest2 rec2)
            (Bool-and (char-equal? ch1 ch2)
                     (string-prefix? rest1 rest2))))))))

;; Check if s1 is a suffix of s2
(type string-suffix? (-> String String Bool))
(define string-suffix?
  (fn (suffix str)
    ;; Simple approach: reverse both and check prefix
    (string-prefix? (string-reverse suffix) (string-reverse str))))

;; Reverse a string
(type string-reverse (-> String String))
(define string-reverse
  (fn (str)
    (string-reverse-acc str empty-string)))

;; Reverse with accumulator (tail recursive)
(type string-reverse-acc (-> String String String))
(define string-reverse-acc
  (fn (str acc)
    (String-elim str
      acc
      (fn (ch rest rec)
        (string-reverse-acc rest (string-cons ch acc))))))

;; String contains substring (simple version)
(type string-contains? (-> String String Bool))
(define string-contains?
  (fn (needle haystack)
    (String-elim haystack
      ;; Empty haystack can only contain empty needle
      (string-empty? needle)
      ;; Non-empty haystack
      (fn (ch rest rec)
        (Bool-or (string-prefix? needle haystack)
                (string-contains? needle rest))))))

;; Join list of strings with separator
(type string-intercalate (-> String (List String) String))
(define string-intercalate
  (fn (sep strings)
    (List-elim strings
      empty-string
      (fn (head tail rec)
        (List-elim tail
          head  ;; single element, no separator
          (fn (h2 t2 r2)
            (string-append head 
              (string-append sep 
                (string-intercalate sep tail)))))))))

;; Concat list of strings (no separator)
(type string-concat (-> (List String) String))
(define string-concat
  (fn (strings)
    (List-elim strings
      empty-string
      (fn (head tail rec)
        (string-append head rec)))))

;; Convert single char to string
(type char-to-string (-> Char String))
(define char-to-string
  (fn (ch)
    (string-cons ch empty-string)))

;; Boolean operations (if not available)
(type Bool-and (-> Bool Bool Bool))
(define Bool-and
  (fn (b1 b2)
    (Bool-elim b1
      b2    ;; true and b2 = b2
      false))) ;; false and b2 = false

(type Bool-or (-> Bool Bool Bool))
(define Bool-or
  (fn (b1 b2)
    (Bool-elim b1
      true   ;; true or b2 = true
      b2)))  ;; false or b2 = b2