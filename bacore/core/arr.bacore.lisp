;; enable name mangle
(in-ns core)


; (template-struct [generic-type+] struct-name [params?])
(template-struct [T] MemBuf
    [
        ^usize cap
        ^{ :type T :addr 'ptr :generic T } ptr
    ]
)


;; ; (template-fn [generic-type+] struct-name [params?] ret [stmt*])
;; (template-fn
;;     :generic [T]
;;     :name index-of
;;     :params [
;;         ^{ :type 'MemBuf :generic [T] } buf
;;         ^usize idx
;;     ]
;;     :ret ^T
;;     :body [
;;         (deref (+ (attr buf ptr) idx))
;;     ]
;; )

;; (template-struct [T] Array
;;     [
;;         ^usize len
;;         ^{ :type 'MemBuf :generic [T] :addr 'ptr } buf
;;     ]
;; )

;; (template-fn [T] index-of
;;     :params [
;;         (param arr (type 'Array :generic [T]))
;;         (param idx (type 'usize))
;;     ]
;;     :ret T
;;     :body [
;;         (index-of (point-to arr buf) idx)
;;     ]
;; )


;; (template-fn [T] point-to
;;     [
;;         (param S ())
;;     ]
;; )
