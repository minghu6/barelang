;; enable name mangle
(in-ns core)


; (template-struct [generic-type+] struct-name [params?])
(def-template-struct [T] MemBuf
    [
        { :type-primitive usize } cap  ; ^{ :type-primitive 'usize }
        { :type-template T :addr ptr :generic [T] } ptr
    ]
)


; (template-fn [generic-type+] struct-name [params?] ret [stmt*])
(def-template-fn [T] index-of
    [
     { :type-struct MemBuf :generic [T] } buf
     { :type-primitive usize } idx
    ]
    { :type-template T :generic [T] }

    (deref (+ (attr buf ptr) idx))
)

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
;;         (index-of (deref-attr arr buf) idx)
;;     ]
;; )


;; (template-fn [T] deref-attr
;;     [
;;         (param S ())
;;     ]
;; )
