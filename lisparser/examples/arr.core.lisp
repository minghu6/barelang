;; enable name mangle



; (template-struct [generic-type+] struct-name [params?])
(template-struct [T] MemBuf
    [
        ^usize cap
        ^{ :type 'ptr :generic [T] } ptr
    ]
)


; (template-fn [generic-type+] struct-name [params?] ret [stmt*])
(template-fn
    :generic [T]
    :name index-of
    :params [
        (param buf (type 'MemBuf :generic [T]))
        (param idx (type 'usize))
    ]
    :ret T
    :body [
        (deref (+ (attr buf ptr) idx))
    ]
)

(template-struct [T] Array
    [
        (param len (type 'usize))
        (param buf (ptr (type 'MemBuf :generic [T])))
    ]
)

(template-fn [T] index-of
    :params [
        (param arr (type 'Array :generic [T]))
        (param idx (type 'usize))
    ]
    :ret T
    :body [
        (index-of (point-to arr buf) idx)
    ]
)


(template-fn [T] point-to
    [
        (param S ())
    ]
)