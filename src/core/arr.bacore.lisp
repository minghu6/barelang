;; enable name mangle



; (template-struct [generic-type+] struct-name [params?])
(template-struct [T] MemBuf
    [
        (param cap (type 'usize))
        (param ptr (type 'ptr :generic [T]))
    ]
)


; (template-fn [generic-type+] struct-name [params?] ret [stmt*])
(template-fn [T] indexof
    [
        (param buf (type 'MemBuf :generic [T]))
        (param idx (type 'usize))
    ]
    T
    [
        (deref (+ (attr buf ptr) idx))
    ]
)

(template-struct [T] Array
    [
        (param len (type 'usize))
        (param buf (ptr (type 'MemBuf :generic [T])))
    ]
)

(template-fn [T] indexof
    [
        (param arr (type 'Array :generic [T]))
        (param idx (type 'usize))
    ]
    T
    [
        (indexof (point-to arr buf) idx)
    ]
)
