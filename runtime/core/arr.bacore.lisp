;; enable name mangle
(in-ns core)


; (template-struct [generic-type+] struct-name [params?])
(def-template-struct [T] MemBuf
    [
        { :type-primitive usize } cap  ; ^{ :type-primitive 'usize }
        { :type-template T :addr ptr :generic [T] } ptr
    ]
)


;; (template-fn [generic-type+] struct-name [params?] ret [stmt*])
(def-template-fn [T] index-of
    [
        { :type-struct MemBuf :generic [T] } buf
        { :type-primitive usize } idx
    ]
    { :type-template T :generic [T] }

    (deref (+ (attr buf ptr) idx))
)

(def-template-fn [T] new-membuf
    [
        { :type-primitive usize } cap
    ]
    { :type-struct MemBuf :generic [T] }

    (template-struct [T]
        MemBuf cap (malloc T cap)
    )
)



;; RawArr
(def-template-struct [T] RawArr
    [
        { :type-struct MemBuf :generic [T] :addr ptr } buf
        { :type-primitive usize } len
    ]
)

(def-template-fn [T] index-of
    [
        { :type-struct RawArr :generic [T] :addr ptr } arr
        { :type-primitive usize } idx
    ]
    { :type-template T :generic [T] }

    (index-of (deref-attr arr buf) idx)
)

(def-template-fn [T] new-rawarr
    [
        { :type-primitive usize } len
    ]
    { :type-struct RawArr :generic [T] }

    (template-struct [T]
        RawArr
        (malloc T cap)
        len
    )
)

(def-template-fn [T] push
    [
        { :type-struct RawArr :generic [T] :addr ptr } arr
        { :type-template T :generic [T] } elem
    ]
    {}

    (
        (let [new-len (+ (attr arr len) 1)
              buf (attr arr buf)
              cap (deref-attr buf cap)]

            (progn
                (if (> new-len (deref-attr cap))
                    (update-attr arr [buf] (realloc buf cap (* cap 2)) )
                    ()
                )

                (update-index (attr arr buf) new-len elem)
            )
        )
    )
)
