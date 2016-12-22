(def ds (into-array [:willie :barnabas :adam]))
(seq ds)

(aset ds 1 :quentin)
(seq ds)

(def ds [:willie :barnabas :adam])
(def ds1 (replace {:barnabas :quentin} ds))
