(ns anthony.dict.test.core
  (:require [anthony.dict.dictclient :as dict]))


(let [conn-shit (dict/connect "dict.org" 2628)] ; testing etc
  (println (dict/define conn-shit "wn" "ice"))
  (println (dict/match conn-shit "*" "exact" "ice"))
  (println (dict/show-server conn-shit))
  (println (dict/show-dictionaries conn-shit))
  (println (dict/show-strategies conn-shit))
  (println (dict/show-info conn-shit "wn"))
  (println (dict/help conn-shit))
  (println (dict/status conn-shit))
  (println (dict/quit conn-shit)))
