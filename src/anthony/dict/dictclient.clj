(ns anthony.dict.dictclient
  (import (java.net Socket)
    (java.io OutputStreamWriter BufferedWriter InputStreamReader BufferedReader))
  (:require [clojure.string :as str]))

(defn write-line [writer line]
  (doto writer
    (.write line)
    (.newLine)
    (.flush)))

(defn get-response-code [line]
  (get (str/split line #"\s") 0))

(defn get-response-data [line]
  (get (str/split line #"\s" 2) 1))

(declare parse-show-response)

(defn connect [host port]
  (let [dict-sock (Socket. host port)
        dict-input (BufferedReader. (InputStreamReader. (.getInputStream dict-sock)))
        dict-output (BufferedWriter. (OutputStreamWriter. (.getOutputStream dict-sock)))
        line (.readLine dict-input)
        response-code (Integer/parseInt (get-response-code line))]
    (if (== response-code 220) ; connection success!
      {:sock dict-sock :reader dict-input :writer dict-output :response response-code}
      (do
        (.close dict-sock)
        {:response response-code :reason (get-response-data line)}))))

(defn define [conn-info db word]
  (let [dict-sock (:sock conn-info)
        dict-input (:reader conn-info)
        dict-output (:writer conn-info)]
    (write-line dict-output (str "DEFINE " db " " word))
    (let [line (.readLine dict-input)
          response-code (Integer/parseInt (get-response-code line))]
      (if (== response-code 150) ; got definition
        (let [definition-info (.readLine dict-input)
              response-data (get-response-data definition-info)]
          (loop [definition-line (.readLine dict-input)
                 definition-lines []]
            (if (.startsWith definition-line ".")
              {:definition-info response-data :definition (str/join "\r\n" definition-lines)}
              (recur (.readLine dict-input) (conj definition-lines definition-line)))))
        (do
          {:response response-code :reason (get-response-data line)})))))

(defn show-dictionaries [conn-info]
  (let [dict-sock (:sock conn-info)
        dict-input (:reader conn-info)
        dict-output (:writer conn-info)]
    (write-line dict-output "SHOW DB")
    (let [line (.readLine dict-input)
          response-code (Integer/parseInt (get-response-code line))]
      (if (== response-code 110)
        (parse-show-response dict-input)
        (do
          {:response response-code :reason (get-response-data line)})))))

(defn show-strategies [conn-info]
  (let [dict-sock (:sock conn-info)
        dict-input (:reader conn-info)
        dict-output (:writer conn-info)]
    (write-line dict-output "SHOW STRAT")
    (let [line (.readLine dict-input)
          response-code (Integer/parseInt (get-response-code line))]
      (if (== response-code 111)
        (parse-show-response dict-input)
        (do
          {:response response-code :reason (get-response-data line)})))))

(defn show-info [conn-info database]
  (let [dict-sock (:sock conn-info)
        dict-input (:reader conn-info)
        dict-output (:writer conn-info)]
    (write-line dict-output (str "SHOW INFO " database))
    (let [line (.readLine dict-input)
          response-code (Integer/parseInt (get-response-code line))]
      (if (== response-code 112)
        (str/join "\r\n" (parse-show-response dict-input))
        (do
          {:response response-code :reason (get-response-data line)})))))

(defn show-server [conn-info]
  (let [dict-sock (:sock conn-info)
        dict-input (:reader conn-info)
        dict-output (:writer conn-info)]
    (write-line dict-output "SHOW SERVER")
    (let [line (.readLine dict-input)
          response-code (Integer/parseInt (get-response-code line))]
      (if (== response-code 114)
        (str/join "\r\n" (parse-show-response dict-input))
        (do
          {:response response-code :reason (get-response-data line)})))))

(defn status [conn-info]
  (let [dict-sock (:sock conn-info)
        dict-input (:reader conn-info)
        dict-output (:writer conn-info)]
    (write-line dict-output "STATUS")
    (let [line (.readLine dict-input)
          response-code (Integer/parseInt (get-response-code line))]
      {:response response-code :reason (get-response-data line)})))

(defn parse-show-response [dict-input]
  (loop [line (.readLine dict-input)
         lines []]
    (if (.startsWith line ".")
      lines
      (recur (.readLine dict-input) (conj lines line)))))


(let [conn-shit (connect "dict.org" 2628)]
  (println (status conn-shit)))