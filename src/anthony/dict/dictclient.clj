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

(declare push-command parse-text-block)

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
        {:definition-info (get-response-data (.readLine dict-input)) :definition (str/join "\r\n" (parse-text-block dict-input))}
        {:response response-code :reason (get-response-data line)}))))

(defn client [conn-info client-info] (push-command conn-info "CLIENT" client-info))

(defn auth [conn-info user auth-str] (push-command conn-info "AUTH" user auth-str))

(defn match [conn-info db strat word] (push-command conn-info "MATCH" db strat word))

(defn show-dictionaries [conn-info] (push-command conn-info "SHOW DB" nil))

(defn show-strategies [conn-info] (push-command conn-info "SHOW STRAT" nil))

(defn show-info [conn-info database] (push-command conn-info "SHOW INFO" database))

(defn help [conn-info] (push-command conn-info "HELP" nil))

(defn show-server [conn-info] (push-command conn-info "SHOW SERVER" nil))

(defn status [conn-info] (push-command conn-info "STATUS" nil))


(defn parse-text-block [dict-input]
  (loop [line (.readLine dict-input)
         lines []]
    (if (.startsWith line ".")
      lines
      (recur (.readLine dict-input) (conj lines line)))))

(defn push-command [conn-info command & args]
  (let [dict-sock (:sock conn-info)
        dict-input (:reader conn-info)
        dict-output (:writer conn-info)]
    (write-line dict-output (str/join " " (cons command args)))
    (let [line (.readLine dict-input)
          response-code (Integer/parseInt (get-response-code line))
          text-responses [114 113 112 111 110 152]
          join-responses [114 113 112 152]]
      (if (some #{response-code} text-responses)
        (if (some #{response-code} join-responses)
          (str/join "\r\n" (parse-text-block dict-input))
          (parse-text-block dict-input))
        {:response response-code :reason (get-response-data line)}))))

(let [conn-shit (connect "dict.org" 2628)]
  (println (show-server conn-shit)))