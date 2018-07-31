(ns adventofcode17.aoc04
  (use adventofcode17.file)
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.string :as str :only reverse]))

(prn "Part One")

(comment
  (def input
    ["pphsv ojtou brvhsj cer ntfhlra udeh ccgtyzc zoyzmh jum lugbnk"
     "aa bb cc dd aa"
     "vxjnf fzqitnj uyfck blnl impo kxoow nngd worcm bdesehw"
     "caibh nfuk kfnu llfdbz uxjty yxjut jcea"
     "qiho qif eupwww avyglnj nxzotsu hio lws"
     "xjty usocjsh pivk qnknunc yjcgh bwya djw zpyr"
     "ycfmfe mgq sjiomg nfzjul bjwkmgu yvsnvgj dcjupu wzz blmn"
     "rdowgbt vpwfdoi blzl laghnk gsa vhnpo cztxzlb rtz hvwonhb eciju pfjtbo"
     "bqs bqs dbutvgf mmzb izpyud rap izpyud xlzeb mnj hjncs"]))

(defn all-distinct? [passphrase]
  (apply distinct? (words passphrase)))

(defn count-valid [passphrases]
  (->> passphrases
       (filter all-distinct?)
       (count)))

(comment
  (prn
    (count-valid
      (lines-of "./aoc04.txt"))))

;---------------------------
(prn "Part Two")
;For added security, yet another system policy has been put in place.
;Now, a valid passphrase must contain no two words that are anagrams of each other - that is, a passphrase is invalid if any word's letters can be rearranged to form any other word in the passphrase.
;
;For example:
;
;abcde fghij is a valid passphrase.
;abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
;a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
;iiii oiii ooii oooi oooo is valid.
;oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.
;
;Under this new system policy, how many passphrases are valid?

(def input2
  ["abcde fghij"
   "abcde xyz ecdab"
   "a ab abc abd abf abj"
   "iiii oiii ooii oooi oooo"
   "oiii ioii iioi iiio"])

(defn anagram? [a b]
  (= (frequencies a) (frequencies b)))

(defn not-any-pair? [pred coll]
  (let [pairs (combo/combinations coll 2)]
    (not-any? #(apply pred %) pairs)))

(defn valid? [passphrase]
  (let
    [passwords (words passphrase)]
    (not-any-pair? anagram? passwords)))

(defn count-valid-2 [passphrases]
  (->> passphrases
       (filter (comp valid?))
       (count)))

(prn
  (count-valid-2
    input2))
(prn
  (count-valid-2
    (lines-of "./aoc04.txt")))
