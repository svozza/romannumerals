(def numerals [["M" 1000] ["CM" 900] ["D" 500] ["CD" 400] ["C" 100] ["XC" 90]
               ["L" 50] ["XL" 40] ["X" 10] ["IX" 9] ["V" 5] ["IV" 4] ["I" 1]])

(defn convert [n]
  (cond
    (<= n 0) (throw (Exception. "Numbers of 0 and below not supported"))
    (>= n 5000) (throw (Exception. "Numbers of 5000 and over not supported"))
    :else (first (reduce (fn [acc pair]
                           (let [div (quot (second acc) (second pair))]
                             (if (= div 0)
                               acc
                               [(str (first acc) (apply str (repeat div (first pair))))
                                (- (second acc) (* div (second pair)))])))
                         ["" n]
                         numerals))))

(try
  (convert 5001)
  (catch Exception e (assert (= "Numbers of 5000 and over not supported" (.getMessage e)))))
(assert (= (convert 4999) "MMMMCMXCIX"))
(assert (= (convert 4000) "MMMM"))
(assert (= (convert 2444) "MMCDXLIV"))
(assert (= (convert 1999) "MCMXCIX"))
(assert (= (convert 1981) "MCMLXXXI"))
(assert (= (convert 1259) "MCCLIX"))
(assert (= (convert 969) "CMLXIX"))
(assert (= (convert 499) "CDXCIX"))
(assert (= (convert 379) "CCCLXXIX"))
(assert (= (convert 130) "CXXX"))
(assert (= (convert 99) "XCIX"))
(assert (= (convert 87) "LXXXVII"))
(assert (= (convert 49) "XLIX"))
(assert (= (convert 34) "XXXIV"))
(assert (= (convert 19) "XIX"))
(assert (= (convert 14) "XIV"))
(assert (= (convert 13) "XIII"))
(assert (= (convert 9) "IX"))
(assert (= (convert 8) "VIII"))
(assert (= (convert 5) "V"))
(assert (= (convert 4) "IV"))
(assert (= (convert 3) "III"))
(assert (= (convert 2) "II"))
(assert (= (convert 1) "I"))
(try
  (convert 0)
  (catch Exception e (assert (= "Numbers of 0 and below not supported" (.getMessage e)))))
(try
  (convert -1)
  (catch Exception e (assert (= "Numbers of 0 and below not supported" (.getMessage e)))))
