(ns ml-a-to-z.file-utils
  (:require [clojure.string :refer [join split split-lines]]))

(defn guess-numbers
  "Converts number strings to numbers from a file"
  [data]
  (map
    (fn [row]
      (map
       (fn [col-val]
         (try
           (if (number? (read-string col-val))
             (read-string col-val)
             col-val)
           (catch Exception e col-val)))
       row))
    data))

(defn read-xsv-file
  "Reads the file from provided path into a collection of maps"
  [{:keys [path separator] :or {separator ","} :as params}]
  (let [file (->> (split-lines (slurp path))
                  (map #(split % (re-pattern separator)))
                  (remove nil?))
        file-data (guess-numbers (rest file))
        file-headers (map keyword (first file))]
    (map (fn [row] (zipmap file-headers row)) file-data)))

(defn write-xsv-file
  "Writes the xsv file to a location with custom separator"
  [data {:keys [path separator] :or {separator ","} :as params}]
  (let [headers (join separator (map name (keys (first data))))]
    (spit path (join "\n" (cons headers (map (fn [row] (join separator (vals row))) data))))))

(defn categorical-to-numerical
  "Converts the categorical values from a specific column in a file to numerical values"
  [data col-name mapped-values]
  (map
    (fn [row]
      (assoc row
        col-name (mapped-values (row col-name))))
    data))

(defn handle-missing-data
  "Handles the numerical missing data in the file by replacing it with the
   mean of all the values in that particular column"
  [data col-names]
  (let [mean-col-values (reduce conj
                          (map
                            (fn [col-name]
                              (let [vals (remove (fn [x] (or (= x "") (= x "NaN"))) (map col-name data))]
                                (assoc {} col-name (float (/ (reduce + vals) (count vals))))))
                            col-names))]
    (map
      (fn [row]
        (into {}
          (map
            (fn [[k v]]
              [k (if (or (nil? v)
                         (= v "NaN")
                         (= v ""))
                   (mean-col-values k)
                   v)]) row)))
      data)))

(defn random-sample-by-percentage
  "Takes a random sample from a file by percentage.
   Used for splitting dataset into train/test"
  [data [train-percentage test-percentage]]
  (let [shuffled-data (shuffle data)]
    (split-at (Math/floor (* (count shuffled-data) (/ train-percentage 100))) shuffled-data)))

(defn coll-mean
  "Computes the mean value of a collection"
  [data]
  (double (/ (reduce + data) (count data))))

(defn standard-deviation
  "Computes good old standard deviation of a collection"
  [data data-mean]
  (Math/sqrt (double (* (/ 1 (count data)) (reduce + (map (fn [n] (Math/pow (- n data-mean) 2)) data))))))

(defn standardise-collection
  "Standardises a collection. Values lie between -1 an 1."
  [data]
  (let [mean-data (coll-mean data)
        std-dev-data (standard-deviation data mean-data)]
    (map (fn [n] (double (/ (- n mean-data) std-dev-data))) data)))

(defn normalise-collection
  "Normalises a collection. Values lie between 0 an 1."
  [data]
  (let [min-val (apply min data)
        max-val (apply max data)
        max-min-diff (- max-val min-val)]
    (map (fn [n] (double (/ (- n min-val) max-min-diff))) data)))

(defn feature-scale-cols
  "Feature scales columns of a dataset by either using standarisation or normalisation.
   Adds new columns with '_scaled' as the suffix to the scaled columns."
  [data col-names scale-type?]
  (let [scaled-cols (map
                      (fn [col-name]
                        (let [vals (map (fn [row] (row col-name)) data)]
                          (condp = scale-type?
                            "standardisation" (standardise-collection vals)
                            "normalisation" (normalise-collection vals))))
                      col-names)]
    (reduce (fn [x y] (map merge x y))
      (map
        (fn [col-name scaled-vals]
          (map
            (fn [row scaled-val] (assoc row (keyword (str (name col-name) "_scaled")) scaled-val))
            data scaled-vals))
        col-names scaled-cols))))
