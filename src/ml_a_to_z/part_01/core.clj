(ns ml-a-to-z.part-01.core
  (:require [ml-a-to-z.file-utils :refer [read-xsv-file write-xsv-file handle-missing-data
                                          categorical-to-numerical feature-scale-cols]]))

(def dataset (-> (read-xsv-file {:path "resources/part-01-data-processing/Data.csv"})
                 (handle-missing-data [:Age :Salary])
                 (categorical-to-numerical :Purchased {"No" 0 "Yes" 1})))

(def feature-scaled-dataset
  (->> (feature-scale-cols dataset [:Age :Salary] "normalisation")
       (map (fn [row] (merge (dissoc row :Age_scaled :Salary_scaled) (zipmap [:Age :Salary] (vals (select-keys row [:Age_scaled :Salary_scaled]))))))))

(write-xsv-file feature-scaled-dataset {:path "resources/part-01-data-processing/Data_preprocessed.csv"})
