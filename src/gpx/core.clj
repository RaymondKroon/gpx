(ns gpx.core
  (:require [clojure.xml :refer :all]
                                        ; [clojure.data.zip :refer :all]
            [clojure.algo.generic.math-functions :refer [pow sin cos asin sqrt round]]
            [incanter.core :refer [view]]
            [incanter.charts :refer [xy-plot line-chart add-categories]]
            [clj-time.format :as tf]
            [clj-time.core :as tc]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(def R 6371)

(defn rad [x]
  (* x (/ Math/PI 180)))

(defn meter [x] (* x 1000))

;; https://gist.github.com/frankvilhelmsen/1787462
(defn haversine [position destination]
  (let [square_half_chord
        (+ (pow (sin (/ (rad (- (destination :lat) (position :lat))) 2)) 2)
           (* (cos (rad (position :lat)))
              (cos (rad (destination :lat)))
              (pow (sin (/ (rad (- (destination :lon) (position :lon))) 2)) 2)))
        angular_distance (* (asin (sqrt square_half_chord)) 2)]
    (* angular_distance R)))

(defn parse-gpx [path]
  (parse (java.io.ByteArrayInputStream. (.getBytes (slurp path)))))

(defn tag? [tag m]
  (when (= tag (:tag m)) m))

(defn find-tag [coll tag]
  (some (partial tag? tag) coll))

(defn filter-tag [tag coll]
  (filter #(= tag (:tag %)) coll))

(defn transform-trkpt [point]
                                        ;(println p)
  (let [time        (-> point :content (find-tag :time) :content first tf/parse)
        elevation   (-> point :content (find-tag :ele) :content first Double/parseDouble)
        speed       (-> point :content (find-tag :speed) :content first Double/parseDouble)
        lat (Double/parseDouble (get-in point [:attrs :lat]))
        lon (Double/parseDouble (get-in point [:attrs :lon]))]
    {:time time
     :elevation elevation
     :speed (* 3.6 speed)
     :lat lat
     :lon lon}))

;; (defn get-data [xml]
;;   (mapcat :content (filter-tags (map :content xml) :trk)))

(defn get-points [file]
  (->> file
       parse-gpx
       :content
       (filter-tag :trk)
       (mapcat :content)
       (filter-tag :trkseg)
       (mapcat :content)
       (filter-tag :trkpt)
       (map transform-trkpt)
                                        ;(filter #(= (->> % :time tc/month) 2))
       (sort-by :time )
       ))

(defn calculate-time [points]
  "interval"
  (let [start (:time (first points))
        stop (:time (last points))]
                                        ;(println start stop)
    (tc/interval start stop)))

(defn speed-in-kph [distance used-time]
  "distance in km, timeused as interval."
  (let [elapsed-time (tc/in-millis used-time)]
    (if (= 0 elapsed-time) 0
      (/ distance (/ elapsed-time 3600000)))))

(defn point-distance [p1 p2]
  "distance between points using haversine for sphere distance and pythagoras for elevation"
  (let [sphere-distance (haversine p1 p2)
        altitude-change (/ (- (:elevation p1) (:elevation p2)) 1000)
        distance (sqrt (+ (pow sphere-distance 2) (pow altitude-change 2)))]
    distance))

(defn point-speed [p1 p2]
  (let [time (tc/interval (:time p1) (:time p2))]
    (speed-in-kph (point-distance p1 p2) time)))

(defn rround [n]
  (/ (round (* n 100)) 100.0))

(defn gpx-files [directory-path]
  (let [dir (clojure.java.io/file directory-path)
        files (file-seq dir)
        gpx-filter #(.endsWith (-> % .getName ) ".gpx" )]
    (filter gpx-filter files)))

(defn enrich-with-interval-data [points]
  (let [next-points (drop 1 points)]
    (map (fn [current next] (assoc current
                             :distance (point-distance current next)
                             :calculated-speed (point-speed current next)))
         points next-points)))


(defn calculate-statistics [points]
  (let [elapsed-time    (calculate-time points)
        elapsed-seconds (tc/in-seconds elapsed-time)
        elapsed-map     (tf/instant->map elapsed-time)
        distance        (reduce + (map :distance points))
        max-speed-logged (apply max (map :speed points))
        max-speed-calculated (apply max (map :calculated-speed points))
        average-speed   (speed-in-kph distance elapsed-time)
        ]
    {:min-time (->> points first :time)
     :max-time (->> points last :time)
     :point-count (count points)
     :distance distance
     :elapsed-time elapsed-map
     :max-speed-logged max-speed-logged
     :max-speed-calculated max-speed-calculated
     :average-speed average-speed})
  )

(defn print-statistics [statistics]
  (let [time (:elapsed-time statistics)]
    (println "Min time:           " (:min-time statistics))
    (println "Max time:           " (:max-time statistics))
    (println "Data points:        " (:point-count statistics))
    (println "Distance:           " (:distance statistics) "km")
    (println "Elapsed time:       " (str (:hours time)":"(:minutes time) ":" (:seconds time)) )
    (println "Max speed (logged): " (:max-speed-logged statistics) "km/h")
    (println "Max speed (calc.):  " (:max-speed-calculated statistics) "km/h")
    (println "Average speed:      " (:average-speed statistics) "km/h"))
  )

(defn enriched-points [directory]
  (let [files           (gpx-files directory)
        points          (sort-by :time (mapcat #(get-points %) files))
        enriched-points (enrich-with-interval-data points)]
    enriched-points))


(defn -main [& args]
  (when-let [path (first args)]
    (let [enriched-points (enriched-points path)
         ; avg-meas-dist   (/ (reduce + (map :distance enriched-points)) (count enriched-points))
          points-per-day  (partition-by #(->> % :time tc/day) enriched-points)
          formatter       (:hour-minute-second tf/formatters)
          ]
      (doseq [stats (map calculate-statistics points-per-day)]
        (print-statistics stats))
      )
    ))

(def test-points
  (let [points ( enriched-points "/home/raymond/Development/skidata")]
    points ))

(defn view-chart [chart]
  (view (chart) :width 1400 :height 600))

(defn speed-chart [points]
  ( let [time (map :time points)
         speed (map :speed points)
         speed-calc (map :calculated-speed points)]
    (view  (add-categories (line-chart time speed :legend true) time speed-calc))))

;(def slider (partition 100 1 test-points))
;(defn middle [coll] (nth coll (quot (count coll) 2)))
; (def high-speeds (filter #(< 70 (:calculated-speed ( middle %))) slider  ))
; (count high-speeds)
; (doseq [ps high-speeds] (speed-chart ps))
