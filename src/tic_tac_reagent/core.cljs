(ns ^:figwheel-always tic-tac-reagent.core
    (:require [reagent.core :as reagent :refer [atom]]))

(def initial-game {:grid (vec (repeat 9 nil))
                   :turn \X
                   :status :playing
                   :winning-cells #{}})

(defn cell-index [x y]
  (+ (* y 3) x))

(defn get-cell [grid x y]
  (grid (cell-index x y)))

(defn set-cell [grid x y val]
  (assoc grid (cell-index x y) val))

(def lines [[0 1 2] [3 4 5] [6 7 8]
            [0 3 6] [1 4 7] [2 5 8]
            [0 4 8] [2 4 6]])

(defn find-win [game]
  (let [player (:turn game)
        grid   (:grid game)]
    ((comp first filter)
      (fn [l]
        (every? #{player} (map grid l)))
      lines)))

(defn draw? [game]
  (= 9 (count (filter identity (:grid game)))))

(defn evaluate-game [game]
  (if-let [winning-line (find-win game)]
    (assoc game :status :win :winning-cells (set winning-line))
    (if (draw? game)
      (assoc game :status :draw)
      (update game :turn #(if (= % \X) \O \X)))))

(defn make-move [game x y]
  (if (and (= :playing (:status game))
           (nil? (get-cell (:grid game) x y)))
    (-> game
        (update-in [:grid] set-cell x y (:turn game))
        (evaluate-game))
    game))

;; State

(def game (atom initial-game))

;; Components

(defn grid-cell [grid x y winning]
  [:td {:class (when winning "winning")
        :on-click #(swap! game make-move x y)}
   (get-cell grid x y)])

(defn grid-view []
  [:table
   (let [g (:grid @game)
         winning-cells (:winning-cells @game)]
     (for [y (range 3)]
       ^{:key y} [:tr
                  (for [x (range 3)
                        :let [winning-cell (get winning-cells (cell-index x y))]]
                    ^{:key x} [grid-cell g x y winning-cell])]))])

(defn status-line []
  [:p.status (case (:status @game)
               :playing (str (:turn @game) "'s turn.")
               :win (str (:turn @game) " won!")
               :draw "It's a draw!")])

(defn reset-button []
  [:div.reset
   [:input {:type :button
            :value "Start Over"
            :on-click #(reset! game initial-game)}]])

(defn app []
  [:div.app
   [:h1 "Tic-Tac-Reagent"]
   [reset-button]
   [grid-view]
   [status-line]])

(reagent/render-component [app]
                          (. js/document (getElementById "app")))
