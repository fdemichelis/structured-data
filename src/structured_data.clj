(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a1 a2 a3] v]
    (+ a1 a3)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
    [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
        (and (contains-point? outer p1)
             (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book) ))


(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors (conj (book :authors) new-author)))

(defn alive? [author]
  (not (contains? author :death-year )))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [x] (first (rest x))) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
     (or (apply <= a-seq)
         (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains?  (set (map :name (book :authors))) (author :name) ))

(defn authors [books]
  (apply clojure.set/union (map :authors books )))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (str (author :name) (if (author :birth-year)
                       (str " (" (author :birth-year) " - " (if (author :death-year) (author :death-year) "") ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))





;; (def china {:name "China Miéville", :birth-year 1972})
;; (def octavia {:name "Octavia E. Butler"
;;               :birth-year 1947
;;               :death-year 2006})
;; (def friedman {:name "Daniel Friedman" :birth-year 1944})
;; (def felleisen {:name "Matthias Felleisen"})

;; (def cities {:title "The City and the City" :authors #{china}})
;; (def wild-seed {:title "Wild Seed", :authors #{octavia}})
;; (def embassytown {:title "Embassytown", :authors #{china}})
;; (def little-schemer {:title "The Little Schemer"
;;                      :authors #{friedman, felleisen}})

;; (def books [cities, wild-seed, embassytown, little-schemer])




(defn book->string [book]
  (str (book :title) ", written by " (authors->string (book :authors))))


(defn books->string [books]
  (let [c (count books)]
    (if (= c 0) "No books."
      (if (= c 1) (str "1 book. "  (book->string (first books)) ".")
        (str c " books. " (apply str (interpose ". " (map book->string books))) ".")))))

(defn books-by-author [author books]
   (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= (a :name) name)) authors)))

(defn living-authors [authors]
  (filter (fn [a] (not (a :death-year))) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors)))))

(defn books-by-living-authors [books]
  (filter (fn [b] (has-a-living-author? b)) books))


; %________%
