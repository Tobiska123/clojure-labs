
; https://stackoverflow.com/questions/3906831/how-do-i-generate-memoized-recursive-functions-in-clojure
(defn calculateTrapArea [f a b]
  (* (/ (+ (f a) (f b)) 2.0) (- b a))
  )



; lab2.1

(defn make-integrate-with-memoization [step]
  (memoize (fn integrate [f x]
             (if (> x 0)
               (+ (calculateTrapArea f (- x step) x) (integrate f (- x step))
                 )
               0
               )
             ))
  )

(defn make-integrate [step]
   (fn integrate [f x]
             (if (> x 0)
               (+ (calculateTrapArea f (- x step) x) (integrate f (- x step))
                 )
               0
               )
             )
  )

(defn make-integrate-right [f step]
  (let [recurIntegrate (fn [recurIntegrate f x]
                         (if (> x 0)
                           (+ (calculateTrapArea f (- x step) x) (recurIntegrate recurIntegrate f (- x step)))
                           0)
                         )]
    (partial recurIntegrate recurIntegrate f)
    )
  )

; lab2.2

(defn lazyAreaCalc [f step]
  (let [calcArea (fn [x] calculateTrapArea (f x) (+ x step)),
        steps (iterate (fn [x] (+ x step)) 0)
        ]
      (reductions + 0 (map calcArea steps))
    )
  )

(defn make-integrate-with-lazy [step]
  (fn [f x] (nth
              (lazyAreaCalc f step)
              (quot x step)
              )
    )
  )







(def step-integrate (make-integrate 1))
(def step-integrate-mem (make-integrate-with-memoization 1))
(def step-integrate-lazy (make-integrate-with-lazy 1))

(prn "Standard")

(prn (time (step-integrate (fn [x] x) 500)))
(prn (time (step-integrate (fn [x] x) 501)))
(prn (time (step-integrate (fn [x] x) 502)))
(prn (time (step-integrate (fn [x] x) 503)))

(prn "Mem")

(prn (time (step-integrate-mem (fn [x] x) 500)))
(prn (time (step-integrate-mem (fn [x] x) 501)))
(prn (time (step-integrate-mem (fn [x] x) 502)))
(prn (time (step-integrate-mem (fn [x] x) 503)))

(prn "Lazy")



(prn (time (step-integrate-lazy (fn [x] x) 500)))
(prn (time (step-integrate-lazy (fn [x] x) 501)))
(prn (time (step-integrate-lazy (fn [x] x) 502)))
(prn (time (step-integrate-lazy (fn [x] x) 503)))