; Igra connect4

(import '(javax.swing JFrame JLabel JTextField JButton JPanel JOptionPane)
        '(java.awt.event ActionListener)
        '(java.awt GridLayout Dimension BorderLayout Color Font))

(def p1 \O)
(def p2 \X)
(def slobodno \-)
(def dubina 6)

(defn drugi-igrac
  "Vraća drugog igrača."
  [p]
  (if (= p1 p) p2 p1))

(def Ploca (atom '{1 {1 \-, 2 \-, 3 \-, 4 \-, 5 \-, 6 \-},
            2 {1 \-, 2 \-, 3 \-, 4 \-, 5 \-, 6 \-},
            3 {1 \-, 2 \-, 3 \-, 4 \-, 5 \-, 6 \-},
            4 {1 \X, 2 \-, 3 \-, 4 \-, 5 \-, 6 \-},
            5 {1 \-, 2 \-, 3 \-, 4 \-, 5 \-, 6 \-},
            6 {1 \-, 2 \-, 3 \-, 4 \-, 5 \-, 6 \-},
            7 {1 \-, 2 \-, 3 \-, 4 \-, 5 \-, 6 \-}}))

(defn polje
  "Dohvat vrijednosti na polju - ako polje ne postoji, vraća se -1."
  [ploca x y]
  (if (or (< x 1) (< y 1) (> x 7) (> y 6))
    -1
    (get (get ploca x) y)))

(defn prvo-slobodno
  "Nalazi prvo slobodno mjesto u n-tom stupcu, ili vraća -1."
  [ploca n]
  (loop [y 1]
    (if (= slobodno (get (get ploca n) y))
      y
      (if (< y 6) (recur (inc y)) -1))))

(defn vrh-stupca
  "Vraća poziciju najvišeg poteza u n-tom stupcu ili -1 ako je stupac prazan."
  [ploca n]
  (let [ps (prvo-slobodno ploca n)]
    (if (= ps -1)
      6
      (if (= ps 1)
        -1
        (- ps 1)))))

(defn odigraj
  "Odigravanje jednog poteza - ako je potez nemoguć, vraća se nepromijenjena ploča."
  [ploca igrac n]
  (if (= slobodno (get (get ploca n) 6))
    (assoc
      ploca n
      (assoc
        (get ploca n)
        (prvo-slobodno ploca n)
        igrac))
    ploca))

(defn provjeri4
  "Provjera 4 polja radi pobjede počevši od zadanog i iterirajući pomoću xiter i yiter funkcija."
  [ploca x y igrac xiter yiter]
  (loop [xst x yst y flds 4]
    (if (= flds 0)
      true
      (if (= igrac (polje ploca xst yst))
        (recur (xiter xst) (yiter yst) (dec flds))
        false))))

(defn provjeriSvaki4
  "Provjera pobjede na svaka 4 polja počevši od zadanog i iterirajući pomoću xiter i yiter funkcija."
  [ploca x y igrac xiter yiter]
  (loop [xst x yst y lps 4]
    (if (= lps 0)
      false
      (if (provjeri4 ploca xst yst igrac xiter yiter)
        true
        (recur (xiter xst) (yiter yst) (dec lps))))))

(defn id "Vraćanje primljene vrijednosi." [x] x)

(defn pobjeda
  "Provjera je li prošli potez doveo u stanje pobjede."
  [ploca n igrac]
  (let [y (vrh-stupca ploca n)]
    (or
      (provjeriSvaki4 ploca (- n 3) (+ y 3) igrac inc dec)
      (provjeriSvaki4 ploca (- n 3) (- y 3) igrac inc inc)
      (provjeriSvaki4 ploca (- n 3) y igrac inc id)
      (provjeriSvaki4 ploca n (+ y 3) igrac id dec))))

;(defn flip 
;  "Flip argumenata funkcije."
;  [f] (fn [& args] (apply f (reverse args))))

(defn drugi-minmax
  "Mijenjanje minimax funkcije."
  [minmax] (= minmax max) min max)

(defn moguc-potez
  "Provjera je li potez moguc, tj. je li stupac slobodan."
  [ploca n]
  (= \- (get (get ploca n) 6)))

;(defn kvaliteta-poteza
;"Određivanje kvalitete poteza do određene dubine. Algoritam min-max."
;[igrac dub ploca minmax n]
;(if (or (moguc-potez ploca n) (= dub 0))
;  0
;  (let [n-ploca (odigraj ploca igrac n)]
;    (if (pobjeda n-ploca n igrac)
;      10
;      (reduce minmax
;        (map (partial kvaliteta-poteza
;          (drugi-igrac igrac)
;          (dec dub)
;          n-ploca
;          (drugi-minmax minmax))
;          [1 2 3 4 5 6 7])
;        )))))

(defn kvaliteta-poteza
  "Određivanje kvalitete poteza do određene dubine."
  [igrac bod dub ploca minmax n]
  (if (not (moguc-potez ploca n))
  [-10 0]
  (if (or (not (moguc-potez ploca n)) (= dub 0))
  [0 0]
  (let [n-ploca (odigraj ploca igrac n)]
    (if (pobjeda n-ploca n igrac)
      [bod bod]
      (let [rez (map (partial kvaliteta-poteza
            (drugi-igrac igrac)
            (- bod)
            (dec dub)
            n-ploca
            (drugi-minmax minmax))
            [1 2 3 4 5 6 7])]
        (do
          (let [tip (map (fn [r] (nth r 1)) rez)]
            (let [st-tip
                (if (contains? tip 1)
                  1
                  (if (contains? tip -1)
                    -1
                    0))]
              [(+ (/ (reduce (fn [acc r] (+ acc (nth r 0))) 0 rez) dub) st-tip) st-tip])))))))))

(defn potez-za-pobjedu
  "Provjera postoji li potez koji vodi u pobjedu. Vraća se pobjednički potez ili 0 ako takav ne postoji."
  [ploca igrac]
  (let [pobjede (map (fn [n] (pobjeda (odigraj ploca igrac n) n igrac)) [1 2 3 4 5 6 7])]
    (inc (.indexOf pobjede true))))

(defn potez-protiv-poraza
  "Provjera postoji li potez koji se spriječava poraz. Vraća se potez ili 0 ako takav ne postoji."
  [ploca igrac]
  (let [pobjede (map (fn [n] (pobjeda (odigraj ploca (drugi-igrac igrac) n) n (drugi-igrac igrac))) [1 2 3 4 5 6 7])]
    (inc (.indexOf pobjede true))))

(defn najbolji-potez
  "Pronalazi najbolji potez na ploči do određene dubine. Vraća broj stupca."
  [ploca igrac dub]
  (let [pobj (potez-za-pobjedu ploca igrac)]
    (if (not (= 0 pobj))
      pobj
      (let [porz (potez-protiv-poraza ploca igrac)]
        (if (not (= 0 porz))
          porz
          (let [pot (max-key
              (fn [k] (first (kvaliteta-poteza igrac 1 dub ploca min k)))
              1 2 3 4 5 6 7)]
            (if (moguc-potez ploca pot)
              pot
              (loop [potez 1]
                (if (moguc-potez ploca potez)
                  potez
                  (if (= potez 7)
                    7
                    (recur (inc potez))))))))))))

;; GUI

(defn build-fig
  [x y]
  (let [lbl (JLabel. (.concat "  " (Character/toString (get (get @Ploca y) x))))]
    (do 
      (.setHorizontalTextPosition lbl (JLabel/CENTER))
      (.setVerticalTextPosition lbl (JLabel/CENTER))
      (.setFont lbl (Font. "Serif" Font/PLAIN 40))
      lbl)))

(defn add-lbl-pane
  [panel]
  (do
    (.removeAll panel)
    (loop [x 6]
      (do
        (loop [y 1]
          (do
            (.add panel (build-fig x y))
            (if-not (= y 7) (recur (inc y)))))
        (if-not (= x 1) (recur (dec x)))))
    (.revalidate panel)
    (javax.swing.SwingUtilities/invokeLater #(.repaint panel))))

(def panel-places (ref (JPanel.)))

(defn objava-kraja
  [p]
  (do
    (JOptionPane/showMessageDialog nil,
      (.concat "Pobjedio je:  " (Character/toString p)),
      "Kraj!", JOptionPane/INFORMATION_MESSAGE)
    (System/exit 0)))

(defn ubaci
  [n]
  (do
    (swap! Ploca (fn [p] (odigraj p p1 n)))
    (add-lbl-pane @panel-places)
    (if (pobjeda @Ploca n p1)
      (objava-kraja p1)
      (let [ komp-n (najbolji-potez @Ploca p2 dubina)]
        (do
          (swap! Ploca 
            (fn [p] (odigraj p p2 komp-n)))
          (add-lbl-pane @panel-places)
          (if (pobjeda @Ploca komp-n p2)
            (objava-kraja p2)
            nil))))
  ))

(defn dodaj-akciju
  [btn n]
  (.addActionListener btn
    (proxy [ActionListener] []
      (actionPerformed [evt]
         (ubaci n)))))

(defn gui []
  (let [frame (JFrame. "Connect4")
        panel-btns (JPanel.)
        stup1-btn (JButton. "Ubaci")
        stup2-btn (JButton. "Ubaci")
        stup3-btn (JButton. "Ubaci")
        stup4-btn (JButton. "Ubaci")
        stup5-btn (JButton. "Ubaci")
        stup6-btn (JButton. "Ubaci")
        stup7-btn (JButton. "Ubaci")]
    (dodaj-akciju stup1-btn 1)
    (dodaj-akciju stup2-btn 2)
    (dodaj-akciju stup3-btn 3)
    (dodaj-akciju stup4-btn 4)
    (dodaj-akciju stup5-btn 5)
    (dodaj-akciju stup6-btn 6)
    (dodaj-akciju stup7-btn 7)
    (doto panel-btns
      (.setLayout (GridLayout. 1 7 2 2))
      (.setPreferredSize (Dimension. 50 50))
      (.add stup1-btn)
      (.add stup2-btn)
      (.add stup3-btn)
      (.add stup4-btn)
      (.add stup5-btn)
      (.add stup6-btn)
      (.add stup7-btn))
    (do
      (.setLayout @panel-places (GridLayout. 6 7 2 2))
      (add-lbl-pane @panel-places))
    (doto frame
      (.setLayout (BorderLayout.))
      (.add panel-btns (BorderLayout/NORTH))
      (.add @panel-places (BorderLayout/CENTER))
      (.setSize 600 600)
      (.setVisible true)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE))))

(gui)

