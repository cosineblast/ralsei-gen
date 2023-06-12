(ns gen.core
  (:import [javax.imageio ImageIO]
           [java.awt.image BufferedImage]
           [java.io File FileOutputStream])
  (:require [clojure.core.typed :as t])
  (:gen-class))

(def tile-size 8)
(def tile-count 16)

(def color-name->pixel
  {:grey 0xffc3c3c3
   :black 0xff000000
   :light-green 0xff68ff10
   :dark-green 0xff18a686
   :white 0xffffffff
   :pink 0xfff9459b})

(def color-name->nes-color
  {:grey 0x00
   :black 0x0f
   :light-green 0x2A
   :dark-green 0x1A
   :white 0x30
   :pink 0x25})

(def color-name->palette-index
  {:grey 0
   :black 1
   :light-green 2
   :dark-green 3
   :white 2
   :pink 3})

(def pixel->color-name
  (into {} (map (fn [[x y]] [y x]) color-name->pixel)))

(defn image->seq
  "Takes a BufferedImage img and returns a sequence with
  16*16 tiles. A tile is a vectors of 8*8 integer pixels"
  [^BufferedImage img]

  (assert (= 128 (.getHeight img)))
  (assert (= 128 (.getWidth img)))

  (for [tile-y (range tile-count)
        tile-x (range tile-count)]
    (mapv #(bit-and % 0xffffffff)
          (.getRGB img
                   (* tile-size tile-x) ;; startX
                   (* tile-size tile-y) ;; startY
                   tile-size ;; width
                   tile-size ;; height
                   nil ;; initial array
                   0 ;; array offset
                   tile-size)))) ;; line size
(defn is-valid-tile
  "Returns true if the given tile is valid (correct-sized and known pixel integers)"
  [tile]

  (and
   (= (* 8 8) (count tile))
   (every? (comp some? pixel->color-name) tile)))

(defn assign-tile-numbers
  "Assigns a numeric id to every distinct tile in the given sequence of tiles.
   returns a map where the keys are tiles and the values their numeric IDs."
  [tiles]


  (reduce (fn [current tile]
            (update current tile #(or % (count current))))
          {} tiles))



(defn nessify-tile
  "Takes a tile and returns a nes-tile, an 8*8
   vector containing the nes-equivalent of original tile colors"
  [tile]

  (mapv (comp color-name->nes-color pixel->color-name) tile))

(defn bits->byte
  "[Int] -> Int, takes an 8 element {0,1} sequence and returns a byte of their elements"
  [bits]
  (assert (= 8 (count bits)))
  (assert (every? #{0 1} bits))

  (first
   (reduce (fn [[result power] x]
             [(if (= x 1) (+ result power) result) (* 2 power)])
           [0 1] (reverse bits))))

(defn tile->palette-pattern
  "Tile -> ([Byte * 8], [Byte * 8])

  Takes a tile and returns a pair [low, high]
  where low and high are 8 byte seqs with the low and high bits
  of palette-table index of the given tile."
  [tile]

  (assert (is-valid-tile tile))

  (let [pixel->palette-index (comp color-name->palette-index pixel->color-name)
        first-bit #(bit-shift-right (bit-and % 0x2) 1)
        second-bit #(bit-and % 0x1)
        full-pattern (map pixel->palette-index tile)
        high-bits (map first-bit full-pattern)
        low-bits (map second-bit full-pattern)]

    [(map bits->byte (partition 8 low-bits))
     (map bits->byte (partition 8 high-bits))]))

(defn partition-2d
  "Receives a vector that represents a side-size X side-size matrix and
   returns a lazy-seq representing a side-size/2 X side-size/2 matrix where
   every element in the new matrix is a block with 4 elements of the old matrix,
   The block of new_matrix[i, j] contains the adjacent elements
  (itself, right, below, and diagonal below) of old_matrix[i * 2, j * 2]."
  [side-size elements]

  (assert (vector? elements))
  (assert (even? side-size))

  (let [at (fn [i j] (elements (+ (* i side-size) j)))]
    (for [block-row (range (/ side-size 2))
          block-col (range (/ side-size 2))]
      (let [i (* block-row 2)
            j (* block-col 2)]
        [(at i j) (at i (inc j))
         (at (inc i) j) (at (inc i) (inc j))]))))

(defn find-block-palette
  "Determines what is the palette of the given tile block and returns 0 if
   it is palette A, and 1 if it is palette B"
  [block]
  (let [type-b-pixels (into #{} (map color-name->pixel [:white :pink]))
        has-type-b-pixel (fn [tile] (some? (some type-b-pixels tile)))]
    (if (some has-type-b-pixel block)
      1
      0)))

(defn palette-superblock->attribute-byte [palette-superblock]
    (apply bit-or (map bit-shift-left palette-superblock [0 2 4 6])))

(defn write-bytes
  "Takes a filename and seq of byte-ranged longs and writes their content to a
   file with the given name."
  [file-name values]
  (with-open [stream (->> file-name File. FileOutputStream.)]
      (doseq [value values]
        (.write stream (int value)))))

(defn -main
  []
  (let [img (->> "../ralsei.bmp" File. ImageIO/read)
        tiles (into [] (image->seq img))

        _ (assert (every? is-valid-tile tiles))

        tile-numbers (assign-tile-numbers tiles)
        nametable (map tile-numbers tiles)

        pattern-table
        (->> tile-numbers
             (sort-by second)
             (map first)
             (map tile->palette-pattern))

        attribute-table
        (->> tiles
             (partition-2d tile-count)
             (map find-block-palette)
             (into [])
             (partition-2d (/ tile-count 2))
             (map palette-superblock->attribute-byte))]

    (write-bytes "ralsei-nametable.bin" nametable)
    (write-bytes "ralsei-pattern-table.bin"  (flatten pattern-table))
    (write-bytes "ralsei-attribute-table.bin" attribute-table)))
