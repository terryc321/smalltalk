(declare-const s_1_1 bool)
(declare-const s_1_2 bool)
(declare-const s_1_3 bool)
(declare-const s_1_4 bool)
(declare-const s_1_5 bool)
(declare-const s_1_6 bool)
(declare-const s_1_7 bool)
(declare-const s_1_8 bool)
(declare-const s_1_9 bool)
(declare-const s_1_10 bool)
(declare-const s_2_1 bool)
(declare-const s_2_2 bool)
(declare-const s_2_3 bool)
(declare-const s_2_4 bool)
(declare-const s_2_5 bool)
(declare-const s_2_6 bool)
(declare-const s_2_7 bool)
(declare-const s_2_8 bool)
(declare-const s_2_9 bool)
(declare-const s_2_10 bool)
(declare-const s_3_1 bool)
(declare-const s_3_2 bool)
(declare-const s_3_3 bool)
(declare-const s_3_4 bool)
(declare-const s_3_5 bool)
(declare-const s_3_6 bool)
(declare-const s_3_7 bool)
(declare-const s_3_8 bool)
(declare-const s_3_9 bool)
(declare-const s_3_10 bool)
(declare-const s_4_1 bool)
(declare-const s_4_2 bool)
(declare-const s_4_3 bool)
(declare-const s_4_4 bool)
(declare-const s_4_5 bool)
(declare-const s_4_6 bool)
(declare-const s_4_7 bool)
(declare-const s_4_8 bool)
(declare-const s_4_9 bool)
(declare-const s_4_10 bool)
(declare-const s_5_1 bool)
(declare-const s_5_2 bool)
(declare-const s_5_3 bool)
(declare-const s_5_4 bool)
(declare-const s_5_5 bool)
(declare-const s_5_6 bool)
(declare-const s_5_7 bool)
(declare-const s_5_8 bool)
(declare-const s_5_9 bool)
(declare-const s_5_10 bool)
(declare-const s_6_1 bool)
(declare-const s_6_2 bool)
(declare-const s_6_3 bool)
(declare-const s_6_4 bool)
(declare-const s_6_5 bool)
(declare-const s_6_6 bool)
(declare-const s_6_7 bool)
(declare-const s_6_8 bool)
(declare-const s_6_9 bool)
(declare-const s_6_10 bool)
(declare-const s_7_1 bool)
(declare-const s_7_2 bool)
(declare-const s_7_3 bool)
(declare-const s_7_4 bool)
(declare-const s_7_5 bool)
(declare-const s_7_6 bool)
(declare-const s_7_7 bool)
(declare-const s_7_8 bool)
(declare-const s_7_9 bool)
(declare-const s_7_10 bool)
(declare-const s_8_1 bool)
(declare-const s_8_2 bool)
(declare-const s_8_3 bool)
(declare-const s_8_4 bool)
(declare-const s_8_5 bool)
(declare-const s_8_6 bool)
(declare-const s_8_7 bool)
(declare-const s_8_8 bool)
(declare-const s_8_9 bool)
(declare-const s_8_10 bool)
(declare-const s_9_1 bool)
(declare-const s_9_2 bool)
(declare-const s_9_3 bool)
(declare-const s_9_4 bool)
(declare-const s_9_5 bool)
(declare-const s_9_6 bool)
(declare-const s_9_7 bool)
(declare-const s_9_8 bool)
(declare-const s_9_9 bool)
(declare-const s_9_10 bool)
(declare-const s_10_1 bool)
(declare-const s_10_2 bool)
(declare-const s_10_3 bool)
(declare-const s_10_4 bool)
(declare-const s_10_5 bool)
(declare-const s_10_6 bool)
(declare-const s_10_7 bool)
(declare-const s_10_8 bool)
(declare-const s_10_9 bool)
(declare-const s_10_10 bool)

;;neighbours of s_1_1 => ((2 1) (1 2)) 
;; corner s_1_1
(assert (xor s_2_1 s_1_2))


;;neighbours of s_1_2 => ((2 2) (1 1) (1 3)) 
;; edge s_1_2
(assert (xor (and s_2_2 (not s_1_1) (not s_1_3))
             (and s_1_1 (not s_2_2) (not s_1_3))
             (and s_1_3 (not s_2_2) (not s_1_1))
             (and s_2_2 s_1_1 s_1_3)))

;;neighbours of s_1_3 => ((2 3) (1 2) (1 4)) 
;; edge s_1_3
(assert (xor (and s_2_3 (not s_1_2) (not s_1_4))
             (and s_1_2 (not s_2_3) (not s_1_4))
             (and s_1_4 (not s_2_3) (not s_1_2))
             (and s_2_3 s_1_2 s_1_4)))

;;neighbours of s_1_4 => ((2 4) (1 3) (1 5)) 
;; edge s_1_4
(assert (xor (and s_2_4 (not s_1_3) (not s_1_5))
             (and s_1_3 (not s_2_4) (not s_1_5))
             (and s_1_5 (not s_2_4) (not s_1_3))
             (and s_2_4 s_1_3 s_1_5)))

;;neighbours of s_1_5 => ((2 5) (1 4) (1 6)) 
;; edge s_1_5
(assert (xor (and s_2_5 (not s_1_4) (not s_1_6))
             (and s_1_4 (not s_2_5) (not s_1_6))
             (and s_1_6 (not s_2_5) (not s_1_4))
             (and s_2_5 s_1_4 s_1_6)))

;;neighbours of s_1_6 => ((2 6) (1 5) (1 7)) 
;; edge s_1_6
(assert (xor (and s_2_6 (not s_1_5) (not s_1_7))
             (and s_1_5 (not s_2_6) (not s_1_7))
             (and s_1_7 (not s_2_6) (not s_1_5))
             (and s_2_6 s_1_5 s_1_7)))

;;neighbours of s_1_7 => ((2 7) (1 6) (1 8)) 
;; edge s_1_7
(assert (xor (and s_2_7 (not s_1_6) (not s_1_8))
             (and s_1_6 (not s_2_7) (not s_1_8))
             (and s_1_8 (not s_2_7) (not s_1_6))
             (and s_2_7 s_1_6 s_1_8)))

;;neighbours of s_1_8 => ((2 8) (1 7) (1 9)) 
;; edge s_1_8
(assert (xor (and s_2_8 (not s_1_7) (not s_1_9))
             (and s_1_7 (not s_2_8) (not s_1_9))
             (and s_1_9 (not s_2_8) (not s_1_7))
             (and s_2_8 s_1_7 s_1_9)))

;;neighbours of s_1_9 => ((2 9) (1 8) (1 10)) 
;; edge s_1_9
(assert (xor (and s_2_9 (not s_1_8) (not s_1_10))
             (and s_1_8 (not s_2_9) (not s_1_10))
             (and s_1_10 (not s_2_9) (not s_1_8))
             (and s_2_9 s_1_8 s_1_10)))

;;neighbours of s_1_10 => ((2 10) (1 9)) 
;; corner s_1_10
(assert (xor s_2_10 s_1_9))


;;neighbours of s_2_1 => ((1 1) (3 1) (2 2)) 
;; edge s_2_1
(assert (xor (and s_1_1 (not s_3_1) (not s_2_2))
             (and s_3_1 (not s_1_1) (not s_2_2))
             (and s_2_2 (not s_1_1) (not s_3_1))
             (and s_1_1 s_3_1 s_2_2)))

;;neighbours of s_2_2 => ((1 2) (3 2) (2 1) (2 3)) 
;; internal s_2_2
(assert (xor (and s_1_2 (not s_3_2) (not s_2_1) (not s_2_3))
             (and s_3_2 (not s_1_2) (not s_2_1) (not s_2_3))
             (and s_2_1 (not s_1_2) (not s_3_2) (not s_2_3))
             (and s_2_3 (not s_1_2) (not s_3_2) (not s_2_1))
             (and (not s_1_2) s_3_2 s_2_1 s_2_3)
             (and (not s_3_2) s_1_2 s_2_1 s_2_3)
             (and (not s_2_1) s_1_2 s_3_2 s_2_3)
             (and (not s_2_3) s_1_2 s_3_2 s_2_1)))

;;neighbours of s_2_3 => ((1 3) (3 3) (2 2) (2 4)) 
;; internal s_2_3
(assert (xor (and s_1_3 (not s_3_3) (not s_2_2) (not s_2_4))
             (and s_3_3 (not s_1_3) (not s_2_2) (not s_2_4))
             (and s_2_2 (not s_1_3) (not s_3_3) (not s_2_4))
             (and s_2_4 (not s_1_3) (not s_3_3) (not s_2_2))
             (and (not s_1_3) s_3_3 s_2_2 s_2_4)
             (and (not s_3_3) s_1_3 s_2_2 s_2_4)
             (and (not s_2_2) s_1_3 s_3_3 s_2_4)
             (and (not s_2_4) s_1_3 s_3_3 s_2_2)))

;;neighbours of s_2_4 => ((1 4) (3 4) (2 3) (2 5)) 
;; internal s_2_4
(assert (xor (and s_1_4 (not s_3_4) (not s_2_3) (not s_2_5))
             (and s_3_4 (not s_1_4) (not s_2_3) (not s_2_5))
             (and s_2_3 (not s_1_4) (not s_3_4) (not s_2_5))
             (and s_2_5 (not s_1_4) (not s_3_4) (not s_2_3))
             (and (not s_1_4) s_3_4 s_2_3 s_2_5)
             (and (not s_3_4) s_1_4 s_2_3 s_2_5)
             (and (not s_2_3) s_1_4 s_3_4 s_2_5)
             (and (not s_2_5) s_1_4 s_3_4 s_2_3)))

;;neighbours of s_2_5 => ((1 5) (3 5) (2 4) (2 6)) 
;; internal s_2_5
(assert (xor (and s_1_5 (not s_3_5) (not s_2_4) (not s_2_6))
             (and s_3_5 (not s_1_5) (not s_2_4) (not s_2_6))
             (and s_2_4 (not s_1_5) (not s_3_5) (not s_2_6))
             (and s_2_6 (not s_1_5) (not s_3_5) (not s_2_4))
             (and (not s_1_5) s_3_5 s_2_4 s_2_6)
             (and (not s_3_5) s_1_5 s_2_4 s_2_6)
             (and (not s_2_4) s_1_5 s_3_5 s_2_6)
             (and (not s_2_6) s_1_5 s_3_5 s_2_4)))

;;neighbours of s_2_6 => ((1 6) (3 6) (2 5) (2 7)) 
;; internal s_2_6
(assert (xor (and s_1_6 (not s_3_6) (not s_2_5) (not s_2_7))
             (and s_3_6 (not s_1_6) (not s_2_5) (not s_2_7))
             (and s_2_5 (not s_1_6) (not s_3_6) (not s_2_7))
             (and s_2_7 (not s_1_6) (not s_3_6) (not s_2_5))
             (and (not s_1_6) s_3_6 s_2_5 s_2_7)
             (and (not s_3_6) s_1_6 s_2_5 s_2_7)
             (and (not s_2_5) s_1_6 s_3_6 s_2_7)
             (and (not s_2_7) s_1_6 s_3_6 s_2_5)))

;;neighbours of s_2_7 => ((1 7) (3 7) (2 6) (2 8)) 
;; internal s_2_7
(assert (xor (and s_1_7 (not s_3_7) (not s_2_6) (not s_2_8))
             (and s_3_7 (not s_1_7) (not s_2_6) (not s_2_8))
             (and s_2_6 (not s_1_7) (not s_3_7) (not s_2_8))
             (and s_2_8 (not s_1_7) (not s_3_7) (not s_2_6))
             (and (not s_1_7) s_3_7 s_2_6 s_2_8)
             (and (not s_3_7) s_1_7 s_2_6 s_2_8)
             (and (not s_2_6) s_1_7 s_3_7 s_2_8)
             (and (not s_2_8) s_1_7 s_3_7 s_2_6)))

;;neighbours of s_2_8 => ((1 8) (3 8) (2 7) (2 9)) 
;; internal s_2_8
(assert (xor (and s_1_8 (not s_3_8) (not s_2_7) (not s_2_9))
             (and s_3_8 (not s_1_8) (not s_2_7) (not s_2_9))
             (and s_2_7 (not s_1_8) (not s_3_8) (not s_2_9))
             (and s_2_9 (not s_1_8) (not s_3_8) (not s_2_7))
             (and (not s_1_8) s_3_8 s_2_7 s_2_9)
             (and (not s_3_8) s_1_8 s_2_7 s_2_9)
             (and (not s_2_7) s_1_8 s_3_8 s_2_9)
             (and (not s_2_9) s_1_8 s_3_8 s_2_7)))

;;neighbours of s_2_9 => ((1 9) (3 9) (2 8) (2 10)) 
;; internal s_2_9
(assert (xor (and s_1_9 (not s_3_9) (not s_2_8) (not s_2_10))
             (and s_3_9 (not s_1_9) (not s_2_8) (not s_2_10))
             (and s_2_8 (not s_1_9) (not s_3_9) (not s_2_10))
             (and s_2_10 (not s_1_9) (not s_3_9) (not s_2_8))
             (and (not s_1_9) s_3_9 s_2_8 s_2_10)
             (and (not s_3_9) s_1_9 s_2_8 s_2_10)
             (and (not s_2_8) s_1_9 s_3_9 s_2_10)
             (and (not s_2_10) s_1_9 s_3_9 s_2_8)))

;;neighbours of s_2_10 => ((1 10) (3 10) (2 9)) 
;; edge s_2_10
(assert (xor (and s_1_10 (not s_3_10) (not s_2_9))
             (and s_3_10 (not s_1_10) (not s_2_9))
             (and s_2_9 (not s_1_10) (not s_3_10))
             (and s_1_10 s_3_10 s_2_9)))

;;neighbours of s_3_1 => ((2 1) (4 1) (3 2)) 
;; edge s_3_1
(assert (xor (and s_2_1 (not s_4_1) (not s_3_2))
             (and s_4_1 (not s_2_1) (not s_3_2))
             (and s_3_2 (not s_2_1) (not s_4_1))
             (and s_2_1 s_4_1 s_3_2)))

;;neighbours of s_3_2 => ((2 2) (4 2) (3 1) (3 3)) 
;; internal s_3_2
(assert (xor (and s_2_2 (not s_4_2) (not s_3_1) (not s_3_3))
             (and s_4_2 (not s_2_2) (not s_3_1) (not s_3_3))
             (and s_3_1 (not s_2_2) (not s_4_2) (not s_3_3))
             (and s_3_3 (not s_2_2) (not s_4_2) (not s_3_1))
             (and (not s_2_2) s_4_2 s_3_1 s_3_3)
             (and (not s_4_2) s_2_2 s_3_1 s_3_3)
             (and (not s_3_1) s_2_2 s_4_2 s_3_3)
             (and (not s_3_3) s_2_2 s_4_2 s_3_1)))

;;neighbours of s_3_3 => ((2 3) (4 3) (3 2) (3 4)) 
;; internal s_3_3
(assert (xor (and s_2_3 (not s_4_3) (not s_3_2) (not s_3_4))
             (and s_4_3 (not s_2_3) (not s_3_2) (not s_3_4))
             (and s_3_2 (not s_2_3) (not s_4_3) (not s_3_4))
             (and s_3_4 (not s_2_3) (not s_4_3) (not s_3_2))
             (and (not s_2_3) s_4_3 s_3_2 s_3_4)
             (and (not s_4_3) s_2_3 s_3_2 s_3_4)
             (and (not s_3_2) s_2_3 s_4_3 s_3_4)
             (and (not s_3_4) s_2_3 s_4_3 s_3_2)))

;;neighbours of s_3_4 => ((2 4) (4 4) (3 3) (3 5)) 
;; internal s_3_4
(assert (xor (and s_2_4 (not s_4_4) (not s_3_3) (not s_3_5))
             (and s_4_4 (not s_2_4) (not s_3_3) (not s_3_5))
             (and s_3_3 (not s_2_4) (not s_4_4) (not s_3_5))
             (and s_3_5 (not s_2_4) (not s_4_4) (not s_3_3))
             (and (not s_2_4) s_4_4 s_3_3 s_3_5)
             (and (not s_4_4) s_2_4 s_3_3 s_3_5)
             (and (not s_3_3) s_2_4 s_4_4 s_3_5)
             (and (not s_3_5) s_2_4 s_4_4 s_3_3)))

;;neighbours of s_3_5 => ((2 5) (4 5) (3 4) (3 6)) 
;; internal s_3_5
(assert (xor (and s_2_5 (not s_4_5) (not s_3_4) (not s_3_6))
             (and s_4_5 (not s_2_5) (not s_3_4) (not s_3_6))
             (and s_3_4 (not s_2_5) (not s_4_5) (not s_3_6))
             (and s_3_6 (not s_2_5) (not s_4_5) (not s_3_4))
             (and (not s_2_5) s_4_5 s_3_4 s_3_6)
             (and (not s_4_5) s_2_5 s_3_4 s_3_6)
             (and (not s_3_4) s_2_5 s_4_5 s_3_6)
             (and (not s_3_6) s_2_5 s_4_5 s_3_4)))

;;neighbours of s_3_6 => ((2 6) (4 6) (3 5) (3 7)) 
;; internal s_3_6
(assert (xor (and s_2_6 (not s_4_6) (not s_3_5) (not s_3_7))
             (and s_4_6 (not s_2_6) (not s_3_5) (not s_3_7))
             (and s_3_5 (not s_2_6) (not s_4_6) (not s_3_7))
             (and s_3_7 (not s_2_6) (not s_4_6) (not s_3_5))
             (and (not s_2_6) s_4_6 s_3_5 s_3_7)
             (and (not s_4_6) s_2_6 s_3_5 s_3_7)
             (and (not s_3_5) s_2_6 s_4_6 s_3_7)
             (and (not s_3_7) s_2_6 s_4_6 s_3_5)))

;;neighbours of s_3_7 => ((2 7) (4 7) (3 6) (3 8)) 
;; internal s_3_7
(assert (xor (and s_2_7 (not s_4_7) (not s_3_6) (not s_3_8))
             (and s_4_7 (not s_2_7) (not s_3_6) (not s_3_8))
             (and s_3_6 (not s_2_7) (not s_4_7) (not s_3_8))
             (and s_3_8 (not s_2_7) (not s_4_7) (not s_3_6))
             (and (not s_2_7) s_4_7 s_3_6 s_3_8)
             (and (not s_4_7) s_2_7 s_3_6 s_3_8)
             (and (not s_3_6) s_2_7 s_4_7 s_3_8)
             (and (not s_3_8) s_2_7 s_4_7 s_3_6)))

;;neighbours of s_3_8 => ((2 8) (4 8) (3 7) (3 9)) 
;; internal s_3_8
(assert (xor (and s_2_8 (not s_4_8) (not s_3_7) (not s_3_9))
             (and s_4_8 (not s_2_8) (not s_3_7) (not s_3_9))
             (and s_3_7 (not s_2_8) (not s_4_8) (not s_3_9))
             (and s_3_9 (not s_2_8) (not s_4_8) (not s_3_7))
             (and (not s_2_8) s_4_8 s_3_7 s_3_9)
             (and (not s_4_8) s_2_8 s_3_7 s_3_9)
             (and (not s_3_7) s_2_8 s_4_8 s_3_9)
             (and (not s_3_9) s_2_8 s_4_8 s_3_7)))

;;neighbours of s_3_9 => ((2 9) (4 9) (3 8) (3 10)) 
;; internal s_3_9
(assert (xor (and s_2_9 (not s_4_9) (not s_3_8) (not s_3_10))
             (and s_4_9 (not s_2_9) (not s_3_8) (not s_3_10))
             (and s_3_8 (not s_2_9) (not s_4_9) (not s_3_10))
             (and s_3_10 (not s_2_9) (not s_4_9) (not s_3_8))
             (and (not s_2_9) s_4_9 s_3_8 s_3_10)
             (and (not s_4_9) s_2_9 s_3_8 s_3_10)
             (and (not s_3_8) s_2_9 s_4_9 s_3_10)
             (and (not s_3_10) s_2_9 s_4_9 s_3_8)))

;;neighbours of s_3_10 => ((2 10) (4 10) (3 9)) 
;; edge s_3_10
(assert (xor (and s_2_10 (not s_4_10) (not s_3_9))
             (and s_4_10 (not s_2_10) (not s_3_9))
             (and s_3_9 (not s_2_10) (not s_4_10))
             (and s_2_10 s_4_10 s_3_9)))

;;neighbours of s_4_1 => ((3 1) (5 1) (4 2)) 
;; edge s_4_1
(assert (xor (and s_3_1 (not s_5_1) (not s_4_2))
             (and s_5_1 (not s_3_1) (not s_4_2))
             (and s_4_2 (not s_3_1) (not s_5_1))
             (and s_3_1 s_5_1 s_4_2)))

;;neighbours of s_4_2 => ((3 2) (5 2) (4 1) (4 3)) 
;; internal s_4_2
(assert (xor (and s_3_2 (not s_5_2) (not s_4_1) (not s_4_3))
             (and s_5_2 (not s_3_2) (not s_4_1) (not s_4_3))
             (and s_4_1 (not s_3_2) (not s_5_2) (not s_4_3))
             (and s_4_3 (not s_3_2) (not s_5_2) (not s_4_1))
             (and (not s_3_2) s_5_2 s_4_1 s_4_3)
             (and (not s_5_2) s_3_2 s_4_1 s_4_3)
             (and (not s_4_1) s_3_2 s_5_2 s_4_3)
             (and (not s_4_3) s_3_2 s_5_2 s_4_1)))

;;neighbours of s_4_3 => ((3 3) (5 3) (4 2) (4 4)) 
;; internal s_4_3
(assert (xor (and s_3_3 (not s_5_3) (not s_4_2) (not s_4_4))
             (and s_5_3 (not s_3_3) (not s_4_2) (not s_4_4))
             (and s_4_2 (not s_3_3) (not s_5_3) (not s_4_4))
             (and s_4_4 (not s_3_3) (not s_5_3) (not s_4_2))
             (and (not s_3_3) s_5_3 s_4_2 s_4_4)
             (and (not s_5_3) s_3_3 s_4_2 s_4_4)
             (and (not s_4_2) s_3_3 s_5_3 s_4_4)
             (and (not s_4_4) s_3_3 s_5_3 s_4_2)))

;;neighbours of s_4_4 => ((3 4) (5 4) (4 3) (4 5)) 
;; internal s_4_4
(assert (xor (and s_3_4 (not s_5_4) (not s_4_3) (not s_4_5))
             (and s_5_4 (not s_3_4) (not s_4_3) (not s_4_5))
             (and s_4_3 (not s_3_4) (not s_5_4) (not s_4_5))
             (and s_4_5 (not s_3_4) (not s_5_4) (not s_4_3))
             (and (not s_3_4) s_5_4 s_4_3 s_4_5)
             (and (not s_5_4) s_3_4 s_4_3 s_4_5)
             (and (not s_4_3) s_3_4 s_5_4 s_4_5)
             (and (not s_4_5) s_3_4 s_5_4 s_4_3)))

;;neighbours of s_4_5 => ((3 5) (5 5) (4 4) (4 6)) 
;; internal s_4_5
(assert (xor (and s_3_5 (not s_5_5) (not s_4_4) (not s_4_6))
             (and s_5_5 (not s_3_5) (not s_4_4) (not s_4_6))
             (and s_4_4 (not s_3_5) (not s_5_5) (not s_4_6))
             (and s_4_6 (not s_3_5) (not s_5_5) (not s_4_4))
             (and (not s_3_5) s_5_5 s_4_4 s_4_6)
             (and (not s_5_5) s_3_5 s_4_4 s_4_6)
             (and (not s_4_4) s_3_5 s_5_5 s_4_6)
             (and (not s_4_6) s_3_5 s_5_5 s_4_4)))

;;neighbours of s_4_6 => ((3 6) (5 6) (4 5) (4 7)) 
;; internal s_4_6
(assert (xor (and s_3_6 (not s_5_6) (not s_4_5) (not s_4_7))
             (and s_5_6 (not s_3_6) (not s_4_5) (not s_4_7))
             (and s_4_5 (not s_3_6) (not s_5_6) (not s_4_7))
             (and s_4_7 (not s_3_6) (not s_5_6) (not s_4_5))
             (and (not s_3_6) s_5_6 s_4_5 s_4_7)
             (and (not s_5_6) s_3_6 s_4_5 s_4_7)
             (and (not s_4_5) s_3_6 s_5_6 s_4_7)
             (and (not s_4_7) s_3_6 s_5_6 s_4_5)))

;;neighbours of s_4_7 => ((3 7) (5 7) (4 6) (4 8)) 
;; internal s_4_7
(assert (xor (and s_3_7 (not s_5_7) (not s_4_6) (not s_4_8))
             (and s_5_7 (not s_3_7) (not s_4_6) (not s_4_8))
             (and s_4_6 (not s_3_7) (not s_5_7) (not s_4_8))
             (and s_4_8 (not s_3_7) (not s_5_7) (not s_4_6))
             (and (not s_3_7) s_5_7 s_4_6 s_4_8)
             (and (not s_5_7) s_3_7 s_4_6 s_4_8)
             (and (not s_4_6) s_3_7 s_5_7 s_4_8)
             (and (not s_4_8) s_3_7 s_5_7 s_4_6)))

;;neighbours of s_4_8 => ((3 8) (5 8) (4 7) (4 9)) 
;; internal s_4_8
(assert (xor (and s_3_8 (not s_5_8) (not s_4_7) (not s_4_9))
             (and s_5_8 (not s_3_8) (not s_4_7) (not s_4_9))
             (and s_4_7 (not s_3_8) (not s_5_8) (not s_4_9))
             (and s_4_9 (not s_3_8) (not s_5_8) (not s_4_7))
             (and (not s_3_8) s_5_8 s_4_7 s_4_9)
             (and (not s_5_8) s_3_8 s_4_7 s_4_9)
             (and (not s_4_7) s_3_8 s_5_8 s_4_9)
             (and (not s_4_9) s_3_8 s_5_8 s_4_7)))

;;neighbours of s_4_9 => ((3 9) (5 9) (4 8) (4 10)) 
;; internal s_4_9
(assert (xor (and s_3_9 (not s_5_9) (not s_4_8) (not s_4_10))
             (and s_5_9 (not s_3_9) (not s_4_8) (not s_4_10))
             (and s_4_8 (not s_3_9) (not s_5_9) (not s_4_10))
             (and s_4_10 (not s_3_9) (not s_5_9) (not s_4_8))
             (and (not s_3_9) s_5_9 s_4_8 s_4_10)
             (and (not s_5_9) s_3_9 s_4_8 s_4_10)
             (and (not s_4_8) s_3_9 s_5_9 s_4_10)
             (and (not s_4_10) s_3_9 s_5_9 s_4_8)))

;;neighbours of s_4_10 => ((3 10) (5 10) (4 9)) 
;; edge s_4_10
(assert (xor (and s_3_10 (not s_5_10) (not s_4_9))
             (and s_5_10 (not s_3_10) (not s_4_9))
             (and s_4_9 (not s_3_10) (not s_5_10))
             (and s_3_10 s_5_10 s_4_9)))

;;neighbours of s_5_1 => ((4 1) (6 1) (5 2)) 
;; edge s_5_1
(assert (xor (and s_4_1 (not s_6_1) (not s_5_2))
             (and s_6_1 (not s_4_1) (not s_5_2))
             (and s_5_2 (not s_4_1) (not s_6_1))
             (and s_4_1 s_6_1 s_5_2)))

;;neighbours of s_5_2 => ((4 2) (6 2) (5 1) (5 3)) 
;; internal s_5_2
(assert (xor (and s_4_2 (not s_6_2) (not s_5_1) (not s_5_3))
             (and s_6_2 (not s_4_2) (not s_5_1) (not s_5_3))
             (and s_5_1 (not s_4_2) (not s_6_2) (not s_5_3))
             (and s_5_3 (not s_4_2) (not s_6_2) (not s_5_1))
             (and (not s_4_2) s_6_2 s_5_1 s_5_3)
             (and (not s_6_2) s_4_2 s_5_1 s_5_3)
             (and (not s_5_1) s_4_2 s_6_2 s_5_3)
             (and (not s_5_3) s_4_2 s_6_2 s_5_1)))

;;neighbours of s_5_3 => ((4 3) (6 3) (5 2) (5 4)) 
;; internal s_5_3
(assert (xor (and s_4_3 (not s_6_3) (not s_5_2) (not s_5_4))
             (and s_6_3 (not s_4_3) (not s_5_2) (not s_5_4))
             (and s_5_2 (not s_4_3) (not s_6_3) (not s_5_4))
             (and s_5_4 (not s_4_3) (not s_6_3) (not s_5_2))
             (and (not s_4_3) s_6_3 s_5_2 s_5_4)
             (and (not s_6_3) s_4_3 s_5_2 s_5_4)
             (and (not s_5_2) s_4_3 s_6_3 s_5_4)
             (and (not s_5_4) s_4_3 s_6_3 s_5_2)))

;;neighbours of s_5_4 => ((4 4) (6 4) (5 3) (5 5)) 
;; internal s_5_4
(assert (xor (and s_4_4 (not s_6_4) (not s_5_3) (not s_5_5))
             (and s_6_4 (not s_4_4) (not s_5_3) (not s_5_5))
             (and s_5_3 (not s_4_4) (not s_6_4) (not s_5_5))
             (and s_5_5 (not s_4_4) (not s_6_4) (not s_5_3))
             (and (not s_4_4) s_6_4 s_5_3 s_5_5)
             (and (not s_6_4) s_4_4 s_5_3 s_5_5)
             (and (not s_5_3) s_4_4 s_6_4 s_5_5)
             (and (not s_5_5) s_4_4 s_6_4 s_5_3)))

;;neighbours of s_5_5 => ((4 5) (6 5) (5 4) (5 6)) 
;; internal s_5_5
(assert (xor (and s_4_5 (not s_6_5) (not s_5_4) (not s_5_6))
             (and s_6_5 (not s_4_5) (not s_5_4) (not s_5_6))
             (and s_5_4 (not s_4_5) (not s_6_5) (not s_5_6))
             (and s_5_6 (not s_4_5) (not s_6_5) (not s_5_4))
             (and (not s_4_5) s_6_5 s_5_4 s_5_6)
             (and (not s_6_5) s_4_5 s_5_4 s_5_6)
             (and (not s_5_4) s_4_5 s_6_5 s_5_6)
             (and (not s_5_6) s_4_5 s_6_5 s_5_4)))

;;neighbours of s_5_6 => ((4 6) (6 6) (5 5) (5 7)) 
;; internal s_5_6
(assert (xor (and s_4_6 (not s_6_6) (not s_5_5) (not s_5_7))
             (and s_6_6 (not s_4_6) (not s_5_5) (not s_5_7))
             (and s_5_5 (not s_4_6) (not s_6_6) (not s_5_7))
             (and s_5_7 (not s_4_6) (not s_6_6) (not s_5_5))
             (and (not s_4_6) s_6_6 s_5_5 s_5_7)
             (and (not s_6_6) s_4_6 s_5_5 s_5_7)
             (and (not s_5_5) s_4_6 s_6_6 s_5_7)
             (and (not s_5_7) s_4_6 s_6_6 s_5_5)))

;;neighbours of s_5_7 => ((4 7) (6 7) (5 6) (5 8)) 
;; internal s_5_7
(assert (xor (and s_4_7 (not s_6_7) (not s_5_6) (not s_5_8))
             (and s_6_7 (not s_4_7) (not s_5_6) (not s_5_8))
             (and s_5_6 (not s_4_7) (not s_6_7) (not s_5_8))
             (and s_5_8 (not s_4_7) (not s_6_7) (not s_5_6))
             (and (not s_4_7) s_6_7 s_5_6 s_5_8)
             (and (not s_6_7) s_4_7 s_5_6 s_5_8)
             (and (not s_5_6) s_4_7 s_6_7 s_5_8)
             (and (not s_5_8) s_4_7 s_6_7 s_5_6)))

;;neighbours of s_5_8 => ((4 8) (6 8) (5 7) (5 9)) 
;; internal s_5_8
(assert (xor (and s_4_8 (not s_6_8) (not s_5_7) (not s_5_9))
             (and s_6_8 (not s_4_8) (not s_5_7) (not s_5_9))
             (and s_5_7 (not s_4_8) (not s_6_8) (not s_5_9))
             (and s_5_9 (not s_4_8) (not s_6_8) (not s_5_7))
             (and (not s_4_8) s_6_8 s_5_7 s_5_9)
             (and (not s_6_8) s_4_8 s_5_7 s_5_9)
             (and (not s_5_7) s_4_8 s_6_8 s_5_9)
             (and (not s_5_9) s_4_8 s_6_8 s_5_7)))

;;neighbours of s_5_9 => ((4 9) (6 9) (5 8) (5 10)) 
;; internal s_5_9
(assert (xor (and s_4_9 (not s_6_9) (not s_5_8) (not s_5_10))
             (and s_6_9 (not s_4_9) (not s_5_8) (not s_5_10))
             (and s_5_8 (not s_4_9) (not s_6_9) (not s_5_10))
             (and s_5_10 (not s_4_9) (not s_6_9) (not s_5_8))
             (and (not s_4_9) s_6_9 s_5_8 s_5_10)
             (and (not s_6_9) s_4_9 s_5_8 s_5_10)
             (and (not s_5_8) s_4_9 s_6_9 s_5_10)
             (and (not s_5_10) s_4_9 s_6_9 s_5_8)))

;;neighbours of s_5_10 => ((4 10) (6 10) (5 9)) 
;; edge s_5_10
(assert (xor (and s_4_10 (not s_6_10) (not s_5_9))
             (and s_6_10 (not s_4_10) (not s_5_9))
             (and s_5_9 (not s_4_10) (not s_6_10))
             (and s_4_10 s_6_10 s_5_9)))

;;neighbours of s_6_1 => ((5 1) (7 1) (6 2)) 
;; edge s_6_1
(assert (xor (and s_5_1 (not s_7_1) (not s_6_2))
             (and s_7_1 (not s_5_1) (not s_6_2))
             (and s_6_2 (not s_5_1) (not s_7_1))
             (and s_5_1 s_7_1 s_6_2)))

;;neighbours of s_6_2 => ((5 2) (7 2) (6 1) (6 3)) 
;; internal s_6_2
(assert (xor (and s_5_2 (not s_7_2) (not s_6_1) (not s_6_3))
             (and s_7_2 (not s_5_2) (not s_6_1) (not s_6_3))
             (and s_6_1 (not s_5_2) (not s_7_2) (not s_6_3))
             (and s_6_3 (not s_5_2) (not s_7_2) (not s_6_1))
             (and (not s_5_2) s_7_2 s_6_1 s_6_3)
             (and (not s_7_2) s_5_2 s_6_1 s_6_3)
             (and (not s_6_1) s_5_2 s_7_2 s_6_3)
             (and (not s_6_3) s_5_2 s_7_2 s_6_1)))

;;neighbours of s_6_3 => ((5 3) (7 3) (6 2) (6 4)) 
;; internal s_6_3
(assert (xor (and s_5_3 (not s_7_3) (not s_6_2) (not s_6_4))
             (and s_7_3 (not s_5_3) (not s_6_2) (not s_6_4))
             (and s_6_2 (not s_5_3) (not s_7_3) (not s_6_4))
             (and s_6_4 (not s_5_3) (not s_7_3) (not s_6_2))
             (and (not s_5_3) s_7_3 s_6_2 s_6_4)
             (and (not s_7_3) s_5_3 s_6_2 s_6_4)
             (and (not s_6_2) s_5_3 s_7_3 s_6_4)
             (and (not s_6_4) s_5_3 s_7_3 s_6_2)))

;;neighbours of s_6_4 => ((5 4) (7 4) (6 3) (6 5)) 
;; internal s_6_4
(assert (xor (and s_5_4 (not s_7_4) (not s_6_3) (not s_6_5))
             (and s_7_4 (not s_5_4) (not s_6_3) (not s_6_5))
             (and s_6_3 (not s_5_4) (not s_7_4) (not s_6_5))
             (and s_6_5 (not s_5_4) (not s_7_4) (not s_6_3))
             (and (not s_5_4) s_7_4 s_6_3 s_6_5)
             (and (not s_7_4) s_5_4 s_6_3 s_6_5)
             (and (not s_6_3) s_5_4 s_7_4 s_6_5)
             (and (not s_6_5) s_5_4 s_7_4 s_6_3)))

;;neighbours of s_6_5 => ((5 5) (7 5) (6 4) (6 6)) 
;; internal s_6_5
(assert (xor (and s_5_5 (not s_7_5) (not s_6_4) (not s_6_6))
             (and s_7_5 (not s_5_5) (not s_6_4) (not s_6_6))
             (and s_6_4 (not s_5_5) (not s_7_5) (not s_6_6))
             (and s_6_6 (not s_5_5) (not s_7_5) (not s_6_4))
             (and (not s_5_5) s_7_5 s_6_4 s_6_6)
             (and (not s_7_5) s_5_5 s_6_4 s_6_6)
             (and (not s_6_4) s_5_5 s_7_5 s_6_6)
             (and (not s_6_6) s_5_5 s_7_5 s_6_4)))

;;neighbours of s_6_6 => ((5 6) (7 6) (6 5) (6 7)) 
;; internal s_6_6
(assert (xor (and s_5_6 (not s_7_6) (not s_6_5) (not s_6_7))
             (and s_7_6 (not s_5_6) (not s_6_5) (not s_6_7))
             (and s_6_5 (not s_5_6) (not s_7_6) (not s_6_7))
             (and s_6_7 (not s_5_6) (not s_7_6) (not s_6_5))
             (and (not s_5_6) s_7_6 s_6_5 s_6_7)
             (and (not s_7_6) s_5_6 s_6_5 s_6_7)
             (and (not s_6_5) s_5_6 s_7_6 s_6_7)
             (and (not s_6_7) s_5_6 s_7_6 s_6_5)))

;;neighbours of s_6_7 => ((5 7) (7 7) (6 6) (6 8)) 
;; internal s_6_7
(assert (xor (and s_5_7 (not s_7_7) (not s_6_6) (not s_6_8))
             (and s_7_7 (not s_5_7) (not s_6_6) (not s_6_8))
             (and s_6_6 (not s_5_7) (not s_7_7) (not s_6_8))
             (and s_6_8 (not s_5_7) (not s_7_7) (not s_6_6))
             (and (not s_5_7) s_7_7 s_6_6 s_6_8)
             (and (not s_7_7) s_5_7 s_6_6 s_6_8)
             (and (not s_6_6) s_5_7 s_7_7 s_6_8)
             (and (not s_6_8) s_5_7 s_7_7 s_6_6)))

;;neighbours of s_6_8 => ((5 8) (7 8) (6 7) (6 9)) 
;; internal s_6_8
(assert (xor (and s_5_8 (not s_7_8) (not s_6_7) (not s_6_9))
             (and s_7_8 (not s_5_8) (not s_6_7) (not s_6_9))
             (and s_6_7 (not s_5_8) (not s_7_8) (not s_6_9))
             (and s_6_9 (not s_5_8) (not s_7_8) (not s_6_7))
             (and (not s_5_8) s_7_8 s_6_7 s_6_9)
             (and (not s_7_8) s_5_8 s_6_7 s_6_9)
             (and (not s_6_7) s_5_8 s_7_8 s_6_9)
             (and (not s_6_9) s_5_8 s_7_8 s_6_7)))

;;neighbours of s_6_9 => ((5 9) (7 9) (6 8) (6 10)) 
;; internal s_6_9
(assert (xor (and s_5_9 (not s_7_9) (not s_6_8) (not s_6_10))
             (and s_7_9 (not s_5_9) (not s_6_8) (not s_6_10))
             (and s_6_8 (not s_5_9) (not s_7_9) (not s_6_10))
             (and s_6_10 (not s_5_9) (not s_7_9) (not s_6_8))
             (and (not s_5_9) s_7_9 s_6_8 s_6_10)
             (and (not s_7_9) s_5_9 s_6_8 s_6_10)
             (and (not s_6_8) s_5_9 s_7_9 s_6_10)
             (and (not s_6_10) s_5_9 s_7_9 s_6_8)))

;;neighbours of s_6_10 => ((5 10) (7 10) (6 9)) 
;; edge s_6_10
(assert (xor (and s_5_10 (not s_7_10) (not s_6_9))
             (and s_7_10 (not s_5_10) (not s_6_9))
             (and s_6_9 (not s_5_10) (not s_7_10))
             (and s_5_10 s_7_10 s_6_9)))

;;neighbours of s_7_1 => ((6 1) (8 1) (7 2)) 
;; edge s_7_1
(assert (xor (and s_6_1 (not s_8_1) (not s_7_2))
             (and s_8_1 (not s_6_1) (not s_7_2))
             (and s_7_2 (not s_6_1) (not s_8_1))
             (and s_6_1 s_8_1 s_7_2)))

;;neighbours of s_7_2 => ((6 2) (8 2) (7 1) (7 3)) 
;; internal s_7_2
(assert (xor (and s_6_2 (not s_8_2) (not s_7_1) (not s_7_3))
             (and s_8_2 (not s_6_2) (not s_7_1) (not s_7_3))
             (and s_7_1 (not s_6_2) (not s_8_2) (not s_7_3))
             (and s_7_3 (not s_6_2) (not s_8_2) (not s_7_1))
             (and (not s_6_2) s_8_2 s_7_1 s_7_3)
             (and (not s_8_2) s_6_2 s_7_1 s_7_3)
             (and (not s_7_1) s_6_2 s_8_2 s_7_3)
             (and (not s_7_3) s_6_2 s_8_2 s_7_1)))

;;neighbours of s_7_3 => ((6 3) (8 3) (7 2) (7 4)) 
;; internal s_7_3
(assert (xor (and s_6_3 (not s_8_3) (not s_7_2) (not s_7_4))
             (and s_8_3 (not s_6_3) (not s_7_2) (not s_7_4))
             (and s_7_2 (not s_6_3) (not s_8_3) (not s_7_4))
             (and s_7_4 (not s_6_3) (not s_8_3) (not s_7_2))
             (and (not s_6_3) s_8_3 s_7_2 s_7_4)
             (and (not s_8_3) s_6_3 s_7_2 s_7_4)
             (and (not s_7_2) s_6_3 s_8_3 s_7_4)
             (and (not s_7_4) s_6_3 s_8_3 s_7_2)))

;;neighbours of s_7_4 => ((6 4) (8 4) (7 3) (7 5)) 
;; internal s_7_4
(assert (xor (and s_6_4 (not s_8_4) (not s_7_3) (not s_7_5))
             (and s_8_4 (not s_6_4) (not s_7_3) (not s_7_5))
             (and s_7_3 (not s_6_4) (not s_8_4) (not s_7_5))
             (and s_7_5 (not s_6_4) (not s_8_4) (not s_7_3))
             (and (not s_6_4) s_8_4 s_7_3 s_7_5)
             (and (not s_8_4) s_6_4 s_7_3 s_7_5)
             (and (not s_7_3) s_6_4 s_8_4 s_7_5)
             (and (not s_7_5) s_6_4 s_8_4 s_7_3)))

;;neighbours of s_7_5 => ((6 5) (8 5) (7 4) (7 6)) 
;; internal s_7_5
(assert (xor (and s_6_5 (not s_8_5) (not s_7_4) (not s_7_6))
             (and s_8_5 (not s_6_5) (not s_7_4) (not s_7_6))
             (and s_7_4 (not s_6_5) (not s_8_5) (not s_7_6))
             (and s_7_6 (not s_6_5) (not s_8_5) (not s_7_4))
             (and (not s_6_5) s_8_5 s_7_4 s_7_6)
             (and (not s_8_5) s_6_5 s_7_4 s_7_6)
             (and (not s_7_4) s_6_5 s_8_5 s_7_6)
             (and (not s_7_6) s_6_5 s_8_5 s_7_4)))

;;neighbours of s_7_6 => ((6 6) (8 6) (7 5) (7 7)) 
;; internal s_7_6
(assert (xor (and s_6_6 (not s_8_6) (not s_7_5) (not s_7_7))
             (and s_8_6 (not s_6_6) (not s_7_5) (not s_7_7))
             (and s_7_5 (not s_6_6) (not s_8_6) (not s_7_7))
             (and s_7_7 (not s_6_6) (not s_8_6) (not s_7_5))
             (and (not s_6_6) s_8_6 s_7_5 s_7_7)
             (and (not s_8_6) s_6_6 s_7_5 s_7_7)
             (and (not s_7_5) s_6_6 s_8_6 s_7_7)
             (and (not s_7_7) s_6_6 s_8_6 s_7_5)))

;;neighbours of s_7_7 => ((6 7) (8 7) (7 6) (7 8)) 
;; internal s_7_7
(assert (xor (and s_6_7 (not s_8_7) (not s_7_6) (not s_7_8))
             (and s_8_7 (not s_6_7) (not s_7_6) (not s_7_8))
             (and s_7_6 (not s_6_7) (not s_8_7) (not s_7_8))
             (and s_7_8 (not s_6_7) (not s_8_7) (not s_7_6))
             (and (not s_6_7) s_8_7 s_7_6 s_7_8)
             (and (not s_8_7) s_6_7 s_7_6 s_7_8)
             (and (not s_7_6) s_6_7 s_8_7 s_7_8)
             (and (not s_7_8) s_6_7 s_8_7 s_7_6)))

;;neighbours of s_7_8 => ((6 8) (8 8) (7 7) (7 9)) 
;; internal s_7_8
(assert (xor (and s_6_8 (not s_8_8) (not s_7_7) (not s_7_9))
             (and s_8_8 (not s_6_8) (not s_7_7) (not s_7_9))
             (and s_7_7 (not s_6_8) (not s_8_8) (not s_7_9))
             (and s_7_9 (not s_6_8) (not s_8_8) (not s_7_7))
             (and (not s_6_8) s_8_8 s_7_7 s_7_9)
             (and (not s_8_8) s_6_8 s_7_7 s_7_9)
             (and (not s_7_7) s_6_8 s_8_8 s_7_9)
             (and (not s_7_9) s_6_8 s_8_8 s_7_7)))

;;neighbours of s_7_9 => ((6 9) (8 9) (7 8) (7 10)) 
;; internal s_7_9
(assert (xor (and s_6_9 (not s_8_9) (not s_7_8) (not s_7_10))
             (and s_8_9 (not s_6_9) (not s_7_8) (not s_7_10))
             (and s_7_8 (not s_6_9) (not s_8_9) (not s_7_10))
             (and s_7_10 (not s_6_9) (not s_8_9) (not s_7_8))
             (and (not s_6_9) s_8_9 s_7_8 s_7_10)
             (and (not s_8_9) s_6_9 s_7_8 s_7_10)
             (and (not s_7_8) s_6_9 s_8_9 s_7_10)
             (and (not s_7_10) s_6_9 s_8_9 s_7_8)))

;;neighbours of s_7_10 => ((6 10) (8 10) (7 9)) 
;; edge s_7_10
(assert (xor (and s_6_10 (not s_8_10) (not s_7_9))
             (and s_8_10 (not s_6_10) (not s_7_9))
             (and s_7_9 (not s_6_10) (not s_8_10))
             (and s_6_10 s_8_10 s_7_9)))

;;neighbours of s_8_1 => ((7 1) (9 1) (8 2)) 
;; edge s_8_1
(assert (xor (and s_7_1 (not s_9_1) (not s_8_2))
             (and s_9_1 (not s_7_1) (not s_8_2))
             (and s_8_2 (not s_7_1) (not s_9_1))
             (and s_7_1 s_9_1 s_8_2)))

;;neighbours of s_8_2 => ((7 2) (9 2) (8 1) (8 3)) 
;; internal s_8_2
(assert (xor (and s_7_2 (not s_9_2) (not s_8_1) (not s_8_3))
             (and s_9_2 (not s_7_2) (not s_8_1) (not s_8_3))
             (and s_8_1 (not s_7_2) (not s_9_2) (not s_8_3))
             (and s_8_3 (not s_7_2) (not s_9_2) (not s_8_1))
             (and (not s_7_2) s_9_2 s_8_1 s_8_3)
             (and (not s_9_2) s_7_2 s_8_1 s_8_3)
             (and (not s_8_1) s_7_2 s_9_2 s_8_3)
             (and (not s_8_3) s_7_2 s_9_2 s_8_1)))

;;neighbours of s_8_3 => ((7 3) (9 3) (8 2) (8 4)) 
;; internal s_8_3
(assert (xor (and s_7_3 (not s_9_3) (not s_8_2) (not s_8_4))
             (and s_9_3 (not s_7_3) (not s_8_2) (not s_8_4))
             (and s_8_2 (not s_7_3) (not s_9_3) (not s_8_4))
             (and s_8_4 (not s_7_3) (not s_9_3) (not s_8_2))
             (and (not s_7_3) s_9_3 s_8_2 s_8_4)
             (and (not s_9_3) s_7_3 s_8_2 s_8_4)
             (and (not s_8_2) s_7_3 s_9_3 s_8_4)
             (and (not s_8_4) s_7_3 s_9_3 s_8_2)))

;;neighbours of s_8_4 => ((7 4) (9 4) (8 3) (8 5)) 
;; internal s_8_4
(assert (xor (and s_7_4 (not s_9_4) (not s_8_3) (not s_8_5))
             (and s_9_4 (not s_7_4) (not s_8_3) (not s_8_5))
             (and s_8_3 (not s_7_4) (not s_9_4) (not s_8_5))
             (and s_8_5 (not s_7_4) (not s_9_4) (not s_8_3))
             (and (not s_7_4) s_9_4 s_8_3 s_8_5)
             (and (not s_9_4) s_7_4 s_8_3 s_8_5)
             (and (not s_8_3) s_7_4 s_9_4 s_8_5)
             (and (not s_8_5) s_7_4 s_9_4 s_8_3)))

;;neighbours of s_8_5 => ((7 5) (9 5) (8 4) (8 6)) 
;; internal s_8_5
(assert (xor (and s_7_5 (not s_9_5) (not s_8_4) (not s_8_6))
             (and s_9_5 (not s_7_5) (not s_8_4) (not s_8_6))
             (and s_8_4 (not s_7_5) (not s_9_5) (not s_8_6))
             (and s_8_6 (not s_7_5) (not s_9_5) (not s_8_4))
             (and (not s_7_5) s_9_5 s_8_4 s_8_6)
             (and (not s_9_5) s_7_5 s_8_4 s_8_6)
             (and (not s_8_4) s_7_5 s_9_5 s_8_6)
             (and (not s_8_6) s_7_5 s_9_5 s_8_4)))

;;neighbours of s_8_6 => ((7 6) (9 6) (8 5) (8 7)) 
;; internal s_8_6
(assert (xor (and s_7_6 (not s_9_6) (not s_8_5) (not s_8_7))
             (and s_9_6 (not s_7_6) (not s_8_5) (not s_8_7))
             (and s_8_5 (not s_7_6) (not s_9_6) (not s_8_7))
             (and s_8_7 (not s_7_6) (not s_9_6) (not s_8_5))
             (and (not s_7_6) s_9_6 s_8_5 s_8_7)
             (and (not s_9_6) s_7_6 s_8_5 s_8_7)
             (and (not s_8_5) s_7_6 s_9_6 s_8_7)
             (and (not s_8_7) s_7_6 s_9_6 s_8_5)))

;;neighbours of s_8_7 => ((7 7) (9 7) (8 6) (8 8)) 
;; internal s_8_7
(assert (xor (and s_7_7 (not s_9_7) (not s_8_6) (not s_8_8))
             (and s_9_7 (not s_7_7) (not s_8_6) (not s_8_8))
             (and s_8_6 (not s_7_7) (not s_9_7) (not s_8_8))
             (and s_8_8 (not s_7_7) (not s_9_7) (not s_8_6))
             (and (not s_7_7) s_9_7 s_8_6 s_8_8)
             (and (not s_9_7) s_7_7 s_8_6 s_8_8)
             (and (not s_8_6) s_7_7 s_9_7 s_8_8)
             (and (not s_8_8) s_7_7 s_9_7 s_8_6)))

;;neighbours of s_8_8 => ((7 8) (9 8) (8 7) (8 9)) 
;; internal s_8_8
(assert (xor (and s_7_8 (not s_9_8) (not s_8_7) (not s_8_9))
             (and s_9_8 (not s_7_8) (not s_8_7) (not s_8_9))
             (and s_8_7 (not s_7_8) (not s_9_8) (not s_8_9))
             (and s_8_9 (not s_7_8) (not s_9_8) (not s_8_7))
             (and (not s_7_8) s_9_8 s_8_7 s_8_9)
             (and (not s_9_8) s_7_8 s_8_7 s_8_9)
             (and (not s_8_7) s_7_8 s_9_8 s_8_9)
             (and (not s_8_9) s_7_8 s_9_8 s_8_7)))

;;neighbours of s_8_9 => ((7 9) (9 9) (8 8) (8 10)) 
;; internal s_8_9
(assert (xor (and s_7_9 (not s_9_9) (not s_8_8) (not s_8_10))
             (and s_9_9 (not s_7_9) (not s_8_8) (not s_8_10))
             (and s_8_8 (not s_7_9) (not s_9_9) (not s_8_10))
             (and s_8_10 (not s_7_9) (not s_9_9) (not s_8_8))
             (and (not s_7_9) s_9_9 s_8_8 s_8_10)
             (and (not s_9_9) s_7_9 s_8_8 s_8_10)
             (and (not s_8_8) s_7_9 s_9_9 s_8_10)
             (and (not s_8_10) s_7_9 s_9_9 s_8_8)))

;;neighbours of s_8_10 => ((7 10) (9 10) (8 9)) 
;; edge s_8_10
(assert (xor (and s_7_10 (not s_9_10) (not s_8_9))
             (and s_9_10 (not s_7_10) (not s_8_9))
             (and s_8_9 (not s_7_10) (not s_9_10))
             (and s_7_10 s_9_10 s_8_9)))

;;neighbours of s_9_1 => ((8 1) (10 1) (9 2)) 
;; edge s_9_1
(assert (xor (and s_8_1 (not s_10_1) (not s_9_2))
             (and s_10_1 (not s_8_1) (not s_9_2))
             (and s_9_2 (not s_8_1) (not s_10_1))
             (and s_8_1 s_10_1 s_9_2)))

;;neighbours of s_9_2 => ((8 2) (10 2) (9 1) (9 3)) 
;; internal s_9_2
(assert (xor (and s_8_2 (not s_10_2) (not s_9_1) (not s_9_3))
             (and s_10_2 (not s_8_2) (not s_9_1) (not s_9_3))
             (and s_9_1 (not s_8_2) (not s_10_2) (not s_9_3))
             (and s_9_3 (not s_8_2) (not s_10_2) (not s_9_1))
             (and (not s_8_2) s_10_2 s_9_1 s_9_3)
             (and (not s_10_2) s_8_2 s_9_1 s_9_3)
             (and (not s_9_1) s_8_2 s_10_2 s_9_3)
             (and (not s_9_3) s_8_2 s_10_2 s_9_1)))

;;neighbours of s_9_3 => ((8 3) (10 3) (9 2) (9 4)) 
;; internal s_9_3
(assert (xor (and s_8_3 (not s_10_3) (not s_9_2) (not s_9_4))
             (and s_10_3 (not s_8_3) (not s_9_2) (not s_9_4))
             (and s_9_2 (not s_8_3) (not s_10_3) (not s_9_4))
             (and s_9_4 (not s_8_3) (not s_10_3) (not s_9_2))
             (and (not s_8_3) s_10_3 s_9_2 s_9_4)
             (and (not s_10_3) s_8_3 s_9_2 s_9_4)
             (and (not s_9_2) s_8_3 s_10_3 s_9_4)
             (and (not s_9_4) s_8_3 s_10_3 s_9_2)))

;;neighbours of s_9_4 => ((8 4) (10 4) (9 3) (9 5)) 
;; internal s_9_4
(assert (xor (and s_8_4 (not s_10_4) (not s_9_3) (not s_9_5))
             (and s_10_4 (not s_8_4) (not s_9_3) (not s_9_5))
             (and s_9_3 (not s_8_4) (not s_10_4) (not s_9_5))
             (and s_9_5 (not s_8_4) (not s_10_4) (not s_9_3))
             (and (not s_8_4) s_10_4 s_9_3 s_9_5)
             (and (not s_10_4) s_8_4 s_9_3 s_9_5)
             (and (not s_9_3) s_8_4 s_10_4 s_9_5)
             (and (not s_9_5) s_8_4 s_10_4 s_9_3)))

;;neighbours of s_9_5 => ((8 5) (10 5) (9 4) (9 6)) 
;; internal s_9_5
(assert (xor (and s_8_5 (not s_10_5) (not s_9_4) (not s_9_6))
             (and s_10_5 (not s_8_5) (not s_9_4) (not s_9_6))
             (and s_9_4 (not s_8_5) (not s_10_5) (not s_9_6))
             (and s_9_6 (not s_8_5) (not s_10_5) (not s_9_4))
             (and (not s_8_5) s_10_5 s_9_4 s_9_6)
             (and (not s_10_5) s_8_5 s_9_4 s_9_6)
             (and (not s_9_4) s_8_5 s_10_5 s_9_6)
             (and (not s_9_6) s_8_5 s_10_5 s_9_4)))

;;neighbours of s_9_6 => ((8 6) (10 6) (9 5) (9 7)) 
;; internal s_9_6
(assert (xor (and s_8_6 (not s_10_6) (not s_9_5) (not s_9_7))
             (and s_10_6 (not s_8_6) (not s_9_5) (not s_9_7))
             (and s_9_5 (not s_8_6) (not s_10_6) (not s_9_7))
             (and s_9_7 (not s_8_6) (not s_10_6) (not s_9_5))
             (and (not s_8_6) s_10_6 s_9_5 s_9_7)
             (and (not s_10_6) s_8_6 s_9_5 s_9_7)
             (and (not s_9_5) s_8_6 s_10_6 s_9_7)
             (and (not s_9_7) s_8_6 s_10_6 s_9_5)))

;;neighbours of s_9_7 => ((8 7) (10 7) (9 6) (9 8)) 
;; internal s_9_7
(assert (xor (and s_8_7 (not s_10_7) (not s_9_6) (not s_9_8))
             (and s_10_7 (not s_8_7) (not s_9_6) (not s_9_8))
             (and s_9_6 (not s_8_7) (not s_10_7) (not s_9_8))
             (and s_9_8 (not s_8_7) (not s_10_7) (not s_9_6))
             (and (not s_8_7) s_10_7 s_9_6 s_9_8)
             (and (not s_10_7) s_8_7 s_9_6 s_9_8)
             (and (not s_9_6) s_8_7 s_10_7 s_9_8)
             (and (not s_9_8) s_8_7 s_10_7 s_9_6)))

;;neighbours of s_9_8 => ((8 8) (10 8) (9 7) (9 9)) 
;; internal s_9_8
(assert (xor (and s_8_8 (not s_10_8) (not s_9_7) (not s_9_9))
             (and s_10_8 (not s_8_8) (not s_9_7) (not s_9_9))
             (and s_9_7 (not s_8_8) (not s_10_8) (not s_9_9))
             (and s_9_9 (not s_8_8) (not s_10_8) (not s_9_7))
             (and (not s_8_8) s_10_8 s_9_7 s_9_9)
             (and (not s_10_8) s_8_8 s_9_7 s_9_9)
             (and (not s_9_7) s_8_8 s_10_8 s_9_9)
             (and (not s_9_9) s_8_8 s_10_8 s_9_7)))

;;neighbours of s_9_9 => ((8 9) (10 9) (9 8) (9 10)) 
;; internal s_9_9
(assert (xor (and s_8_9 (not s_10_9) (not s_9_8) (not s_9_10))
             (and s_10_9 (not s_8_9) (not s_9_8) (not s_9_10))
             (and s_9_8 (not s_8_9) (not s_10_9) (not s_9_10))
             (and s_9_10 (not s_8_9) (not s_10_9) (not s_9_8))
             (and (not s_8_9) s_10_9 s_9_8 s_9_10)
             (and (not s_10_9) s_8_9 s_9_8 s_9_10)
             (and (not s_9_8) s_8_9 s_10_9 s_9_10)
             (and (not s_9_10) s_8_9 s_10_9 s_9_8)))

;;neighbours of s_9_10 => ((8 10) (10 10) (9 9)) 
;; edge s_9_10
(assert (xor (and s_8_10 (not s_10_10) (not s_9_9))
             (and s_10_10 (not s_8_10) (not s_9_9))
             (and s_9_9 (not s_8_10) (not s_10_10))
             (and s_8_10 s_10_10 s_9_9)))

;;neighbours of s_10_1 => ((9 1) (10 2)) 
;; corner s_10_1
(assert (xor s_9_1 s_10_2))


;;neighbours of s_10_2 => ((9 2) (10 1) (10 3)) 
;; edge s_10_2
(assert (xor (and s_9_2 (not s_10_1) (not s_10_3))
             (and s_10_1 (not s_9_2) (not s_10_3))
             (and s_10_3 (not s_9_2) (not s_10_1))
             (and s_9_2 s_10_1 s_10_3)))

;;neighbours of s_10_3 => ((9 3) (10 2) (10 4)) 
;; edge s_10_3
(assert (xor (and s_9_3 (not s_10_2) (not s_10_4))
             (and s_10_2 (not s_9_3) (not s_10_4))
             (and s_10_4 (not s_9_3) (not s_10_2))
             (and s_9_3 s_10_2 s_10_4)))

;;neighbours of s_10_4 => ((9 4) (10 3) (10 5)) 
;; edge s_10_4
(assert (xor (and s_9_4 (not s_10_3) (not s_10_5))
             (and s_10_3 (not s_9_4) (not s_10_5))
             (and s_10_5 (not s_9_4) (not s_10_3))
             (and s_9_4 s_10_3 s_10_5)))

;;neighbours of s_10_5 => ((9 5) (10 4) (10 6)) 
;; edge s_10_5
(assert (xor (and s_9_5 (not s_10_4) (not s_10_6))
             (and s_10_4 (not s_9_5) (not s_10_6))
             (and s_10_6 (not s_9_5) (not s_10_4))
             (and s_9_5 s_10_4 s_10_6)))

;;neighbours of s_10_6 => ((9 6) (10 5) (10 7)) 
;; edge s_10_6
(assert (xor (and s_9_6 (not s_10_5) (not s_10_7))
             (and s_10_5 (not s_9_6) (not s_10_7))
             (and s_10_7 (not s_9_6) (not s_10_5))
             (and s_9_6 s_10_5 s_10_7)))

;;neighbours of s_10_7 => ((9 7) (10 6) (10 8)) 
;; edge s_10_7
(assert (xor (and s_9_7 (not s_10_6) (not s_10_8))
             (and s_10_6 (not s_9_7) (not s_10_8))
             (and s_10_8 (not s_9_7) (not s_10_6))
             (and s_9_7 s_10_6 s_10_8)))

;;neighbours of s_10_8 => ((9 8) (10 7) (10 9)) 
;; edge s_10_8
(assert (xor (and s_9_8 (not s_10_7) (not s_10_9))
             (and s_10_7 (not s_9_8) (not s_10_9))
             (and s_10_9 (not s_9_8) (not s_10_7))
             (and s_9_8 s_10_7 s_10_9)))

;;neighbours of s_10_9 => ((9 9) (10 8) (10 10)) 
;; edge s_10_9
(assert (xor (and s_9_9 (not s_10_8) (not s_10_10))
             (and s_10_8 (not s_9_9) (not s_10_10))
             (and s_10_10 (not s_9_9) (not s_10_8))
             (and s_9_9 s_10_8 s_10_10)))

;;neighbours of s_10_10 => ((9 10) (10 9)) 
;; corner s_10_10
(assert (xor s_9_10 s_10_9))


(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_1_3 s_2_3 s_6_3 s_9_3 s_2_4 s_3_4 s_5_4 s_8_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_7_6 s_8_6 s_9_6 s_1_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_9_1 s_2_2 s_3_2 s_5_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_3_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_3_8 s_7_8 s_9_8 s_1_9 s_2_9 s_3_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_3_2 s_5_2 s_6_2 s_8_2 s_9_2 s_2_3 s_3_3 s_5_3 s_6_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_7_6 s_9_6 s_1_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_8_9 s_10_9 s_1_10 s_3_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_2_2 s_3_2 s_5_2 s_6_2 s_8_2 s_9_2 s_2_3 s_5_3 s_6_3 s_9_3 s_3_4 s_5_4 s_6_4 s_8_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_7_6 s_9_6 s_1_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_7_1 s_9_1 s_3_2 s_4_2 s_5_2 s_8_2 s_9_2 s_2_3 s_6_3 s_7_3 s_9_3 s_2_4 s_3_4 s_5_4 s_2_5 s_4_5 s_5_5 s_6_5 s_8_5 s_10_5 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_3_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_7_1 s_9_1 s_2_2 s_3_2 s_4_2 s_5_2 s_8_2 s_9_2 s_2_3 s_3_3 s_6_3 s_7_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_2_5 s_4_5 s_6_5 s_8_5 s_10_5 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_9_1 s_3_2 s_5_2 s_8_2 s_9_2 s_1_3 s_2_3 s_5_3 s_6_3 s_7_3 s_9_3 s_3_4 s_5_4 s_6_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_7_6 s_9_6 s_10_6 s_1_7 s_3_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_3_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_6_9 s_8_9 s_10_9 s_1_10 s_3_10 s_6_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_6_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_8_4 s_1_5 s_2_5 s_4_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_9_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_3_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_7_1 s_9_1 s_10_1 s_3_2 s_4_2 s_5_2 s_8_2 s_2_3 s_6_3 s_7_3 s_8_3 s_9_3 s_2_4 s_3_4 s_5_4 s_7_4 s_2_5 s_4_5 s_5_5 s_8_5 s_10_5 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_6_9 s_7_9 s_8_9 s_10_9 s_3_10 s_6_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_3_2 s_5_2 s_6_2 s_8_2 s_2_3 s_3_3 s_5_3 s_6_3 s_8_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_1_5 s_2_5 s_3_5 s_4_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_1_7 s_4_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_4_8 s_7_8 s_9_8 s_10_8 s_1_9 s_3_9 s_5_9 s_6_9 s_8_9 s_10_9 s_3_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_7_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_8_2 s_2_3 s_3_3 s_6_3 s_7_3 s_8_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_2_5 s_4_5 s_8_5 s_10_5 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_3_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_3_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_2_2 s_3_2 s_5_2 s_6_2 s_8_2 s_2_3 s_5_3 s_6_3 s_8_3 s_9_3 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_7_6 s_9_6 s_1_7 s_4_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_4_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_3_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_9_1 s_10_1 s_3_2 s_5_2 s_8_2 s_1_3 s_2_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_3_4 s_5_4 s_6_4 s_7_4 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_7_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_7_8 s_8_8 s_9_8 s_1_9 s_3_9 s_6_9 s_8_9 s_10_9 s_3_10 s_6_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_1_3 s_2_3 s_3_3 s_6_3 s_8_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_1_5 s_2_5 s_4_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_4_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_6_8 s_7_8 s_9_8 s_1_9 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_3_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_9_1 s_10_1 s_2_2 s_3_2 s_5_2 s_8_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_2_5 s_3_5 s_4_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_7_8 s_9_8 s_1_9 s_3_9 s_6_9 s_8_9 s_9_9 s_10_9 s_3_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_1_3 s_2_3 s_6_3 s_8_3 s_9_3 s_2_4 s_3_4 s_5_4 s_7_4 s_8_4 s_1_5 s_2_5 s_4_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_7_6 s_8_6 s_9_6 s_1_7 s_4_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_3_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_9_1 s_10_1 s_3_2 s_4_2 s_5_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_7_6 s_9_6 s_3_7 s_4_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_7_8 s_9_8 s_1_9 s_6_9 s_7_9 s_8_9 s_10_9 s_3_10 s_4_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_9_1 s_3_2 s_4_2 s_5_2 s_9_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_2_4 s_3_4 s_5_4 s_6_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_7_6 s_9_6 s_3_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_3_8 s_7_8 s_9_8 s_1_9 s_2_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_3_10 s_4_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_9_1 s_2_2 s_3_2 s_4_2 s_5_2 s_9_2 s_1_3 s_2_3 s_5_3 s_6_3 s_7_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_7_6 s_9_6 s_3_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_3_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_9_1 s_3_2 s_5_2 s_9_2 s_2_3 s_3_3 s_6_3 s_7_3 s_3_4 s_5_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_8_5 s_10_5 s_7_6 s_8_6 s_9_6 s_3_7 s_6_7 s_10_7 s_1_8 s_3_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_6_9 s_8_9 s_10_9 s_1_10 s_3_10 s_4_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_9_1 s_2_2 s_3_2 s_5_2 s_9_2 s_2_3 s_6_3 s_7_3 s_3_4 s_4_4 s_5_4 s_10_4 s_1_5 s_2_5 s_4_5 s_6_5 s_8_5 s_10_5 s_6_6 s_7_6 s_8_6 s_9_6 s_3_7 s_6_7 s_7_7 s_10_7 s_1_8 s_3_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_1_3 s_2_3 s_5_3 s_6_3 s_7_3 s_8_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_7_8 s_8_8 s_9_8 s_1_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_3_10 s_4_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_9_1 s_10_1 s_2_2 s_3_2 s_5_2 s_2_3 s_6_3 s_7_3 s_8_3 s_3_4 s_4_4 s_5_4 s_7_4 s_10_4 s_1_5 s_2_5 s_4_5 s_8_5 s_10_5 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_3_7 s_4_7 s_6_7 s_7_7 s_10_7 s_1_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_6_9 s_8_9 s_9_9 s_10_9 s_3_10 s_4_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_9_1 s_10_1 s_3_2 s_5_2 s_2_3 s_3_3 s_6_3 s_7_3 s_8_3 s_3_4 s_5_4 s_7_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_8_5 s_10_5 s_5_6 s_7_6 s_8_6 s_9_6 s_3_7 s_4_7 s_6_7 s_10_7 s_1_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_6_9 s_8_9 s_10_9 s_3_10 s_4_10 s_8_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_3_2 s_5_2 s_9_2 s_10_2 s_1_3 s_2_3 s_5_3 s_6_3 s_3_4 s_5_4 s_8_4 s_2_5 s_3_5 s_4_5 s_6_5 s_8_5 s_10_5 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_6_7 s_10_7 s_1_8 s_3_8 s_4_8 s_7_8 s_8_8 s_9_8 s_2_9 s_6_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_2_2 s_3_2 s_5_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_3_4 s_4_4 s_5_4 s_8_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_10_5 s_7_6 s_9_6 s_10_6 s_1_7 s_6_7 s_7_7 s_10_7 s_1_8 s_3_8 s_4_8 s_7_8 s_9_8 s_2_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_7_1 s_3_2 s_4_2 s_5_2 s_9_2 s_10_2 s_2_3 s_6_3 s_2_4 s_3_4 s_5_4 s_6_4 s_8_4 s_2_5 s_4_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_7_1 s_2_2 s_3_2 s_4_2 s_5_2 s_9_2 s_10_2 s_2_3 s_3_3 s_6_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_6_8 s_7_8 s_9_8 s_10_8 s_2_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_9_2 s_10_2 s_1_3 s_2_3 s_6_3 s_7_3 s_2_4 s_3_4 s_5_4 s_6_4 s_1_5 s_2_5 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_3_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_3_8 s_6_8 s_7_8 s_8_8 s_9_8 s_2_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_3_2 s_4_2 s_5_2 s_6_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_6_3 s_7_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_7_6 s_8_6 s_9_6 s_1_7 s_3_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_3_8 s_6_8 s_7_8 s_9_8 s_2_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_3_2 s_5_2 s_6_2 s_9_2 s_10_2 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_3_4 s_4_4 s_5_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_7_6 s_9_6 s_1_7 s_3_7 s_6_7 s_7_7 s_10_7 s_1_8 s_3_8 s_7_8 s_9_8 s_10_8 s_2_9 s_5_9 s_6_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_2_2 s_3_2 s_5_2 s_6_2 s_9_2 s_10_2 s_2_3 s_5_3 s_6_3 s_7_3 s_3_4 s_5_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_7_6 s_9_6 s_1_7 s_3_7 s_6_7 s_10_7 s_1_8 s_3_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_9_3 s_2_4 s_3_4 s_5_4 s_10_4 s_2_5 s_3_5 s_4_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_7_6 s_9_6 s_10_6 s_3_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_7_8 s_9_8 s_10_8 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_2_3 s_5_3 s_6_3 s_7_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_7_6 s_9_6 s_10_6 s_3_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_2_2 s_3_2 s_5_2 s_8_2 s_9_2 s_10_2 s_2_3 s_6_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_7_6 s_8_6 s_9_6 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_3_2 s_5_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_6_3 s_9_3 s_3_4 s_5_4 s_6_4 s_8_4 s_10_4 s_1_5 s_2_5 s_4_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_9_8 s_10_8 s_2_9 s_3_9 s_6_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_2_2 s_3_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_6_3 s_7_3 s_9_3 s_3_4 s_5_4 s_6_4 s_10_4 s_2_5 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_3_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_3_8 s_6_8 s_7_8 s_9_8 s_2_9 s_3_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_3_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_6_3 s_7_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_10_4 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_7_6 s_8_6 s_9_6 s_10_6 s_3_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_3_8 s_6_8 s_7_8 s_8_8 s_9_8 s_2_9 s_3_9 s_5_9 s_6_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_3_2 s_4_2 s_5_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_9_3 s_2_4 s_3_4 s_5_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_8_5 s_10_5 s_6_6 s_7_6 s_9_6 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_7_8 s_9_8 s_2_9 s_3_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_2_2 s_3_2 s_4_2 s_5_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_5_3 s_6_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_10_5 s_7_6 s_9_6 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_7_8 s_8_8 s_9_8 s_2_9 s_3_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_9_1 s_2_2 s_3_2 s_5_2 s_6_2 s_9_2 s_1_3 s_2_3 s_3_3 s_6_3 s_3_4 s_5_4 s_8_4 s_10_4 s_2_5 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_7_6 s_8_6 s_9_6 s_10_6 s_6_7 s_10_7 s_1_8 s_3_8 s_4_8 s_6_8 s_7_8 s_9_8 s_1_9 s_2_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_6_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_9_1 s_3_2 s_5_2 s_6_2 s_9_2 s_1_3 s_2_3 s_6_3 s_3_4 s_4_4 s_5_4 s_8_4 s_10_4 s_2_5 s_4_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_6_7 s_7_7 s_10_7 s_1_8 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_5_9 s_6_9 s_8_9 s_10_9 s_1_10 s_3_10 s_4_10 s_6_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_9_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_9_2 s_2_3 s_3_3 s_5_3 s_6_3 s_2_4 s_3_4 s_5_4 s_6_4 s_8_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_7_6 s_9_6 s_10_6 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_9_1 s_3_2 s_4_2 s_5_2 s_6_2 s_9_2 s_2_3 s_5_3 s_6_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_10_4 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_7_6 s_9_6 s_10_6 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_3_10 s_4_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_2_3 s_3_3 s_5_3 s_6_3 s_8_3 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_7_6 s_9_6 s_10_6 s_4_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_4_8 s_7_8 s_9_8 s_10_8 s_1_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_3_10 s_4_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_9_1 s_10_1 s_3_2 s_4_2 s_5_2 s_6_2 s_2_3 s_5_3 s_6_3 s_8_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_2_5 s_3_5 s_4_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_4_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_3_10 s_4_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_9_1 s_10_1 s_3_2 s_5_2 s_6_2 s_1_3 s_2_3 s_6_3 s_8_3 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_10_4 s_2_5 s_4_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_4_7 s_6_7 s_7_7 s_10_7 s_1_8 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_5_9 s_6_9 s_8_9 s_10_9 s_3_10 s_4_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_9_1 s_10_1 s_2_2 s_3_2 s_5_2 s_6_2 s_1_3 s_2_3 s_3_3 s_6_3 s_8_3 s_3_4 s_5_4 s_7_4 s_8_4 s_10_4 s_2_5 s_4_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_4_7 s_6_7 s_10_7 s_1_8 s_4_8 s_6_8 s_7_8 s_9_8 s_1_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_3_10 s_4_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_10_2 s_1_3 s_2_3 s_6_3 s_7_3 s_8_3 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_1_5 s_2_5 s_4_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_6_8 s_7_8 s_8_8 s_9_8 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_10_1 s_3_2 s_4_2 s_5_2 s_6_2 s_10_2 s_1_3 s_2_3 s_3_3 s_6_3 s_7_3 s_8_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_1_5 s_2_5 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_6_8 s_7_8 s_9_8 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_2_10 s_3_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_10_1 s_2_2 s_3_2 s_5_2 s_6_2 s_10_2 s_2_3 s_5_3 s_6_3 s_7_3 s_8_3 s_3_4 s_5_4 s_7_4 s_1_5 s_2_5 s_3_5 s_4_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_7_6 s_9_6 s_1_7 s_3_7 s_4_7 s_6_7 s_10_7 s_1_8 s_7_8 s_8_8 s_9_8 s_10_8 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_10_1 s_3_2 s_5_2 s_6_2 s_10_2 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_3_4 s_4_4 s_5_4 s_7_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_7_6 s_9_6 s_1_7 s_3_7 s_4_7 s_6_7 s_7_7 s_10_7 s_1_8 s_7_8 s_9_8 s_10_8 s_5_9 s_6_9 s_8_9 s_10_9 s_2_10 s_3_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_10_2 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_2_4 s_3_4 s_5_4 s_7_4 s_10_4 s_2_5 s_3_5 s_4_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_3_7 s_4_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_7_8 s_9_8 s_10_8 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_10_1 s_2_2 s_3_2 s_5_2 s_6_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_6_3 s_7_3 s_8_3 s_9_3 s_3_4 s_5_4 s_6_4 s_7_4 s_10_4 s_2_5 s_4_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_3_7 s_4_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_6_8 s_7_8 s_9_8 s_3_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_10_1 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_10_2 s_2_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_7_6 s_9_6 s_10_6 s_3_7 s_4_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_7_8 s_8_8 s_9_8 s_10_8 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_2_10 s_3_10 s_4_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_10_1 s_3_2 s_5_2 s_6_2 s_8_2 s_10_2 s_1_3 s_2_3 s_6_3 s_7_3 s_8_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_10_4 s_2_5 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_6_8 s_7_8 s_8_8 s_9_8 s_3_9 s_5_9 s_6_9 s_8_9 s_10_9 s_2_10 s_3_10 s_4_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_7_1 s_10_1 s_3_2 s_4_2 s_5_2 s_10_2 s_2_3 s_6_3 s_8_3 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_2_5 s_4_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_4_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_6_9 s_7_9 s_8_9 s_10_9 s_2_10 s_3_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_7_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_10_2 s_2_3 s_3_3 s_6_3 s_8_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_2_5 s_4_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_4_8 s_6_8 s_7_8 s_9_8 s_10_8 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_10_1 s_3_2 s_4_2 s_5_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_8_3 s_9_3 s_2_4 s_3_4 s_5_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_8_5 s_10_5 s_5_6 s_6_6 s_7_6 s_9_6 s_4_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_7_8 s_9_8 s_3_9 s_6_9 s_7_9 s_8_9 s_10_9 s_2_10 s_3_10 s_4_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_8_2 s_10_2 s_1_3 s_2_3 s_5_3 s_6_3 s_8_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_8_5 s_10_5 s_5_6 s_7_6 s_9_6 s_4_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_7_8 s_8_8 s_9_8 s_3_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_10_1 s_3_2 s_5_2 s_10_2 s_1_3 s_2_3 s_5_3 s_6_3 s_8_3 s_3_4 s_5_4 s_7_4 s_8_4 s_2_5 s_3_5 s_4_5 s_8_5 s_10_5 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_4_7 s_6_7 s_10_7 s_1_8 s_4_8 s_7_8 s_8_8 s_9_8 s_6_9 s_8_9 s_10_9 s_2_10 s_3_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_10_1 s_2_2 s_3_2 s_5_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_8_3 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_2_5 s_3_5 s_4_5 s_5_5 s_8_5 s_10_5 s_5_6 s_7_6 s_9_6 s_10_6 s_1_7 s_4_7 s_6_7 s_7_7 s_10_7 s_1_8 s_4_8 s_7_8 s_9_8 s_6_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_10_1 s_3_2 s_5_2 s_8_2 s_10_2 s_2_3 s_3_3 s_6_3 s_8_3 s_9_3 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_4_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_4_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_4_8 s_6_8 s_7_8 s_9_8 s_10_8 s_3_9 s_6_9 s_8_9 s_10_9 s_2_10 s_3_10 s_4_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_10_1 s_2_2 s_3_2 s_5_2 s_8_2 s_10_2 s_2_3 s_6_3 s_8_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_4_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_3_9 s_6_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_8_10 s_10_10 )))

(assert (not (and s_4_1 s_6_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_2_4 s_10_4 s_2_5 s_3_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_9_6 s_10_6 s_3_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_10_8 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_6_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_8_2 s_10_2 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_2_4 s_7_4 s_10_4 s_2_5 s_3_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_9_6 s_10_6 s_3_7 s_4_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_10_8 s_3_9 s_5_9 s_6_9 s_7_9 s_9_9 s_2_10 s_3_10 s_4_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_7_1 s_10_1 s_1_2 s_2_2 s_5_2 s_6_2 s_10_2 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_7_4 s_1_5 s_2_5 s_3_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_9_6 s_1_7 s_3_7 s_4_7 s_8_7 s_10_7 s_1_8 s_8_8 s_10_8 s_5_9 s_6_9 s_9_9 s_2_10 s_3_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_7_1 s_1_2 s_2_2 s_5_2 s_6_2 s_9_2 s_10_2 s_4_3 s_5_3 s_6_3 s_7_3 s_1_5 s_2_5 s_3_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_9_6 s_1_7 s_3_7 s_8_7 s_10_7 s_1_8 s_3_8 s_8_8 s_10_8 s_2_9 s_5_9 s_6_9 s_9_9 s_1_10 s_2_10 s_3_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_1_2 s_4_2 s_5_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_9_3 s_2_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_8_5 s_10_5 s_5_6 s_6_6 s_9_6 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_2_9 s_3_9 s_6_9 s_7_9 s_1_10 s_2_10 s_3_10 s_4_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_10_1 s_1_2 s_4_2 s_5_2 s_8_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_2_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_6_5 s_8_5 s_10_5 s_6_6 s_9_6 s_4_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_3_9 s_6_9 s_7_9 s_2_10 s_3_10 s_4_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_7_1 s_1_2 s_5_2 s_9_2 s_10_2 s_1_3 s_4_3 s_5_3 s_6_3 s_8_4 s_2_5 s_3_5 s_8_5 s_10_5 s_5_6 s_6_6 s_9_6 s_10_6 s_1_7 s_8_7 s_10_7 s_1_8 s_3_8 s_4_8 s_8_8 s_2_9 s_6_9 s_1_10 s_2_10 s_3_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_7_1 s_10_1 s_1_2 s_5_2 s_10_2 s_1_3 s_4_3 s_5_3 s_6_3 s_8_3 s_7_4 s_8_4 s_2_5 s_3_5 s_6_5 s_8_5 s_10_5 s_6_6 s_9_6 s_10_6 s_1_7 s_4_7 s_8_7 s_10_7 s_1_8 s_4_8 s_8_8 s_6_9 s_2_10 s_3_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_1_2 s_2_2 s_4_2 s_5_2 s_8_2 s_9_2 s_10_2 s_1_3 s_4_3 s_5_3 s_6_3 s_9_3 s_2_4 s_4_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_8_5 s_10_5 s_5_6 s_9_6 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_8_8 s_2_9 s_3_9 s_6_9 s_7_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_8_2 s_10_2 s_1_3 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_2_4 s_4_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_8_5 s_10_5 s_9_6 s_4_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_8_8 s_3_9 s_6_9 s_7_9 s_9_9 s_2_10 s_3_10 s_4_10 s_9_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_10_1 s_1_2 s_2_2 s_5_2 s_8_2 s_10_2 s_4_3 s_6_3 s_8_3 s_9_3 s_4_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_8_6 s_9_6 s_4_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_4_8 s_6_8 s_8_8 s_10_8 s_3_9 s_6_9 s_9_9 s_2_10 s_3_10 s_4_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_1_2 s_2_2 s_5_2 s_8_2 s_9_2 s_10_2 s_4_3 s_6_3 s_9_3 s_4_4 s_6_4 s_8_4 s_10_4 s_1_5 s_2_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_8_6 s_9_6 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_8_8 s_10_8 s_2_9 s_3_9 s_6_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_10_1 s_1_2 s_5_2 s_8_2 s_10_2 s_3_3 s_4_3 s_6_3 s_8_3 s_9_3 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_8_6 s_9_6 s_4_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_4_8 s_6_8 s_10_8 s_3_9 s_6_9 s_2_10 s_3_10 s_4_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_1_2 s_5_2 s_8_2 s_9_2 s_10_2 s_3_3 s_4_3 s_6_3 s_9_3 s_6_4 s_8_4 s_10_4 s_1_5 s_2_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_10_8 s_2_9 s_3_9 s_6_9 s_1_10 s_2_10 s_3_10 s_4_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_7_1 s_10_1 s_1_2 s_2_2 s_5_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_4_4 s_7_4 s_8_4 s_2_5 s_3_5 s_5_5 s_6_5 s_8_5 s_10_5 s_9_6 s_10_6 s_1_7 s_4_7 s_7_7 s_8_7 s_10_7 s_1_8 s_4_8 s_6_9 s_9_9 s_2_10 s_3_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_7_1 s_1_2 s_2_2 s_5_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_4_4 s_8_4 s_2_5 s_3_5 s_5_5 s_8_5 s_10_5 s_5_6 s_9_6 s_10_6 s_1_7 s_7_7 s_8_7 s_10_7 s_1_8 s_3_8 s_4_8 s_2_9 s_6_9 s_9_9 s_1_10 s_2_10 s_3_10 s_6_10 s_9_10 )))

(assert (not (and s_4_1 s_6_1 s_7_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_10_2 s_3_3 s_4_3 s_6_3 s_8_3 s_2_4 s_4_4 s_6_4 s_7_4 s_8_4 s_2_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_8_6 s_9_6 s_10_6 s_1_7 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_4_8 s_6_8 s_10_8 s_6_9 s_7_9 s_9_9 s_2_10 s_3_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_6_1 s_7_1 s_1_2 s_2_2 s_4_2 s_5_2 s_9_2 s_10_2 s_3_3 s_4_3 s_6_3 s_2_4 s_4_4 s_6_4 s_8_4 s_2_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_8_6 s_9_6 s_10_6 s_1_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_6_8 s_10_8 s_2_9 s_6_9 s_7_9 s_9_9 s_1_10 s_2_10 s_3_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_7_1 s_10_1 s_1_2 s_4_2 s_5_2 s_10_2 s_4_3 s_6_3 s_8_3 s_2_4 s_6_4 s_7_4 s_8_4 s_2_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_4_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_4_8 s_6_8 s_8_8 s_10_8 s_6_9 s_7_9 s_2_10 s_3_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_7_1 s_1_2 s_4_2 s_5_2 s_9_2 s_10_2 s_4_3 s_6_3 s_2_4 s_6_4 s_8_4 s_2_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_6_8 s_8_8 s_10_8 s_2_9 s_6_9 s_7_9 s_1_10 s_2_10 s_3_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_9_1 s_10_1 s_1_2 s_2_2 s_5_2 s_6_2 s_1_3 s_3_3 s_4_3 s_6_3 s_8_3 s_7_4 s_8_4 s_10_4 s_2_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_8_6 s_9_6 s_10_6 s_4_7 s_8_7 s_10_7 s_1_8 s_4_8 s_6_8 s_1_9 s_5_9 s_6_9 s_9_9 s_3_10 s_4_10 s_6_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_9_1 s_10_1 s_1_2 s_5_2 s_6_2 s_1_3 s_4_3 s_6_3 s_8_3 s_4_4 s_7_4 s_8_4 s_10_4 s_2_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_8_6 s_9_6 s_10_6 s_4_7 s_7_7 s_8_7 s_10_7 s_1_8 s_4_8 s_6_8 s_8_8 s_1_9 s_5_9 s_6_9 s_3_10 s_4_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_9_1 s_1_2 s_2_2 s_5_2 s_6_2 s_9_2 s_1_3 s_3_3 s_4_3 s_6_3 s_8_4 s_10_4 s_2_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_8_6 s_9_6 s_10_6 s_8_7 s_10_7 s_1_8 s_3_8 s_4_8 s_6_8 s_1_9 s_2_9 s_5_9 s_6_9 s_9_9 s_1_10 s_3_10 s_4_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_8_2 s_1_3 s_4_3 s_6_3 s_8_3 s_9_3 s_2_4 s_7_4 s_8_4 s_1_5 s_2_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_8_6 s_9_6 s_1_7 s_4_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_6_8 s_8_8 s_1_9 s_3_9 s_5_9 s_6_9 s_7_9 s_9_9 s_3_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_1_3 s_4_3 s_6_3 s_9_3 s_2_4 s_8_4 s_1_5 s_2_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_8_6 s_9_6 s_1_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_8_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_9_9 s_1_10 s_3_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_4_2 s_5_2 s_6_2 s_8_2 s_1_3 s_3_3 s_4_3 s_6_3 s_8_3 s_9_3 s_2_4 s_4_4 s_7_4 s_8_4 s_1_5 s_2_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_8_6 s_9_6 s_1_7 s_4_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_6_8 s_1_9 s_3_9 s_5_9 s_6_9 s_7_9 s_3_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_9_1 s_1_2 s_5_2 s_6_2 s_9_2 s_1_3 s_4_3 s_6_3 s_4_4 s_8_4 s_10_4 s_2_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_7_7 s_8_7 s_10_7 s_1_8 s_3_8 s_4_8 s_6_8 s_8_8 s_1_9 s_2_9 s_5_9 s_6_9 s_1_10 s_3_10 s_4_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_1_2 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_1_3 s_3_3 s_4_3 s_6_3 s_9_3 s_2_4 s_4_4 s_8_4 s_1_5 s_2_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_8_6 s_9_6 s_1_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_1_10 s_3_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_1_2 s_2_2 s_5_2 s_6_2 s_8_2 s_9_2 s_4_3 s_5_3 s_6_3 s_9_3 s_6_4 s_8_4 s_1_5 s_2_5 s_3_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_9_6 s_1_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_9_9 s_1_10 s_3_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_5_2 s_6_2 s_8_2 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_6_4 s_7_4 s_8_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_9_6 s_1_7 s_4_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_4_8 s_8_8 s_10_8 s_1_9 s_3_9 s_5_9 s_6_9 s_9_9 s_3_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_1_2 s_5_2 s_6_2 s_8_2 s_9_2 s_3_3 s_4_3 s_5_3 s_6_3 s_9_3 s_4_4 s_6_4 s_8_4 s_1_5 s_2_5 s_3_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_9_6 s_1_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_1_10 s_3_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_5_2 s_6_2 s_8_2 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_4_4 s_6_4 s_7_4 s_8_4 s_1_5 s_2_5 s_3_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_9_6 s_1_7 s_4_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_4_8 s_10_8 s_1_9 s_3_9 s_5_9 s_6_9 s_3_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_6_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_2_4 s_6_4 s_7_4 s_8_4 s_10_4 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_9_6 s_10_6 s_4_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_4_8 s_10_8 s_1_9 s_5_9 s_6_9 s_7_9 s_9_9 s_3_10 s_4_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_6_1 s_9_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_9_2 s_3_3 s_4_3 s_5_3 s_6_3 s_2_4 s_6_4 s_8_4 s_10_4 s_2_5 s_3_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_9_6 s_10_6 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_10_8 s_1_9 s_2_9 s_5_9 s_6_9 s_7_9 s_9_9 s_1_10 s_3_10 s_4_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_9_1 s_1_2 s_4_2 s_5_2 s_6_2 s_9_2 s_4_3 s_5_3 s_6_3 s_2_4 s_4_4 s_6_4 s_8_4 s_10_4 s_2_5 s_3_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_9_6 s_10_6 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_8_8 s_10_8 s_1_9 s_2_9 s_5_9 s_6_9 s_7_9 s_1_10 s_3_10 s_4_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_9_1 s_10_1 s_1_2 s_4_2 s_5_2 s_6_2 s_4_3 s_5_3 s_6_3 s_8_3 s_2_4 s_4_4 s_6_4 s_7_4 s_8_4 s_10_4 s_2_5 s_3_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_9_6 s_10_6 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_4_8 s_8_8 s_10_8 s_1_9 s_5_9 s_6_9 s_7_9 s_3_10 s_4_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_7_1 s_10_1 s_1_2 s_5_2 s_6_2 s_10_2 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_4_4 s_7_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_9_6 s_1_7 s_3_7 s_4_7 s_7_7 s_8_7 s_10_7 s_1_8 s_10_8 s_5_9 s_6_9 s_2_10 s_3_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_7_1 s_1_2 s_5_2 s_6_2 s_9_2 s_10_2 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_4_4 s_1_5 s_2_5 s_3_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_9_6 s_1_7 s_3_7 s_7_7 s_8_7 s_10_7 s_1_8 s_3_8 s_10_8 s_2_9 s_5_9 s_6_9 s_1_10 s_2_10 s_3_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_1_2 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_2_4 s_4_4 s_10_4 s_2_5 s_3_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_9_6 s_10_6 s_3_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_8_8 s_10_8 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_10_1 s_1_2 s_4_2 s_5_2 s_6_2 s_8_2 s_10_2 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_2_4 s_4_4 s_7_4 s_10_4 s_2_5 s_3_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_9_6 s_10_6 s_3_7 s_4_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_8_8 s_10_8 s_3_9 s_5_9 s_6_9 s_7_9 s_2_10 s_3_10 s_4_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_1_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_4_3 s_6_3 s_7_3 s_9_3 s_4_4 s_6_4 s_10_4 s_2_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_8_6 s_9_6 s_10_6 s_3_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_6_8 s_8_8 s_2_9 s_3_9 s_5_9 s_6_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_10_1 s_1_2 s_5_2 s_6_2 s_8_2 s_10_2 s_1_3 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_4_4 s_6_4 s_7_4 s_10_4 s_2_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_8_6 s_9_6 s_10_6 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_6_8 s_8_8 s_3_9 s_5_9 s_6_9 s_2_10 s_3_10 s_4_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_1_2 s_2_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_6_3 s_7_3 s_9_3 s_6_4 s_10_4 s_2_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_3_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_6_8 s_2_9 s_3_9 s_5_9 s_6_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_10_1 s_1_2 s_2_2 s_5_2 s_6_2 s_8_2 s_10_2 s_1_3 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_6_4 s_7_4 s_10_4 s_2_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_8_6 s_9_6 s_10_6 s_3_7 s_4_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_6_8 s_3_9 s_5_9 s_6_9 s_9_9 s_2_10 s_3_10 s_4_10 s_6_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_1_2 s_4_2 s_5_2 s_6_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_6_3 s_7_3 s_2_4 s_4_4 s_6_4 s_1_5 s_2_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_8_6 s_9_6 s_1_7 s_3_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_6_8 s_2_9 s_5_9 s_6_9 s_7_9 s_1_10 s_2_10 s_3_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_10_1 s_1_2 s_4_2 s_5_2 s_6_2 s_10_2 s_1_3 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_2_4 s_4_4 s_6_4 s_7_4 s_1_5 s_2_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_8_6 s_9_6 s_1_7 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_6_8 s_5_9 s_6_9 s_7_9 s_2_10 s_3_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_10_2 s_1_3 s_4_3 s_6_3 s_7_3 s_8_3 s_2_4 s_6_4 s_7_4 s_1_5 s_2_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_8_6 s_9_6 s_1_7 s_3_7 s_4_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_6_8 s_8_8 s_5_9 s_6_9 s_7_9 s_9_9 s_2_10 s_3_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_9_2 s_10_2 s_1_3 s_4_3 s_6_3 s_7_3 s_2_4 s_6_4 s_1_5 s_2_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_1_7 s_3_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_6_8 s_8_8 s_2_9 s_5_9 s_6_9 s_7_9 s_9_9 s_1_10 s_2_10 s_3_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_7_1 s_9_1 s_1_2 s_4_2 s_5_2 s_8_2 s_9_2 s_4_3 s_6_3 s_7_3 s_9_3 s_2_4 s_2_5 s_5_5 s_8_5 s_10_5 s_5_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_6_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_6_9 s_7_9 s_1_10 s_3_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_4_2 s_5_2 s_8_2 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_2_4 s_7_4 s_2_5 s_5_5 s_6_5 s_8_5 s_10_5 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_6_8 s_8_8 s_10_8 s_1_9 s_3_9 s_6_9 s_7_9 s_3_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_8_2 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_2_4 s_4_4 s_7_4 s_2_5 s_6_5 s_8_5 s_10_5 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_6_8 s_10_8 s_1_9 s_3_9 s_6_9 s_7_9 s_9_9 s_3_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_6_1 s_7_1 s_9_1 s_1_2 s_2_2 s_4_2 s_5_2 s_8_2 s_9_2 s_3_3 s_4_3 s_6_3 s_7_3 s_9_3 s_2_4 s_4_4 s_2_5 s_8_5 s_10_5 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_6_8 s_10_8 s_1_9 s_2_9 s_3_9 s_6_9 s_7_9 s_9_9 s_1_10 s_3_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_9_1 s_10_1 s_1_2 s_2_2 s_5_2 s_4_3 s_6_3 s_7_3 s_8_3 s_4_4 s_7_4 s_10_4 s_1_5 s_2_5 s_6_5 s_8_5 s_10_5 s_6_6 s_8_6 s_9_6 s_3_7 s_4_7 s_7_7 s_8_7 s_10_7 s_1_8 s_6_8 s_8_8 s_10_8 s_1_9 s_6_9 s_9_9 s_3_10 s_4_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_9_1 s_1_2 s_2_2 s_5_2 s_9_2 s_4_3 s_6_3 s_7_3 s_4_4 s_10_4 s_1_5 s_2_5 s_8_5 s_10_5 s_5_6 s_6_6 s_8_6 s_9_6 s_3_7 s_7_7 s_8_7 s_10_7 s_1_8 s_3_8 s_6_8 s_8_8 s_10_8 s_1_9 s_2_9 s_6_9 s_9_9 s_1_10 s_3_10 s_4_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_9_1 s_10_1 s_1_2 s_5_2 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_7_4 s_10_4 s_1_5 s_2_5 s_5_5 s_6_5 s_8_5 s_10_5 s_8_6 s_9_6 s_3_7 s_4_7 s_8_7 s_10_7 s_1_8 s_6_8 s_10_8 s_1_9 s_6_9 s_3_10 s_4_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_9_1 s_1_2 s_5_2 s_9_2 s_3_3 s_4_3 s_6_3 s_7_3 s_10_4 s_1_5 s_2_5 s_5_5 s_8_5 s_10_5 s_5_6 s_8_6 s_9_6 s_3_7 s_8_7 s_10_7 s_1_8 s_3_8 s_6_8 s_10_8 s_1_9 s_2_9 s_6_9 s_1_10 s_3_10 s_4_10 s_8_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_9_1 s_10_1 s_1_2 s_4_2 s_5_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_2_4 s_6_4 s_7_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_9_6 s_3_7 s_4_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_1_9 s_6_9 s_7_9 s_3_10 s_4_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_9_1 s_1_2 s_4_2 s_5_2 s_9_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_2_4 s_6_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_9_6 s_3_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_1_9 s_2_9 s_6_9 s_7_9 s_1_10 s_3_10 s_4_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_7_1 s_9_1 s_1_2 s_5_2 s_8_2 s_9_2 s_1_3 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_6_4 s_2_5 s_3_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_9_6 s_10_6 s_1_7 s_3_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_8_8 s_1_9 s_2_9 s_3_9 s_6_9 s_1_10 s_3_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_5_2 s_8_2 s_1_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_6_4 s_7_4 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_8_8 s_1_9 s_3_9 s_6_9 s_3_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_1_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_2_4 s_4_4 s_6_4 s_7_4 s_10_4 s_1_5 s_2_5 s_3_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_9_6 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_8_8 s_1_9 s_6_9 s_7_9 s_9_9 s_3_10 s_4_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_9_1 s_1_2 s_2_2 s_4_2 s_5_2 s_9_2 s_1_3 s_4_3 s_5_3 s_6_3 s_7_3 s_2_4 s_4_4 s_6_4 s_10_4 s_1_5 s_2_5 s_3_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_9_6 s_3_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_8_8 s_1_9 s_2_9 s_6_9 s_7_9 s_9_9 s_1_10 s_3_10 s_4_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_7_1 s_9_1 s_1_2 s_2_2 s_5_2 s_8_2 s_9_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_4_4 s_6_4 s_2_5 s_3_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_9_6 s_10_6 s_1_7 s_3_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_1_9 s_2_9 s_3_9 s_6_9 s_9_9 s_1_10 s_3_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_5_2 s_8_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_4_4 s_6_4 s_7_4 s_2_5 s_3_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_1_9 s_3_9 s_6_9 s_9_9 s_3_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_7_1 s_10_1 s_4_2 s_5_2 s_7_2 s_10_2 s_6_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_2_5 s_4_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_9_7 s_1_8 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_10_8 s_4_9 s_6_9 s_7_9 s_10_9 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_3_1 s_7_1 s_10_1 s_5_2 s_7_2 s_10_2 s_1_3 s_5_3 s_6_3 s_1_4 s_3_4 s_5_4 s_7_4 s_8_4 s_9_4 s_2_5 s_3_5 s_4_5 s_8_5 s_1_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_6_7 s_1_8 s_3_8 s_4_8 s_7_8 s_8_8 s_4_9 s_6_9 s_10_9 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_7_1 s_10_1 s_2_2 s_5_2 s_7_2 s_10_2 s_1_3 s_3_3 s_5_3 s_6_3 s_1_4 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_2_5 s_3_5 s_4_5 s_5_5 s_8_5 s_1_6 s_5_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_6_7 s_7_7 s_1_8 s_3_8 s_4_8 s_7_8 s_4_9 s_6_9 s_9_9 s_10_9 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_7_1 s_10_1 s_2_2 s_4_2 s_5_2 s_7_2 s_10_2 s_3_3 s_6_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_2_5 s_4_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_3_8 s_4_8 s_6_8 s_7_8 s_10_8 s_4_9 s_6_9 s_7_9 s_9_9 s_10_9 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_5_1 s_10_1 s_2_2 s_5_2 s_7_2 s_8_2 s_10_2 s_6_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_10_8 s_3_9 s_4_9 s_6_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_10_1 s_2_2 s_4_2 s_5_2 s_7_2 s_8_2 s_10_2 s_1_3 s_5_3 s_6_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_8_5 s_1_6 s_5_6 s_7_6 s_9_6 s_2_7 s_4_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_7_8 s_8_8 s_3_9 s_4_9 s_6_9 s_7_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_10_1 s_5_2 s_7_2 s_8_2 s_10_2 s_3_3 s_6_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_2_7 s_4_7 s_5_7 s_6_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_10_8 s_3_9 s_4_9 s_6_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_10_1 s_4_2 s_5_2 s_7_2 s_8_2 s_10_2 s_1_3 s_3_3 s_5_3 s_6_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_8_5 s_1_6 s_5_6 s_6_6 s_7_6 s_9_6 s_2_7 s_4_7 s_6_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_7_8 s_3_9 s_4_9 s_6_9 s_7_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_7_1 s_8_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_9_2 s_10_2 s_2_3 s_4_3 s_8_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_8_4 s_2_5 s_4_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_5_7 s_6_7 s_8_7 s_9_7 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_2_9 s_6_9 s_7_9 s_8_9 s_2_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_3_1 s_7_1 s_8_1 s_10_1 s_1_2 s_3_2 s_5_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_8_3 s_10_3 s_1_4 s_5_4 s_8_4 s_2_5 s_3_5 s_4_5 s_1_6 s_3_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_6_7 s_8_7 s_4_8 s_5_8 s_8_8 s_9_8 s_2_9 s_6_9 s_8_9 s_2_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_8_3 s_10_3 s_1_4 s_4_4 s_5_4 s_8_4 s_2_5 s_3_5 s_4_5 s_5_5 s_1_6 s_3_6 s_5_6 s_7_6 s_9_6 s_10_6 s_1_7 s_6_7 s_7_7 s_8_7 s_4_8 s_5_8 s_9_8 s_2_9 s_6_9 s_8_9 s_9_9 s_2_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_5_1 s_8_1 s_10_1 s_1_2 s_3_2 s_5_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_8_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_8_4 s_10_4 s_1_5 s_2_5 s_4_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_5_7 s_6_7 s_8_7 s_2_8 s_4_8 s_5_8 s_6_8 s_9_8 s_10_8 s_2_9 s_3_9 s_6_9 s_8_9 s_2_10 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_5_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_8_2 s_9_2 s_10_2 s_2_3 s_4_3 s_8_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_8_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_6_9 s_8_9 s_9_9 s_2_10 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_1_6 s_3_6 s_5_6 s_7_6 s_9_6 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_4_8 s_5_8 s_8_8 s_9_8 s_2_9 s_3_9 s_6_9 s_7_9 s_8_9 s_9_9 s_2_10 s_4_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_8_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_1_6 s_3_6 s_5_6 s_6_6 s_7_6 s_9_6 s_6_7 s_8_7 s_9_7 s_2_8 s_4_8 s_5_8 s_9_8 s_2_9 s_3_9 s_6_9 s_7_9 s_8_9 s_2_10 s_4_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_8_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_8_4 s_2_5 s_4_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_4_8 s_5_8 s_6_8 s_9_8 s_10_8 s_2_9 s_6_9 s_7_9 s_8_9 s_9_9 s_2_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_9_2 s_2_3 s_3_3 s_4_3 s_5_3 s_8_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_8_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_9_6 s_10_6 s_5_7 s_6_7 s_8_7 s_9_7 s_4_8 s_5_8 s_9_8 s_10_8 s_1_9 s_2_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_9_2 s_2_3 s_4_3 s_5_3 s_8_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_8_4 s_10_4 s_2_5 s_3_5 s_4_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_4_8 s_5_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_5_9 s_6_9 s_7_9 s_8_9 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_2_1 s_8_1 s_9_1 s_10_1 s_2_2 s_4_2 s_5_2 s_6_2 s_9_2 s_3_3 s_5_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_8_4 s_10_4 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_9_6 s_10_6 s_5_7 s_9_7 s_4_8 s_5_8 s_7_8 s_10_8 s_1_9 s_2_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_8_1 s_9_1 s_10_1 s_4_2 s_5_2 s_6_2 s_9_2 s_5_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_8_4 s_10_4 s_2_5 s_3_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_9_6 s_10_6 s_5_7 s_7_7 s_9_7 s_4_8 s_5_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_5_9 s_6_9 s_7_9 s_10_9 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_3_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_8_4 s_1_5 s_2_5 s_4_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_4_8 s_5_8 s_6_8 s_9_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_1_3 s_2_3 s_4_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_8_4 s_1_5 s_2_5 s_4_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_7_6 s_8_6 s_9_6 s_1_7 s_6_7 s_8_7 s_9_7 s_2_8 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_1_3 s_3_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_8_4 s_1_5 s_2_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_8_6 s_9_6 s_1_7 s_7_7 s_9_7 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_10_9 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_1_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_8_4 s_1_5 s_2_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_8_6 s_9_6 s_1_7 s_9_7 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_5_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_5_2 s_6_2 s_8_2 s_9_2 s_3_3 s_5_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_8_4 s_1_5 s_2_5 s_3_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_9_6 s_1_7 s_5_7 s_7_7 s_2_8 s_4_8 s_5_8 s_7_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_10_9 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_5_2 s_6_2 s_8_2 s_9_2 s_2_3 s_3_3 s_4_3 s_5_3 s_8_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_8_4 s_1_5 s_2_5 s_3_5 s_4_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_1_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_4_8 s_5_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_8_9 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_2_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_5_2 s_6_2 s_8_2 s_9_2 s_5_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_8_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_9_6 s_1_7 s_5_7 s_2_8 s_4_8 s_5_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_9_9 s_10_9 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_8_2 s_9_2 s_2_3 s_4_3 s_5_3 s_8_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_8_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_9_6 s_1_7 s_5_7 s_6_7 s_8_7 s_2_8 s_4_8 s_5_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_6_9 s_8_9 s_9_9 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_8_1 s_9_1 s_10_1 s_5_2 s_6_2 s_9_2 s_1_3 s_8_3 s_10_3 s_1_4 s_3_4 s_4_4 s_8_4 s_10_4 s_2_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_8_6 s_9_6 s_10_6 s_7_7 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_1_9 s_2_9 s_5_9 s_6_9 s_10_9 s_4_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_3_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_5_2 s_6_2 s_9_2 s_1_3 s_2_3 s_4_3 s_8_3 s_10_3 s_1_4 s_4_4 s_5_4 s_8_4 s_10_4 s_2_5 s_4_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_6_7 s_7_7 s_8_7 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_1_9 s_2_9 s_5_9 s_6_9 s_8_9 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_8_1 s_9_1 s_10_1 s_2_2 s_5_2 s_6_2 s_9_2 s_1_3 s_3_3 s_8_3 s_10_3 s_1_4 s_3_4 s_8_4 s_10_4 s_2_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_8_6 s_9_6 s_10_6 s_4_8 s_5_8 s_6_8 s_7_8 s_1_9 s_2_9 s_5_9 s_6_9 s_9_9 s_10_9 s_4_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_1_1 s_3_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_8_3 s_10_3 s_1_4 s_5_4 s_8_4 s_10_4 s_2_5 s_4_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_6_7 s_8_7 s_4_8 s_5_8 s_6_8 s_9_8 s_1_9 s_2_9 s_5_9 s_6_9 s_8_9 s_9_9 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_7_1 s_8_1 s_10_1 s_2_2 s_5_2 s_9_2 s_10_2 s_1_3 s_3_3 s_5_3 s_8_3 s_10_3 s_1_4 s_3_4 s_4_4 s_8_4 s_2_5 s_3_5 s_5_5 s_6_5 s_1_6 s_3_6 s_9_6 s_10_6 s_1_7 s_7_7 s_4_8 s_5_8 s_7_8 s_2_9 s_6_9 s_9_9 s_10_9 s_2_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_8_1 s_10_1 s_2_2 s_4_2 s_5_2 s_8_2 s_9_2 s_10_2 s_1_3 s_5_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_1_6 s_3_6 s_9_6 s_7_7 s_9_7 s_2_8 s_4_8 s_5_8 s_7_8 s_8_8 s_2_9 s_3_9 s_6_9 s_7_9 s_9_9 s_10_9 s_2_10 s_4_10 s_5_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_8_1 s_10_1 s_4_2 s_5_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_5_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_6_5 s_1_6 s_3_6 s_6_6 s_9_6 s_9_7 s_2_8 s_4_8 s_5_8 s_7_8 s_2_9 s_3_9 s_6_9 s_7_9 s_10_9 s_2_10 s_4_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_7_1 s_8_1 s_10_1 s_5_2 s_9_2 s_10_2 s_1_3 s_5_3 s_8_3 s_10_3 s_1_4 s_3_4 s_8_4 s_2_5 s_3_5 s_6_5 s_1_6 s_3_6 s_6_6 s_9_6 s_10_6 s_1_7 s_4_8 s_5_8 s_7_8 s_8_8 s_2_9 s_6_9 s_10_9 s_2_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_8_1 s_10_1 s_5_2 s_8_2 s_9_2 s_10_2 s_3_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_8_4 s_10_4 s_1_5 s_2_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_8_6 s_9_6 s_5_7 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_10_8 s_2_9 s_3_9 s_6_9 s_10_9 s_2_10 s_4_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_8_1 s_10_1 s_2_2 s_5_2 s_8_2 s_9_2 s_10_2 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_8_4 s_10_4 s_1_5 s_2_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_8_6 s_9_6 s_5_7 s_7_7 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_2_9 s_3_9 s_6_9 s_9_9 s_10_9 s_2_10 s_4_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_7_1 s_8_1 s_10_1 s_2_2 s_4_2 s_5_2 s_9_2 s_10_2 s_3_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_8_4 s_2_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_8_6 s_9_6 s_10_6 s_1_7 s_5_7 s_7_7 s_9_7 s_4_8 s_5_8 s_6_8 s_7_8 s_10_8 s_2_9 s_6_9 s_7_9 s_9_9 s_10_9 s_2_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_7_1 s_8_1 s_10_1 s_4_2 s_5_2 s_9_2 s_10_2 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_8_4 s_2_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_5_7 s_9_7 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_2_9 s_6_9 s_7_9 s_10_9 s_2_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_3_1 s_5_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_7_6 s_9_6 s_3_7 s_5_7 s_6_7 s_8_7 s_9_7 s_5_8 s_9_8 s_1_9 s_2_9 s_6_9 s_7_9 s_8_9 s_4_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_9_2 s_1_3 s_2_3 s_4_3 s_5_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_5_8 s_8_8 s_9_8 s_1_9 s_2_9 s_6_9 s_7_9 s_8_9 s_9_9 s_4_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_8_1 s_9_1 s_10_1 s_2_2 s_4_2 s_5_2 s_9_2 s_1_3 s_5_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_10_4 s_1_5 s_2_5 s_3_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_9_6 s_3_7 s_5_7 s_7_7 s_9_7 s_5_8 s_7_8 s_8_8 s_1_9 s_2_9 s_6_9 s_7_9 s_9_9 s_10_9 s_4_10 s_5_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_8_1 s_9_1 s_10_1 s_4_2 s_5_2 s_9_2 s_1_3 s_3_3 s_5_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_9_6 s_3_7 s_5_7 s_9_7 s_5_8 s_7_8 s_1_9 s_2_9 s_6_9 s_7_9 s_10_9 s_4_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_5_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_9_2 s_2_3 s_4_3 s_7_3 s_8_3 s_10_3 s_1_4 s_4_4 s_5_4 s_10_4 s_1_5 s_2_5 s_4_5 s_1_6 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_3_7 s_6_7 s_7_7 s_8_7 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_6_9 s_8_9 s_9_9 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_5_2 s_9_2 s_2_3 s_3_3 s_4_3 s_7_3 s_8_3 s_10_3 s_1_4 s_5_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_1_6 s_3_6 s_5_6 s_7_6 s_8_6 s_9_6 s_3_7 s_6_7 s_8_7 s_5_8 s_6_8 s_9_8 s_10_8 s_1_9 s_2_9 s_6_9 s_8_9 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_8_1 s_9_1 s_10_1 s_5_2 s_9_2 s_3_3 s_7_3 s_8_3 s_10_3 s_1_4 s_3_4 s_10_4 s_1_5 s_2_5 s_5_5 s_6_5 s_1_6 s_3_6 s_8_6 s_9_6 s_3_7 s_5_8 s_6_8 s_7_8 s_10_8 s_1_9 s_2_9 s_6_9 s_10_9 s_4_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_8_1 s_9_1 s_10_1 s_2_2 s_5_2 s_9_2 s_7_3 s_8_3 s_10_3 s_1_4 s_3_4 s_4_4 s_10_4 s_1_5 s_2_5 s_6_5 s_1_6 s_3_6 s_6_6 s_8_6 s_9_6 s_3_7 s_7_7 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_6_9 s_9_9 s_10_9 s_4_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_7_1 s_8_1 s_9_1 s_10_1 s_5_2 s_8_2 s_9_2 s_1_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_9_6 s_10_6 s_1_7 s_3_7 s_5_7 s_2_8 s_5_8 s_7_8 s_8_8 s_1_9 s_2_9 s_3_9 s_6_9 s_10_9 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_5_2 s_8_2 s_9_2 s_1_3 s_3_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_2_5 s_3_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_9_6 s_10_6 s_1_7 s_3_7 s_5_7 s_7_7 s_2_8 s_5_8 s_7_8 s_1_9 s_2_9 s_3_9 s_6_9 s_9_9 s_10_9 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_3_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_5_2 s_8_2 s_9_2 s_1_3 s_2_3 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_7_6 s_9_6 s_10_6 s_1_7 s_3_7 s_5_7 s_6_7 s_8_7 s_2_8 s_5_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_6_9 s_8_9 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_2_5 s_3_5 s_4_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_5_8 s_9_8 s_1_9 s_2_9 s_3_9 s_6_9 s_8_9 s_9_9 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_7_1 s_8_1 s_9_1 s_10_1 s_4_2 s_5_2 s_8_2 s_9_2 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_2_5 s_5_5 s_6_5 s_1_6 s_3_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_9_7 s_2_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_6_9 s_7_9 s_10_9 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_4_2 s_5_2 s_8_2 s_9_2 s_3_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_2_5 s_6_5 s_1_6 s_3_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_7_7 s_9_7 s_2_8 s_5_8 s_6_8 s_7_8 s_10_8 s_1_9 s_2_9 s_3_9 s_6_9 s_7_9 s_9_9 s_10_9 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_8_2 s_9_2 s_2_3 s_4_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_2_5 s_4_5 s_5_5 s_1_6 s_3_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_6_7 s_8_7 s_9_7 s_2_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_6_9 s_7_9 s_8_9 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_8_2 s_9_2 s_2_3 s_3_3 s_4_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_2_5 s_4_5 s_1_6 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_5_8 s_6_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_6_9 s_7_9 s_8_9 s_9_9 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_7_1 s_8_1 s_10_1 s_4_2 s_5_2 s_6_2 s_9_2 s_10_2 s_1_3 s_3_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_1_5 s_2_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_8_6 s_9_6 s_1_7 s_3_7 s_5_7 s_7_7 s_9_7 s_5_8 s_6_8 s_7_8 s_2_9 s_5_9 s_6_9 s_7_9 s_10_9 s_2_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_8_1 s_10_1 s_2_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_10_4 s_2_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_8_6 s_9_6 s_10_6 s_3_7 s_5_7 s_2_8 s_5_8 s_6_8 s_7_8 s_2_9 s_3_9 s_5_9 s_6_9 s_9_9 s_10_9 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_8_1 s_10_1 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_10_4 s_2_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_8_6 s_9_6 s_10_6 s_3_7 s_5_7 s_7_7 s_2_8 s_5_8 s_6_8 s_7_8 s_8_8 s_2_9 s_3_9 s_5_9 s_6_9 s_10_9 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_7_1 s_8_1 s_10_1 s_2_2 s_4_2 s_5_2 s_6_2 s_9_2 s_10_2 s_1_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_1_5 s_2_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_8_6 s_9_6 s_1_7 s_3_7 s_5_7 s_9_7 s_5_8 s_6_8 s_7_8 s_8_8 s_2_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_2_10 s_5_10 s_7_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_1_5 s_2_5 s_4_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_3_7 s_5_7 s_6_7 s_8_7 s_9_7 s_5_8 s_6_8 s_8_8 s_9_8 s_2_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_2_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_7_1 s_8_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_1_5 s_2_5 s_4_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_1_7 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_5_8 s_6_8 s_9_8 s_2_9 s_5_9 s_6_9 s_7_9 s_8_9 s_2_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_10_4 s_2_5 s_4_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_3_7 s_5_7 s_6_7 s_8_7 s_2_8 s_5_8 s_6_8 s_9_8 s_2_9 s_3_9 s_5_9 s_6_9 s_8_9 s_9_9 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_8_1 s_10_1 s_1_2 s_3_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_10_4 s_2_5 s_4_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_5_8 s_6_8 s_8_8 s_9_8 s_2_9 s_3_9 s_5_9 s_6_9 s_8_9 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_10_4 s_2_5 s_3_5 s_4_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_3_7 s_6_7 s_8_7 s_9_7 s_2_8 s_5_8 s_9_8 s_10_8 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_8_1 s_10_1 s_2_2 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_3_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_10_4 s_2_5 s_3_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_9_6 s_10_6 s_3_7 s_9_7 s_2_8 s_5_8 s_7_8 s_10_8 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_8_1 s_10_1 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_10_4 s_2_5 s_3_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_9_6 s_10_6 s_3_7 s_7_7 s_9_7 s_2_8 s_5_8 s_7_8 s_8_8 s_10_8 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_10_9 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_8_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_9_2 s_10_2 s_2_3 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_7_6 s_9_6 s_10_6 s_3_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_5_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_2_1 s_5_1 s_7_1 s_8_1 s_10_1 s_2_2 s_5_2 s_6_2 s_9_2 s_10_2 s_5_3 s_7_3 s_8_3 s_10_3 s_1_4 s_3_4 s_1_5 s_2_5 s_3_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_9_6 s_1_7 s_3_7 s_5_8 s_7_8 s_8_8 s_10_8 s_2_9 s_5_9 s_6_9 s_9_9 s_10_9 s_2_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_7_1 s_8_1 s_10_1 s_5_2 s_6_2 s_9_2 s_10_2 s_3_3 s_5_3 s_7_3 s_8_3 s_10_3 s_1_4 s_3_4 s_4_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_9_6 s_1_7 s_3_7 s_7_7 s_5_8 s_7_8 s_10_8 s_2_9 s_5_9 s_6_9 s_10_9 s_2_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_5_1 s_7_1 s_8_1 s_10_1 s_1_2 s_3_2 s_5_2 s_6_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_10_3 s_1_4 s_4_4 s_5_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_7_6 s_9_6 s_1_7 s_3_7 s_6_7 s_7_7 s_8_7 s_5_8 s_9_8 s_10_8 s_2_9 s_5_9 s_6_9 s_8_9 s_2_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_5_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_9_2 s_10_2 s_2_3 s_4_3 s_5_3 s_7_3 s_8_3 s_10_3 s_1_4 s_5_4 s_1_5 s_2_5 s_3_5 s_4_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_9_6 s_1_7 s_3_7 s_6_7 s_8_7 s_5_8 s_8_8 s_9_8 s_10_8 s_2_9 s_5_9 s_6_9 s_8_9 s_9_9 s_2_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_8_1 s_10_1 s_2_2 s_3_2 s_4_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_7_4 s_8_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_8_5 s_1_6 s_5_6 s_7_6 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_4_8 s_8_8 s_9_8 s_2_9 s_3_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_4_10 s_5_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_8_1 s_10_1 s_3_2 s_4_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_2_4 s_5_4 s_7_4 s_8_4 s_10_4 s_1_5 s_3_5 s_4_5 s_8_5 s_1_6 s_5_6 s_6_6 s_7_6 s_4_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_4_8 s_9_8 s_2_9 s_3_9 s_7_9 s_8_9 s_10_9 s_2_10 s_4_10 s_5_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_8_1 s_10_1 s_3_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_6_3 s_8_3 s_9_3 s_10_3 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_4_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_4_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_4_8 s_6_8 s_9_8 s_10_8 s_2_9 s_3_9 s_8_9 s_10_9 s_2_10 s_4_10 s_5_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_8_1 s_10_1 s_2_2 s_3_2 s_8_2 s_9_2 s_10_2 s_2_3 s_4_3 s_6_3 s_8_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_4_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_7_6 s_8_6 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_4_8 s_6_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_8_9 s_9_9 s_10_9 s_2_10 s_4_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_7_1 s_8_1 s_10_1 s_2_2 s_3_2 s_4_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_6_3 s_8_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_4_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_7_6 s_8_6 s_10_6 s_1_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_4_8 s_6_8 s_9_8 s_10_8 s_2_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_7_1 s_8_1 s_10_1 s_3_2 s_4_2 s_9_2 s_10_2 s_2_3 s_4_3 s_6_3 s_8_3 s_10_3 s_2_4 s_5_4 s_6_4 s_7_4 s_8_4 s_4_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_4_8 s_6_8 s_8_8 s_9_8 s_10_8 s_2_9 s_7_9 s_8_9 s_10_9 s_2_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_7_1 s_8_1 s_10_1 s_3_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_8_3 s_10_3 s_5_4 s_7_4 s_8_4 s_3_5 s_4_5 s_8_5 s_1_6 s_5_6 s_6_6 s_7_6 s_10_6 s_1_7 s_4_7 s_6_7 s_8_7 s_10_7 s_4_8 s_8_8 s_9_8 s_2_9 s_8_9 s_10_9 s_2_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_7_1 s_8_1 s_10_1 s_2_2 s_3_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_10_3 s_4_4 s_5_4 s_7_4 s_8_4 s_3_5 s_4_5 s_5_5 s_8_5 s_1_6 s_5_6 s_7_6 s_10_6 s_1_7 s_4_7 s_6_7 s_7_7 s_8_7 s_10_7 s_4_8 s_9_8 s_2_9 s_8_9 s_9_9 s_10_9 s_2_10 s_5_10 s_6_10 )))

(assert (not (and s_4_1 s_5_1 s_8_1 s_10_1 s_1_2 s_2_2 s_8_2 s_9_2 s_10_2 s_6_3 s_8_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_8_6 s_4_7 s_5_7 s_7_7 s_10_7 s_2_8 s_4_8 s_6_8 s_7_8 s_8_8 s_10_8 s_2_9 s_3_9 s_9_9 s_2_10 s_4_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_8_1 s_10_1 s_1_2 s_8_2 s_9_2 s_10_2 s_3_3 s_6_3 s_8_3 s_9_3 s_10_3 s_3_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_8_6 s_4_7 s_5_7 s_10_7 s_2_8 s_4_8 s_6_8 s_7_8 s_10_8 s_2_9 s_3_9 s_2_10 s_4_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_7_1 s_8_1 s_10_1 s_1_2 s_4_2 s_9_2 s_10_2 s_6_3 s_8_3 s_10_3 s_2_4 s_3_4 s_6_4 s_7_4 s_8_4 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_8_6 s_10_6 s_1_7 s_4_7 s_5_7 s_9_7 s_10_7 s_4_8 s_6_8 s_7_8 s_8_8 s_10_8 s_2_9 s_7_9 s_2_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_4_2 s_9_2 s_10_2 s_3_3 s_6_3 s_8_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_8_6 s_10_6 s_1_7 s_4_7 s_5_7 s_7_7 s_9_7 s_10_7 s_4_8 s_6_8 s_7_8 s_10_8 s_2_9 s_7_9 s_9_9 s_2_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_7_1 s_8_1 s_10_1 s_1_2 s_9_2 s_10_2 s_1_3 s_5_3 s_6_3 s_8_3 s_10_3 s_3_4 s_7_4 s_8_4 s_3_5 s_6_5 s_8_5 s_1_6 s_6_6 s_10_6 s_1_7 s_4_7 s_10_7 s_4_8 s_7_8 s_8_8 s_2_9 s_2_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_9_2 s_10_2 s_1_3 s_3_3 s_5_3 s_6_3 s_8_3 s_10_3 s_3_4 s_4_4 s_7_4 s_8_4 s_3_5 s_5_5 s_6_5 s_8_5 s_1_6 s_10_6 s_1_7 s_4_7 s_7_7 s_10_7 s_4_8 s_7_8 s_2_9 s_9_9 s_2_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_8_1 s_10_1 s_1_2 s_2_2 s_4_2 s_8_2 s_9_2 s_10_2 s_1_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_7_4 s_8_4 s_10_4 s_1_5 s_3_5 s_5_5 s_6_5 s_8_5 s_1_6 s_4_7 s_7_7 s_9_7 s_10_7 s_2_8 s_4_8 s_7_8 s_8_8 s_2_9 s_3_9 s_7_9 s_9_9 s_2_10 s_4_10 s_5_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_8_1 s_10_1 s_1_2 s_4_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_7_4 s_8_4 s_10_4 s_1_5 s_3_5 s_6_5 s_8_5 s_1_6 s_6_6 s_4_7 s_9_7 s_10_7 s_2_8 s_4_8 s_7_8 s_2_9 s_3_9 s_7_9 s_2_10 s_4_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_6_2 s_8_2 s_9_2 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_3_4 s_6_4 s_7_4 s_8_4 s_1_5 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_1_7 s_4_7 s_5_7 s_10_7 s_2_8 s_4_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_9_9 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_6_2 s_8_2 s_9_2 s_3_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_1_5 s_3_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_1_7 s_4_7 s_5_7 s_7_7 s_10_7 s_2_8 s_4_8 s_7_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_3_2 s_6_2 s_8_2 s_9_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_1_5 s_3_5 s_4_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_1_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_4_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_8_9 s_10_9 s_5_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_6_2 s_8_2 s_9_2 s_2_3 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_5_4 s_6_4 s_7_4 s_8_4 s_1_5 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_7_6 s_1_7 s_4_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_4_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_8_9 s_9_9 s_10_9 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_6_2 s_9_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_10_3 s_2_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_7_6 s_10_6 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_4_8 s_9_8 s_10_8 s_1_9 s_2_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_4_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_8_1 s_9_1 s_10_1 s_3_2 s_4_2 s_6_2 s_9_2 s_2_3 s_4_3 s_5_3 s_6_3 s_8_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_3_5 s_4_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_10_6 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_4_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_5_9 s_7_9 s_8_9 s_10_9 s_4_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_4_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_6_2 s_9_2 s_3_3 s_5_3 s_6_3 s_8_3 s_10_3 s_2_4 s_3_4 s_6_4 s_7_4 s_8_4 s_10_4 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_10_6 s_4_7 s_5_7 s_9_7 s_10_7 s_4_8 s_7_8 s_10_8 s_1_9 s_2_9 s_5_9 s_7_9 s_9_9 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_8_1 s_9_1 s_10_1 s_1_2 s_4_2 s_6_2 s_9_2 s_5_3 s_6_3 s_8_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_10_4 s_3_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_10_6 s_4_7 s_5_7 s_7_7 s_9_7 s_10_7 s_4_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_5_9 s_7_9 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_6_2 s_9_2 s_1_3 s_3_3 s_6_3 s_8_3 s_10_3 s_3_4 s_7_4 s_8_4 s_10_4 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_8_6 s_10_6 s_4_7 s_10_7 s_4_8 s_6_8 s_7_8 s_1_9 s_2_9 s_5_9 s_9_9 s_4_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_8_1 s_9_1 s_10_1 s_1_2 s_6_2 s_9_2 s_1_3 s_6_3 s_8_3 s_10_3 s_3_4 s_4_4 s_7_4 s_8_4 s_10_4 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_8_6 s_10_6 s_4_7 s_7_7 s_10_7 s_4_8 s_6_8 s_7_8 s_8_8 s_1_9 s_2_9 s_5_9 s_4_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_6_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_8_3 s_10_3 s_5_4 s_7_4 s_8_4 s_10_4 s_4_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_7_6 s_8_6 s_10_6 s_4_7 s_6_7 s_8_7 s_10_7 s_4_8 s_6_8 s_9_8 s_1_9 s_2_9 s_5_9 s_8_9 s_9_9 s_10_9 s_4_10 s_5_10 s_6_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_8_1 s_9_1 s_10_1 s_3_2 s_6_2 s_9_2 s_1_3 s_2_3 s_4_3 s_6_3 s_8_3 s_10_3 s_4_4 s_5_4 s_7_4 s_8_4 s_10_4 s_4_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_4_7 s_6_7 s_7_7 s_8_7 s_10_7 s_4_8 s_6_8 s_8_8 s_9_8 s_1_9 s_2_9 s_5_9 s_8_9 s_10_9 s_4_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_4_2 s_6_2 s_8_2 s_9_2 s_1_3 s_3_3 s_6_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_7_4 s_8_4 s_1_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_8_6 s_1_7 s_4_7 s_7_7 s_9_7 s_10_7 s_2_8 s_4_8 s_6_8 s_7_8 s_1_9 s_2_9 s_3_9 s_5_9 s_7_9 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_6_2 s_8_2 s_9_2 s_1_3 s_6_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_7_4 s_8_4 s_1_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_8_6 s_1_7 s_4_7 s_9_7 s_10_7 s_2_8 s_4_8 s_6_8 s_7_8 s_8_8 s_1_9 s_2_9 s_3_9 s_5_9 s_7_9 s_9_9 s_5_10 s_9_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_3_2 s_4_2 s_6_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_8_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_7_4 s_8_4 s_1_5 s_4_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_7_6 s_8_6 s_1_7 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_4_8 s_6_8 s_9_8 s_1_9 s_2_9 s_3_9 s_5_9 s_7_9 s_8_9 s_10_9 s_5_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_6_2 s_8_2 s_9_2 s_1_3 s_2_3 s_4_3 s_6_3 s_8_3 s_9_3 s_10_3 s_2_4 s_5_4 s_7_4 s_8_4 s_1_5 s_4_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_7_6 s_8_6 s_1_7 s_4_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_4_8 s_6_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_5_10 )))

(assert (not (and s_4_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_8_2 s_9_2 s_3_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_7_4 s_6_5 s_8_5 s_1_6 s_6_6 s_8_6 s_10_6 s_1_7 s_3_7 s_4_7 s_7_7 s_9_7 s_10_7 s_2_8 s_6_8 s_7_8 s_10_8 s_1_9 s_2_9 s_3_9 s_7_9 s_9_9 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_8_2 s_9_2 s_1_3 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_7_4 s_3_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_7_7 s_10_7 s_2_8 s_7_8 s_1_9 s_2_9 s_3_9 s_9_9 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_8_2 s_9_2 s_1_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_3_4 s_6_4 s_7_4 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_10_7 s_2_8 s_7_8 s_8_8 s_1_9 s_2_9 s_3_9 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_4_2 s_8_2 s_9_2 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_7_4 s_5_5 s_6_5 s_8_5 s_1_6 s_8_6 s_10_6 s_1_7 s_3_7 s_4_7 s_9_7 s_10_7 s_2_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_7_9 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_7_4 s_3_5 s_4_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_7_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_9_8 s_1_9 s_2_9 s_3_9 s_8_9 s_9_9 s_10_9 s_5_10 s_6_10 )))

(assert (not (and s_2_1 s_4_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_8_2 s_9_2 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_7_4 s_4_5 s_8_5 s_1_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_6_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_7_9 s_8_9 s_9_9 s_10_9 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_7_1 s_8_1 s_9_1 s_10_1 s_3_2 s_8_2 s_9_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_5_4 s_6_4 s_7_4 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_7_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_8_9 s_10_9 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_7_1 s_8_1 s_9_1 s_10_1 s_3_2 s_4_2 s_8_2 s_9_2 s_2_3 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_5_4 s_7_4 s_4_5 s_5_5 s_8_5 s_1_6 s_5_6 s_7_6 s_8_6 s_10_6 s_1_7 s_3_7 s_4_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_7_9 s_8_9 s_10_9 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_4_1 s_8_1 s_10_1 s_1_2 s_2_2 s_4_2 s_6_2 s_8_2 s_9_2 s_10_2 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_7_4 s_10_4 s_3_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_10_6 s_3_7 s_4_7 s_9_7 s_10_7 s_2_8 s_7_8 s_10_8 s_2_9 s_3_9 s_5_9 s_7_9 s_9_9 s_2_10 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_8_1 s_10_1 s_2_2 s_3_2 s_4_2 s_6_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_5_4 s_7_4 s_10_4 s_3_5 s_4_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_7_6 s_10_6 s_3_7 s_4_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_9_8 s_10_8 s_2_9 s_3_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_4_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_8_1 s_10_1 s_1_2 s_2_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_3_4 s_6_4 s_7_4 s_10_4 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_8_6 s_10_6 s_3_7 s_4_7 s_5_7 s_10_7 s_2_8 s_6_8 s_7_8 s_2_9 s_3_9 s_5_9 s_9_9 s_2_10 s_4_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_8_1 s_10_1 s_2_2 s_3_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_5_4 s_6_4 s_7_4 s_10_4 s_4_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_6_8 s_9_8 s_2_9 s_3_9 s_5_9 s_8_9 s_9_9 s_10_9 s_2_10 s_4_10 s_5_10 s_6_10 )))

(assert (not (and s_1_1 s_4_1 s_8_1 s_10_1 s_1_2 s_4_2 s_6_2 s_8_2 s_9_2 s_10_2 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_7_4 s_10_4 s_3_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_10_6 s_3_7 s_4_7 s_7_7 s_9_7 s_10_7 s_2_8 s_7_8 s_8_8 s_10_8 s_2_9 s_3_9 s_5_9 s_7_9 s_2_10 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_8_1 s_10_1 s_3_2 s_4_2 s_6_2 s_8_2 s_9_2 s_10_2 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_7_4 s_10_4 s_3_5 s_4_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_7_6 s_10_6 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_5_9 s_7_9 s_8_9 s_10_9 s_2_10 s_4_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_3_1 s_4_1 s_8_1 s_10_1 s_1_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_7_4 s_10_4 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_8_6 s_10_6 s_3_7 s_4_7 s_5_7 s_7_7 s_10_7 s_2_8 s_6_8 s_7_8 s_8_8 s_2_9 s_3_9 s_5_9 s_2_10 s_4_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_8_1 s_10_1 s_3_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_7_4 s_10_4 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_7_6 s_8_6 s_10_6 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_6_8 s_8_8 s_9_8 s_2_9 s_3_9 s_5_9 s_8_9 s_10_9 s_2_10 s_4_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_8_1 s_9_1 s_10_1 s_1_2 s_4_2 s_9_2 s_1_3 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_2_4 s_3_4 s_6_4 s_7_4 s_10_4 s_1_5 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_3_7 s_4_7 s_5_7 s_9_7 s_10_7 s_7_8 s_1_9 s_2_9 s_7_9 s_4_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_9_2 s_1_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_10_4 s_1_5 s_3_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_3_7 s_4_7 s_5_7 s_7_7 s_9_7 s_10_7 s_7_8 s_8_8 s_1_9 s_2_9 s_7_9 s_9_9 s_4_10 s_5_10 s_9_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_8_1 s_9_1 s_10_1 s_3_2 s_4_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_2_4 s_5_4 s_6_4 s_7_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_7_6 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_9_8 s_1_9 s_2_9 s_7_9 s_8_9 s_10_9 s_4_10 s_5_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_9_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_10_4 s_1_5 s_3_5 s_4_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_7_6 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_8_8 s_9_8 s_1_9 s_2_9 s_7_9 s_8_9 s_9_9 s_10_9 s_4_10 s_5_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_10_1 s_1_2 s_4_2 s_6_2 s_9_2 s_10_2 s_1_3 s_3_3 s_6_3 s_7_3 s_8_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_1_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_8_6 s_1_7 s_3_7 s_4_7 s_5_7 s_7_7 s_9_7 s_10_7 s_6_8 s_7_8 s_2_9 s_5_9 s_7_9 s_2_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_10_1 s_3_2 s_4_2 s_6_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_1_5 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_7_6 s_8_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_6_8 s_9_8 s_2_9 s_5_9 s_7_9 s_8_9 s_10_9 s_2_10 s_5_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_4_2 s_6_2 s_9_2 s_10_2 s_1_3 s_6_3 s_7_3 s_8_3 s_10_3 s_2_4 s_3_4 s_6_4 s_7_4 s_1_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_8_6 s_1_7 s_3_7 s_4_7 s_5_7 s_9_7 s_10_7 s_6_8 s_7_8 s_8_8 s_2_9 s_5_9 s_7_9 s_9_9 s_2_10 s_5_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_10_1 s_2_2 s_3_2 s_4_2 s_6_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_6_3 s_7_3 s_8_3 s_10_3 s_2_4 s_5_4 s_6_4 s_7_4 s_1_5 s_4_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_6_8 s_8_8 s_9_8 s_2_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_5_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_7_1 s_8_1 s_10_1 s_1_2 s_6_2 s_9_2 s_10_2 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_3_4 s_4_4 s_7_4 s_1_5 s_3_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_1_7 s_3_7 s_4_7 s_7_7 s_10_7 s_7_8 s_10_8 s_2_9 s_5_9 s_2_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_7_1 s_8_1 s_10_1 s_3_2 s_6_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_4_4 s_5_4 s_7_4 s_1_5 s_3_5 s_4_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_7_6 s_1_7 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_10_7 s_9_8 s_10_8 s_2_9 s_5_9 s_8_9 s_10_9 s_2_10 s_5_10 s_8_10 )))

(assert (not (and s_4_1 s_5_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_6_2 s_9_2 s_10_2 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_3_4 s_7_4 s_1_5 s_3_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_1_7 s_3_7 s_4_7 s_10_7 s_7_8 s_8_8 s_10_8 s_2_9 s_5_9 s_9_9 s_2_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_7_1 s_8_1 s_10_1 s_2_2 s_3_2 s_6_2 s_9_2 s_10_2 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_5_4 s_7_4 s_1_5 s_3_5 s_4_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_7_6 s_1_7 s_3_7 s_4_7 s_6_7 s_8_7 s_10_7 s_8_8 s_9_8 s_10_8 s_2_9 s_5_9 s_8_9 s_9_9 s_10_9 s_2_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_8_1 s_9_1 s_10_1 s_1_2 s_9_2 s_3_3 s_6_3 s_7_3 s_8_3 s_10_3 s_3_4 s_7_4 s_10_4 s_1_5 s_5_5 s_6_5 s_8_5 s_1_6 s_8_6 s_3_7 s_4_7 s_10_7 s_6_8 s_7_8 s_10_8 s_1_9 s_2_9 s_4_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_8_1 s_9_1 s_10_1 s_3_2 s_9_2 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_10_3 s_5_4 s_7_4 s_10_4 s_1_5 s_4_5 s_5_5 s_8_5 s_1_6 s_5_6 s_7_6 s_8_6 s_3_7 s_4_7 s_6_7 s_8_7 s_10_7 s_6_8 s_9_8 s_10_8 s_1_9 s_2_9 s_8_9 s_10_9 s_4_10 s_5_10 s_8_10 )))

(assert (not (and s_4_1 s_5_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_9_2 s_6_3 s_7_3 s_8_3 s_10_3 s_3_4 s_4_4 s_7_4 s_10_4 s_1_5 s_6_5 s_8_5 s_1_6 s_6_6 s_8_6 s_3_7 s_4_7 s_7_7 s_10_7 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_9_9 s_4_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_9_2 s_2_3 s_4_3 s_6_3 s_7_3 s_8_3 s_10_3 s_4_4 s_5_4 s_7_4 s_10_4 s_1_5 s_4_5 s_8_5 s_1_6 s_5_6 s_6_6 s_7_6 s_8_6 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_10_7 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_8_9 s_9_9 s_10_9 s_4_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_2_3 s_3_3 s_5_3 s_10_3 s_2_4 s_3_4 s_6_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_9_6 s_10_6 s_2_7 s_5_7 s_9_7 s_10_7 s_3_8 s_4_8 s_5_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_4_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_8_1 s_9_1 s_10_1 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_2_3 s_5_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_9_6 s_10_6 s_2_7 s_5_7 s_7_7 s_9_7 s_10_7 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_4_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_2_3 s_5_3 s_9_3 s_10_3 s_3_4 s_6_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_9_6 s_1_7 s_2_7 s_5_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_2_3 s_3_3 s_5_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_9_6 s_1_7 s_2_7 s_5_7 s_7_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_10_9 s_8_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_3_2 s_5_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_5_3 s_7_3 s_9_3 s_10_3 s_3_4 s_6_4 s_9_4 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_10_7 s_2_8 s_3_8 s_5_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_8_9 s_10_9 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_5_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_5_3 s_7_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_9_4 s_2_5 s_3_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_7_7 s_10_7 s_2_8 s_3_8 s_5_8 s_7_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_8_9 s_9_9 s_10_9 s_6_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_3_2 s_4_2 s_5_2 s_7_2 s_9_2 s_1_3 s_2_3 s_3_3 s_5_3 s_7_3 s_10_3 s_2_4 s_3_4 s_6_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_9_6 s_2_7 s_3_7 s_5_7 s_9_7 s_10_7 s_3_8 s_5_8 s_7_8 s_9_8 s_1_9 s_2_9 s_4_9 s_6_9 s_7_9 s_8_9 s_10_9 s_4_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_9_2 s_1_3 s_2_3 s_5_3 s_7_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_9_6 s_2_7 s_3_7 s_5_7 s_7_7 s_9_7 s_10_7 s_3_8 s_5_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_4_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_4_3 s_5_3 s_9_3 s_10_3 s_5_4 s_6_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_9_6 s_1_7 s_2_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_9_9 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_3_3 s_4_3 s_5_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_1_7 s_2_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_8_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_5_2 s_7_2 s_8_2 s_9_2 s_1_3 s_4_3 s_5_3 s_7_3 s_9_3 s_10_3 s_5_4 s_6_4 s_9_4 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_3_8 s_5_8 s_8_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_5_2 s_7_2 s_8_2 s_9_2 s_1_3 s_3_3 s_4_3 s_5_3 s_7_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_9_4 s_2_5 s_3_5 s_4_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_3_8 s_5_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_9_9 s_6_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_4_2 s_5_2 s_7_2 s_9_2 s_1_3 s_3_3 s_4_3 s_5_3 s_7_3 s_10_3 s_2_4 s_5_4 s_6_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_7_6 s_9_6 s_2_7 s_3_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_3_8 s_5_8 s_1_9 s_2_9 s_4_9 s_6_9 s_7_9 s_4_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_7_2 s_9_2 s_1_3 s_4_3 s_5_3 s_7_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_3_8 s_5_8 s_8_8 s_1_9 s_2_9 s_4_9 s_6_9 s_7_9 s_9_9 s_4_10 s_9_10 )))

(assert (not (and s_4_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_3_3 s_4_3 s_5_3 s_10_3 s_2_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_9_6 s_10_6 s_2_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_3_8 s_4_8 s_5_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_4_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_4_3 s_5_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_2_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_3_8 s_4_8 s_5_8 s_8_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_4_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_9_2 s_2_3 s_3_3 s_5_3 s_6_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_10_6 s_2_7 s_4_7 s_5_7 s_9_7 s_3_8 s_4_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_6_2 s_7_2 s_9_2 s_2_3 s_5_3 s_6_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_3_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_10_6 s_2_7 s_4_7 s_5_7 s_7_7 s_9_7 s_3_8 s_4_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_7_9 s_8_9 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_2_1 s_6_1 s_8_1 s_9_1 s_10_1 s_2_2 s_4_2 s_6_2 s_7_2 s_9_2 s_3_3 s_4_3 s_5_3 s_6_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_7_6 s_10_6 s_2_7 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_3_8 s_4_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_7_9 s_9_9 s_10_9 s_4_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_8_1 s_9_1 s_10_1 s_4_2 s_6_2 s_7_2 s_9_2 s_4_3 s_5_3 s_6_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_3_5 s_4_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_10_6 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_3_8 s_4_8 s_8_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_7_9 s_10_9 s_4_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_4_2 s_7_2 s_9_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_7_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_3_8 s_1_9 s_2_9 s_4_9 s_7_9 s_10_9 s_4_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_2_2 s_4_2 s_7_2 s_9_2 s_1_3 s_4_3 s_5_3 s_6_3 s_7_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_7_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_3_8 s_8_8 s_1_9 s_2_9 s_4_9 s_7_9 s_9_9 s_10_9 s_4_10 s_7_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_7_2 s_9_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_2_7 s_3_7 s_4_7 s_5_7 s_9_7 s_3_8 s_7_8 s_9_8 s_1_9 s_2_9 s_4_9 s_7_9 s_8_9 s_4_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_7_2 s_9_2 s_1_3 s_2_3 s_5_3 s_6_3 s_7_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_3_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_9_7 s_3_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_4_9 s_7_9 s_8_9 s_9_9 s_4_10 s_7_10 s_9_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_7_2 s_8_2 s_9_2 s_1_3 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_7_4 s_9_4 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_7_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_2_8 s_3_8 s_8_8 s_1_9 s_2_9 s_3_9 s_4_9 s_10_9 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_7_2 s_8_2 s_9_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_3_5 s_4_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_7_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_3_8 s_1_9 s_2_9 s_3_9 s_4_9 s_9_9 s_10_9 s_6_10 s_7_10 )))

(assert (not (and s_3_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_7_4 s_9_4 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_2_8 s_3_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_8_9 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_7_4 s_9_4 s_3_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_2_8 s_3_8 s_7_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_8_9 s_9_9 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_6_2 s_7_2 s_8_2 s_9_2 s_4_3 s_5_3 s_6_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_7_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_8_7 s_2_8 s_3_8 s_4_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_9_9 s_10_9 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_6_2 s_7_2 s_8_2 s_9_2 s_3_3 s_4_3 s_5_3 s_6_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_3_5 s_4_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_3_8 s_4_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_10_9 s_7_10 s_8_10 )))

(assert (not (and s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_6_2 s_7_2 s_8_2 s_9_2 s_2_3 s_5_3 s_6_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_1_7 s_2_7 s_4_7 s_5_7 s_2_8 s_3_8 s_4_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_8_9 s_9_9 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_6_2 s_7_2 s_8_2 s_9_2 s_2_3 s_3_3 s_5_3 s_6_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_3_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_1_7 s_2_7 s_4_7 s_5_7 s_7_7 s_2_8 s_3_8 s_4_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_8_9 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_6_2 s_7_2 s_9_2 s_10_2 s_2_3 s_5_3 s_6_3 s_7_3 s_10_3 s_1_4 s_3_4 s_7_4 s_9_4 s_1_5 s_3_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_1_7 s_2_7 s_3_7 s_4_7 s_3_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_4_9 s_5_9 s_8_9 s_9_9 s_2_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_4_3 s_5_3 s_7_3 s_10_3 s_5_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_9_6 s_1_7 s_2_7 s_3_7 s_6_7 s_8_7 s_10_7 s_3_8 s_5_8 s_8_8 s_10_8 s_2_9 s_4_9 s_5_9 s_6_9 s_9_9 s_2_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_2_3 s_5_3 s_7_3 s_10_3 s_3_4 s_9_4 s_1_5 s_2_5 s_3_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_9_6 s_1_7 s_2_7 s_3_7 s_10_7 s_3_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_2_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_2_2 s_6_2 s_7_2 s_9_2 s_10_2 s_4_3 s_5_3 s_6_3 s_7_3 s_10_3 s_1_4 s_5_4 s_7_4 s_9_4 s_1_5 s_3_5 s_4_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_7_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_8_7 s_3_8 s_8_8 s_10_8 s_2_9 s_4_9 s_5_9 s_9_9 s_10_9 s_2_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_3_2 s_6_2 s_7_2 s_9_2 s_10_2 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_10_3 s_1_4 s_3_4 s_4_4 s_7_4 s_9_4 s_1_5 s_3_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_1_7 s_2_7 s_3_7 s_4_7 s_7_7 s_3_8 s_7_8 s_9_8 s_10_8 s_2_9 s_4_9 s_5_9 s_8_9 s_2_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_3_3 s_4_3 s_5_3 s_7_3 s_10_3 s_4_4 s_5_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_7_6 s_9_6 s_1_7 s_2_7 s_3_7 s_6_7 s_7_7 s_8_7 s_10_7 s_3_8 s_5_8 s_10_8 s_2_9 s_4_9 s_5_9 s_6_9 s_2_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_3_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_2_3 s_3_3 s_5_3 s_7_3 s_10_3 s_3_4 s_4_4 s_9_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_9_6 s_1_7 s_2_7 s_3_7 s_7_7 s_10_7 s_3_8 s_5_8 s_7_8 s_9_8 s_10_8 s_2_9 s_4_9 s_5_9 s_6_9 s_8_9 s_10_9 s_2_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_6_2 s_7_2 s_9_2 s_10_2 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_10_3 s_1_4 s_4_4 s_5_4 s_7_4 s_9_4 s_1_5 s_3_5 s_4_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_7_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_3_8 s_10_8 s_2_9 s_4_9 s_5_9 s_10_9 s_2_10 s_7_10 s_8_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_8_1 s_10_1 s_1_2 s_3_2 s_4_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_6_5 s_8_5 s_10_5 s_6_6 s_2_7 s_4_7 s_9_7 s_2_8 s_3_8 s_4_8 s_7_8 s_9_8 s_2_9 s_3_9 s_4_9 s_7_9 s_8_9 s_2_10 s_4_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_5_3 s_6_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_5_5 s_6_5 s_8_5 s_10_5 s_2_7 s_4_7 s_7_7 s_9_7 s_2_8 s_3_8 s_4_8 s_7_8 s_8_8 s_9_8 s_2_9 s_3_9 s_4_9 s_7_9 s_8_9 s_9_9 s_2_10 s_4_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_10_1 s_1_2 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_9_3 s_10_3 s_2_4 s_5_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_10_5 s_3_6 s_5_6 s_6_6 s_7_6 s_9_6 s_2_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_2_10 s_4_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_4_3 s_5_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_10_5 s_3_6 s_5_6 s_7_6 s_9_6 s_2_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_8_8 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_9_9 s_2_10 s_4_10 s_9_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_8_1 s_10_1 s_4_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_8_5 s_10_5 s_5_6 s_6_6 s_7_6 s_2_7 s_4_7 s_6_7 s_8_7 s_9_7 s_2_8 s_3_8 s_4_8 s_2_9 s_3_9 s_4_9 s_7_9 s_10_9 s_2_10 s_4_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_8_1 s_10_1 s_2_2 s_4_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_4_3 s_5_3 s_6_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_8_5 s_10_5 s_5_6 s_7_6 s_2_7 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_3_8 s_4_8 s_8_8 s_2_9 s_3_9 s_4_9 s_7_9 s_9_9 s_10_9 s_2_10 s_4_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_10_1 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_9_3 s_10_3 s_2_4 s_3_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_6_5 s_10_5 s_3_6 s_6_6 s_9_6 s_2_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_9_8 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_10_9 s_2_10 s_4_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_5_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_10_5 s_3_6 s_9_6 s_2_7 s_7_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_4_10 )))

(assert (not (and s_6_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_7_4 s_9_4 s_10_4 s_3_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_10_6 s_2_7 s_3_7 s_4_7 s_9_7 s_2_8 s_3_8 s_7_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_2_10 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_6_1 s_8_1 s_10_1 s_1_2 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_7_4 s_9_4 s_10_4 s_3_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_10_6 s_2_7 s_3_7 s_4_7 s_7_7 s_9_7 s_2_8 s_3_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_2_10 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_6_1 s_8_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_3_3 s_4_3 s_5_3 s_7_3 s_9_3 s_10_3 s_2_4 s_5_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_2_7 s_3_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_5_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_2_10 s_4_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_8_1 s_10_1 s_1_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_4_3 s_5_3 s_7_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_7_6 s_9_6 s_10_6 s_2_7 s_3_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_5_8 s_8_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_2_10 s_4_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_8_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_5_3 s_7_3 s_9_3 s_10_3 s_2_4 s_3_4 s_9_4 s_10_4 s_2_5 s_3_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_9_6 s_10_6 s_2_7 s_3_7 s_9_7 s_10_7 s_2_8 s_3_8 s_5_8 s_7_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_4_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_8_1 s_10_1 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_5_3 s_7_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_9_4 s_10_4 s_2_5 s_3_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_9_6 s_10_6 s_2_7 s_3_7 s_7_7 s_9_7 s_10_7 s_2_8 s_3_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_2_10 s_4_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_6_1 s_8_1 s_10_1 s_2_2 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_7_4 s_9_4 s_10_4 s_3_5 s_4_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_7_6 s_10_6 s_2_7 s_3_7 s_4_7 s_6_7 s_8_7 s_9_7 s_2_8 s_3_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_9_9 s_10_9 s_2_10 s_4_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_8_1 s_10_1 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_7_4 s_9_4 s_10_4 s_3_5 s_4_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_7_6 s_10_6 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_3_8 s_8_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_10_9 s_2_10 s_4_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_7_1 s_8_1 s_10_1 s_7_2 s_9_2 s_10_2 s_1_3 s_4_3 s_5_3 s_6_3 s_10_3 s_1_4 s_5_4 s_7_4 s_8_4 s_9_4 s_3_5 s_4_5 s_8_5 s_10_5 s_5_6 s_6_6 s_7_6 s_10_6 s_1_7 s_2_7 s_4_7 s_6_7 s_8_7 s_3_8 s_4_8 s_8_8 s_2_9 s_4_9 s_10_9 s_2_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_10_1 s_3_2 s_5_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_5_3 s_10_3 s_3_4 s_8_4 s_9_4 s_2_5 s_3_5 s_6_5 s_10_5 s_3_6 s_6_6 s_9_6 s_10_6 s_1_7 s_2_7 s_10_7 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_2_9 s_4_9 s_6_9 s_8_9 s_10_9 s_2_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_7_1 s_8_1 s_10_1 s_2_2 s_7_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_10_3 s_1_4 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_3_5 s_4_5 s_5_5 s_8_5 s_10_5 s_5_6 s_7_6 s_10_6 s_1_7 s_2_7 s_4_7 s_6_7 s_7_7 s_8_7 s_3_8 s_4_8 s_2_9 s_4_9 s_9_9 s_10_9 s_2_10 s_6_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_10_1 s_2_2 s_3_2 s_5_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_10_3 s_3_4 s_4_4 s_8_4 s_9_4 s_2_5 s_3_5 s_5_5 s_6_5 s_10_5 s_3_6 s_9_6 s_10_6 s_1_7 s_2_7 s_7_7 s_10_7 s_3_8 s_4_8 s_5_8 s_7_8 s_9_8 s_2_9 s_4_9 s_6_9 s_8_9 s_9_9 s_10_9 s_2_10 s_6_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_5_2 s_7_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_10_3 s_4_4 s_5_4 s_8_4 s_9_4 s_2_5 s_3_5 s_4_5 s_5_5 s_10_5 s_3_6 s_5_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_6_7 s_7_7 s_8_7 s_10_7 s_3_8 s_4_8 s_5_8 s_2_9 s_4_9 s_6_9 s_9_9 s_2_10 s_6_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_5_2 s_7_2 s_9_2 s_10_2 s_1_3 s_4_3 s_5_3 s_10_3 s_5_4 s_8_4 s_9_4 s_2_5 s_3_5 s_4_5 s_10_5 s_3_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_6_7 s_8_7 s_10_7 s_3_8 s_4_8 s_5_8 s_8_8 s_2_9 s_4_9 s_6_9 s_2_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_10_3 s_1_4 s_3_4 s_4_4 s_7_4 s_8_4 s_9_4 s_3_5 s_5_5 s_6_5 s_8_5 s_10_5 s_10_6 s_1_7 s_2_7 s_4_7 s_7_7 s_3_8 s_4_8 s_7_8 s_9_8 s_2_9 s_4_9 s_8_9 s_9_9 s_2_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_3_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_5_3 s_6_3 s_10_3 s_1_4 s_3_4 s_7_4 s_8_4 s_9_4 s_3_5 s_6_5 s_8_5 s_10_5 s_6_6 s_10_6 s_1_7 s_2_7 s_4_7 s_3_8 s_4_8 s_7_8 s_8_8 s_9_8 s_2_9 s_4_9 s_8_9 s_2_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_7_1 s_8_1 s_10_1 s_4_2 s_7_2 s_9_2 s_10_2 s_4_3 s_6_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_4_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_3_8 s_4_8 s_6_8 s_8_8 s_10_8 s_2_9 s_4_9 s_7_9 s_10_9 s_2_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_4_2 s_7_2 s_8_2 s_9_2 s_4_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_7_4 s_9_4 s_4_5 s_5_5 s_8_5 s_10_5 s_5_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_8_7 s_9_7 s_2_8 s_3_8 s_6_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_7_9 s_10_9 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_7_1 s_8_1 s_10_1 s_3_2 s_4_2 s_5_2 s_7_2 s_9_2 s_10_2 s_2_3 s_10_3 s_2_4 s_3_4 s_6_4 s_8_4 s_9_4 s_2_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_5_7 s_9_7 s_10_7 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_4_9 s_6_9 s_7_9 s_8_9 s_10_9 s_2_10 s_6_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_2_3 s_7_3 s_9_3 s_10_3 s_2_4 s_3_4 s_9_4 s_2_5 s_5_5 s_6_5 s_10_5 s_3_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_9_7 s_10_7 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_10_9 s_6_10 s_8_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_4_2 s_5_2 s_7_2 s_9_2 s_10_2 s_4_3 s_10_3 s_2_4 s_5_4 s_6_4 s_8_4 s_9_4 s_2_5 s_4_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_10_8 s_2_9 s_4_9 s_6_9 s_7_9 s_2_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_4_3 s_7_3 s_9_3 s_10_3 s_2_4 s_5_4 s_9_4 s_2_5 s_4_5 s_5_5 s_10_5 s_3_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_5_8 s_6_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_3_2 s_4_2 s_7_2 s_9_2 s_10_2 s_2_3 s_6_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_7_4 s_8_4 s_9_4 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_8_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_9_7 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_4_9 s_7_9 s_8_9 s_2_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_7_2 s_8_2 s_9_2 s_2_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_7_4 s_9_4 s_5_5 s_6_5 s_8_5 s_10_5 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_9_7 s_2_8 s_3_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_7_9 s_8_9 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_2_1 s_6_1 s_7_1 s_8_1 s_10_1 s_2_2 s_4_2 s_7_2 s_9_2 s_10_2 s_3_3 s_4_3 s_6_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_4_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_3_8 s_4_8 s_6_8 s_10_8 s_2_9 s_4_9 s_7_9 s_9_9 s_10_9 s_2_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_4_2 s_7_2 s_8_2 s_9_2 s_3_3 s_4_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_7_4 s_9_4 s_4_5 s_8_5 s_10_5 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_3_8 s_6_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_7_9 s_9_9 s_10_9 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_7_1 s_8_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_9_2 s_10_2 s_2_3 s_3_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_8_4 s_9_4 s_2_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_5_7 s_7_7 s_9_7 s_10_7 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_2_9 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_2_3 s_3_3 s_7_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_9_4 s_2_5 s_6_5 s_10_5 s_3_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_7_7 s_9_7 s_10_7 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_7_2 s_9_2 s_10_2 s_2_3 s_3_3 s_6_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_8_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_7_7 s_9_7 s_3_8 s_4_8 s_6_8 s_7_8 s_9_8 s_10_8 s_2_9 s_4_9 s_7_9 s_8_9 s_9_9 s_2_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_7_2 s_8_2 s_9_2 s_2_3 s_3_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_7_4 s_9_4 s_6_5 s_8_5 s_10_5 s_6_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_7_7 s_9_7 s_2_8 s_3_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_7_9 s_8_9 s_9_9 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_7_2 s_9_2 s_10_2 s_3_3 s_4_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_2_5 s_4_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_3_8 s_4_8 s_5_8 s_6_8 s_10_8 s_2_9 s_4_9 s_6_9 s_7_9 s_9_9 s_2_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_3_3 s_4_3 s_7_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_9_4 s_2_5 s_4_5 s_10_5 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_5_8 s_6_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_9_9 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_8_1 s_10_1 s_2_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_4_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_2_8 s_3_8 s_6_8 s_2_9 s_3_9 s_4_9 s_5_9 s_9_9 s_10_9 s_2_10 s_4_10 s_6_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_8_1 s_9_1 s_10_1 s_2_2 s_6_2 s_7_2 s_9_2 s_1_3 s_3_3 s_4_3 s_6_3 s_10_3 s_1_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_4_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_7_6 s_8_6 s_10_6 s_2_7 s_4_7 s_6_7 s_8_7 s_3_8 s_4_8 s_6_8 s_1_9 s_2_9 s_4_9 s_5_9 s_9_9 s_10_9 s_4_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_8_1 s_10_1 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_4_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_7_6 s_8_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_3_8 s_6_8 s_8_8 s_2_9 s_3_9 s_4_9 s_5_9 s_10_9 s_2_10 s_4_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_8_1 s_9_1 s_10_1 s_6_2 s_7_2 s_9_2 s_1_3 s_4_3 s_6_3 s_10_3 s_1_4 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_4_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_2_7 s_4_7 s_6_7 s_7_7 s_8_7 s_3_8 s_4_8 s_6_8 s_8_8 s_1_9 s_2_9 s_4_9 s_5_9 s_10_9 s_4_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_8_1 s_10_1 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_7_3 s_9_3 s_10_3 s_3_4 s_6_4 s_9_4 s_10_4 s_2_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_5_7 s_10_7 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_9_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_2_10 s_4_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_9_2 s_1_3 s_2_3 s_3_3 s_10_3 s_3_4 s_8_4 s_9_4 s_10_4 s_2_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_8_6 s_9_6 s_10_6 s_2_7 s_10_7 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_4_10 s_6_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_8_1 s_10_1 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_7_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_9_4 s_10_4 s_2_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_5_7 s_7_7 s_10_7 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_10_9 s_2_10 s_4_10 s_6_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_8_1 s_9_1 s_10_1 s_3_2 s_5_2 s_6_2 s_7_2 s_9_2 s_1_3 s_2_3 s_10_3 s_3_4 s_4_4 s_8_4 s_9_4 s_10_4 s_2_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_8_6 s_9_6 s_10_6 s_2_7 s_7_7 s_10_7 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_8_9 s_10_9 s_4_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_7_4 s_9_4 s_10_4 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_8_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_2_8 s_3_8 s_6_8 s_7_8 s_9_8 s_2_9 s_3_9 s_4_9 s_5_9 s_8_9 s_9_9 s_2_10 s_4_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_6_2 s_7_2 s_9_2 s_1_3 s_2_3 s_3_3 s_6_3 s_10_3 s_1_4 s_3_4 s_7_4 s_8_4 s_9_4 s_10_4 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_8_6 s_10_6 s_2_7 s_4_7 s_3_8 s_4_8 s_6_8 s_7_8 s_9_8 s_1_9 s_2_9 s_4_9 s_5_9 s_8_9 s_9_9 s_4_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_6_1 s_8_1 s_10_1 s_1_2 s_3_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_6_3 s_7_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_7_4 s_9_4 s_10_4 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_8_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_2_8 s_3_8 s_6_8 s_7_8 s_8_8 s_9_8 s_2_9 s_3_9 s_4_9 s_5_9 s_8_9 s_2_10 s_4_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_6_2 s_7_2 s_9_2 s_1_3 s_2_3 s_6_3 s_10_3 s_1_4 s_3_4 s_4_4 s_7_4 s_8_4 s_9_4 s_10_4 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_8_6 s_10_6 s_2_7 s_4_7 s_7_7 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_4_9 s_5_9 s_8_9 s_4_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_8_1 s_10_1 s_1_2 s_2_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_7_3 s_9_3 s_10_3 s_5_4 s_6_4 s_9_4 s_10_4 s_2_5 s_4_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_3_8 s_5_8 s_6_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_9_9 s_2_10 s_4_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_5_2 s_6_2 s_7_2 s_9_2 s_1_3 s_3_3 s_4_3 s_10_3 s_5_4 s_8_4 s_9_4 s_10_4 s_2_5 s_4_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_6_7 s_8_7 s_10_7 s_3_8 s_4_8 s_5_8 s_6_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_9_9 s_4_10 s_6_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_8_1 s_10_1 s_1_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_4_3 s_7_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_9_4 s_10_4 s_2_5 s_4_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_3_8 s_5_8 s_6_8 s_8_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_2_10 s_4_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_5_2 s_6_2 s_7_2 s_9_2 s_1_3 s_4_3 s_10_3 s_4_4 s_5_4 s_8_4 s_9_4 s_10_4 s_2_5 s_4_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_6_7 s_7_7 s_8_7 s_10_7 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_4_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_8_1 s_10_1 s_7_2 s_8_2 s_9_2 s_10_2 s_3_3 s_4_3 s_6_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_4_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_2_7 s_4_7 s_5_7 s_6_7 s_8_7 s_2_8 s_3_8 s_4_8 s_6_8 s_10_8 s_2_9 s_3_9 s_4_9 s_10_9 s_2_10 s_4_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_7_2 s_9_2 s_3_3 s_4_3 s_6_3 s_7_3 s_10_3 s_1_4 s_5_4 s_7_4 s_9_4 s_10_4 s_1_5 s_4_5 s_5_5 s_8_5 s_10_5 s_5_6 s_7_6 s_8_6 s_2_7 s_3_7 s_4_7 s_6_7 s_8_7 s_3_8 s_6_8 s_10_8 s_1_9 s_2_9 s_4_9 s_10_9 s_4_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_8_1 s_10_1 s_3_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_9_3 s_10_3 s_3_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_8_6 s_9_6 s_2_7 s_5_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_6_9 s_8_9 s_10_9 s_2_10 s_4_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_3_2 s_5_2 s_7_2 s_9_2 s_2_3 s_3_3 s_7_3 s_10_3 s_3_4 s_9_4 s_10_4 s_1_5 s_2_5 s_5_5 s_6_5 s_10_5 s_3_6 s_8_6 s_9_6 s_2_7 s_3_7 s_10_7 s_3_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_6_9 s_8_9 s_10_9 s_4_10 s_8_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_8_1 s_10_1 s_1_2 s_3_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_6_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_8_6 s_2_7 s_4_7 s_5_7 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_8_9 s_2_10 s_4_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_7_2 s_9_2 s_2_3 s_3_3 s_6_3 s_7_3 s_10_3 s_1_4 s_3_4 s_7_4 s_9_4 s_10_4 s_1_5 s_5_5 s_6_5 s_8_5 s_10_5 s_8_6 s_2_7 s_3_7 s_4_7 s_3_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_8_9 s_4_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_8_1 s_10_1 s_1_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_3_3 s_4_3 s_9_3 s_10_3 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_2_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_10_8 s_2_9 s_3_9 s_4_9 s_6_9 s_2_10 s_4_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_5_2 s_7_2 s_9_2 s_3_3 s_4_3 s_7_3 s_10_3 s_5_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_10_5 s_3_6 s_5_6 s_7_6 s_8_6 s_9_6 s_2_7 s_3_7 s_6_7 s_8_7 s_10_7 s_3_8 s_5_8 s_6_8 s_10_8 s_1_9 s_2_9 s_4_9 s_6_9 s_4_10 s_8_10 s_9_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_8_1 s_10_1 s_2_2 s_7_2 s_8_2 s_9_2 s_10_2 s_4_3 s_6_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_4_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_7_6 s_8_6 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_3_8 s_4_8 s_6_8 s_8_8 s_10_8 s_2_9 s_3_9 s_4_9 s_9_9 s_10_9 s_2_10 s_4_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_2_2 s_7_2 s_9_2 s_4_3 s_6_3 s_7_3 s_10_3 s_1_4 s_4_4 s_5_4 s_7_4 s_9_4 s_10_4 s_1_5 s_4_5 s_8_5 s_10_5 s_5_6 s_6_6 s_7_6 s_8_6 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_3_8 s_6_8 s_8_8 s_10_8 s_1_9 s_2_9 s_4_9 s_9_9 s_10_9 s_4_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_4_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_6_3 s_7_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_1_5 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_7_6 s_8_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_3_8 s_6_8 s_2_9 s_4_9 s_5_9 s_7_9 s_10_9 s_2_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_3_3 s_4_3 s_6_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_1_5 s_4_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_7_6 s_8_6 s_1_7 s_2_7 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_3_8 s_4_8 s_6_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_10_9 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_8_1 s_10_1 s_2_2 s_3_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_8_6 s_9_6 s_2_7 s_5_7 s_7_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_6_9 s_8_9 s_9_9 s_10_9 s_2_10 s_4_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_5_2 s_7_2 s_9_2 s_2_3 s_7_3 s_10_3 s_3_4 s_4_4 s_9_4 s_10_4 s_1_5 s_2_5 s_6_5 s_10_5 s_3_6 s_6_6 s_8_6 s_9_6 s_2_7 s_3_7 s_7_7 s_10_7 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_6_9 s_8_9 s_9_9 s_10_9 s_4_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_7_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_9_4 s_1_5 s_2_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_5_7 s_7_7 s_9_7 s_10_7 s_3_8 s_5_8 s_6_8 s_7_8 s_9_8 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_2_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_8_4 s_9_4 s_1_5 s_2_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_8_6 s_9_6 s_1_7 s_2_7 s_7_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_10_10 )))

(assert (not (and s_5_1 s_6_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_6_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_8_6 s_2_7 s_4_7 s_5_7 s_7_7 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_8_9 s_9_9 s_2_10 s_4_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_7_2 s_9_2 s_2_3 s_6_3 s_7_3 s_10_3 s_1_4 s_3_4 s_4_4 s_7_4 s_9_4 s_10_4 s_1_5 s_6_5 s_8_5 s_10_5 s_6_6 s_8_6 s_2_7 s_3_7 s_4_7 s_7_7 s_3_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_8_9 s_9_9 s_4_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_3_2 s_4_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_6_3 s_7_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_9_4 s_1_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_8_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_9_7 s_3_8 s_6_8 s_7_8 s_9_8 s_2_9 s_4_9 s_5_9 s_7_9 s_8_9 s_2_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_6_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_7_4 s_8_4 s_9_4 s_1_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_8_6 s_1_7 s_2_7 s_4_7 s_7_7 s_9_7 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_8_1 s_10_1 s_1_2 s_2_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_4_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_2_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_10_8 s_2_9 s_3_9 s_4_9 s_6_9 s_9_9 s_2_10 s_4_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_5_2 s_7_2 s_9_2 s_4_3 s_7_3 s_10_3 s_4_4 s_5_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_10_5 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_2_7 s_3_7 s_6_7 s_7_7 s_8_7 s_10_7 s_3_8 s_5_8 s_6_8 s_8_8 s_10_8 s_1_9 s_2_9 s_4_9 s_6_9 s_9_9 s_4_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_7_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_9_4 s_1_5 s_2_5 s_4_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_3_8 s_5_8 s_6_8 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_2_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_3_3 s_4_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_8_4 s_9_4 s_1_5 s_2_5 s_4_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_2_2 s_4_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_4_3 s_6_3 s_7_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_7_4 s_9_4 s_1_5 s_4_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_3_8 s_6_8 s_8_8 s_2_9 s_4_9 s_5_9 s_7_9 s_9_9 s_10_9 s_2_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_4_3 s_6_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_7_4 s_8_4 s_9_4 s_1_5 s_4_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_7_6 s_8_6 s_1_7 s_2_7 s_4_7 s_6_7 s_8_7 s_9_7 s_2_8 s_3_8 s_4_8 s_6_8 s_8_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_9_9 s_10_9 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_7_3 s_10_3 s_2_4 s_3_4 s_6_4 s_9_4 s_1_5 s_2_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_5_7 s_9_7 s_10_7 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_9_3 s_10_3 s_2_4 s_3_4 s_8_4 s_9_4 s_1_5 s_2_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_8_6 s_9_6 s_1_7 s_2_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_6_3 s_7_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_7_4 s_9_4 s_1_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_8_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_9_7 s_3_8 s_6_8 s_7_8 s_8_8 s_9_8 s_2_9 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_2_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_6_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_7_4 s_8_4 s_9_4 s_1_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_8_6 s_1_7 s_2_7 s_4_7 s_9_7 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_4_3 s_7_3 s_10_3 s_2_4 s_5_4 s_6_4 s_9_4 s_1_5 s_2_5 s_4_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_3_8 s_5_8 s_6_8 s_8_8 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_2_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_4_3 s_9_3 s_10_3 s_2_4 s_5_4 s_8_4 s_9_4 s_1_5 s_2_5 s_4_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_10_1 s_4_2 s_6_2 s_8_2 s_10_2 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_4_4 s_10_4 s_3_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_10_6 s_3_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_5_8 s_8_8 s_10_8 s_3_9 s_5_9 s_7_9 s_10_9 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_6_1 s_10_1 s_2_2 s_4_2 s_6_2 s_8_2 s_10_2 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_10_4 s_3_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_10_6 s_3_7 s_8_7 s_9_7 s_1_8 s_2_8 s_5_8 s_10_8 s_3_9 s_5_9 s_7_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_7_1 s_10_1 s_6_2 s_10_2 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_1_4 s_4_4 s_1_5 s_3_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_1_7 s_3_7 s_7_7 s_8_7 s_1_8 s_5_8 s_10_8 s_5_9 s_10_9 s_2_10 s_3_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_7_1 s_10_1 s_2_2 s_6_2 s_10_2 s_4_3 s_5_3 s_7_3 s_8_3 s_1_4 s_1_5 s_3_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_1_7 s_3_7 s_8_7 s_1_8 s_5_8 s_8_8 s_10_8 s_5_9 s_9_9 s_10_9 s_2_10 s_3_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_2_2 s_6_2 s_8_2 s_4_3 s_5_3 s_8_3 s_9_3 s_1_4 s_6_4 s_8_4 s_1_5 s_3_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_1_7 s_5_7 s_8_7 s_1_8 s_2_8 s_4_8 s_5_8 s_8_8 s_10_8 s_1_9 s_3_9 s_5_9 s_9_9 s_10_9 s_3_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_6_2 s_8_2 s_3_3 s_4_3 s_5_3 s_8_3 s_9_3 s_1_4 s_4_4 s_6_4 s_8_4 s_1_5 s_3_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_1_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_4_8 s_5_8 s_10_8 s_1_9 s_3_9 s_5_9 s_10_9 s_3_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_6_1 s_9_1 s_10_1 s_2_2 s_4_2 s_6_2 s_3_3 s_4_3 s_5_3 s_8_3 s_1_4 s_2_4 s_6_4 s_8_4 s_10_4 s_3_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_10_6 s_5_7 s_8_7 s_9_7 s_1_8 s_4_8 s_5_8 s_10_8 s_1_9 s_5_9 s_7_9 s_9_9 s_10_9 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_9_1 s_10_1 s_4_2 s_6_2 s_4_3 s_5_3 s_8_3 s_1_4 s_2_4 s_4_4 s_6_4 s_8_4 s_10_4 s_3_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_10_6 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_4_8 s_5_8 s_8_8 s_10_8 s_1_9 s_5_9 s_7_9 s_10_9 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_10_1 s_6_2 s_8_2 s_10_2 s_1_3 s_4_3 s_7_3 s_8_3 s_9_3 s_1_4 s_4_4 s_6_4 s_10_4 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_8_6 s_10_6 s_3_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_5_8 s_6_8 s_8_8 s_3_9 s_5_9 s_10_9 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_10_1 s_2_2 s_6_2 s_8_2 s_10_2 s_1_3 s_3_3 s_4_3 s_7_3 s_8_3 s_9_3 s_1_4 s_6_4 s_10_4 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_8_6 s_10_6 s_3_7 s_5_7 s_8_7 s_1_8 s_2_8 s_5_8 s_6_8 s_3_9 s_5_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_9_1 s_10_1 s_2_2 s_6_2 s_1_3 s_3_3 s_4_3 s_8_3 s_1_4 s_8_4 s_10_4 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_8_6 s_10_6 s_8_7 s_1_8 s_4_8 s_5_8 s_6_8 s_1_9 s_5_9 s_9_9 s_10_9 s_3_10 s_4_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_9_1 s_10_1 s_6_2 s_1_3 s_4_3 s_8_3 s_1_4 s_4_4 s_8_4 s_10_4 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_8_6 s_10_6 s_7_7 s_8_7 s_1_8 s_4_8 s_5_8 s_6_8 s_8_8 s_1_9 s_5_9 s_10_9 s_3_10 s_4_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_10_1 s_4_2 s_6_2 s_10_2 s_1_3 s_3_3 s_4_3 s_7_3 s_8_3 s_1_4 s_2_4 s_4_4 s_6_4 s_1_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_8_6 s_1_7 s_3_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_5_8 s_6_8 s_5_9 s_7_9 s_10_9 s_2_10 s_3_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_10_1 s_2_2 s_4_2 s_6_2 s_10_2 s_1_3 s_4_3 s_7_3 s_8_3 s_1_4 s_2_4 s_6_4 s_1_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_8_6 s_1_7 s_3_7 s_5_7 s_8_7 s_9_7 s_1_8 s_5_8 s_6_8 s_8_8 s_5_9 s_7_9 s_9_9 s_10_9 s_2_10 s_3_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_2_2 s_4_2 s_6_2 s_8_2 s_1_3 s_4_3 s_8_3 s_9_3 s_1_4 s_2_4 s_8_4 s_1_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_8_6 s_1_7 s_8_7 s_9_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_8_8 s_1_9 s_3_9 s_5_9 s_7_9 s_9_9 s_10_9 s_3_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_4_2 s_6_2 s_8_2 s_1_3 s_3_3 s_4_3 s_8_3 s_9_3 s_1_4 s_2_4 s_4_4 s_8_4 s_1_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_8_6 s_1_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_1_9 s_3_9 s_5_9 s_7_9 s_10_9 s_3_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_9_1 s_10_1 s_4_2 s_1_3 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_1_4 s_2_4 s_6_4 s_10_4 s_1_5 s_3_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_3_7 s_5_7 s_8_7 s_9_7 s_1_8 s_5_8 s_1_9 s_7_9 s_10_9 s_3_10 s_4_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_7_1 s_9_1 s_10_1 s_8_2 s_1_3 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_1_4 s_6_4 s_3_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_10_6 s_1_7 s_3_7 s_5_7 s_8_7 s_1_8 s_2_8 s_5_8 s_8_8 s_1_9 s_3_9 s_10_9 s_3_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_9_1 s_10_1 s_2_2 s_4_2 s_1_3 s_4_3 s_5_3 s_7_3 s_8_3 s_1_4 s_2_4 s_4_4 s_6_4 s_10_4 s_1_5 s_3_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_3_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_5_8 s_8_8 s_1_9 s_7_9 s_9_9 s_10_9 s_3_10 s_4_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_7_1 s_9_1 s_10_1 s_2_2 s_8_2 s_1_3 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_1_4 s_4_4 s_6_4 s_3_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_10_6 s_1_7 s_3_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_5_8 s_1_9 s_3_9 s_9_9 s_10_9 s_3_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_7_1 s_10_1 s_10_2 s_1_3 s_4_3 s_5_3 s_8_3 s_1_4 s_8_4 s_3_5 s_6_5 s_10_5 s_3_6 s_6_6 s_10_6 s_1_7 s_8_7 s_1_8 s_4_8 s_5_8 s_8_8 s_10_9 s_2_10 s_3_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_7_1 s_10_1 s_2_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_8_3 s_1_4 s_4_4 s_8_4 s_3_5 s_5_5 s_6_5 s_10_5 s_3_6 s_10_6 s_1_7 s_7_7 s_8_7 s_1_8 s_4_8 s_5_8 s_9_9 s_10_9 s_2_10 s_3_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_10_1 s_4_2 s_8_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_8_3 s_9_3 s_1_4 s_2_4 s_8_4 s_10_4 s_1_5 s_3_5 s_6_5 s_10_5 s_3_6 s_6_6 s_8_7 s_9_7 s_1_8 s_2_8 s_4_8 s_5_8 s_3_9 s_7_9 s_10_9 s_2_10 s_3_10 s_4_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_10_1 s_2_2 s_4_2 s_8_2 s_10_2 s_1_3 s_4_3 s_5_3 s_8_3 s_9_3 s_1_4 s_2_4 s_4_4 s_8_4 s_10_4 s_1_5 s_3_5 s_5_5 s_6_5 s_10_5 s_3_6 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_4_8 s_5_8 s_8_8 s_3_9 s_7_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_7_1 s_9_1 s_10_1 s_4_2 s_8_2 s_4_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_5_5 s_6_5 s_10_5 s_3_6 s_8_6 s_10_6 s_1_7 s_3_7 s_8_7 s_9_7 s_1_8 s_2_8 s_5_8 s_6_8 s_8_8 s_10_8 s_1_9 s_3_9 s_7_9 s_10_9 s_3_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_6_1 s_7_1 s_9_1 s_10_1 s_2_2 s_4_2 s_8_2 s_3_3 s_4_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_4_4 s_6_5 s_10_5 s_3_6 s_6_6 s_8_6 s_10_6 s_1_7 s_3_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_5_8 s_6_8 s_10_8 s_1_9 s_3_9 s_7_9 s_9_9 s_10_9 s_3_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_9_1 s_10_1 s_3_3 s_4_3 s_7_3 s_8_3 s_1_4 s_10_4 s_1_5 s_5_5 s_6_5 s_10_5 s_3_6 s_8_6 s_3_7 s_8_7 s_1_8 s_5_8 s_6_8 s_10_8 s_1_9 s_10_9 s_3_10 s_4_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_9_1 s_10_1 s_2_2 s_4_3 s_7_3 s_8_3 s_1_4 s_4_4 s_10_4 s_1_5 s_6_5 s_10_5 s_3_6 s_6_6 s_8_6 s_3_7 s_7_7 s_8_7 s_1_8 s_5_8 s_6_8 s_8_8 s_10_8 s_1_9 s_9_9 s_10_9 s_3_10 s_4_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_10_1 s_2_2 s_8_2 s_10_2 s_4_3 s_8_3 s_9_3 s_1_4 s_4_4 s_6_4 s_8_4 s_10_4 s_1_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_8_6 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_8_8 s_10_8 s_3_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_10_1 s_8_2 s_10_2 s_3_3 s_4_3 s_8_3 s_9_3 s_1_4 s_6_4 s_8_4 s_10_4 s_1_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_8_6 s_5_7 s_8_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_10_8 s_3_9 s_10_9 s_2_10 s_3_10 s_4_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_6_1 s_7_1 s_10_1 s_2_2 s_4_2 s_10_2 s_3_3 s_4_3 s_8_3 s_1_4 s_2_4 s_4_4 s_6_4 s_8_4 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_8_6 s_10_6 s_1_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_4_8 s_5_8 s_6_8 s_10_8 s_7_9 s_9_9 s_10_9 s_2_10 s_3_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_7_1 s_10_1 s_4_2 s_10_2 s_4_3 s_8_3 s_1_4 s_2_4 s_6_4 s_8_4 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_8_6 s_10_6 s_1_7 s_5_7 s_8_7 s_9_7 s_1_8 s_4_8 s_5_8 s_6_8 s_8_8 s_10_8 s_7_9 s_10_9 s_2_10 s_3_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_6_1 s_7_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_10_2 s_2_3 s_3_3 s_8_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_4_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_10_6 s_1_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_7_9 s_8_9 s_9_9 s_2_10 s_3_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_6_1 s_10_1 s_1_2 s_3_2 s_6_2 s_8_2 s_10_2 s_1_3 s_2_3 s_7_3 s_8_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_10_4 s_4_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_10_6 s_3_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_3_9 s_5_9 s_8_9 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_5_1 s_6_1 s_10_1 s_1_2 s_2_2 s_3_2 s_8_2 s_10_2 s_2_3 s_8_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_10_4 s_1_5 s_4_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_3_9 s_8_9 s_9_9 s_2_10 s_3_10 s_4_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_7_1 s_10_1 s_1_2 s_3_2 s_4_2 s_6_2 s_10_2 s_1_3 s_2_3 s_3_3 s_7_3 s_8_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_1_5 s_4_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_1_7 s_3_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_5_8 s_6_8 s_7_8 s_9_8 s_5_9 s_7_9 s_8_9 s_2_10 s_3_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_6_1 s_7_1 s_10_1 s_1_2 s_3_2 s_4_2 s_10_2 s_2_3 s_8_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_8_4 s_4_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_5_7 s_6_7 s_9_7 s_1_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_7_9 s_8_9 s_2_10 s_3_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_10_1 s_1_2 s_2_2 s_3_2 s_6_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_7_3 s_8_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_10_4 s_4_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_3_7 s_5_7 s_6_7 s_1_8 s_2_8 s_5_8 s_6_8 s_7_8 s_9_8 s_3_9 s_5_9 s_8_9 s_9_9 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_10_1 s_1_2 s_3_2 s_8_2 s_10_2 s_2_3 s_3_3 s_8_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_8_4 s_10_4 s_1_5 s_4_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_5_7 s_6_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_3_9 s_8_9 s_2_10 s_3_10 s_4_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_7_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_10_2 s_1_3 s_2_3 s_7_3 s_8_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_1_5 s_4_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_1_7 s_3_7 s_5_7 s_6_7 s_9_7 s_1_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_5_9 s_7_9 s_8_9 s_9_9 s_2_10 s_3_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_7_1 s_10_1 s_1_2 s_2_2 s_3_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_8_3 s_1_4 s_3_4 s_4_4 s_5_4 s_8_4 s_3_5 s_4_5 s_5_5 s_10_5 s_3_6 s_5_6 s_7_6 s_10_6 s_1_7 s_6_7 s_7_7 s_1_8 s_4_8 s_5_8 s_7_8 s_9_8 s_8_9 s_9_9 s_2_10 s_3_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_6_1 s_7_1 s_10_1 s_1_2 s_3_2 s_10_2 s_1_3 s_2_3 s_5_3 s_8_3 s_1_4 s_3_4 s_5_4 s_8_4 s_3_5 s_4_5 s_10_5 s_3_6 s_5_6 s_6_6 s_7_6 s_10_6 s_1_7 s_6_7 s_1_8 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_8_9 s_2_10 s_3_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_6_1 s_10_1 s_1_2 s_3_2 s_4_2 s_6_2 s_8_2 s_10_2 s_2_3 s_5_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_10_4 s_3_5 s_4_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_7_6 s_10_6 s_3_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_3_9 s_5_9 s_7_9 s_8_9 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_6_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_8_2 s_10_2 s_2_3 s_3_3 s_5_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_10_4 s_3_5 s_4_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_10_6 s_3_7 s_6_7 s_9_7 s_1_8 s_2_8 s_5_8 s_7_8 s_9_8 s_10_8 s_3_9 s_5_9 s_7_9 s_8_9 s_9_9 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_5_1 s_6_1 s_7_1 s_10_1 s_1_2 s_2_2 s_3_2 s_6_2 s_10_2 s_2_3 s_5_3 s_7_3 s_8_3 s_1_4 s_3_4 s_5_4 s_1_5 s_3_5 s_4_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_1_7 s_3_7 s_6_7 s_1_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_5_9 s_8_9 s_9_9 s_2_10 s_3_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_7_1 s_10_1 s_1_2 s_3_2 s_6_2 s_10_2 s_2_3 s_3_3 s_5_3 s_7_3 s_8_3 s_1_4 s_3_4 s_4_4 s_5_4 s_1_5 s_3_5 s_4_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_7_6 s_1_7 s_3_7 s_6_7 s_7_7 s_1_8 s_5_8 s_7_8 s_9_8 s_10_8 s_5_9 s_8_9 s_2_10 s_3_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_10_1 s_1_2 s_3_2 s_4_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_8_4 s_10_4 s_1_5 s_3_5 s_4_5 s_10_5 s_3_6 s_5_6 s_6_6 s_7_6 s_6_7 s_9_7 s_1_8 s_2_8 s_4_8 s_5_8 s_7_8 s_9_8 s_3_9 s_7_9 s_8_9 s_2_10 s_3_10 s_4_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_8_2 s_10_2 s_1_3 s_2_3 s_5_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_8_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_10_5 s_3_6 s_5_6 s_7_6 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_3_9 s_7_9 s_8_9 s_9_9 s_2_10 s_3_10 s_4_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_1_3 s_2_3 s_3_3 s_5_3 s_7_3 s_8_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_7_6 s_3_7 s_5_7 s_6_7 s_9_7 s_1_8 s_5_8 s_7_8 s_9_8 s_1_9 s_7_9 s_8_9 s_3_10 s_4_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_9_1 s_10_1 s_1_2 s_3_2 s_2_3 s_3_3 s_7_3 s_8_3 s_1_4 s_3_4 s_5_4 s_10_4 s_1_5 s_4_5 s_5_5 s_10_5 s_3_6 s_5_6 s_7_6 s_8_6 s_3_7 s_6_7 s_1_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_8_9 s_3_10 s_4_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_1_3 s_2_3 s_5_3 s_7_3 s_8_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_10_4 s_1_5 s_3_5 s_4_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_3_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_5_8 s_7_8 s_8_8 s_9_8 s_1_9 s_7_9 s_8_9 s_9_9 s_3_10 s_4_10 s_7_10 s_9_10 )))

(assert (not (and s_5_1 s_6_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_2_3 s_7_3 s_8_3 s_1_4 s_3_4 s_4_4 s_5_4 s_10_4 s_1_5 s_4_5 s_10_5 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_3_7 s_6_7 s_7_7 s_1_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_8_9 s_9_9 s_3_10 s_4_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_6_2 s_8_2 s_2_3 s_5_3 s_8_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_8_4 s_1_5 s_3_5 s_4_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_1_7 s_5_7 s_6_7 s_1_8 s_2_8 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_5_9 s_8_9 s_9_9 s_3_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_8_2 s_1_3 s_2_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_8_4 s_1_5 s_4_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_7_6 s_8_6 s_1_7 s_6_7 s_9_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_3_9 s_5_9 s_7_9 s_8_9 s_9_9 s_3_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_3_2 s_6_2 s_8_2 s_2_3 s_3_3 s_5_3 s_8_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_1_5 s_3_5 s_4_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_1_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_4_8 s_5_8 s_7_8 s_9_8 s_10_8 s_1_9 s_3_9 s_5_9 s_8_9 s_3_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_6_2 s_8_2 s_1_3 s_2_3 s_3_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_8_4 s_1_5 s_4_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_1_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_1_9 s_3_9 s_5_9 s_7_9 s_8_9 s_3_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_6_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_2_3 s_3_3 s_5_3 s_8_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_8_4 s_10_4 s_3_5 s_4_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_10_6 s_5_7 s_6_7 s_9_7 s_1_8 s_4_8 s_5_8 s_7_8 s_9_8 s_10_8 s_1_9 s_5_9 s_7_9 s_8_9 s_9_9 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_3_2 s_8_2 s_1_3 s_2_3 s_5_3 s_7_3 s_8_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_3_5 s_4_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_7_6 s_10_6 s_1_7 s_3_7 s_5_7 s_6_7 s_1_8 s_2_8 s_5_8 s_7_8 s_8_8 s_9_8 s_1_9 s_3_9 s_8_9 s_3_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_6_2 s_1_3 s_2_3 s_3_3 s_8_3 s_1_4 s_3_4 s_5_4 s_8_4 s_10_4 s_4_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_7_6 s_8_6 s_10_6 s_6_7 s_1_8 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_1_9 s_5_9 s_8_9 s_9_9 s_3_10 s_4_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_8_2 s_2_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_4_5 s_5_5 s_10_5 s_3_6 s_5_6 s_7_6 s_8_6 s_10_6 s_1_7 s_3_7 s_6_7 s_9_7 s_1_8 s_2_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_7_9 s_8_9 s_3_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_6_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_6_2 s_2_3 s_5_3 s_8_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_10_4 s_3_5 s_4_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_10_6 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_5_9 s_7_9 s_8_9 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_8_2 s_1_3 s_2_3 s_3_3 s_5_3 s_7_3 s_8_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_3_5 s_4_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_10_6 s_1_7 s_3_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_5_8 s_7_8 s_9_8 s_1_9 s_3_9 s_8_9 s_9_9 s_3_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_6_1 s_9_1 s_10_1 s_1_2 s_3_2 s_6_2 s_1_3 s_2_3 s_8_3 s_1_4 s_3_4 s_4_4 s_5_4 s_8_4 s_10_4 s_4_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_6_7 s_7_7 s_1_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_5_9 s_8_9 s_3_10 s_4_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_6_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_8_2 s_2_3 s_3_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_4_5 s_10_5 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_3_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_3_9 s_7_9 s_8_9 s_9_9 s_3_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_5_1 s_7_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_10_2 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_1_4 s_7_4 s_9_4 s_1_5 s_2_5 s_3_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_8_7 s_1_8 s_3_8 s_8_8 s_9_8 s_10_8 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_2_10 s_3_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_7_1 s_10_1 s_1_2 s_2_2 s_6_2 s_7_2 s_10_2 s_5_3 s_7_3 s_3_4 s_5_4 s_9_4 s_1_5 s_3_5 s_4_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_1_7 s_2_7 s_3_7 s_6_7 s_10_7 s_1_8 s_3_8 s_5_8 s_7_8 s_8_8 s_10_8 s_4_9 s_5_9 s_9_9 s_2_10 s_3_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_5_1 s_7_1 s_10_1 s_2_2 s_5_2 s_6_2 s_7_2 s_10_2 s_5_3 s_6_3 s_7_3 s_1_4 s_3_4 s_5_4 s_7_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_7_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_1_8 s_3_8 s_7_8 s_8_8 s_10_8 s_4_9 s_5_9 s_6_9 s_9_9 s_10_9 s_2_10 s_3_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_7_1 s_10_1 s_2_2 s_3_2 s_6_2 s_7_2 s_10_2 s_2_3 s_4_3 s_5_3 s_7_3 s_9_4 s_1_5 s_3_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_1_7 s_2_7 s_3_7 s_8_7 s_10_7 s_1_8 s_3_8 s_5_8 s_8_8 s_9_8 s_10_8 s_4_9 s_5_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_7_1 s_10_1 s_1_2 s_3_2 s_5_2 s_6_2 s_7_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_1_4 s_4_4 s_7_4 s_9_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_7_7 s_8_7 s_1_8 s_3_8 s_9_8 s_10_8 s_4_9 s_5_9 s_6_9 s_8_9 s_2_10 s_3_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_7_1 s_10_1 s_1_2 s_6_2 s_7_2 s_10_2 s_3_3 s_5_3 s_7_3 s_3_4 s_4_4 s_5_4 s_9_4 s_1_5 s_3_5 s_4_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_7_6 s_1_7 s_2_7 s_3_7 s_6_7 s_7_7 s_10_7 s_1_8 s_3_8 s_5_8 s_7_8 s_10_8 s_4_9 s_5_9 s_2_10 s_3_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_7_1 s_10_1 s_5_2 s_6_2 s_7_2 s_10_2 s_3_3 s_5_3 s_6_3 s_7_3 s_1_4 s_3_4 s_4_4 s_5_4 s_7_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_7_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_1_8 s_3_8 s_7_8 s_10_8 s_4_9 s_5_9 s_6_9 s_10_9 s_2_10 s_3_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_7_1 s_10_1 s_3_2 s_6_2 s_7_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_4_4 s_9_4 s_1_5 s_3_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_1_7 s_2_7 s_3_7 s_7_7 s_8_7 s_10_7 s_1_8 s_3_8 s_5_8 s_9_8 s_10_8 s_4_9 s_5_9 s_8_9 s_10_9 s_2_10 s_3_10 s_5_10 s_8_10 )))

(assert (not (and s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_1_4 s_2_4 s_7_4 s_9_4 s_10_4 s_2_5 s_3_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_9_8 s_10_8 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_10_1 s_1_2 s_2_2 s_4_2 s_6_2 s_7_2 s_8_2 s_10_2 s_3_3 s_5_3 s_7_3 s_9_3 s_2_4 s_3_4 s_5_4 s_9_4 s_10_4 s_3_5 s_4_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_10_6 s_2_7 s_3_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_7_8 s_10_8 s_3_9 s_4_9 s_5_9 s_7_9 s_9_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_10_1 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_3_3 s_5_3 s_6_3 s_7_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_7_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_6_7 s_9_7 s_1_8 s_2_8 s_3_8 s_7_8 s_10_8 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_10_1 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_9_3 s_2_4 s_9_4 s_10_4 s_3_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_10_6 s_2_7 s_3_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_9_8 s_10_8 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_1_4 s_2_4 s_4_4 s_7_4 s_9_4 s_10_4 s_2_5 s_3_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_8_8 s_9_8 s_10_8 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_10_1 s_1_2 s_4_2 s_6_2 s_7_2 s_8_2 s_10_2 s_5_3 s_7_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_9_4 s_10_4 s_3_5 s_4_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_7_6 s_10_6 s_2_7 s_3_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_7_8 s_8_8 s_10_8 s_3_9 s_4_9 s_5_9 s_7_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_10_1 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_5_3 s_6_3 s_7_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_7_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_3_8 s_7_8 s_8_8 s_10_8 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_10_1 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_10_2 s_2_3 s_4_3 s_5_3 s_7_3 s_9_3 s_2_4 s_4_4 s_9_4 s_10_4 s_3_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_10_6 s_2_7 s_3_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_8_8 s_9_8 s_10_8 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_3_1 s_5_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_9_3 s_1_4 s_2_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_6_5 s_8_5 s_1_6 s_6_6 s_9_6 s_2_7 s_4_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_9_8 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_9_3 s_1_4 s_2_4 s_4_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_8_5 s_1_6 s_9_6 s_2_7 s_4_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_8_8 s_9_8 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_10_1 s_1_2 s_4_2 s_7_2 s_8_2 s_10_2 s_1_3 s_3_3 s_5_3 s_9_3 s_2_4 s_3_4 s_5_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_1_6 s_3_6 s_5_6 s_6_6 s_7_6 s_2_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_3_9 s_4_9 s_7_9 s_2_10 s_3_10 s_4_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_10_1 s_1_2 s_2_2 s_4_2 s_7_2 s_8_2 s_10_2 s_1_3 s_5_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_1_6 s_3_6 s_5_6 s_7_6 s_2_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_3_9 s_4_9 s_7_9 s_9_9 s_2_10 s_3_10 s_4_10 s_5_10 s_9_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_10_1 s_3_2 s_4_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_9_3 s_2_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_6_5 s_1_6 s_3_6 s_6_6 s_2_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_9_8 s_3_9 s_4_9 s_7_9 s_8_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_10_1 s_2_2 s_3_2 s_4_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_9_3 s_2_4 s_4_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_5_5 s_6_5 s_1_6 s_3_6 s_2_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_8_8 s_9_8 s_3_9 s_4_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 )))

(assert (not (and s_5_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_2_3 s_4_3 s_5_3 s_6_3 s_9_3 s_1_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_9_6 s_1_7 s_2_7 s_4_7 s_5_7 s_8_7 s_1_8 s_2_8 s_3_8 s_4_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_3_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_7_1 s_9_1 s_10_1 s_1_2 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_9_3 s_1_4 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_9_6 s_1_7 s_2_7 s_4_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_3_8 s_4_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_3_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_5_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_6_2 s_7_2 s_8_2 s_5_3 s_9_3 s_3_4 s_5_4 s_6_4 s_8_4 s_9_4 s_1_5 s_3_5 s_4_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_1_7 s_2_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_9_9 s_3_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_7_1 s_9_1 s_10_1 s_1_2 s_6_2 s_7_2 s_8_2 s_3_3 s_5_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_1_5 s_3_5 s_4_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_1_7 s_2_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_3_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_7_1 s_9_1 s_10_1 s_2_2 s_3_2 s_6_2 s_7_2 s_8_2 s_2_3 s_4_3 s_5_3 s_9_3 s_6_4 s_8_4 s_9_4 s_1_5 s_3_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_1_7 s_2_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_8_9 s_9_9 s_10_9 s_3_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_7_1 s_9_1 s_10_1 s_3_2 s_6_2 s_7_2 s_8_2 s_2_3 s_3_3 s_4_3 s_5_3 s_9_3 s_4_4 s_6_4 s_8_4 s_9_4 s_1_5 s_3_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_1_7 s_2_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_8_9 s_10_9 s_3_10 s_5_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_7_1 s_9_1 s_10_1 s_2_2 s_5_2 s_6_2 s_7_2 s_8_2 s_5_3 s_6_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_7_6 s_9_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_1_8 s_2_8 s_3_8 s_4_8 s_7_8 s_8_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_9_9 s_10_9 s_3_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_7_1 s_9_1 s_10_1 s_5_2 s_6_2 s_7_2 s_8_2 s_3_3 s_5_3 s_6_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_3_8 s_4_8 s_7_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_10_9 s_3_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_9_1 s_10_1 s_4_2 s_5_2 s_7_2 s_1_3 s_3_3 s_5_3 s_6_3 s_7_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_7_6 s_9_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_9_7 s_1_8 s_3_8 s_7_8 s_1_9 s_4_9 s_6_9 s_7_9 s_10_9 s_3_10 s_4_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_9_1 s_10_1 s_2_2 s_4_2 s_5_2 s_7_2 s_1_3 s_5_3 s_6_3 s_7_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_3_8 s_7_8 s_8_8 s_1_9 s_4_9 s_6_9 s_7_9 s_9_9 s_10_9 s_3_10 s_4_10 s_5_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_9_1 s_10_1 s_3_2 s_4_2 s_7_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_2_4 s_6_4 s_9_4 s_10_4 s_1_5 s_3_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_2_7 s_3_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_5_8 s_9_8 s_1_9 s_4_9 s_7_9 s_8_9 s_10_9 s_3_10 s_4_10 s_5_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_7_2 s_1_3 s_2_3 s_4_3 s_5_3 s_7_3 s_2_4 s_4_4 s_6_4 s_9_4 s_10_4 s_1_5 s_3_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_2_7 s_3_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_5_8 s_8_8 s_9_8 s_1_9 s_4_9 s_7_9 s_8_9 s_9_9 s_10_9 s_3_10 s_4_10 s_5_10 )))

(assert (not (and s_3_1 s_5_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_7_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_1_4 s_2_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_9_6 s_2_7 s_3_7 s_4_7 s_5_7 s_8_7 s_9_7 s_1_8 s_3_8 s_9_8 s_1_9 s_4_9 s_6_9 s_7_9 s_8_9 s_3_10 s_4_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_1_4 s_2_4 s_4_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_9_6 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_3_8 s_8_8 s_9_8 s_1_9 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_3_10 s_4_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_9_1 s_10_1 s_1_2 s_4_2 s_7_2 s_1_3 s_3_3 s_5_3 s_7_3 s_2_4 s_3_4 s_5_4 s_6_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_7_6 s_2_7 s_3_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_3_8 s_5_8 s_7_8 s_1_9 s_4_9 s_7_9 s_3_10 s_4_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_7_2 s_1_3 s_5_3 s_7_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_3_8 s_5_8 s_7_8 s_8_8 s_1_9 s_4_9 s_7_9 s_9_9 s_3_10 s_4_10 s_5_10 s_9_10 )))

(assert (not (and s_2_1 s_3_1 s_7_1 s_9_1 s_10_1 s_5_2 s_7_2 s_8_2 s_1_3 s_5_3 s_6_3 s_7_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_7_4 s_9_4 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_1_8 s_2_8 s_3_8 s_7_8 s_8_8 s_1_9 s_3_9 s_4_9 s_6_9 s_10_9 s_3_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_7_1 s_9_1 s_10_1 s_2_2 s_5_2 s_7_2 s_8_2 s_1_3 s_3_3 s_5_3 s_6_3 s_7_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_2_5 s_3_5 s_4_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_3_8 s_7_8 s_1_9 s_3_9 s_4_9 s_6_9 s_9_9 s_10_9 s_3_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_7_1 s_9_1 s_10_1 s_3_2 s_7_2 s_8_2 s_1_3 s_2_3 s_4_3 s_5_3 s_7_3 s_9_3 s_6_4 s_9_4 s_3_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_8_8 s_9_8 s_1_9 s_3_9 s_4_9 s_8_9 s_10_9 s_3_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_7_1 s_9_1 s_10_1 s_2_2 s_3_2 s_7_2 s_8_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_9_3 s_4_4 s_6_4 s_9_4 s_3_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_9_8 s_1_9 s_3_9 s_4_9 s_8_9 s_9_9 s_10_9 s_3_10 s_5_10 s_6_10 )))

(assert (not (and s_3_1 s_4_1 s_7_1 s_9_1 s_10_1 s_1_2 s_7_2 s_8_2 s_1_3 s_5_3 s_7_3 s_9_3 s_3_4 s_5_4 s_6_4 s_9_4 s_3_5 s_4_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_7_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_7_8 s_8_8 s_1_9 s_3_9 s_4_9 s_3_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_7_2 s_8_2 s_1_3 s_3_3 s_5_3 s_7_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_9_4 s_3_5 s_4_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_7_8 s_1_9 s_3_9 s_4_9 s_9_9 s_3_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_3_1 s_7_1 s_9_1 s_10_1 s_1_2 s_3_2 s_5_2 s_7_2 s_8_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_1_4 s_6_4 s_7_4 s_9_4 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_8_7 s_1_8 s_2_8 s_3_8 s_8_8 s_9_8 s_1_9 s_3_9 s_4_9 s_6_9 s_8_9 s_3_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_7_2 s_8_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_1_4 s_4_4 s_6_4 s_7_4 s_9_4 s_2_5 s_3_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_3_8 s_9_8 s_1_9 s_3_9 s_4_9 s_6_9 s_8_9 s_9_9 s_3_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_2_1 s_9_1 s_10_1 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_3_3 s_5_3 s_6_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_7_6 s_9_6 s_10_6 s_2_7 s_4_7 s_5_7 s_6_7 s_9_7 s_1_8 s_3_8 s_4_8 s_7_8 s_10_8 s_1_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_9_1 s_10_1 s_4_2 s_5_2 s_6_2 s_7_2 s_5_3 s_6_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_9_6 s_10_6 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_3_8 s_4_8 s_7_8 s_8_8 s_10_8 s_1_9 s_4_9 s_5_9 s_6_9 s_7_9 s_10_9 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_2_3 s_3_3 s_4_3 s_5_3 s_2_4 s_6_4 s_8_4 s_9_4 s_10_4 s_3_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_10_6 s_2_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_9_8 s_10_8 s_1_9 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_9_1 s_10_1 s_3_2 s_4_2 s_6_2 s_7_2 s_2_3 s_4_3 s_5_3 s_2_4 s_4_4 s_6_4 s_8_4 s_9_4 s_10_4 s_3_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_10_6 s_2_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_8_8 s_9_8 s_10_8 s_1_9 s_4_9 s_5_9 s_7_9 s_8_9 s_10_9 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_1_4 s_2_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_9_6 s_10_6 s_2_7 s_4_7 s_5_7 s_8_7 s_9_7 s_1_8 s_3_8 s_4_8 s_9_8 s_10_8 s_1_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_2_3 s_4_3 s_5_3 s_6_3 s_1_4 s_2_4 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_9_6 s_10_6 s_2_7 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_3_8 s_4_8 s_8_8 s_9_8 s_10_8 s_1_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_6_2 s_7_2 s_3_3 s_5_3 s_2_4 s_3_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_3_5 s_4_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_10_6 s_2_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_7_8 s_10_8 s_1_9 s_4_9 s_5_9 s_7_9 s_9_9 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_9_1 s_10_1 s_1_2 s_4_2 s_6_2 s_7_2 s_5_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_3_5 s_4_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_10_6 s_2_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_10_8 s_1_9 s_4_9 s_5_9 s_7_9 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_7_1 s_10_1 s_1_2 s_7_2 s_10_2 s_1_3 s_5_3 s_3_4 s_5_4 s_8_4 s_9_4 s_3_5 s_4_5 s_1_6 s_3_6 s_5_6 s_6_6 s_7_6 s_10_6 s_1_7 s_2_7 s_6_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_4_9 s_2_10 s_3_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_7_1 s_10_1 s_1_2 s_2_2 s_7_2 s_10_2 s_1_3 s_3_3 s_5_3 s_3_4 s_4_4 s_5_4 s_8_4 s_9_4 s_3_5 s_4_5 s_5_5 s_1_6 s_3_6 s_5_6 s_7_6 s_10_6 s_1_7 s_2_7 s_6_7 s_7_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_7_8 s_4_9 s_9_9 s_2_10 s_3_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_3_1 s_7_1 s_10_1 s_1_2 s_3_2 s_5_2 s_7_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_1_4 s_7_4 s_8_4 s_9_4 s_2_5 s_3_5 s_6_5 s_8_5 s_1_6 s_6_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_8_7 s_1_8 s_3_8 s_4_8 s_8_8 s_9_8 s_4_9 s_6_9 s_8_9 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_7_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_7_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_1_4 s_4_4 s_7_4 s_8_4 s_9_4 s_2_5 s_3_5 s_5_5 s_6_5 s_8_5 s_1_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_7_7 s_8_7 s_1_8 s_3_8 s_4_8 s_9_8 s_4_9 s_6_9 s_8_9 s_9_9 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_7_1 s_10_1 s_3_2 s_7_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_8_4 s_9_4 s_3_5 s_6_5 s_1_6 s_3_6 s_6_6 s_10_6 s_1_7 s_2_7 s_8_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_8_8 s_9_8 s_4_9 s_8_9 s_10_9 s_2_10 s_3_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_7_1 s_10_1 s_2_2 s_3_2 s_7_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_4_4 s_8_4 s_9_4 s_3_5 s_5_5 s_6_5 s_1_6 s_3_6 s_10_6 s_1_7 s_2_7 s_7_7 s_8_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_9_8 s_4_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_5_10 s_6_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_9_1 s_10_1 s_1_2 s_2_2 s_6_2 s_7_2 s_1_3 s_3_3 s_3_4 s_5_4 s_8_4 s_9_4 s_10_4 s_4_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_7_6 s_8_6 s_10_6 s_2_7 s_6_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_1_9 s_4_9 s_5_9 s_9_9 s_3_10 s_4_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_1_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_8_6 s_9_6 s_10_6 s_2_7 s_4_7 s_8_7 s_1_8 s_3_8 s_4_8 s_6_8 s_9_8 s_1_9 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_9_1 s_10_1 s_1_2 s_6_2 s_7_2 s_1_3 s_3_4 s_4_4 s_5_4 s_8_4 s_9_4 s_10_4 s_4_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_2_7 s_6_7 s_7_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_1_9 s_4_9 s_5_9 s_3_10 s_4_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_9_1 s_10_1 s_1_2 s_3_2 s_5_2 s_6_2 s_7_2 s_1_3 s_2_3 s_4_3 s_6_3 s_1_4 s_4_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_8_6 s_9_6 s_10_6 s_2_7 s_4_7 s_7_7 s_8_7 s_1_8 s_3_8 s_4_8 s_6_8 s_8_8 s_9_8 s_1_9 s_4_9 s_5_9 s_6_9 s_8_9 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_7_1 s_9_1 s_10_1 s_1_2 s_4_2 s_7_2 s_8_2 s_7_3 s_9_3 s_2_4 s_3_4 s_5_4 s_9_4 s_4_5 s_5_5 s_1_6 s_3_6 s_5_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_3_9 s_4_9 s_7_9 s_3_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_7_2 s_8_2 s_3_3 s_7_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_9_4 s_4_5 s_1_6 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_10_8 s_1_9 s_3_9 s_4_9 s_7_9 s_9_9 s_3_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_7_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_2_3 s_4_3 s_6_3 s_7_3 s_9_3 s_1_4 s_2_4 s_7_4 s_9_4 s_2_5 s_5_5 s_6_5 s_8_5 s_1_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_9_3 s_1_4 s_2_4 s_4_4 s_7_4 s_9_4 s_2_5 s_6_5 s_8_5 s_1_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_6_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_9_3 s_1_4 s_6_4 s_7_4 s_9_4 s_10_4 s_2_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_8_7 s_1_8 s_2_8 s_3_8 s_6_8 s_9_8 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_10_1 s_1_2 s_2_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_3_3 s_7_3 s_9_3 s_3_4 s_5_4 s_6_4 s_9_4 s_10_4 s_4_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_2_7 s_3_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_3_9 s_4_9 s_5_9 s_9_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_7_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_7_2 s_10_2 s_2_3 s_4_3 s_6_3 s_1_4 s_2_4 s_6_4 s_7_4 s_8_4 s_9_4 s_2_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_8_7 s_9_7 s_1_8 s_3_8 s_4_8 s_6_8 s_8_8 s_9_8 s_10_8 s_4_9 s_6_9 s_7_9 s_8_9 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_7_1 s_10_1 s_1_2 s_4_2 s_7_2 s_10_2 s_2_4 s_3_4 s_5_4 s_6_4 s_8_4 s_9_4 s_4_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_4_9 s_7_9 s_2_10 s_3_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_7_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_10_2 s_2_3 s_3_3 s_4_3 s_6_3 s_1_4 s_2_4 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_2_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_3_8 s_4_8 s_6_8 s_9_8 s_10_8 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_10_1 s_1_2 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_4_3 s_6_3 s_7_3 s_9_3 s_1_4 s_4_4 s_6_4 s_7_4 s_9_4 s_10_4 s_2_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_3_8 s_6_8 s_8_8 s_9_8 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_7_1 s_10_1 s_1_2 s_2_2 s_4_2 s_7_2 s_10_2 s_3_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_4_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_10_8 s_4_9 s_7_9 s_9_9 s_2_10 s_3_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_10_1 s_1_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_7_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_9_4 s_10_4 s_4_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_10_6 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_3_9 s_4_9 s_5_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_9_1 s_10_1 s_2_2 s_5_2 s_6_2 s_7_2 s_1_3 s_3_3 s_6_3 s_1_4 s_3_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_4_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_4_7 s_6_7 s_1_8 s_3_8 s_4_8 s_6_8 s_7_8 s_1_9 s_4_9 s_5_9 s_6_9 s_9_9 s_10_9 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_7_1 s_9_1 s_10_1 s_4_2 s_5_2 s_7_2 s_8_2 s_6_3 s_7_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_7_4 s_9_4 s_2_5 s_4_5 s_5_5 s_8_5 s_1_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_9_7 s_1_8 s_2_8 s_3_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_3_9 s_4_9 s_6_9 s_7_9 s_10_9 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_7_1 s_9_1 s_10_1 s_2_2 s_4_2 s_5_2 s_7_2 s_8_2 s_3_3 s_6_3 s_7_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_9_4 s_2_5 s_4_5 s_8_5 s_1_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_3_8 s_6_8 s_7_8 s_10_8 s_1_9 s_3_9 s_4_9 s_6_9 s_7_9 s_9_9 s_10_9 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_9_1 s_10_1 s_5_2 s_6_2 s_7_2 s_1_3 s_6_3 s_1_4 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_4_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_4_7 s_6_7 s_7_7 s_1_8 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_1_9 s_4_9 s_5_9 s_6_9 s_10_9 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_10_1 s_2_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_3_3 s_6_3 s_7_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_2_5 s_4_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_1_8 s_2_8 s_3_8 s_6_8 s_7_8 s_3_9 s_4_9 s_5_9 s_6_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_10_1 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_6_3 s_7_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_2_5 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_3_8 s_6_8 s_7_8 s_8_8 s_3_9 s_4_9 s_5_9 s_6_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_9_1 s_10_1 s_2_2 s_3_2 s_6_2 s_7_2 s_1_3 s_2_3 s_3_3 s_4_3 s_8_4 s_9_4 s_10_4 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_8_6 s_10_6 s_2_7 s_8_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_9_8 s_1_9 s_4_9 s_5_9 s_8_9 s_9_9 s_10_9 s_3_10 s_4_10 s_5_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_7_1 s_9_1 s_10_1 s_3_2 s_4_2 s_7_2 s_8_2 s_2_3 s_4_3 s_7_3 s_9_3 s_2_4 s_9_4 s_5_5 s_6_5 s_1_6 s_3_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_7_9 s_8_9 s_10_9 s_3_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_7_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_7_2 s_8_2 s_2_3 s_3_3 s_4_3 s_7_3 s_9_3 s_2_4 s_4_4 s_9_4 s_6_5 s_1_6 s_3_6 s_6_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_7_9 s_8_9 s_9_9 s_10_9 s_3_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_9_1 s_10_1 s_3_2 s_6_2 s_7_2 s_1_3 s_2_3 s_4_3 s_4_4 s_8_4 s_9_4 s_10_4 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_8_6 s_10_6 s_2_7 s_7_7 s_8_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_1_9 s_4_9 s_5_9 s_8_9 s_10_9 s_3_10 s_4_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_10_1 s_2_2 s_3_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_7_3 s_9_3 s_6_4 s_9_4 s_10_4 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_8_6 s_10_6 s_2_7 s_3_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_9_8 s_3_9 s_4_9 s_5_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_7_1 s_10_1 s_3_2 s_4_2 s_7_2 s_10_2 s_2_3 s_4_3 s_2_4 s_6_4 s_8_4 s_9_4 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_8_6 s_10_6 s_1_7 s_2_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_4_9 s_7_9 s_8_9 s_10_9 s_2_10 s_3_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_7_1 s_10_1 s_2_2 s_3_2 s_4_2 s_7_2 s_10_2 s_2_3 s_3_3 s_4_3 s_2_4 s_4_4 s_6_4 s_8_4 s_9_4 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_8_6 s_10_6 s_1_7 s_2_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_9_8 s_10_8 s_4_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_10_1 s_3_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_4_3 s_7_3 s_9_3 s_4_4 s_6_4 s_9_4 s_10_4 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_8_6 s_10_6 s_2_7 s_3_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_8_8 s_9_8 s_3_9 s_4_9 s_5_9 s_8_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_9_1 s_10_1 s_1_2 s_3_2 s_5_2 s_7_2 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_1_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_5_5 s_6_5 s_8_5 s_1_6 s_8_6 s_9_6 s_2_7 s_3_7 s_4_7 s_8_7 s_1_8 s_3_8 s_6_8 s_9_8 s_10_8 s_1_9 s_4_9 s_6_9 s_8_9 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_1_3 s_2_3 s_4_3 s_6_3 s_9_3 s_1_4 s_2_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_8_6 s_9_6 s_1_7 s_2_7 s_4_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_8_8 s_9_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_3_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_7_1 s_9_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_9_3 s_1_4 s_2_4 s_4_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_8_6 s_9_6 s_1_7 s_2_7 s_4_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_9_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_3_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_5_1 s_9_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_7_2 s_2_3 s_4_3 s_6_3 s_7_3 s_1_4 s_4_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_6_5 s_8_5 s_1_6 s_6_6 s_8_6 s_9_6 s_2_7 s_3_7 s_4_7 s_7_7 s_8_7 s_1_8 s_3_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_4_9 s_6_9 s_8_9 s_9_9 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_10_1 s_1_2 s_3_2 s_5_2 s_7_2 s_8_2 s_10_2 s_2_3 s_3_3 s_4_3 s_6_3 s_9_3 s_1_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_8_6 s_9_6 s_2_7 s_4_7 s_5_7 s_8_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_9_8 s_10_8 s_3_9 s_4_9 s_6_9 s_8_9 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_7_1 s_10_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_10_2 s_1_3 s_2_3 s_4_3 s_6_3 s_7_3 s_1_4 s_2_4 s_6_4 s_7_4 s_9_4 s_1_5 s_2_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_8_7 s_9_7 s_1_8 s_3_8 s_6_8 s_8_8 s_9_8 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_2_10 s_3_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_7_1 s_10_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_1_4 s_2_4 s_4_4 s_6_4 s_7_4 s_9_4 s_1_5 s_2_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_3_8 s_6_8 s_9_8 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_2_10 s_3_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_5_1 s_10_1 s_1_2 s_2_2 s_3_2 s_5_2 s_7_2 s_8_2 s_10_2 s_2_3 s_4_3 s_6_3 s_9_3 s_1_4 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_8_6 s_9_6 s_2_7 s_4_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_8_8 s_9_8 s_10_8 s_3_9 s_4_9 s_6_9 s_8_9 s_9_9 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_9_1 s_10_1 s_5_2 s_7_2 s_3_3 s_6_3 s_7_3 s_1_4 s_3_4 s_5_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_8_5 s_1_6 s_5_6 s_7_6 s_8_6 s_9_6 s_2_7 s_3_7 s_4_7 s_6_7 s_1_8 s_3_8 s_6_8 s_7_8 s_10_8 s_1_9 s_4_9 s_6_9 s_10_9 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_7_1 s_9_1 s_10_1 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_1_3 s_6_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_4_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_4_7 s_6_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_3_10 s_5_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_7_1 s_9_1 s_10_1 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_1_3 s_3_3 s_6_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_4_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_4_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_10_9 s_3_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_5_1 s_9_1 s_10_1 s_2_2 s_5_2 s_7_2 s_6_3 s_7_3 s_1_4 s_3_4 s_4_4 s_5_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_8_5 s_1_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_1_8 s_3_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_4_9 s_6_9 s_9_9 s_10_9 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_7_1 s_10_1 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_10_2 s_1_3 s_6_3 s_7_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_9_4 s_1_5 s_2_5 s_4_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_9_7 s_1_8 s_3_8 s_6_8 s_7_8 s_8_8 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_2_10 s_3_10 s_5_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_7_1 s_10_1 s_4_2 s_5_2 s_6_2 s_7_2 s_10_2 s_1_3 s_3_3 s_6_3 s_7_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_1_5 s_2_5 s_4_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_3_8 s_6_8 s_7_8 s_4_9 s_5_9 s_6_9 s_7_9 s_10_9 s_2_10 s_3_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_9_1 s_10_1 s_1_2 s_7_2 s_3_3 s_7_3 s_3_4 s_5_4 s_9_4 s_10_4 s_1_5 s_4_5 s_5_5 s_1_6 s_3_6 s_5_6 s_7_6 s_8_6 s_2_7 s_3_7 s_6_7 s_10_7 s_1_8 s_3_8 s_5_8 s_6_8 s_7_8 s_10_8 s_1_9 s_4_9 s_3_10 s_4_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_7_1 s_9_1 s_10_1 s_1_2 s_2_2 s_4_2 s_6_2 s_7_2 s_8_2 s_1_3 s_9_3 s_2_4 s_3_4 s_5_4 s_8_4 s_9_4 s_1_5 s_4_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_7_6 s_8_6 s_1_7 s_2_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_1_9 s_3_9 s_4_9 s_5_9 s_7_9 s_9_9 s_3_10 s_5_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_7_1 s_9_1 s_10_1 s_1_2 s_4_2 s_6_2 s_7_2 s_8_2 s_1_3 s_3_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_8_4 s_9_4 s_1_5 s_4_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_1_7 s_2_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_1_9 s_3_9 s_4_9 s_5_9 s_7_9 s_3_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_9_1 s_10_1 s_1_2 s_2_2 s_7_2 s_7_3 s_3_4 s_4_4 s_5_4 s_9_4 s_10_4 s_1_5 s_4_5 s_1_6 s_3_6 s_5_6 s_6_6 s_7_6 s_8_6 s_2_7 s_3_7 s_6_7 s_7_7 s_10_7 s_1_8 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_4_9 s_9_9 s_3_10 s_4_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_10_1 s_1_2 s_7_2 s_8_2 s_10_2 s_3_3 s_9_3 s_3_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_4_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_2_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_10_8 s_3_9 s_4_9 s_2_10 s_3_10 s_4_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_7_1 s_10_1 s_1_2 s_2_2 s_4_2 s_6_2 s_7_2 s_10_2 s_1_3 s_7_3 s_2_4 s_3_4 s_5_4 s_6_4 s_9_4 s_1_5 s_4_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_7_6 s_8_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_4_9 s_5_9 s_7_9 s_9_9 s_2_10 s_3_10 s_5_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_7_1 s_10_1 s_1_2 s_4_2 s_6_2 s_7_2 s_10_2 s_1_3 s_3_3 s_7_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_9_4 s_1_5 s_4_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_3_8 s_5_8 s_6_8 s_7_8 s_4_9 s_5_9 s_7_9 s_2_10 s_3_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_10_1 s_1_2 s_2_2 s_7_2 s_8_2 s_10_2 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_4_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_7_6 s_8_6 s_2_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_3_9 s_4_9 s_9_9 s_2_10 s_3_10 s_4_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_9_1 s_10_1 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_1_3 s_2_3 s_4_3 s_9_3 s_2_4 s_8_4 s_9_4 s_1_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_8_6 s_1_7 s_2_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_1_9 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_3_10 s_5_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_9_1 s_10_1 s_3_2 s_7_2 s_2_3 s_3_3 s_4_3 s_7_3 s_9_4 s_10_4 s_1_5 s_5_5 s_6_5 s_1_6 s_3_6 s_8_6 s_2_7 s_3_7 s_8_7 s_10_7 s_1_8 s_3_8 s_5_8 s_6_8 s_9_8 s_10_8 s_1_9 s_4_9 s_8_9 s_10_9 s_3_10 s_4_10 s_5_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_9_1 s_10_1 s_2_2 s_3_2 s_7_2 s_2_3 s_4_3 s_7_3 s_4_4 s_9_4 s_10_4 s_1_5 s_6_5 s_1_6 s_3_6 s_6_6 s_8_6 s_2_7 s_3_7 s_7_7 s_8_7 s_10_7 s_1_8 s_3_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_4_9 s_8_9 s_9_9 s_10_9 s_3_10 s_4_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_9_1 s_10_1 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_1_3 s_2_3 s_3_3 s_4_3 s_9_3 s_2_4 s_4_4 s_8_4 s_9_4 s_1_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_8_6 s_1_7 s_2_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_9_8 s_1_9 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_10_9 s_3_10 s_5_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_10_1 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_10_2 s_1_3 s_2_3 s_4_3 s_7_3 s_2_4 s_6_4 s_9_4 s_1_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_8_6 s_1_7 s_2_7 s_3_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_5_8 s_6_8 s_8_8 s_9_8 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_5_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_10_1 s_3_2 s_4_2 s_6_2 s_7_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_7_3 s_2_4 s_4_4 s_6_4 s_9_4 s_1_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_8_6 s_1_7 s_2_7 s_3_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_3_8 s_5_8 s_6_8 s_9_8 s_4_9 s_5_9 s_7_9 s_8_9 s_10_9 s_2_10 s_3_10 s_5_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_10_1 s_3_2 s_7_2 s_8_2 s_10_2 s_2_3 s_3_3 s_4_3 s_9_3 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_8_6 s_2_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_9_8 s_10_8 s_3_9 s_4_9 s_8_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_10_1 s_2_2 s_3_2 s_7_2 s_8_2 s_10_2 s_2_3 s_4_3 s_9_3 s_4_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_8_6 s_2_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_3_9 s_4_9 s_8_9 s_9_9 s_10_9 s_2_10 s_3_10 s_4_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_3_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_8_3 s_9_3 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_2_7 s_4_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_8_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_1_2 s_7_2 s_8_2 s_9_2 s_10_2 s_3_3 s_8_3 s_9_3 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_4_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_2_7 s_4_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_10_8 s_2_9 s_3_9 s_4_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_8_1 s_3_2 s_5_2 s_7_2 s_8_2 s_10_2 s_2_3 s_3_3 s_8_3 s_9_3 s_10_3 s_3_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_2_7 s_4_7 s_5_7 s_10_7 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_3_9 s_4_9 s_6_9 s_8_9 s_10_9 s_1_10 s_2_10 s_4_10 s_8_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_8_1 s_1_2 s_5_2 s_7_2 s_8_2 s_10_2 s_3_3 s_4_3 s_8_3 s_9_3 s_10_3 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_2_7 s_4_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_4_8 s_5_8 s_6_8 s_10_8 s_3_9 s_4_9 s_6_9 s_1_10 s_2_10 s_4_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_8_1 s_3_2 s_8_2 s_10_2 s_2_3 s_3_3 s_4_3 s_6_3 s_9_3 s_10_3 s_5_4 s_6_4 s_8_4 s_10_4 s_1_5 s_4_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_7_6 s_8_6 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_3_8 s_4_8 s_6_8 s_9_8 s_10_8 s_3_9 s_8_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_8_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_8_1 s_1_2 s_8_2 s_10_2 s_3_3 s_6_3 s_9_3 s_10_3 s_3_4 s_6_4 s_8_4 s_10_4 s_1_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_8_6 s_5_7 s_10_7 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_10_8 s_3_9 s_1_10 s_2_10 s_4_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_8_1 s_5_2 s_8_2 s_10_2 s_3_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_4_7 s_5_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_10_8 s_3_9 s_6_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_3_3 s_6_3 s_8_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_2_7 s_5_7 s_6_7 s_1_8 s_2_8 s_4_8 s_6_8 s_7_8 s_10_8 s_2_9 s_3_9 s_4_9 s_6_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_5_1 s_8_1 s_1_2 s_3_2 s_5_2 s_8_2 s_10_2 s_2_3 s_3_3 s_4_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_4_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_4_7 s_5_7 s_6_7 s_8_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_9_8 s_10_8 s_3_9 s_6_9 s_8_9 s_1_10 s_2_10 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_5_1 s_1_2 s_3_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_6_3 s_8_3 s_9_3 s_1_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_2_7 s_5_7 s_8_7 s_1_8 s_2_8 s_4_8 s_6_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_6_9 s_8_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_8_2 s_9_2 s_10_2 s_3_3 s_4_3 s_9_3 s_1_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_4_7 s_5_7 s_8_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_10_8 s_2_9 s_3_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_7_10 s_8_10 )))

(assert (not (and s_4_1 s_1_2 s_2_2 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_3_3 s_5_3 s_7_3 s_8_3 s_9_3 s_2_4 s_3_4 s_5_4 s_7_4 s_9_4 s_10_4 s_3_5 s_4_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_7_6 s_10_6 s_2_7 s_3_7 s_4_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_5_8 s_7_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_1_2 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_5_3 s_7_3 s_8_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_9_4 s_10_4 s_3_5 s_4_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_7_6 s_10_6 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_5_8 s_7_8 s_8_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_1_2 s_2_2 s_4_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_5_3 s_8_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_1_6 s_3_6 s_7_6 s_2_7 s_4_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_7_8 s_8_8 s_2_9 s_3_9 s_4_9 s_7_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_1_2 s_4_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_5_3 s_8_3 s_9_3 s_2_4 s_3_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_6_5 s_1_6 s_3_6 s_6_6 s_7_6 s_2_7 s_4_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_7_8 s_2_9 s_3_9 s_4_9 s_7_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_9_1 s_1_2 s_4_2 s_6_2 s_7_2 s_9_2 s_5_3 s_8_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_3_5 s_4_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_10_6 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_4_8 s_5_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_7_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_9_1 s_1_2 s_2_2 s_4_2 s_7_2 s_9_2 s_1_3 s_5_3 s_7_3 s_8_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_7_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_5_8 s_7_8 s_8_8 s_1_9 s_2_9 s_4_9 s_7_9 s_9_9 s_1_10 s_3_10 s_4_10 s_5_10 s_9_10 )))

(assert (not (and s_4_1 s_9_1 s_1_2 s_2_2 s_4_2 s_6_2 s_7_2 s_9_2 s_3_3 s_5_3 s_8_3 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_7_6 s_10_6 s_2_7 s_4_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_4_8 s_5_8 s_7_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_7_9 s_9_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_9_1 s_1_2 s_4_2 s_7_2 s_9_2 s_1_3 s_3_3 s_5_3 s_7_3 s_8_3 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_7_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_5_8 s_7_8 s_1_9 s_2_9 s_4_9 s_7_9 s_1_10 s_3_10 s_4_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_4_4 s_9_4 s_10_4 s_2_5 s_3_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_9_6 s_10_6 s_2_7 s_3_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_9_4 s_10_4 s_2_5 s_3_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_9_6 s_10_6 s_2_7 s_3_7 s_8_7 s_9_7 s_1_8 s_2_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_1_4 s_2_4 s_4_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_8_5 s_1_6 s_5_6 s_9_6 s_2_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_4_8 s_8_8 s_9_8 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_1_2 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_1_4 s_2_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_8_5 s_1_6 s_5_6 s_6_6 s_9_6 s_2_7 s_8_7 s_9_7 s_1_8 s_2_8 s_4_8 s_9_8 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_9_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_2_3 s_4_3 s_5_3 s_6_3 s_8_3 s_1_4 s_2_4 s_4_4 s_6_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_9_6 s_10_6 s_2_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_4_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_1_4 s_2_4 s_6_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_9_6 s_10_6 s_2_7 s_5_7 s_8_7 s_9_7 s_1_8 s_4_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_9_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_1_4 s_2_4 s_4_4 s_6_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_9_6 s_2_7 s_3_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_8_8 s_9_8 s_1_9 s_2_9 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_3_10 s_4_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_9_1 s_1_2 s_3_2 s_4_2 s_5_2 s_7_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_1_4 s_2_4 s_6_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_9_6 s_2_7 s_3_7 s_5_7 s_8_7 s_9_7 s_1_8 s_9_8 s_1_9 s_2_9 s_4_9 s_6_9 s_7_9 s_8_9 s_1_10 s_3_10 s_4_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_6_1 s_9_1 s_1_2 s_3_2 s_4_2 s_6_2 s_9_2 s_2_3 s_5_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_3_5 s_4_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_10_6 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_5_9 s_7_9 s_8_9 s_1_10 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_6_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_9_2 s_2_3 s_3_3 s_5_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_7_6 s_10_6 s_4_7 s_5_7 s_6_7 s_9_7 s_1_8 s_3_8 s_4_8 s_5_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_5_9 s_7_9 s_8_9 s_9_9 s_1_10 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_6_1 s_1_2 s_3_2 s_4_2 s_6_2 s_8_2 s_9_2 s_10_2 s_2_3 s_5_3 s_7_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_10_4 s_3_5 s_4_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_7_6 s_10_6 s_3_7 s_4_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_3_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_5_9 s_7_9 s_8_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_6_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_5_3 s_7_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_7_4 s_10_4 s_3_5 s_4_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_7_6 s_10_6 s_3_7 s_4_7 s_6_7 s_9_7 s_1_8 s_2_8 s_3_8 s_5_8 s_7_8 s_9_8 s_10_8 s_2_9 s_3_9 s_5_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_2_4 s_4_4 s_7_4 s_9_4 s_10_4 s_3_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_10_6 s_2_7 s_3_7 s_4_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_5_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_2_4 s_7_4 s_9_4 s_10_4 s_3_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_10_6 s_2_7 s_3_7 s_4_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_5_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_9_1 s_3_2 s_4_2 s_6_2 s_7_2 s_9_2 s_2_3 s_4_3 s_5_3 s_8_3 s_2_4 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_3_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_10_6 s_2_7 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_4_8 s_5_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_7_9 s_8_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_9_1 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_9_2 s_2_3 s_3_3 s_4_3 s_5_3 s_8_3 s_2_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_3_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_10_6 s_2_7 s_4_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_4_8 s_5_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_9_1 s_4_2 s_6_2 s_9_2 s_4_3 s_5_3 s_1_4 s_2_4 s_4_4 s_6_4 s_7_4 s_8_4 s_10_4 s_3_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_10_6 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_3_8 s_4_8 s_5_8 s_8_8 s_10_8 s_1_9 s_2_9 s_5_9 s_7_9 s_10_9 s_1_10 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_6_1 s_9_1 s_2_2 s_4_2 s_6_2 s_9_2 s_3_3 s_4_3 s_5_3 s_1_4 s_2_4 s_6_4 s_7_4 s_8_4 s_10_4 s_3_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_10_6 s_4_7 s_5_7 s_8_7 s_9_7 s_1_8 s_3_8 s_4_8 s_5_8 s_10_8 s_1_9 s_2_9 s_5_9 s_7_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_4_2 s_6_2 s_8_2 s_9_2 s_10_2 s_4_3 s_5_3 s_7_3 s_9_3 s_1_4 s_2_4 s_4_4 s_7_4 s_10_4 s_3_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_10_6 s_3_7 s_4_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_5_8 s_8_8 s_10_8 s_2_9 s_3_9 s_5_9 s_7_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_6_1 s_2_2 s_4_2 s_6_2 s_8_2 s_9_2 s_10_2 s_3_3 s_4_3 s_5_3 s_7_3 s_9_3 s_1_4 s_2_4 s_7_4 s_10_4 s_3_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_10_6 s_3_7 s_4_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_5_8 s_10_8 s_2_9 s_3_9 s_5_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_7_6 s_9_6 s_10_6 s_2_7 s_3_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_7_8 s_8_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_7_6 s_9_6 s_10_6 s_2_7 s_3_7 s_6_7 s_9_7 s_1_8 s_2_8 s_7_8 s_10_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_9_1 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_5_3 s_6_3 s_8_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_7_6 s_9_6 s_10_6 s_2_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_4_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_9_1 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_3_3 s_5_3 s_6_3 s_8_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_7_6 s_9_6 s_10_6 s_2_7 s_5_7 s_6_7 s_9_7 s_1_8 s_4_8 s_7_8 s_10_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_7_1 s_1_2 s_6_2 s_7_2 s_9_2 s_10_2 s_3_3 s_5_3 s_7_3 s_8_3 s_3_4 s_4_4 s_5_4 s_7_4 s_9_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_7_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_10_7 s_1_8 s_5_8 s_7_8 s_10_8 s_2_9 s_4_9 s_5_9 s_1_10 s_2_10 s_3_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_5_1 s_7_1 s_1_2 s_2_2 s_6_2 s_7_2 s_9_2 s_10_2 s_5_3 s_7_3 s_8_3 s_3_4 s_5_4 s_7_4 s_9_4 s_1_5 s_3_5 s_4_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_7_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_10_7 s_1_8 s_5_8 s_7_8 s_8_8 s_10_8 s_2_9 s_4_9 s_5_9 s_9_9 s_1_10 s_2_10 s_3_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_7_1 s_1_2 s_3_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_1_4 s_4_4 s_9_4 s_1_5 s_2_5 s_3_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_9_6 s_1_7 s_2_7 s_3_7 s_7_7 s_8_7 s_1_8 s_9_8 s_10_8 s_2_9 s_4_9 s_5_9 s_6_9 s_8_9 s_1_10 s_2_10 s_3_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_5_1 s_7_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_1_4 s_9_4 s_1_5 s_2_5 s_3_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_9_6 s_1_7 s_2_7 s_3_7 s_8_7 s_1_8 s_8_8 s_9_8 s_10_8 s_2_9 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_7_1 s_8_1 s_1_2 s_3_2 s_5_2 s_6_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_10_3 s_1_4 s_4_4 s_5_4 s_7_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_7_6 s_9_6 s_1_7 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_3_8 s_5_8 s_9_8 s_10_8 s_5_9 s_6_9 s_8_9 s_1_10 s_2_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_5_1 s_7_1 s_8_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_10_2 s_2_3 s_4_3 s_5_3 s_7_3 s_10_3 s_1_4 s_5_4 s_7_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_7_6 s_9_6 s_1_7 s_3_7 s_4_7 s_6_7 s_8_7 s_3_8 s_5_8 s_8_8 s_9_8 s_10_8 s_5_9 s_6_9 s_8_9 s_9_9 s_1_10 s_2_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_7_1 s_8_1 s_1_2 s_6_2 s_10_2 s_3_3 s_5_3 s_6_3 s_7_3 s_10_3 s_3_4 s_4_4 s_1_5 s_3_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_1_7 s_3_7 s_7_7 s_10_7 s_3_8 s_7_8 s_10_8 s_5_9 s_1_10 s_2_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_5_1 s_7_1 s_8_1 s_1_2 s_2_2 s_6_2 s_10_2 s_5_3 s_6_3 s_7_3 s_10_3 s_3_4 s_1_5 s_3_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_1_7 s_3_7 s_10_7 s_3_8 s_7_8 s_8_8 s_10_8 s_5_9 s_9_9 s_1_10 s_2_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_7_1 s_1_2 s_3_2 s_6_2 s_9_2 s_10_2 s_2_3 s_3_3 s_5_3 s_7_3 s_1_4 s_3_4 s_4_4 s_5_4 s_7_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_7_6 s_1_7 s_3_7 s_4_7 s_6_7 s_7_7 s_1_8 s_3_8 s_5_8 s_7_8 s_9_8 s_10_8 s_2_9 s_5_9 s_8_9 s_1_10 s_2_10 s_3_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_5_1 s_6_1 s_7_1 s_1_2 s_2_2 s_3_2 s_6_2 s_9_2 s_10_2 s_2_3 s_5_3 s_7_3 s_1_4 s_3_4 s_5_4 s_7_4 s_1_5 s_3_5 s_4_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_7_6 s_1_7 s_3_7 s_4_7 s_6_7 s_1_8 s_3_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_5_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_7_1 s_8_1 s_1_2 s_3_2 s_6_2 s_7_2 s_10_2 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_3_4 s_4_4 s_9_4 s_1_5 s_3_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_1_7 s_2_7 s_3_7 s_7_7 s_7_8 s_9_8 s_10_8 s_4_9 s_5_9 s_8_9 s_1_10 s_2_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_5_1 s_6_1 s_7_1 s_8_1 s_1_2 s_2_2 s_3_2 s_6_2 s_7_2 s_10_2 s_2_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_3_4 s_9_4 s_1_5 s_3_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_1_7 s_2_7 s_3_7 s_7_8 s_8_8 s_9_8 s_10_8 s_4_9 s_5_9 s_8_9 s_9_9 s_1_10 s_2_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_1_2 s_5_2 s_6_2 s_7_2 s_10_2 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_10_3 s_4_4 s_5_4 s_7_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_7_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_10_7 s_5_8 s_10_8 s_4_9 s_5_9 s_6_9 s_1_10 s_2_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_1_2 s_2_2 s_5_2 s_6_2 s_7_2 s_10_2 s_4_3 s_5_3 s_7_3 s_8_3 s_10_3 s_5_4 s_7_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_7_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_8_7 s_10_7 s_5_8 s_8_8 s_10_8 s_4_9 s_5_9 s_6_9 s_9_9 s_1_10 s_2_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_7_1 s_8_1 s_6_2 s_7_2 s_10_2 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_4_4 s_5_4 s_9_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_7_6 s_1_7 s_2_7 s_3_7 s_6_7 s_7_7 s_8_7 s_10_8 s_4_9 s_5_9 s_10_9 s_1_10 s_2_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_7_1 s_8_1 s_2_2 s_6_2 s_7_2 s_10_2 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_5_4 s_9_4 s_1_5 s_3_5 s_4_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_7_6 s_1_7 s_2_7 s_3_7 s_6_7 s_8_7 s_8_8 s_10_8 s_4_9 s_5_9 s_9_9 s_10_9 s_1_10 s_2_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_3_2 s_5_2 s_6_2 s_7_2 s_10_2 s_2_3 s_3_3 s_5_3 s_7_3 s_8_3 s_10_3 s_3_4 s_4_4 s_7_4 s_9_4 s_1_5 s_2_5 s_3_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_7_7 s_10_7 s_5_8 s_7_8 s_9_8 s_10_8 s_4_9 s_5_9 s_6_9 s_8_9 s_10_9 s_1_10 s_2_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_10_2 s_2_3 s_5_3 s_7_3 s_8_3 s_10_3 s_3_4 s_7_4 s_9_4 s_1_5 s_2_5 s_3_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_10_7 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_7_1 s_6_2 s_9_2 s_10_2 s_3_3 s_4_3 s_5_3 s_7_3 s_1_4 s_4_4 s_7_4 s_1_5 s_3_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_1_7 s_3_7 s_4_7 s_7_7 s_8_7 s_1_8 s_3_8 s_5_8 s_10_8 s_2_9 s_5_9 s_10_9 s_1_10 s_2_10 s_3_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_7_1 s_2_2 s_6_2 s_9_2 s_10_2 s_4_3 s_5_3 s_7_3 s_1_4 s_7_4 s_1_5 s_3_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_1_7 s_3_7 s_4_7 s_8_7 s_1_8 s_3_8 s_5_8 s_8_8 s_10_8 s_2_9 s_5_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_7_1 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_1_4 s_3_4 s_4_4 s_5_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_7_6 s_9_6 s_1_7 s_2_7 s_3_7 s_6_7 s_7_7 s_1_8 s_7_8 s_10_8 s_2_9 s_4_9 s_5_9 s_6_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_7_1 s_2_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_5_3 s_6_3 s_7_3 s_8_3 s_1_4 s_3_4 s_5_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_7_6 s_9_6 s_1_7 s_2_7 s_3_7 s_6_7 s_1_8 s_7_8 s_8_8 s_10_8 s_2_9 s_4_9 s_5_9 s_6_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_7_1 s_3_2 s_6_2 s_7_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_4_4 s_7_4 s_9_4 s_1_5 s_3_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_1_7 s_2_7 s_3_7 s_4_7 s_7_7 s_8_7 s_10_7 s_1_8 s_5_8 s_9_8 s_10_8 s_2_9 s_4_9 s_5_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_7_1 s_2_2 s_3_2 s_6_2 s_7_2 s_9_2 s_10_2 s_2_3 s_4_3 s_5_3 s_7_3 s_8_3 s_7_4 s_9_4 s_1_5 s_3_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_1_7 s_2_7 s_3_7 s_4_7 s_8_7 s_10_7 s_1_8 s_5_8 s_8_8 s_9_8 s_10_8 s_2_9 s_4_9 s_5_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_7_1 s_8_1 s_3_2 s_6_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_10_3 s_4_4 s_5_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_7_6 s_1_7 s_3_7 s_6_7 s_7_7 s_8_7 s_10_7 s_3_8 s_9_8 s_10_8 s_5_9 s_8_9 s_10_9 s_1_10 s_2_10 s_5_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_7_1 s_8_1 s_2_2 s_3_2 s_6_2 s_10_2 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_10_3 s_5_4 s_1_5 s_3_5 s_4_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_7_6 s_1_7 s_3_7 s_6_7 s_8_7 s_10_7 s_3_8 s_8_8 s_9_8 s_10_8 s_5_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_7_1 s_8_1 s_5_2 s_6_2 s_10_2 s_3_3 s_5_3 s_7_3 s_10_3 s_1_4 s_3_4 s_4_4 s_7_4 s_1_5 s_2_5 s_3_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_9_6 s_1_7 s_3_7 s_4_7 s_7_7 s_3_8 s_5_8 s_7_8 s_10_8 s_5_9 s_6_9 s_10_9 s_1_10 s_2_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_7_1 s_8_1 s_2_2 s_5_2 s_6_2 s_10_2 s_5_3 s_7_3 s_10_3 s_1_4 s_3_4 s_7_4 s_1_5 s_2_5 s_3_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_9_6 s_1_7 s_3_7 s_4_7 s_3_8 s_5_8 s_7_8 s_8_8 s_10_8 s_5_9 s_6_9 s_9_9 s_10_9 s_1_10 s_2_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_5_1 s_7_1 s_8_1 s_9_1 s_2_2 s_5_2 s_6_2 s_8_2 s_5_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_7_4 s_8_4 s_1_5 s_2_5 s_3_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_9_6 s_1_7 s_4_7 s_5_7 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_10_8 s_1_9 s_3_9 s_5_9 s_6_9 s_9_9 s_10_9 s_1_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_7_1 s_8_1 s_9_1 s_5_2 s_6_2 s_8_2 s_3_3 s_5_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_1_5 s_2_5 s_3_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_9_6 s_1_7 s_4_7 s_5_7 s_7_7 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_10_8 s_1_9 s_3_9 s_5_9 s_6_9 s_10_9 s_1_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_2_2 s_3_2 s_6_2 s_8_2 s_2_3 s_4_3 s_5_3 s_6_3 s_9_3 s_10_3 s_5_4 s_6_4 s_8_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_7_6 s_1_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_3_8 s_4_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_5_9 s_8_9 s_9_9 s_10_9 s_1_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_3_2 s_6_2 s_8_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_8_4 s_1_5 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_7_6 s_1_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_3_8 s_4_8 s_9_8 s_10_8 s_1_9 s_3_9 s_5_9 s_8_9 s_10_9 s_1_10 s_5_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_7_1 s_9_1 s_2_2 s_3_2 s_6_2 s_7_2 s_8_2 s_9_2 s_2_3 s_4_3 s_5_3 s_8_3 s_9_3 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_3_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_1_7 s_2_7 s_4_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_7_1 s_9_1 s_3_2 s_6_2 s_7_2 s_8_2 s_9_2 s_2_3 s_3_3 s_4_3 s_5_3 s_8_3 s_9_3 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_3_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_1_7 s_2_7 s_4_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_8_9 s_10_9 s_1_10 s_3_10 s_5_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_7_1 s_9_1 s_2_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_5_3 s_6_3 s_8_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_7_6 s_9_6 s_1_7 s_2_7 s_5_7 s_6_7 s_1_8 s_2_8 s_4_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_9_9 s_10_9 s_1_10 s_3_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_7_1 s_9_1 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_3_3 s_5_3 s_6_3 s_8_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_7_6 s_9_6 s_1_7 s_2_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_4_8 s_7_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_10_9 s_1_10 s_3_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_5_1 s_7_1 s_9_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_2_3 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_1_4 s_6_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_9_6 s_1_7 s_2_7 s_5_7 s_8_7 s_1_8 s_2_8 s_4_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_1_10 s_3_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_7_1 s_9_1 s_1_2 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_1_4 s_4_4 s_6_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_9_6 s_1_7 s_2_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_4_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_1_10 s_3_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_5_1 s_7_1 s_9_1 s_1_2 s_2_2 s_6_2 s_7_2 s_8_2 s_9_2 s_5_3 s_8_3 s_9_3 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_7_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_9_9 s_1_10 s_3_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_7_1 s_9_1 s_1_2 s_6_2 s_7_2 s_8_2 s_9_2 s_3_3 s_5_3 s_8_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_3_5 s_4_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_7_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_1_10 s_3_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_6_2 s_8_2 s_5_3 s_6_3 s_9_3 s_10_3 s_3_4 s_6_4 s_8_4 s_1_5 s_3_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_1_7 s_5_7 s_10_7 s_2_8 s_3_8 s_4_8 s_7_8 s_8_8 s_10_8 s_1_9 s_3_9 s_5_9 s_9_9 s_1_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_1_2 s_6_2 s_8_2 s_3_3 s_5_3 s_6_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_8_4 s_1_5 s_3_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_1_7 s_5_7 s_7_7 s_10_7 s_2_8 s_3_8 s_4_8 s_7_8 s_10_8 s_1_9 s_3_9 s_5_9 s_1_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_5_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_8_2 s_2_3 s_4_3 s_5_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_7_4 s_8_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_7_6 s_9_6 s_1_7 s_4_7 s_5_7 s_6_7 s_8_7 s_2_8 s_3_8 s_4_8 s_5_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_5_9 s_6_9 s_8_9 s_9_9 s_1_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_7_1 s_8_1 s_9_1 s_1_2 s_3_2 s_5_2 s_6_2 s_8_2 s_2_3 s_3_3 s_4_3 s_5_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_9_6 s_1_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_3_8 s_4_8 s_5_8 s_9_8 s_10_8 s_1_9 s_3_9 s_5_9 s_6_9 s_8_9 s_1_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_6_2 s_7_2 s_8_2 s_2_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_8_4 s_9_4 s_1_5 s_3_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_1_7 s_2_7 s_5_7 s_2_8 s_4_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_8_9 s_9_9 s_1_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_3_2 s_6_2 s_7_2 s_8_2 s_2_3 s_3_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_8_4 s_9_4 s_1_5 s_3_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_1_7 s_2_7 s_5_7 s_7_7 s_2_8 s_4_8 s_7_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_8_9 s_1_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_5_2 s_6_2 s_7_2 s_8_2 s_4_3 s_5_3 s_8_3 s_9_3 s_10_3 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_7_6 s_9_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_4_8 s_5_8 s_8_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_9_9 s_1_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_5_2 s_6_2 s_7_2 s_8_2 s_3_3 s_4_3 s_5_3 s_8_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_9_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_4_8 s_5_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_1_10 s_8_10 s_9_10 )))

(assert (not (and s_5_1 s_6_1 s_7_1 s_9_1 s_1_2 s_2_2 s_3_2 s_6_2 s_8_2 s_9_2 s_2_3 s_5_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_7_6 s_1_7 s_4_7 s_5_7 s_6_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_8_9 s_9_9 s_1_10 s_3_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_7_1 s_9_1 s_1_2 s_3_2 s_6_2 s_8_2 s_9_2 s_2_3 s_3_3 s_5_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_1_5 s_3_5 s_4_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_1_7 s_4_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_8_9 s_1_10 s_3_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_7_1 s_9_1 s_2_2 s_6_2 s_8_2 s_9_2 s_4_3 s_5_3 s_9_3 s_1_4 s_6_4 s_7_4 s_8_4 s_1_5 s_3_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_1_7 s_4_7 s_5_7 s_8_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_9_9 s_10_9 s_1_10 s_3_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_7_1 s_9_1 s_6_2 s_8_2 s_9_2 s_3_3 s_4_3 s_5_3 s_9_3 s_1_4 s_4_4 s_6_4 s_7_4 s_8_4 s_1_5 s_3_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_1_7 s_4_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_10_8 s_1_9 s_2_9 s_3_9 s_5_9 s_10_9 s_1_10 s_3_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_2_2 s_6_2 s_7_2 s_8_2 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_8_4 s_9_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_7_6 s_1_7 s_2_7 s_5_7 s_6_7 s_8_7 s_2_8 s_4_8 s_8_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_9_9 s_10_9 s_1_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_6_2 s_7_2 s_8_2 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_1_5 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_7_6 s_1_7 s_2_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_4_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_10_9 s_1_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_2_3 s_5_3 s_8_3 s_9_3 s_10_3 s_3_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_9_6 s_1_7 s_2_7 s_4_7 s_5_7 s_10_7 s_2_8 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_2_3 s_3_3 s_5_3 s_8_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_3_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_9_6 s_1_7 s_2_7 s_4_7 s_5_7 s_7_7 s_10_7 s_2_8 s_4_8 s_5_8 s_7_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_10_9 s_1_10 s_8_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_3_2 s_5_2 s_7_2 s_8_2 s_1_3 s_2_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_3_4 s_6_4 s_7_4 s_9_4 s_2_5 s_3_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_10_7 s_2_8 s_5_8 s_7_8 s_8_8 s_9_8 s_1_9 s_3_9 s_4_9 s_6_9 s_8_9 s_10_9 s_1_10 s_6_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_7_1 s_8_1 s_9_1 s_7_2 s_8_2 s_1_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_9_4 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_7_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_8_7 s_2_8 s_8_8 s_1_9 s_3_9 s_4_9 s_10_9 s_1_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_2_2 s_3_2 s_5_2 s_7_2 s_8_2 s_1_3 s_2_3 s_3_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_7_4 s_9_4 s_2_5 s_3_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_10_7 s_2_8 s_5_8 s_7_8 s_9_8 s_1_9 s_3_9 s_4_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_7_1 s_8_1 s_9_1 s_2_2 s_7_2 s_8_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_9_4 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_7_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_1_9 s_3_9 s_4_9 s_9_9 s_10_9 s_1_10 s_6_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_7_1 s_9_1 s_2_2 s_8_2 s_9_2 s_1_3 s_3_3 s_4_3 s_5_3 s_7_3 s_9_3 s_1_4 s_4_4 s_6_4 s_7_4 s_3_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_3_8 s_5_8 s_1_9 s_2_9 s_3_9 s_9_9 s_10_9 s_1_10 s_3_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_7_1 s_9_1 s_8_2 s_9_2 s_1_3 s_4_3 s_5_3 s_7_3 s_9_3 s_1_4 s_6_4 s_7_4 s_3_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_8_7 s_1_8 s_2_8 s_3_8 s_5_8 s_8_8 s_1_9 s_2_9 s_3_9 s_10_9 s_1_10 s_3_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_7_1 s_8_1 s_9_1 s_3_2 s_8_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_5_4 s_6_4 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_7_6 s_10_6 s_1_7 s_3_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_3_8 s_8_8 s_9_8 s_1_9 s_3_9 s_8_9 s_10_9 s_1_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_7_1 s_8_1 s_9_1 s_2_2 s_3_2 s_8_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_7_6 s_10_6 s_1_7 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_3_8 s_9_8 s_1_9 s_3_9 s_8_9 s_9_9 s_10_9 s_1_10 s_5_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_7_1 s_9_1 s_2_2 s_3_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_4_4 s_6_4 s_7_4 s_9_4 s_3_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_5_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_5_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_7_1 s_8_1 s_9_1 s_2_2 s_5_2 s_8_2 s_1_3 s_3_3 s_5_3 s_7_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_7_4 s_2_5 s_3_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_7_7 s_2_8 s_3_8 s_5_8 s_7_8 s_1_9 s_3_9 s_6_9 s_9_9 s_10_9 s_1_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_5_2 s_7_2 s_8_2 s_1_3 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_5_4 s_6_4 s_7_4 s_9_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_5_8 s_8_8 s_1_9 s_3_9 s_4_9 s_6_9 s_1_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_3_2 s_7_2 s_8_2 s_1_3 s_2_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_9_4 s_3_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_2_8 s_7_8 s_8_8 s_9_8 s_1_9 s_3_9 s_4_9 s_8_9 s_1_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_6_1 s_7_1 s_9_1 s_1_2 s_3_2 s_8_2 s_9_2 s_1_3 s_2_3 s_5_3 s_7_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_7_4 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_7_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_1_8 s_2_8 s_3_8 s_5_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_8_9 s_1_10 s_3_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_7_1 s_9_1 s_3_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_6_4 s_7_4 s_9_4 s_3_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_5_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_8_9 s_10_9 s_1_10 s_3_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_5_2 s_7_2 s_8_2 s_1_3 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_5_8 s_1_9 s_3_9 s_4_9 s_6_9 s_9_9 s_1_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_7_2 s_8_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_9_4 s_3_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_7_7 s_2_8 s_7_8 s_9_8 s_1_9 s_3_9 s_4_9 s_8_9 s_9_9 s_1_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_7_1 s_9_1 s_1_2 s_2_2 s_3_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_5_3 s_7_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_3_5 s_4_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_7_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_3_8 s_5_8 s_7_8 s_9_8 s_1_9 s_2_9 s_3_9 s_8_9 s_9_9 s_1_10 s_3_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_2_1 s_3_1 s_7_1 s_8_1 s_9_1 s_5_2 s_8_2 s_1_3 s_5_3 s_7_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_7_4 s_2_5 s_3_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_2_8 s_3_8 s_5_8 s_7_8 s_8_8 s_1_9 s_3_9 s_6_9 s_10_9 s_1_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_7_1 s_9_1 s_5_2 s_7_2 s_8_2 s_9_2 s_1_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_9_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_1_8 s_2_8 s_7_8 s_8_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_10_9 s_1_10 s_3_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_7_1 s_9_1 s_2_2 s_5_2 s_7_2 s_8_2 s_9_2 s_1_3 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_9_4 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_7_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_9_9 s_10_9 s_1_10 s_3_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_3_1 s_4_1 s_7_1 s_8_1 s_9_1 s_1_2 s_8_2 s_1_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_3_4 s_6_4 s_3_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_10_6 s_1_7 s_3_7 s_5_7 s_10_7 s_2_8 s_3_8 s_7_8 s_8_8 s_1_9 s_3_9 s_1_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_7_1 s_9_1 s_1_2 s_7_2 s_8_2 s_9_2 s_1_3 s_5_3 s_7_3 s_8_3 s_9_3 s_3_4 s_5_4 s_6_4 s_7_4 s_9_4 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_7_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_5_8 s_7_8 s_8_8 s_1_9 s_2_9 s_3_9 s_4_9 s_1_10 s_3_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_8_2 s_1_3 s_3_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_3_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_10_6 s_1_7 s_3_7 s_5_7 s_7_7 s_10_7 s_2_8 s_3_8 s_7_8 s_1_9 s_3_9 s_9_9 s_1_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_7_1 s_9_1 s_1_2 s_2_2 s_7_2 s_8_2 s_9_2 s_1_3 s_3_3 s_5_3 s_7_3 s_8_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_3_5 s_4_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_7_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_5_8 s_7_8 s_1_9 s_2_9 s_3_9 s_4_9 s_9_9 s_1_10 s_3_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_3_1 s_7_1 s_8_1 s_9_1 s_1_2 s_3_2 s_5_2 s_8_2 s_1_3 s_2_3 s_4_3 s_5_3 s_7_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_7_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_7_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_2_8 s_3_8 s_5_8 s_8_8 s_9_8 s_1_9 s_3_9 s_6_9 s_8_9 s_1_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_7_1 s_9_1 s_1_2 s_3_2 s_5_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_6_4 s_9_4 s_2_5 s_3_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_8_7 s_1_8 s_2_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_8_9 s_1_10 s_3_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_5_2 s_8_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_7_4 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_3_8 s_5_8 s_9_8 s_1_9 s_3_9 s_6_9 s_8_9 s_9_9 s_1_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_7_1 s_9_1 s_1_2 s_2_2 s_3_2 s_5_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_4_4 s_6_4 s_9_4 s_2_5 s_3_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_8_9 s_9_9 s_1_10 s_3_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_2_2 s_3_2 s_5_2 s_7_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_8_3 s_10_3 s_3_4 s_4_4 s_7_4 s_8_4 s_9_4 s_2_5 s_3_5 s_5_5 s_10_5 s_3_6 s_5_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_7_7 s_10_7 s_4_8 s_5_8 s_7_8 s_9_8 s_4_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_7_1 s_8_1 s_2_2 s_7_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_10_3 s_1_4 s_4_4 s_5_4 s_8_4 s_9_4 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_10_5 s_7_6 s_10_6 s_1_7 s_2_7 s_6_7 s_7_7 s_8_7 s_4_8 s_4_9 s_9_9 s_10_9 s_1_10 s_2_10 s_6_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_7_1 s_2_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_1_4 s_4_4 s_7_4 s_8_4 s_3_5 s_5_5 s_10_5 s_3_6 s_5_6 s_10_6 s_1_7 s_4_7 s_7_7 s_8_7 s_1_8 s_3_8 s_4_8 s_5_8 s_2_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_6_10 s_7_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_1_2 s_2_2 s_5_2 s_7_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_8_3 s_10_3 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_10_5 s_3_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_6_7 s_7_7 s_8_7 s_10_7 s_4_8 s_5_8 s_4_9 s_6_9 s_9_9 s_1_10 s_2_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_7_1 s_8_1 s_1_2 s_2_2 s_3_2 s_7_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_8_3 s_10_3 s_1_4 s_3_4 s_4_4 s_8_4 s_9_4 s_3_5 s_5_5 s_8_5 s_10_5 s_5_6 s_10_6 s_1_7 s_2_7 s_7_7 s_4_8 s_7_8 s_9_8 s_4_9 s_8_9 s_9_9 s_1_10 s_2_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_7_1 s_1_2 s_2_2 s_3_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_1_4 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_3_5 s_4_5 s_5_5 s_6_5 s_10_5 s_3_6 s_7_6 s_10_6 s_1_7 s_4_7 s_6_7 s_7_7 s_1_8 s_3_8 s_4_8 s_5_8 s_7_8 s_9_8 s_2_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_7_1 s_8_1 s_2_2 s_3_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_10_3 s_4_4 s_5_4 s_8_4 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_1_6 s_7_6 s_10_6 s_1_7 s_6_7 s_7_7 s_8_7 s_10_7 s_3_8 s_4_8 s_9_8 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_5_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_7_1 s_2_2 s_3_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_8_3 s_4_4 s_7_4 s_8_4 s_9_4 s_3_5 s_5_5 s_1_6 s_3_6 s_5_6 s_10_6 s_1_7 s_2_7 s_4_7 s_7_7 s_8_7 s_10_7 s_1_8 s_4_8 s_5_8 s_9_8 s_2_9 s_4_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_7_1 s_8_1 s_2_2 s_5_2 s_10_2 s_1_3 s_3_3 s_5_3 s_10_3 s_1_4 s_3_4 s_4_4 s_7_4 s_8_4 s_2_5 s_3_5 s_5_5 s_1_6 s_3_6 s_5_6 s_9_6 s_10_6 s_1_7 s_4_7 s_7_7 s_3_8 s_4_8 s_5_8 s_7_8 s_6_9 s_9_9 s_10_9 s_1_10 s_2_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_7_1 s_2_2 s_5_2 s_7_2 s_9_2 s_10_2 s_1_3 s_3_3 s_5_3 s_6_3 s_8_3 s_1_4 s_3_4 s_4_4 s_5_4 s_8_4 s_9_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_1_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_6_7 s_7_7 s_1_8 s_4_8 s_7_8 s_2_9 s_4_9 s_6_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_7_1 s_8_1 s_1_2 s_2_2 s_10_2 s_1_3 s_3_3 s_5_3 s_6_3 s_10_3 s_3_4 s_4_4 s_8_4 s_3_5 s_5_5 s_8_5 s_1_6 s_5_6 s_10_6 s_1_7 s_7_7 s_10_7 s_3_8 s_4_8 s_7_8 s_9_9 s_1_10 s_2_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_7_1 s_1_2 s_2_2 s_7_2 s_9_2 s_10_2 s_1_3 s_3_3 s_5_3 s_8_3 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_3_5 s_4_5 s_5_5 s_6_5 s_1_6 s_3_6 s_7_6 s_10_6 s_1_7 s_2_7 s_4_7 s_6_7 s_7_7 s_10_7 s_1_8 s_4_8 s_5_8 s_7_8 s_2_9 s_4_9 s_9_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_7_1 s_8_1 s_1_2 s_2_2 s_3_2 s_5_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_10_3 s_1_4 s_4_4 s_5_4 s_7_4 s_8_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_1_6 s_3_6 s_7_6 s_9_6 s_10_6 s_1_7 s_4_7 s_6_7 s_7_7 s_8_7 s_3_8 s_4_8 s_5_8 s_9_8 s_6_9 s_8_9 s_9_9 s_1_10 s_2_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_7_1 s_1_2 s_2_2 s_3_2 s_5_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_1_4 s_4_4 s_8_4 s_9_4 s_2_5 s_3_5 s_5_5 s_8_5 s_1_6 s_5_6 s_9_6 s_10_6 s_1_7 s_2_7 s_7_7 s_8_7 s_1_8 s_4_8 s_9_8 s_2_9 s_4_9 s_6_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_3_2 s_5_2 s_7_2 s_10_2 s_1_3 s_2_3 s_5_3 s_8_3 s_10_3 s_3_4 s_7_4 s_8_4 s_9_4 s_2_5 s_3_5 s_10_5 s_3_6 s_5_6 s_6_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_10_7 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_4_9 s_6_9 s_8_9 s_10_9 s_1_10 s_2_10 s_6_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_7_1 s_8_1 s_7_2 s_10_2 s_1_3 s_4_3 s_5_3 s_6_3 s_8_3 s_10_3 s_1_4 s_5_4 s_8_4 s_9_4 s_3_5 s_4_5 s_6_5 s_8_5 s_10_5 s_6_6 s_7_6 s_10_6 s_1_7 s_2_7 s_6_7 s_8_7 s_4_8 s_8_8 s_4_9 s_10_9 s_1_10 s_2_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_7_1 s_9_2 s_10_2 s_1_3 s_4_3 s_5_3 s_1_4 s_7_4 s_8_4 s_3_5 s_10_5 s_3_6 s_5_6 s_6_6 s_10_6 s_1_7 s_4_7 s_8_7 s_1_8 s_3_8 s_4_8 s_5_8 s_8_8 s_2_9 s_10_9 s_1_10 s_2_10 s_3_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_7_1 s_8_1 s_3_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_10_3 s_5_4 s_8_4 s_3_5 s_4_5 s_6_5 s_8_5 s_1_6 s_6_6 s_7_6 s_10_6 s_1_7 s_6_7 s_8_7 s_10_7 s_3_8 s_4_8 s_8_8 s_9_8 s_8_9 s_10_9 s_1_10 s_2_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_7_1 s_3_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_8_3 s_7_4 s_8_4 s_9_4 s_3_5 s_1_6 s_3_6 s_5_6 s_6_6 s_10_6 s_1_7 s_2_7 s_4_7 s_8_7 s_10_7 s_1_8 s_4_8 s_5_8 s_8_8 s_9_8 s_2_9 s_4_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_7_1 s_8_1 s_5_2 s_10_2 s_1_3 s_5_3 s_10_3 s_1_4 s_3_4 s_7_4 s_8_4 s_2_5 s_3_5 s_1_6 s_3_6 s_5_6 s_6_6 s_9_6 s_10_6 s_1_7 s_4_7 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_6_9 s_10_9 s_1_10 s_2_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_7_1 s_5_2 s_7_2 s_9_2 s_10_2 s_1_3 s_5_3 s_6_3 s_8_3 s_1_4 s_3_4 s_5_4 s_8_4 s_9_4 s_2_5 s_3_5 s_4_5 s_6_5 s_8_5 s_1_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_6_7 s_1_8 s_4_8 s_7_8 s_8_8 s_2_9 s_4_9 s_6_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_7_1 s_8_1 s_1_2 s_5_2 s_7_2 s_10_2 s_1_3 s_4_3 s_5_3 s_8_3 s_10_3 s_5_4 s_7_4 s_8_4 s_9_4 s_2_5 s_3_5 s_4_5 s_6_5 s_10_5 s_3_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_6_7 s_8_7 s_10_7 s_4_8 s_5_8 s_8_8 s_4_9 s_6_9 s_1_10 s_2_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_6_1 s_7_1 s_8_1 s_1_2 s_3_2 s_7_2 s_10_2 s_1_3 s_2_3 s_5_3 s_6_3 s_8_3 s_10_3 s_1_4 s_3_4 s_8_4 s_9_4 s_3_5 s_8_5 s_10_5 s_5_6 s_6_6 s_10_6 s_1_7 s_2_7 s_4_8 s_7_8 s_8_8 s_9_8 s_4_9 s_8_9 s_1_10 s_2_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_6_1 s_7_1 s_1_2 s_3_2 s_9_2 s_10_2 s_1_3 s_2_3 s_5_3 s_1_4 s_3_4 s_5_4 s_7_4 s_8_4 s_3_5 s_4_5 s_6_5 s_10_5 s_3_6 s_6_6 s_7_6 s_10_6 s_1_7 s_4_7 s_6_7 s_1_8 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_2_9 s_8_9 s_1_10 s_2_10 s_3_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_7_1 s_8_1 s_1_2 s_10_2 s_1_3 s_5_3 s_6_3 s_10_3 s_3_4 s_8_4 s_3_5 s_8_5 s_1_6 s_5_6 s_6_6 s_10_6 s_1_7 s_10_7 s_3_8 s_4_8 s_7_8 s_8_8 s_1_10 s_2_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_7_1 s_1_2 s_7_2 s_9_2 s_10_2 s_1_3 s_5_3 s_8_3 s_3_4 s_5_4 s_7_4 s_8_4 s_9_4 s_3_5 s_4_5 s_6_5 s_1_6 s_3_6 s_6_6 s_7_6 s_10_6 s_1_7 s_2_7 s_4_7 s_6_7 s_10_7 s_1_8 s_4_8 s_5_8 s_7_8 s_8_8 s_2_9 s_4_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_7_1 s_8_1 s_1_2 s_3_2 s_5_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_10_3 s_1_4 s_5_4 s_7_4 s_8_4 s_2_5 s_3_5 s_4_5 s_6_5 s_1_6 s_3_6 s_6_6 s_7_6 s_9_6 s_10_6 s_1_7 s_4_7 s_6_7 s_8_7 s_3_8 s_4_8 s_5_8 s_8_8 s_9_8 s_6_9 s_8_9 s_1_10 s_2_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_7_1 s_1_2 s_3_2 s_5_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_8_3 s_1_4 s_8_4 s_9_4 s_2_5 s_3_5 s_8_5 s_1_6 s_5_6 s_6_6 s_9_6 s_10_6 s_1_7 s_2_7 s_8_7 s_1_8 s_4_8 s_8_8 s_9_8 s_2_9 s_4_9 s_6_9 s_8_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_1_3 s_2_3 s_3_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_8_6 s_9_6 s_1_7 s_2_7 s_4_7 s_7_7 s_9_7 s_10_7 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_4_2 s_6_2 s_7_2 s_8_2 s_1_3 s_3_3 s_4_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_8_4 s_9_4 s_1_5 s_4_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_7_6 s_8_6 s_1_7 s_2_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_4_8 s_6_8 s_1_9 s_3_9 s_4_9 s_5_9 s_7_9 s_10_9 s_1_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_9_1 s_4_2 s_6_2 s_8_2 s_9_2 s_1_3 s_3_3 s_4_3 s_9_3 s_1_4 s_2_4 s_4_4 s_7_4 s_8_4 s_1_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_8_6 s_1_7 s_4_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_1_9 s_2_9 s_3_9 s_5_9 s_7_9 s_10_9 s_1_10 s_3_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_1_3 s_2_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_8_6 s_9_6 s_1_7 s_2_7 s_4_7 s_9_7 s_10_7 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_2_2 s_4_2 s_6_2 s_7_2 s_8_2 s_1_3 s_4_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_8_4 s_9_4 s_1_5 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_7_6 s_8_6 s_1_7 s_2_7 s_6_7 s_8_7 s_9_7 s_2_8 s_4_8 s_6_8 s_8_8 s_1_9 s_3_9 s_4_9 s_5_9 s_7_9 s_9_9 s_10_9 s_1_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_9_1 s_2_2 s_4_2 s_6_2 s_8_2 s_9_2 s_1_3 s_4_3 s_9_3 s_1_4 s_2_4 s_7_4 s_8_4 s_1_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_8_6 s_1_7 s_4_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_1_9 s_2_9 s_3_9 s_5_9 s_7_9 s_9_9 s_10_9 s_1_10 s_3_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_3_2 s_4_2 s_6_2 s_8_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_8_4 s_1_5 s_4_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_7_6 s_8_6 s_1_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_6_8 s_9_8 s_1_9 s_3_9 s_5_9 s_7_9 s_8_9 s_10_9 s_1_10 s_5_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_9_1 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_8_3 s_9_3 s_2_4 s_4_4 s_7_4 s_8_4 s_9_4 s_1_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_8_6 s_1_7 s_2_7 s_4_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_10_9 s_1_10 s_3_10 s_5_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_2_2 s_3_2 s_4_2 s_6_2 s_8_2 s_1_3 s_2_3 s_4_3 s_6_3 s_9_3 s_10_3 s_2_4 s_5_4 s_8_4 s_1_5 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_7_6 s_8_6 s_1_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_6_8 s_8_8 s_9_8 s_1_9 s_3_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_5_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_9_1 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_4_3 s_8_3 s_9_3 s_2_4 s_7_4 s_8_4 s_9_4 s_1_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_8_6 s_1_7 s_2_7 s_4_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_5_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_7_1 s_8_1 s_9_1 s_4_2 s_5_2 s_6_2 s_8_2 s_1_3 s_3_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_7_4 s_8_4 s_1_5 s_2_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_8_6 s_9_6 s_1_7 s_4_7 s_7_7 s_9_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_1_9 s_3_9 s_5_9 s_6_9 s_7_9 s_10_9 s_1_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_7_1 s_9_1 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_3_3 s_6_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_8_4 s_9_4 s_1_5 s_2_5 s_4_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_4_8 s_6_8 s_7_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_10_9 s_1_10 s_3_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_7_1 s_8_1 s_9_1 s_2_2 s_4_2 s_5_2 s_6_2 s_8_2 s_1_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_7_4 s_8_4 s_1_5 s_2_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_8_6 s_9_6 s_1_7 s_4_7 s_9_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_1_9 s_3_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_5_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_7_1 s_9_1 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_6_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_8_4 s_9_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_6_7 s_9_7 s_1_8 s_2_8 s_4_8 s_6_8 s_7_8 s_8_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_3_10 s_5_10 s_7_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_1_3 s_3_3 s_4_3 s_8_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_4_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_4_8 s_5_8 s_6_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_1_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_1_3 s_2_3 s_3_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_8_4 s_9_4 s_1_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_8_6 s_1_7 s_2_7 s_7_7 s_9_7 s_2_8 s_4_8 s_6_8 s_7_8 s_9_8 s_1_9 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_1_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_7_1 s_9_1 s_1_2 s_3_2 s_4_2 s_6_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_1_5 s_4_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_7_6 s_8_6 s_1_7 s_4_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_1_9 s_2_9 s_3_9 s_5_9 s_7_9 s_8_9 s_1_10 s_3_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_1_3 s_4_3 s_8_3 s_9_3 s_10_3 s_2_4 s_5_4 s_7_4 s_8_4 s_9_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_4_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_4_8 s_5_8 s_6_8 s_8_8 s_1_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_1_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_1_3 s_2_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_8_4 s_9_4 s_1_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_8_6 s_1_7 s_2_7 s_9_7 s_2_8 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_1_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_7_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_8_2 s_9_2 s_1_3 s_2_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_7_4 s_8_4 s_1_5 s_4_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_7_6 s_8_6 s_1_7 s_4_7 s_6_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_5_9 s_7_9 s_8_9 s_9_9 s_1_10 s_3_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_1_2 s_4_2 s_6_2 s_8_2 s_1_3 s_3_3 s_6_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_8_4 s_1_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_8_6 s_1_7 s_7_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_1_9 s_3_9 s_5_9 s_7_9 s_1_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_7_1 s_9_1 s_1_2 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_3_3 s_8_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_1_5 s_4_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_7_6 s_8_6 s_1_7 s_2_7 s_4_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_1_10 s_3_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_4_2 s_6_2 s_8_2 s_1_3 s_6_3 s_9_3 s_10_3 s_2_4 s_3_4 s_8_4 s_1_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_8_6 s_1_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_1_9 s_3_9 s_5_9 s_7_9 s_9_9 s_1_10 s_5_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_7_1 s_9_1 s_1_2 s_2_2 s_4_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_8_3 s_9_3 s_2_4 s_3_4 s_5_4 s_7_4 s_8_4 s_9_4 s_1_5 s_4_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_7_6 s_8_6 s_1_7 s_2_7 s_4_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_7_9 s_9_9 s_1_10 s_3_10 s_5_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_7_1 s_8_1 s_9_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_1_3 s_2_3 s_3_3 s_4_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_7_4 s_8_4 s_1_5 s_2_5 s_4_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_9_8 s_1_9 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_1_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_5_1 s_7_1 s_9_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_8_3 s_9_3 s_1_4 s_2_4 s_4_4 s_8_4 s_9_4 s_1_5 s_2_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_8_6 s_9_6 s_1_7 s_2_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_4_8 s_6_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_1_10 s_3_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_1_3 s_2_3 s_4_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_7_4 s_8_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_7_6 s_8_6 s_9_6 s_1_7 s_4_7 s_6_7 s_8_7 s_9_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_1_9 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_8_1 s_2_2 s_3_2 s_5_2 s_7_2 s_8_2 s_10_2 s_2_3 s_8_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_8_6 s_9_6 s_2_7 s_4_7 s_5_7 s_7_7 s_10_7 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_3_9 s_4_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_8_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_8_1 s_1_2 s_2_2 s_5_2 s_7_2 s_8_2 s_10_2 s_4_3 s_8_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_7_6 s_8_6 s_9_6 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_4_8 s_5_8 s_6_8 s_8_8 s_10_8 s_3_9 s_4_9 s_6_9 s_9_9 s_1_10 s_2_10 s_4_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_1_2 s_2_2 s_7_2 s_8_2 s_9_2 s_10_2 s_8_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_4_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_7_6 s_8_6 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_2_9 s_3_9 s_4_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_8_1 s_1_2 s_2_2 s_8_2 s_10_2 s_6_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_8_4 s_10_4 s_1_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_8_6 s_5_7 s_7_7 s_10_7 s_2_8 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_10_8 s_3_9 s_9_9 s_1_10 s_2_10 s_4_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_8_1 s_2_2 s_3_2 s_8_2 s_10_2 s_2_3 s_4_3 s_6_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_8_4 s_10_4 s_1_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_7_6 s_8_6 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_3_8 s_4_8 s_6_8 s_8_8 s_9_8 s_10_8 s_3_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_2_2 s_3_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_4_3 s_8_3 s_9_3 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_8_6 s_2_7 s_4_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_5_1 s_2_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_6_3 s_8_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_7_6 s_8_6 s_9_6 s_2_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_4_8 s_6_8 s_7_8 s_8_8 s_10_8 s_2_9 s_3_9 s_4_9 s_6_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_5_1 s_8_1 s_2_2 s_5_2 s_8_2 s_10_2 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_8_6 s_9_6 s_4_7 s_5_7 s_7_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_3_9 s_6_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_8_1 s_2_2 s_7_2 s_8_2 s_10_2 s_4_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_7_6 s_8_6 s_2_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_4_8 s_6_8 s_8_8 s_10_8 s_3_9 s_4_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_8_1 s_7_2 s_8_2 s_10_2 s_3_3 s_4_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_4_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_7_6 s_8_6 s_2_7 s_5_7 s_6_7 s_8_7 s_2_8 s_4_8 s_6_8 s_10_8 s_3_9 s_4_9 s_10_9 s_1_10 s_2_10 s_4_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_2_2 s_8_2 s_9_2 s_10_2 s_4_3 s_9_3 s_1_4 s_4_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_8_6 s_4_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_10_8 s_2_9 s_3_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_5_1 s_6_1 s_1_2 s_2_2 s_3_2 s_8_2 s_9_2 s_10_2 s_2_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_4_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_7_6 s_8_6 s_4_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_5_1 s_1_2 s_2_2 s_3_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_2_3 s_4_3 s_6_3 s_8_3 s_9_3 s_1_4 s_4_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_8_6 s_9_6 s_2_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_4_8 s_6_8 s_8_8 s_9_8 s_10_8 s_2_9 s_3_9 s_4_9 s_6_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_5_1 s_6_1 s_8_1 s_1_2 s_2_2 s_3_2 s_7_2 s_8_2 s_10_2 s_2_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_8_6 s_2_7 s_5_7 s_7_7 s_2_8 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_3_9 s_4_9 s_8_9 s_9_9 s_1_10 s_2_10 s_4_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_5_1 s_8_1 s_1_2 s_2_2 s_3_2 s_5_2 s_8_2 s_10_2 s_2_3 s_4_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_7_6 s_8_6 s_9_6 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_3_9 s_6_9 s_8_9 s_9_9 s_1_10 s_2_10 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_1_2 s_3_2 s_8_2 s_9_2 s_10_2 s_2_3 s_3_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_1_5 s_4_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_4_7 s_5_7 s_6_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_2_9 s_3_9 s_8_9 s_1_10 s_2_10 s_3_10 s_4_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_8_1 s_1_2 s_3_2 s_7_2 s_8_2 s_10_2 s_2_3 s_3_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_8_4 s_9_4 s_10_4 s_1_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_8_6 s_2_7 s_5_7 s_2_8 s_4_8 s_6_8 s_7_8 s_9_8 s_10_8 s_3_9 s_4_9 s_8_9 s_1_10 s_2_10 s_4_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_8_1 s_4_2 s_5_2 s_6_2 s_8_2 s_10_2 s_5_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_7_4 s_10_4 s_2_5 s_3_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_9_6 s_10_6 s_3_7 s_4_7 s_7_7 s_9_7 s_2_8 s_3_8 s_5_8 s_7_8 s_8_8 s_10_8 s_3_9 s_5_9 s_6_9 s_7_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_8_1 s_2_2 s_4_2 s_5_2 s_6_2 s_8_2 s_10_2 s_3_3 s_5_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_7_4 s_10_4 s_2_5 s_3_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_9_6 s_10_6 s_3_7 s_4_7 s_9_7 s_2_8 s_3_8 s_5_8 s_7_8 s_10_8 s_3_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_8_1 s_4_2 s_6_2 s_7_2 s_8_2 s_10_2 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_9_4 s_10_4 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_7_6 s_10_6 s_2_7 s_3_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_8_8 s_10_8 s_3_9 s_4_9 s_5_9 s_7_9 s_10_9 s_1_10 s_2_10 s_4_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_6_1 s_8_1 s_2_2 s_4_2 s_6_2 s_7_2 s_8_2 s_10_2 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_9_4 s_10_4 s_3_5 s_4_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_7_6 s_10_6 s_2_7 s_3_7 s_6_7 s_8_7 s_9_7 s_2_8 s_10_8 s_3_9 s_4_9 s_5_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_8_1 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_2_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_7_4 s_9_4 s_10_4 s_2_5 s_3_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_7_7 s_9_7 s_10_7 s_2_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_4_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_8_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_2_3 s_3_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_7_4 s_9_4 s_10_4 s_2_5 s_3_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_9_7 s_10_7 s_2_8 s_5_8 s_7_8 s_9_8 s_10_8 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_8_1 s_3_2 s_4_2 s_6_2 s_8_2 s_10_2 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_10_4 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_7_6 s_10_6 s_3_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_8_8 s_9_8 s_10_8 s_3_9 s_5_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_8_1 s_2_2 s_3_2 s_4_2 s_6_2 s_8_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_2_4 s_5_4 s_10_4 s_3_5 s_4_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_7_6 s_10_6 s_3_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_9_8 s_10_8 s_3_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_6_1 s_8_1 s_1_2 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_10_2 s_2_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_9_4 s_10_4 s_3_5 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_10_6 s_2_7 s_3_7 s_7_7 s_9_7 s_2_8 s_7_8 s_8_8 s_9_8 s_10_8 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_1_10 s_2_10 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_6_1 s_8_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_8_2 s_10_2 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_9_4 s_10_4 s_3_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_10_6 s_2_7 s_3_7 s_9_7 s_2_8 s_7_8 s_9_8 s_10_8 s_3_9 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_8_1 s_1_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_7_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_7_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_5_8 s_8_8 s_10_8 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_1_10 s_2_10 s_4_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_6_1 s_8_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_5_4 s_7_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_7_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_5_8 s_10_8 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_1_10 s_2_10 s_4_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_8_1 s_1_2 s_4_2 s_6_2 s_8_2 s_10_2 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_10_4 s_3_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_10_6 s_3_7 s_7_7 s_9_7 s_10_7 s_2_8 s_3_8 s_7_8 s_8_8 s_10_8 s_3_9 s_5_9 s_7_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_8_1 s_1_2 s_2_2 s_4_2 s_6_2 s_8_2 s_10_2 s_3_3 s_5_3 s_6_3 s_7_3 s_9_3 s_10_3 s_2_4 s_3_4 s_10_4 s_3_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_10_6 s_3_7 s_9_7 s_10_7 s_2_8 s_3_8 s_7_8 s_10_8 s_3_9 s_5_9 s_7_9 s_9_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_8_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_10_2 s_2_3 s_4_3 s_5_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_7_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_7_6 s_9_6 s_10_6 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_3_8 s_5_8 s_8_8 s_9_8 s_10_8 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_8_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_8_2 s_10_2 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_7_4 s_10_4 s_2_5 s_3_5 s_4_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_7_6 s_9_6 s_10_6 s_3_7 s_4_7 s_6_7 s_8_7 s_9_7 s_2_8 s_3_8 s_5_8 s_9_8 s_10_8 s_3_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_7_1 s_8_1 s_9_1 s_1_2 s_3_2 s_4_2 s_5_2 s_8_2 s_2_3 s_4_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_7_4 s_2_5 s_4_5 s_5_5 s_6_5 s_1_6 s_3_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_6_7 s_8_7 s_9_7 s_2_8 s_3_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_6_9 s_7_9 s_8_9 s_1_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_8_2 s_2_3 s_3_3 s_4_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_7_4 s_2_5 s_4_5 s_6_5 s_1_6 s_3_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_3_8 s_5_8 s_6_8 s_9_8 s_10_8 s_1_9 s_3_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_7_1 s_9_1 s_1_2 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_2_3 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_9_4 s_2_5 s_5_5 s_8_5 s_1_6 s_5_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_8_7 s_9_7 s_1_8 s_2_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_1_10 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_7_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_4_4 s_9_4 s_2_5 s_8_5 s_1_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_6_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_7_1 s_9_1 s_1_2 s_4_2 s_7_2 s_8_2 s_9_2 s_7_3 s_8_3 s_9_3 s_2_4 s_3_4 s_5_4 s_7_4 s_9_4 s_4_5 s_5_5 s_6_5 s_1_6 s_3_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_9_7 s_10_7 s_1_8 s_2_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_7_9 s_1_10 s_3_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_7_1 s_9_1 s_1_2 s_2_2 s_4_2 s_7_2 s_8_2 s_9_2 s_3_3 s_7_3 s_8_3 s_9_3 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_9_4 s_4_5 s_6_5 s_1_6 s_3_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_2_8 s_5_8 s_6_8 s_7_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_7_9 s_9_9 s_1_10 s_3_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_7_1 s_8_1 s_9_1 s_1_2 s_4_2 s_8_2 s_6_3 s_7_3 s_9_3 s_10_3 s_2_4 s_3_4 s_5_5 s_8_5 s_1_6 s_5_6 s_8_6 s_10_6 s_1_7 s_3_7 s_9_7 s_10_7 s_2_8 s_3_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_3_9 s_7_9 s_1_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_4_2 s_8_2 s_3_3 s_6_3 s_7_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_8_5 s_1_6 s_5_6 s_6_6 s_8_6 s_10_6 s_1_7 s_3_7 s_7_7 s_9_7 s_10_7 s_2_8 s_3_8 s_6_8 s_7_8 s_10_8 s_1_9 s_3_9 s_7_9 s_9_9 s_1_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_4_2 s_5_2 s_7_2 s_8_2 s_4_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_5_4 s_7_4 s_9_4 s_2_5 s_4_5 s_5_5 s_6_5 s_10_5 s_3_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_5_8 s_6_8 s_8_8 s_10_8 s_1_9 s_3_9 s_4_9 s_6_9 s_7_9 s_1_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_4_2 s_5_2 s_7_2 s_8_2 s_3_3 s_4_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_7_4 s_9_4 s_2_5 s_4_5 s_6_5 s_10_5 s_3_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_5_8 s_6_8 s_10_8 s_1_9 s_3_9 s_4_9 s_6_9 s_7_9 s_9_9 s_1_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_3_2 s_4_2 s_7_2 s_8_2 s_2_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_9_4 s_5_5 s_8_5 s_10_5 s_5_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_9_7 s_2_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_7_9 s_8_9 s_1_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_6_1 s_7_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_7_2 s_8_2 s_2_3 s_3_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_9_4 s_8_5 s_10_5 s_5_6 s_6_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_7_7 s_9_7 s_2_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_7_9 s_8_9 s_9_9 s_1_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_6_1 s_7_1 s_9_1 s_1_2 s_3_2 s_4_2 s_8_2 s_9_2 s_2_3 s_7_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_7_4 s_4_5 s_5_5 s_6_5 s_10_5 s_3_6 s_7_6 s_8_6 s_10_6 s_1_7 s_3_7 s_4_7 s_6_7 s_9_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_7_9 s_8_9 s_1_10 s_3_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_6_1 s_7_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_8_2 s_9_2 s_2_3 s_3_3 s_7_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_4_5 s_6_5 s_10_5 s_3_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_3_7 s_4_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_7_9 s_8_9 s_9_9 s_1_10 s_3_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_7_1 s_9_1 s_4_2 s_8_2 s_9_2 s_4_3 s_7_3 s_9_3 s_1_4 s_2_4 s_7_4 s_5_5 s_10_5 s_3_6 s_5_6 s_8_6 s_10_6 s_1_7 s_3_7 s_4_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_7_9 s_10_9 s_1_10 s_3_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_6_1 s_7_1 s_9_1 s_2_2 s_4_2 s_8_2 s_9_2 s_3_3 s_4_3 s_7_3 s_9_3 s_1_4 s_2_4 s_4_4 s_7_4 s_10_5 s_3_6 s_5_6 s_6_6 s_8_6 s_10_6 s_1_7 s_3_7 s_4_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_10_8 s_1_9 s_2_9 s_3_9 s_7_9 s_9_9 s_10_9 s_1_10 s_3_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_7_1 s_8_1 s_9_1 s_4_2 s_7_2 s_8_2 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_9_4 s_4_5 s_5_5 s_6_5 s_8_5 s_10_5 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_6_7 s_8_7 s_9_7 s_2_8 s_6_8 s_8_8 s_10_8 s_1_9 s_3_9 s_4_9 s_7_9 s_10_9 s_1_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_2_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_7_4 s_9_4 s_2_5 s_5_5 s_10_5 s_3_6 s_5_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_9_7 s_10_7 s_2_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_6_1 s_7_1 s_8_1 s_9_1 s_2_2 s_4_2 s_7_2 s_8_2 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_9_4 s_4_5 s_6_5 s_8_5 s_10_5 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_6_8 s_10_8 s_1_9 s_3_9 s_4_9 s_7_9 s_9_9 s_10_9 s_1_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_7_1 s_8_1 s_9_1 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_2_3 s_3_3 s_7_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_7_4 s_9_4 s_2_5 s_10_5 s_3_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_7_7 s_9_7 s_10_7 s_2_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_7_1 s_9_1 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_9_4 s_2_5 s_4_5 s_5_5 s_6_5 s_8_5 s_1_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_6_7 s_9_7 s_1_8 s_2_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_10_9 s_1_10 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_7_1 s_9_1 s_2_2 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_3_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_9_4 s_2_5 s_4_5 s_6_5 s_8_5 s_1_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_3_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_6_8 s_7_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_7_1 s_9_1 s_3_2 s_4_2 s_7_2 s_8_2 s_9_2 s_2_3 s_4_3 s_7_3 s_8_3 s_9_3 s_2_4 s_7_4 s_9_4 s_5_5 s_1_6 s_3_6 s_5_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_7_9 s_8_9 s_10_9 s_1_10 s_3_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_7_1 s_9_1 s_2_2 s_3_2 s_4_2 s_7_2 s_8_2 s_9_2 s_2_3 s_3_3 s_4_3 s_7_3 s_8_3 s_9_3 s_2_4 s_4_4 s_7_4 s_9_4 s_1_6 s_3_6 s_5_6 s_6_6 s_8_6 s_10_6 s_1_7 s_2_7 s_3_7 s_4_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_5_8 s_6_8 s_9_8 s_10_8 s_1_9 s_2_9 s_3_9 s_4_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_7_1 s_8_1 s_9_1 s_4_2 s_5_2 s_8_2 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_7_4 s_2_5 s_5_5 s_1_6 s_3_6 s_5_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_9_7 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_3_9 s_6_9 s_7_9 s_10_9 s_1_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_7_1 s_8_1 s_9_1 s_2_2 s_4_2 s_5_2 s_8_2 s_3_3 s_7_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_7_4 s_2_5 s_1_6 s_3_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_3_7 s_4_7 s_7_7 s_9_7 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_10_8 s_1_9 s_3_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_7_1 s_8_1 s_9_1 s_2_2 s_3_2 s_4_2 s_8_2 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_4_5 s_6_5 s_8_5 s_1_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_3_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_6_8 s_9_8 s_10_8 s_1_9 s_3_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_7_1 s_8_1 s_9_1 s_3_2 s_4_2 s_8_2 s_2_3 s_4_3 s_6_3 s_7_3 s_9_3 s_10_3 s_2_4 s_5_4 s_4_5 s_5_5 s_6_5 s_8_5 s_1_6 s_7_6 s_8_6 s_10_6 s_1_7 s_3_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_3_9 s_7_9 s_8_9 s_10_9 s_1_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_8_1 s_1_2 s_4_2 s_8_2 s_10_2 s_1_3 s_3_3 s_5_3 s_6_3 s_9_3 s_10_3 s_2_4 s_3_4 s_8_4 s_10_4 s_1_5 s_3_5 s_8_5 s_1_6 s_5_6 s_6_6 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_7_8 s_3_9 s_7_9 s_1_10 s_2_10 s_4_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_8_1 s_1_2 s_2_2 s_4_2 s_8_2 s_10_2 s_1_3 s_5_3 s_6_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_8_4 s_10_4 s_1_5 s_3_5 s_5_5 s_8_5 s_1_6 s_5_6 s_7_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_7_8 s_8_8 s_3_9 s_7_9 s_9_9 s_1_10 s_2_10 s_4_10 s_5_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_8_1 s_1_2 s_6_2 s_8_2 s_10_2 s_1_3 s_6_3 s_7_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_10_4 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_8_6 s_10_6 s_3_7 s_5_7 s_7_7 s_10_7 s_2_8 s_3_8 s_6_8 s_7_8 s_8_8 s_3_9 s_5_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_8_1 s_1_2 s_2_2 s_6_2 s_8_2 s_10_2 s_1_3 s_3_3 s_6_3 s_7_3 s_9_3 s_10_3 s_3_4 s_6_4 s_10_4 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_8_6 s_10_6 s_3_7 s_5_7 s_10_7 s_2_8 s_3_8 s_6_8 s_7_8 s_3_9 s_5_9 s_9_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_1_2 s_4_2 s_5_2 s_7_2 s_8_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_8_3 s_9_3 s_10_3 s_2_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_10_5 s_3_6 s_6_6 s_7_6 s_9_6 s_2_7 s_4_7 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_4_8 s_5_8 s_3_9 s_4_9 s_6_9 s_7_9 s_1_10 s_2_10 s_4_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_1_2 s_2_2 s_4_2 s_5_2 s_7_2 s_8_2 s_10_2 s_1_3 s_4_3 s_5_3 s_8_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_10_5 s_3_6 s_7_6 s_9_6 s_2_7 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_4_8 s_5_8 s_8_8 s_3_9 s_4_9 s_6_9 s_7_9 s_9_9 s_1_10 s_2_10 s_4_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_8_1 s_1_2 s_2_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_3_3 s_4_3 s_7_3 s_8_3 s_9_3 s_10_3 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_2_5 s_4_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_5_8 s_6_8 s_3_9 s_4_9 s_5_9 s_6_9 s_9_9 s_1_10 s_2_10 s_4_10 s_6_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_8_1 s_1_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_4_3 s_7_3 s_8_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_5_8 s_6_8 s_8_8 s_3_9 s_4_9 s_5_9 s_6_9 s_1_10 s_2_10 s_4_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_8_1 s_1_2 s_3_2 s_5_2 s_6_2 s_8_2 s_10_2 s_1_3 s_2_3 s_4_3 s_7_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_7_4 s_10_4 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_7_6 s_8_6 s_9_6 s_10_6 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_3_8 s_5_8 s_6_8 s_8_8 s_9_8 s_3_9 s_5_9 s_6_9 s_8_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_8_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_7_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_7_4 s_10_4 s_2_5 s_4_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_2_8 s_3_8 s_5_8 s_6_8 s_9_8 s_3_9 s_5_9 s_6_9 s_8_9 s_9_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_8_1 s_1_2 s_2_2 s_3_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_9_4 s_10_4 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_8_6 s_10_6 s_2_7 s_3_7 s_5_7 s_2_8 s_6_8 s_7_8 s_9_8 s_3_9 s_4_9 s_5_9 s_8_9 s_9_9 s_1_10 s_2_10 s_4_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_6_1 s_8_1 s_1_2 s_3_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_9_4 s_10_4 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_8_6 s_10_6 s_2_7 s_3_7 s_5_7 s_7_7 s_2_8 s_6_8 s_7_8 s_8_8 s_9_8 s_3_9 s_4_9 s_5_9 s_8_9 s_1_10 s_2_10 s_4_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_5_1 s_8_1 s_1_2 s_3_2 s_4_2 s_5_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_1_6 s_3_6 s_6_6 s_7_6 s_9_6 s_4_7 s_6_7 s_8_7 s_9_7 s_2_8 s_3_8 s_4_8 s_5_8 s_9_8 s_3_9 s_6_9 s_7_9 s_8_9 s_1_10 s_2_10 s_4_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_8_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_8_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_1_6 s_3_6 s_7_6 s_9_6 s_4_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_3_8 s_4_8 s_5_8 s_8_8 s_9_8 s_3_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_4_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_8_1 s_1_2 s_2_2 s_3_2 s_4_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_5_5 s_8_5 s_10_5 s_5_6 s_2_7 s_7_7 s_9_7 s_2_8 s_4_8 s_7_8 s_8_8 s_9_8 s_3_9 s_4_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_4_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_8_1 s_1_2 s_3_2 s_4_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_8_5 s_10_5 s_5_6 s_6_6 s_2_7 s_9_7 s_2_8 s_4_8 s_7_8 s_9_8 s_3_9 s_4_9 s_7_9 s_8_9 s_1_10 s_2_10 s_4_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_1_2 s_2_2 s_3_2 s_4_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_5_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_10_5 s_3_6 s_7_6 s_4_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_2_9 s_3_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_1_2 s_3_2 s_4_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_7_4 s_8_4 s_10_4 s_1_5 s_3_5 s_4_5 s_6_5 s_10_5 s_3_6 s_6_6 s_7_6 s_4_7 s_6_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_9_8 s_2_9 s_3_9 s_7_9 s_8_9 s_1_10 s_2_10 s_3_10 s_4_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_7_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_1_3 s_2_3 s_4_3 s_6_3 s_8_3 s_9_3 s_1_4 s_2_4 s_8_4 s_9_4 s_1_5 s_2_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_8_6 s_9_6 s_1_7 s_2_7 s_8_7 s_9_7 s_1_8 s_2_8 s_4_8 s_6_8 s_8_8 s_9_8 s_1_9 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_3_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_8_1 s_2_2 s_4_2 s_5_2 s_8_2 s_10_2 s_1_3 s_5_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_1_6 s_3_6 s_5_6 s_9_6 s_4_7 s_7_7 s_9_7 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_3_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_8_1 s_4_2 s_5_2 s_8_2 s_10_2 s_1_3 s_3_3 s_5_3 s_9_3 s_10_3 s_1_4 s_2_4 s_3_4 s_7_4 s_8_4 s_10_4 s_1_5 s_2_5 s_3_5 s_1_6 s_3_6 s_5_6 s_6_6 s_9_6 s_4_7 s_9_7 s_2_8 s_3_8 s_4_8 s_5_8 s_7_8 s_3_9 s_6_9 s_7_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_8_1 s_2_2 s_3_2 s_4_2 s_8_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_9_3 s_10_3 s_2_4 s_4_4 s_5_4 s_8_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_1_6 s_7_6 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_8_8 s_9_8 s_3_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_8_1 s_3_2 s_4_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_9_3 s_10_3 s_2_4 s_5_4 s_8_4 s_10_4 s_1_5 s_3_5 s_4_5 s_6_5 s_8_5 s_1_6 s_6_6 s_7_6 s_6_7 s_8_7 s_9_7 s_10_7 s_2_8 s_3_8 s_4_8 s_9_8 s_3_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_2_2 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_5_3 s_6_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_1_6 s_7_6 s_9_6 s_2_7 s_6_7 s_7_7 s_9_7 s_1_8 s_2_8 s_4_8 s_7_8 s_8_8 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_4_2 s_5_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_5_3 s_6_3 s_8_3 s_9_3 s_1_4 s_2_4 s_3_4 s_5_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_8_5 s_1_6 s_6_6 s_7_6 s_9_6 s_2_7 s_6_7 s_9_7 s_1_8 s_2_8 s_4_8 s_7_8 s_2_9 s_3_9 s_4_9 s_6_9 s_7_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_2_2 s_3_2 s_4_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_5_3 s_8_3 s_9_3 s_2_4 s_4_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_5_5 s_1_6 s_3_6 s_5_6 s_2_7 s_4_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_8_8 s_9_8 s_2_9 s_3_9 s_4_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_3_2 s_4_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_8_3 s_9_3 s_2_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_1_6 s_3_6 s_5_6 s_6_6 s_2_7 s_4_7 s_8_7 s_9_7 s_10_7 s_1_8 s_2_8 s_4_8 s_5_8 s_9_8 s_2_9 s_3_9 s_4_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_5_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_4_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_10_5 s_3_6 s_5_6 s_9_6 s_2_7 s_4_7 s_7_7 s_9_7 s_10_7 s_2_8 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_3_2 s_4_2 s_5_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_5_3 s_8_3 s_9_3 s_10_3 s_2_4 s_3_4 s_7_4 s_8_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_10_5 s_3_6 s_5_6 s_6_6 s_9_6 s_2_7 s_4_7 s_9_7 s_10_7 s_2_8 s_4_8 s_5_8 s_7_8 s_9_8 s_3_9 s_4_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_4_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_8_1 s_2_2 s_4_2 s_7_2 s_8_2 s_10_2 s_1_3 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_8_5 s_10_5 s_7_6 s_2_7 s_6_7 s_7_7 s_8_7 s_9_7 s_2_8 s_4_8 s_8_8 s_3_9 s_4_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_8_1 s_4_2 s_7_2 s_8_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_9_3 s_10_3 s_1_4 s_2_4 s_5_4 s_8_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_6_5 s_8_5 s_10_5 s_6_6 s_7_6 s_2_7 s_6_7 s_8_7 s_9_7 s_2_8 s_4_8 s_3_9 s_4_9 s_7_9 s_10_9 s_1_10 s_2_10 s_4_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_2_2 s_4_2 s_8_2 s_9_2 s_10_2 s_1_3 s_4_3 s_5_3 s_9_3 s_1_4 s_2_4 s_4_4 s_7_4 s_8_4 s_10_4 s_1_5 s_3_5 s_5_5 s_10_5 s_3_6 s_5_6 s_4_7 s_7_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_8_8 s_2_9 s_3_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_4_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_5_3 s_9_3 s_1_4 s_2_4 s_7_4 s_8_4 s_10_4 s_1_5 s_3_5 s_10_5 s_3_6 s_5_6 s_6_6 s_4_7 s_8_7 s_9_7 s_1_8 s_2_8 s_3_8 s_4_8 s_5_8 s_2_9 s_3_9 s_7_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_8_1 s_5_2 s_6_2 s_8_2 s_10_2 s_1_3 s_7_3 s_9_3 s_10_3 s_1_4 s_3_4 s_4_4 s_6_4 s_7_4 s_10_4 s_2_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_8_6 s_9_6 s_10_6 s_3_7 s_4_7 s_5_7 s_7_7 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_3_9 s_5_9 s_6_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_8_1 s_2_2 s_5_2 s_6_2 s_8_2 s_10_2 s_1_3 s_3_3 s_7_3 s_9_3 s_10_3 s_1_4 s_3_4 s_6_4 s_7_4 s_10_4 s_2_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_3_7 s_4_7 s_5_7 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_3_9 s_5_9 s_6_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_8_1 s_3_2 s_6_2 s_8_2 s_10_2 s_1_3 s_2_3 s_4_3 s_6_3 s_7_3 s_9_3 s_10_3 s_4_4 s_5_4 s_6_4 s_10_4 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_7_6 s_8_6 s_10_6 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_10_7 s_2_8 s_3_8 s_6_8 s_8_8 s_9_8 s_3_9 s_5_9 s_8_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_8_1 s_2_2 s_3_2 s_6_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_9_3 s_10_3 s_5_4 s_6_4 s_10_4 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_7_6 s_8_6 s_10_6 s_3_7 s_5_7 s_6_7 s_8_7 s_10_7 s_2_8 s_3_8 s_6_8 s_9_8 s_3_9 s_5_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_5_10 s_6_10 )))

(assert (not (and s_2_1 s_3_1 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_9_4 s_10_4 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_6_8 s_7_8 s_8_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_2_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_9_4 s_10_4 s_2_5 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_5_7 s_6_7 s_1_8 s_2_8 s_6_8 s_7_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_3_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_7_3 s_8_3 s_9_3 s_4_4 s_6_4 s_7_4 s_9_4 s_10_4 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_8_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_10_7 s_1_8 s_2_8 s_5_8 s_6_8 s_8_8 s_9_8 s_2_9 s_3_9 s_4_9 s_5_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_2_2 s_3_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_7_3 s_8_3 s_9_3 s_6_4 s_7_4 s_9_4 s_10_4 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_8_7 s_10_7 s_1_8 s_2_8 s_5_8 s_6_8 s_9_8 s_2_9 s_3_9 s_4_9 s_5_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_8_1 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_3_3 s_7_3 s_8_3 s_9_3 s_10_3 s_3_4 s_6_4 s_7_4 s_9_4 s_10_4 s_2_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_10_7 s_2_8 s_5_8 s_6_8 s_7_8 s_9_8 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_6_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_8_1 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_2_3 s_7_3 s_8_3 s_9_3 s_10_3 s_3_4 s_4_4 s_6_4 s_7_4 s_9_4 s_10_4 s_2_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_10_7 s_2_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_10_9 s_1_10 s_2_10 s_4_10 s_6_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_4_3 s_7_3 s_9_3 s_1_4 s_4_4 s_6_4 s_7_4 s_10_4 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_8_6 s_10_6 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_8_8 s_2_9 s_3_9 s_5_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_2_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_7_3 s_9_3 s_1_4 s_6_4 s_7_4 s_10_4 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_10_6 s_3_7 s_4_7 s_5_7 s_8_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_2_9 s_3_9 s_5_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_8_1 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_4_4 s_5_4 s_6_4 s_9_4 s_10_4 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_7_6 s_8_6 s_10_6 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_2_8 s_6_8 s_8_8 s_3_9 s_4_9 s_5_9 s_10_9 s_1_10 s_2_10 s_4_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_8_1 s_2_2 s_6_2 s_7_2 s_8_2 s_10_2 s_1_3 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_10_3 s_1_4 s_5_4 s_6_4 s_9_4 s_10_4 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_7_6 s_8_6 s_10_6 s_2_7 s_3_7 s_5_7 s_6_7 s_8_7 s_2_8 s_6_8 s_3_9 s_4_9 s_5_9 s_9_9 s_10_9 s_1_10 s_2_10 s_4_10 s_6_10 s_7_10 )))

(assert (not (and s_3_1 s_6_1 s_1_2 s_3_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_7_3 s_9_3 s_1_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_10_4 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_7_6 s_8_6 s_10_6 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_2_9 s_3_9 s_5_9 s_8_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_1_2 s_2_2 s_3_2 s_6_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_7_3 s_9_3 s_1_4 s_3_4 s_5_4 s_6_4 s_7_4 s_10_4 s_4_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_10_6 s_3_7 s_4_7 s_5_7 s_6_7 s_1_8 s_2_8 s_3_8 s_5_8 s_6_8 s_7_8 s_9_8 s_2_9 s_3_9 s_5_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_1_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_7_3 s_8_3 s_9_3 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_7_6 s_8_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_10_7 s_1_8 s_2_8 s_5_8 s_6_8 s_7_8 s_8_8 s_2_9 s_3_9 s_4_9 s_5_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_1_2 s_2_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_3_3 s_7_3 s_8_3 s_9_3 s_3_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_4_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_10_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_10_7 s_1_8 s_2_8 s_5_8 s_6_8 s_7_8 s_2_9 s_3_9 s_4_9 s_5_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_6_4 s_9_4 s_10_4 s_2_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_5_7 s_8_7 s_1_8 s_2_8 s_6_8 s_9_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_1_2 s_3_2 s_5_2 s_6_2 s_7_2 s_8_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_6_3 s_7_3 s_8_3 s_9_3 s_1_4 s_4_4 s_6_4 s_9_4 s_10_4 s_2_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_8_6 s_9_6 s_10_6 s_2_7 s_3_7 s_5_7 s_7_7 s_8_7 s_1_8 s_2_8 s_6_8 s_8_8 s_9_8 s_2_9 s_3_9 s_4_9 s_5_9 s_6_9 s_8_9 s_1_10 s_2_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_6_2 s_7_2 s_1_3 s_2_3 s_3_3 s_6_3 s_8_3 s_10_3 s_1_4 s_3_4 s_8_4 s_9_4 s_10_4 s_5_5 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_8_6 s_10_6 s_2_7 s_4_8 s_6_8 s_7_8 s_9_8 s_1_9 s_4_9 s_5_9 s_8_9 s_9_9 s_1_10 s_4_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_6_1 s_8_1 s_9_1 s_1_2 s_2_2 s_5_2 s_6_2 s_7_2 s_1_3 s_3_3 s_4_3 s_8_3 s_10_3 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_4_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_4_7 s_6_7 s_8_7 s_10_7 s_4_8 s_5_8 s_6_8 s_1_9 s_4_9 s_5_9 s_6_9 s_9_9 s_1_10 s_4_10 s_6_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_6_1 s_8_1 s_9_1 s_1_2 s_5_2 s_6_2 s_7_2 s_1_3 s_4_3 s_8_3 s_10_3 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_4_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_4_7 s_6_7 s_7_7 s_8_7 s_10_7 s_4_8 s_5_8 s_6_8 s_8_8 s_1_9 s_4_9 s_5_9 s_6_9 s_1_10 s_4_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_6_1 s_8_1 s_9_1 s_1_2 s_3_2 s_6_2 s_7_2 s_1_3 s_2_3 s_6_3 s_8_3 s_10_3 s_1_4 s_3_4 s_4_4 s_8_4 s_9_4 s_10_4 s_8_5 s_9_5 s_10_5 s_2_6 s_5_6 s_6_6 s_8_6 s_10_6 s_2_7 s_7_7 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_4_9 s_5_9 s_8_9 s_1_10 s_4_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_6_1 s_9_1 s_1_2 s_3_2 s_6_2 s_9_2 s_1_3 s_2_3 s_1_4 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_10_4 s_4_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_6_6 s_7_6 s_8_6 s_10_6 s_4_7 s_6_7 s_7_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_5_9 s_8_9 s_1_10 s_3_10 s_4_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_6_1 s_9_1 s_1_2 s_2_2 s_3_2 s_6_2 s_9_2 s_1_3 s_2_3 s_3_3 s_1_4 s_3_4 s_5_4 s_7_4 s_8_4 s_10_4 s_4_5 s_5_5 s_6_5 s_9_5 s_10_5 s_2_6 s_3_6 s_7_6 s_8_6 s_10_6 s_4_7 s_6_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_1_9 s_2_9 s_5_9 s_8_9 s_9_9 s_1_10 s_3_10 s_4_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_8_1 s_9_1 s_1_2 s_3_2 s_5_2 s_6_2 s_1_3 s_2_3 s_4_3 s_10_3 s_1_4 s_4_4 s_5_4 s_7_4 s_8_4 s_10_4 s_2_5 s_4_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_4_7 s_6_7 s_7_7 s_8_7 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_1_9 s_5_9 s_6_9 s_8_9 s_1_10 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_1_3 s_2_3 s_3_3 s_4_3 s_10_3 s_1_4 s_5_4 s_7_4 s_8_4 s_10_4 s_2_5 s_4_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_7_6 s_8_6 s_9_6 s_10_6 s_4_7 s_6_7 s_8_7 s_3_8 s_4_8 s_5_8 s_6_8 s_9_8 s_1_9 s_5_9 s_6_9 s_8_9 s_9_9 s_1_10 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_9_1 s_1_2 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_8_3 s_1_4 s_8_4 s_9_4 s_10_4 s_2_5 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_8_6 s_9_6 s_10_6 s_2_7 s_8_7 s_1_8 s_4_8 s_6_8 s_9_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_9_1 s_1_2 s_3_2 s_5_2 s_6_2 s_7_2 s_9_2 s_1_3 s_2_3 s_4_3 s_6_3 s_8_3 s_1_4 s_4_4 s_8_4 s_9_4 s_10_4 s_2_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_2_7 s_7_7 s_8_7 s_1_8 s_4_8 s_6_8 s_8_8 s_9_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_8_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_8_1 s_9_1 s_1_2 s_6_2 s_1_3 s_6_3 s_10_3 s_3_4 s_4_4 s_8_4 s_10_4 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_6_6 s_8_6 s_10_6 s_7_7 s_10_7 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_1_9 s_5_9 s_1_10 s_4_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_8_1 s_9_1 s_1_2 s_2_2 s_6_2 s_1_3 s_3_3 s_6_3 s_10_3 s_3_4 s_8_4 s_10_4 s_5_5 s_8_5 s_9_5 s_1_6 s_2_6 s_5_6 s_8_6 s_10_6 s_10_7 s_3_8 s_4_8 s_6_8 s_7_8 s_1_9 s_5_9 s_9_9 s_1_10 s_4_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_9_1 s_1_2 s_6_2 s_7_2 s_9_2 s_1_3 s_8_3 s_3_4 s_4_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_4_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_6_6 s_7_6 s_8_6 s_10_6 s_2_7 s_4_7 s_6_7 s_7_7 s_10_7 s_1_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_1_9 s_2_9 s_4_9 s_5_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_9_1 s_1_2 s_2_2 s_6_2 s_7_2 s_9_2 s_1_3 s_3_3 s_8_3 s_3_4 s_5_4 s_7_4 s_8_4 s_9_4 s_10_4 s_4_5 s_5_5 s_6_5 s_9_5 s_1_6 s_2_6 s_3_6 s_7_6 s_8_6 s_10_6 s_2_7 s_4_7 s_6_7 s_10_7 s_1_8 s_4_8 s_5_8 s_6_8 s_7_8 s_1_9 s_2_9 s_4_9 s_5_9 s_9_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_9_10 )))

(assert (not (and s_4_1 s_6_1 s_8_1 s_9_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_3_3 s_4_3 s_5_3 s_8_3 s_10_3 s_2_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_7_6 s_9_6 s_10_6 s_2_7 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_4_8 s_5_8 s_10_8 s_1_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_1_10 s_4_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_8_1 s_9_1 s_1_2 s_4_2 s_5_2 s_6_2 s_7_2 s_4_3 s_5_3 s_8_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_9_6 s_10_6 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_4_8 s_5_8 s_8_8 s_10_8 s_1_9 s_4_9 s_5_9 s_6_9 s_7_9 s_1_10 s_4_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_6_1 s_8_1 s_9_1 s_1_2 s_3_2 s_4_2 s_6_2 s_7_2 s_2_3 s_5_3 s_6_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_8_4 s_9_4 s_10_4 s_3_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_10_6 s_2_7 s_5_7 s_7_7 s_9_7 s_4_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_4_9 s_5_9 s_7_9 s_8_9 s_1_10 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_6_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_2_3 s_3_3 s_5_3 s_6_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_8_4 s_9_4 s_10_4 s_3_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_10_6 s_2_7 s_5_7 s_9_7 s_4_8 s_7_8 s_9_8 s_10_8 s_1_9 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_1_10 s_4_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_8_1 s_9_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_2_3 s_4_3 s_5_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_9_6 s_10_6 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_3_8 s_4_8 s_5_8 s_8_8 s_9_8 s_10_8 s_1_9 s_5_9 s_6_9 s_7_9 s_8_9 s_1_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_2_3 s_3_3 s_4_3 s_5_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_7_4 s_8_4 s_10_4 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_7_6 s_9_6 s_10_6 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_3_8 s_4_8 s_5_8 s_9_8 s_10_8 s_1_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_8_1 s_9_1 s_1_2 s_4_2 s_6_2 s_5_3 s_6_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_8_4 s_10_4 s_3_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_10_6 s_5_7 s_7_7 s_9_7 s_10_7 s_3_8 s_4_8 s_7_8 s_8_8 s_10_8 s_1_9 s_5_9 s_7_9 s_1_10 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_8_1 s_9_1 s_1_2 s_2_2 s_4_2 s_6_2 s_3_3 s_5_3 s_6_3 s_10_3 s_2_4 s_3_4 s_6_4 s_8_4 s_10_4 s_3_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_10_6 s_5_7 s_9_7 s_10_7 s_3_8 s_4_8 s_7_8 s_10_8 s_1_9 s_5_9 s_7_9 s_9_9 s_1_10 s_4_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_8_1 s_9_1 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_2_3 s_5_3 s_8_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_9_6 s_10_6 s_2_7 s_4_7 s_5_7 s_7_7 s_9_7 s_10_7 s_4_8 s_5_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_4_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_8_1 s_9_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_2_3 s_3_3 s_5_3 s_8_3 s_10_3 s_2_4 s_3_4 s_6_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_3_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_9_6 s_10_6 s_2_7 s_4_7 s_5_7 s_9_7 s_10_7 s_4_8 s_5_8 s_7_8 s_9_8 s_10_8 s_1_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_4_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_6_1 s_8_1 s_9_1 s_2_2 s_3_2 s_5_2 s_6_2 s_7_2 s_1_3 s_2_3 s_3_3 s_8_3 s_10_3 s_3_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_8_6 s_9_6 s_10_6 s_2_7 s_4_7 s_10_7 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_1_9 s_4_9 s_5_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_4_10 s_6_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_6_1 s_8_1 s_9_1 s_3_2 s_5_2 s_6_2 s_7_2 s_1_3 s_2_3 s_8_3 s_10_3 s_3_4 s_4_4 s_7_4 s_8_4 s_9_4 s_10_4 s_2_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_2_7 s_4_7 s_7_7 s_10_7 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_1_9 s_4_9 s_5_9 s_6_9 s_8_9 s_10_9 s_1_10 s_4_10 s_6_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_9_1 s_6_2 s_9_2 s_1_3 s_4_3 s_1_4 s_4_4 s_7_4 s_8_4 s_10_4 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_6_6 s_8_6 s_10_6 s_4_7 s_7_7 s_8_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_1_9 s_2_9 s_5_9 s_10_9 s_1_10 s_3_10 s_4_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_9_1 s_2_2 s_6_2 s_9_2 s_1_3 s_3_3 s_4_3 s_1_4 s_7_4 s_8_4 s_10_4 s_5_5 s_9_5 s_10_5 s_2_6 s_3_6 s_5_6 s_8_6 s_10_6 s_4_7 s_8_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_1_9 s_2_9 s_5_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_6_1 s_8_1 s_9_1 s_2_2 s_4_2 s_6_2 s_7_2 s_3_3 s_4_3 s_5_3 s_6_3 s_8_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_7_6 s_10_6 s_2_7 s_5_7 s_6_7 s_8_7 s_9_7 s_4_8 s_10_8 s_1_9 s_4_9 s_5_9 s_7_9 s_9_9 s_10_9 s_1_10 s_4_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_8_1 s_9_1 s_4_2 s_6_2 s_7_2 s_4_3 s_5_3 s_6_3 s_8_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_10_4 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_7_6 s_10_6 s_2_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_4_8 s_8_8 s_10_8 s_1_9 s_4_9 s_5_9 s_7_9 s_10_9 s_1_10 s_4_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_8_1 s_9_1 s_2_2 s_4_2 s_5_2 s_6_2 s_3_3 s_5_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_7_4 s_8_4 s_10_4 s_2_5 s_3_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_9_6 s_10_6 s_4_7 s_5_7 s_9_7 s_3_8 s_4_8 s_5_8 s_7_8 s_10_8 s_1_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_8_1 s_9_1 s_4_2 s_5_2 s_6_2 s_5_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_10_4 s_2_5 s_3_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_9_6 s_10_6 s_4_7 s_5_7 s_7_7 s_9_7 s_3_8 s_4_8 s_5_8 s_7_8 s_8_8 s_10_8 s_1_9 s_5_9 s_6_9 s_7_9 s_10_9 s_1_10 s_4_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_8_1 s_9_1 s_2_2 s_3_2 s_4_2 s_6_2 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_10_3 s_2_4 s_5_4 s_6_4 s_8_4 s_10_4 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_7_6 s_10_6 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_3_8 s_4_8 s_9_8 s_10_8 s_1_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_4_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_8_1 s_9_1 s_3_2 s_4_2 s_6_2 s_2_3 s_4_3 s_5_3 s_6_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_8_4 s_10_4 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_7_6 s_10_6 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_3_8 s_4_8 s_8_8 s_9_8 s_10_8 s_1_9 s_5_9 s_7_9 s_8_9 s_10_9 s_1_10 s_4_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_2_1 s_3_1 s_6_1 s_8_1 s_9_1 s_6_2 s_7_2 s_1_3 s_4_3 s_6_3 s_8_3 s_10_3 s_1_4 s_4_4 s_5_4 s_8_4 s_9_4 s_10_4 s_4_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_6_6 s_7_6 s_8_6 s_10_6 s_2_7 s_6_7 s_7_7 s_8_7 s_4_8 s_6_8 s_8_8 s_1_9 s_4_9 s_5_9 s_10_9 s_1_10 s_4_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_6_1 s_8_1 s_9_1 s_2_2 s_6_2 s_7_2 s_1_3 s_3_3 s_4_3 s_6_3 s_8_3 s_10_3 s_1_4 s_5_4 s_8_4 s_9_4 s_10_4 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_10_5 s_2_6 s_7_6 s_8_6 s_10_6 s_2_7 s_6_7 s_8_7 s_4_8 s_6_8 s_1_9 s_4_9 s_5_9 s_9_9 s_10_9 s_1_10 s_4_10 s_6_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_8_1 s_9_1 s_2_2 s_5_2 s_6_2 s_1_3 s_3_3 s_10_3 s_1_4 s_3_4 s_7_4 s_8_4 s_10_4 s_2_5 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_8_6 s_9_6 s_10_6 s_4_7 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_1_9 s_5_9 s_6_9 s_9_9 s_10_9 s_1_10 s_4_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_8_1 s_9_1 s_5_2 s_6_2 s_1_3 s_10_3 s_1_4 s_3_4 s_4_4 s_7_4 s_8_4 s_10_4 s_2_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_4_7 s_7_7 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_1_9 s_5_9 s_6_9 s_10_9 s_1_10 s_4_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_8_1 s_9_1 s_3_2 s_6_2 s_1_3 s_2_3 s_4_3 s_6_3 s_10_3 s_4_4 s_5_4 s_8_4 s_10_4 s_4_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_7_6 s_8_6 s_10_6 s_6_7 s_7_7 s_8_7 s_10_7 s_3_8 s_4_8 s_6_8 s_8_8 s_9_8 s_1_9 s_5_9 s_8_9 s_10_9 s_1_10 s_4_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_8_1 s_9_1 s_2_2 s_3_2 s_6_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_10_3 s_5_4 s_8_4 s_10_4 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_7_6 s_8_6 s_10_6 s_6_7 s_8_7 s_10_7 s_3_8 s_4_8 s_6_8 s_9_8 s_1_9 s_5_9 s_8_9 s_9_9 s_10_9 s_1_10 s_4_10 s_5_10 s_6_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_9_1 s_2_2 s_3_2 s_6_2 s_7_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_8_3 s_7_4 s_8_4 s_9_4 s_10_4 s_5_5 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_8_6 s_10_6 s_2_7 s_4_7 s_8_7 s_10_7 s_1_8 s_4_8 s_5_8 s_6_8 s_9_8 s_1_9 s_2_9 s_4_9 s_5_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_9_1 s_3_2 s_6_2 s_7_2 s_9_2 s_1_3 s_2_3 s_4_3 s_8_3 s_4_4 s_7_4 s_8_4 s_9_4 s_10_4 s_9_5 s_1_6 s_2_6 s_3_6 s_5_6 s_6_6 s_8_6 s_10_6 s_2_7 s_4_7 s_7_7 s_8_7 s_10_7 s_1_8 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_1_9 s_2_9 s_4_9 s_5_9 s_8_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_9_1 s_2_2 s_5_2 s_6_2 s_7_2 s_9_2 s_1_3 s_3_3 s_6_3 s_8_3 s_1_4 s_3_4 s_5_4 s_8_4 s_9_4 s_10_4 s_2_5 s_4_5 s_5_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_6_7 s_1_8 s_4_8 s_6_8 s_7_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_9_1 s_5_2 s_6_2 s_7_2 s_9_2 s_1_3 s_6_3 s_8_3 s_1_4 s_3_4 s_4_4 s_5_4 s_8_4 s_9_4 s_10_4 s_2_5 s_4_5 s_6_5 s_8_5 s_9_5 s_1_6 s_2_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_2_7 s_6_7 s_7_7 s_1_8 s_4_8 s_6_8 s_7_8 s_8_8 s_1_9 s_2_9 s_4_9 s_5_9 s_6_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_6_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_7_1 s_3_2 s_4_2 s_7_2 s_9_2 s_10_2 s_2_3 s_4_3 s_8_3 s_2_4 s_6_4 s_7_4 s_8_4 s_9_4 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_2_9 s_4_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_7_1 s_4_2 s_5_2 s_7_2 s_9_2 s_10_2 s_6_3 s_8_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_8_4 s_9_4 s_2_5 s_4_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_5_7 s_6_7 s_9_7 s_1_8 s_4_8 s_6_8 s_7_8 s_8_8 s_10_8 s_2_9 s_4_9 s_6_9 s_7_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_7_1 s_8_1 s_3_2 s_4_2 s_10_2 s_2_3 s_4_3 s_6_3 s_10_3 s_2_4 s_5_4 s_6_4 s_8_4 s_4_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_3_8 s_4_8 s_6_8 s_8_8 s_9_8 s_10_8 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_5_10 s_6_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_7_1 s_8_1 s_4_2 s_5_2 s_10_2 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_7_4 s_8_4 s_2_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_4_7 s_5_7 s_9_7 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_6_9 s_7_9 s_10_9 s_1_10 s_2_10 s_5_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_6_1 s_7_1 s_8_1 s_3_2 s_4_2 s_5_2 s_7_2 s_10_2 s_2_3 s_8_3 s_10_3 s_2_4 s_3_4 s_6_4 s_7_4 s_8_4 s_9_4 s_2_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_9_7 s_10_7 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_4_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_6_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_7_1 s_8_1 s_4_2 s_7_2 s_10_2 s_4_3 s_6_3 s_8_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_8_4 s_9_4 s_4_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_5_7 s_6_7 s_8_7 s_9_7 s_4_8 s_6_8 s_8_8 s_10_8 s_4_9 s_7_9 s_10_9 s_1_10 s_2_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_6_1 s_7_1 s_4_2 s_9_2 s_10_2 s_4_3 s_1_4 s_2_4 s_6_4 s_7_4 s_8_4 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_10_6 s_1_7 s_4_7 s_5_7 s_8_7 s_9_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_10_8 s_2_9 s_7_9 s_10_9 s_1_10 s_2_10 s_3_10 s_6_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_4_1 s_6_1 s_7_1 s_8_1 s_1_2 s_4_2 s_5_2 s_7_2 s_10_2 s_4_3 s_8_3 s_10_3 s_2_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_2_5 s_4_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_4_8 s_5_8 s_6_8 s_8_8 s_10_8 s_4_9 s_6_9 s_7_9 s_1_10 s_2_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_6_1 s_7_1 s_8_1 s_1_2 s_3_2 s_4_2 s_7_2 s_10_2 s_2_3 s_6_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_8_4 s_9_4 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_8_6 s_10_6 s_1_7 s_2_7 s_5_7 s_9_7 s_4_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_4_9 s_7_9 s_8_9 s_1_10 s_2_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_6_1 s_7_1 s_1_2 s_3_2 s_4_2 s_9_2 s_10_2 s_2_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_4_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_4_7 s_5_7 s_6_7 s_9_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_2_9 s_7_9 s_8_9 s_1_10 s_2_10 s_3_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_7_1 s_8_1 s_1_2 s_4_2 s_10_2 s_6_3 s_10_3 s_2_4 s_3_4 s_6_4 s_8_4 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_8_6 s_10_6 s_1_7 s_5_7 s_9_7 s_10_7 s_3_8 s_4_8 s_6_8 s_7_8 s_8_8 s_10_8 s_7_9 s_1_10 s_2_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_7_1 s_8_1 s_1_2 s_3_2 s_4_2 s_5_2 s_10_2 s_2_3 s_4_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_7_4 s_8_4 s_2_5 s_4_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_3_8 s_4_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_6_9 s_7_9 s_8_9 s_1_10 s_2_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_7_1 s_1_2 s_3_2 s_4_2 s_5_2 s_7_2 s_9_2 s_10_2 s_2_3 s_4_3 s_6_3 s_8_3 s_1_4 s_2_4 s_6_4 s_8_4 s_9_4 s_2_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_5_7 s_8_7 s_9_7 s_1_8 s_4_8 s_6_8 s_8_8 s_9_8 s_10_8 s_2_9 s_4_9 s_6_9 s_7_9 s_8_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_7_1 s_1_2 s_4_2 s_7_2 s_9_2 s_10_2 s_8_3 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_4_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_4_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_2_9 s_4_9 s_7_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_7_1 s_1_2 s_2_2 s_4_2 s_7_2 s_9_2 s_10_2 s_3_3 s_8_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_4_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_4_8 s_5_8 s_6_8 s_7_8 s_10_8 s_2_9 s_4_9 s_7_9 s_9_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_7_1 s_2_2 s_3_2 s_4_2 s_7_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_8_3 s_2_4 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_8_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_4_8 s_5_8 s_6_8 s_9_8 s_10_8 s_2_9 s_4_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_7_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_9_2 s_10_2 s_2_3 s_3_3 s_4_3 s_6_3 s_8_3 s_1_4 s_2_4 s_4_4 s_6_4 s_8_4 s_9_4 s_2_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_4_8 s_6_8 s_9_8 s_10_8 s_2_9 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_7_1 s_2_2 s_4_2 s_5_2 s_7_2 s_9_2 s_10_2 s_3_3 s_6_3 s_8_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_4_8 s_6_8 s_7_8 s_10_8 s_2_9 s_4_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_4_1 s_7_1 s_8_1 s_1_2 s_2_2 s_4_2 s_10_2 s_3_3 s_6_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_8_4 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_8_6 s_10_6 s_1_7 s_5_7 s_7_7 s_9_7 s_10_7 s_3_8 s_4_8 s_6_8 s_7_8 s_10_8 s_7_9 s_9_9 s_1_10 s_2_10 s_5_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_7_1 s_8_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_10_2 s_2_3 s_3_3 s_4_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_3_8 s_4_8 s_5_8 s_6_8 s_9_8 s_10_8 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_5_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_7_1 s_8_1 s_2_2 s_3_2 s_4_2 s_10_2 s_2_3 s_3_3 s_4_3 s_6_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_8_4 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_7_6 s_8_6 s_10_6 s_1_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_3_8 s_4_8 s_6_8 s_9_8 s_10_8 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_5_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_7_1 s_8_1 s_2_2 s_4_2 s_5_2 s_10_2 s_3_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_2_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_8_6 s_9_6 s_10_6 s_1_7 s_4_7 s_5_7 s_7_7 s_9_7 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_10_8 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_5_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_6_1 s_7_1 s_8_1 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_10_2 s_2_3 s_3_3 s_8_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_8_4 s_9_4 s_2_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_7_7 s_9_7 s_10_7 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_6_10 s_8_10 s_10_10 )))

(assert (not (and s_4_1 s_6_1 s_7_1 s_8_1 s_1_2 s_2_2 s_4_2 s_5_2 s_7_2 s_10_2 s_3_3 s_4_3 s_8_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_9_4 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_7_6 s_8_6 s_9_6 s_10_6 s_1_7 s_2_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_4_8 s_5_8 s_6_8 s_10_8 s_4_9 s_6_9 s_7_9 s_9_9 s_1_10 s_2_10 s_6_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_6_1 s_7_1 s_1_2 s_2_2 s_3_2 s_4_2 s_9_2 s_10_2 s_2_3 s_3_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_8_4 s_4_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_7_6 s_8_6 s_10_6 s_1_7 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_2_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_6_1 s_7_1 s_8_1 s_1_2 s_2_2 s_3_2 s_4_2 s_7_2 s_10_2 s_2_3 s_3_3 s_6_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_8_4 s_9_4 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_8_6 s_10_6 s_1_7 s_2_7 s_5_7 s_7_7 s_9_7 s_4_8 s_6_8 s_7_8 s_9_8 s_10_8 s_4_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_6_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_2_1 s_6_1 s_7_1 s_8_1 s_2_2 s_4_2 s_7_2 s_10_2 s_3_3 s_4_3 s_6_3 s_8_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_8_4 s_9_4 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_7_6 s_8_6 s_10_6 s_1_7 s_2_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_4_8 s_6_8 s_10_8 s_4_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_6_1 s_7_1 s_2_2 s_4_2 s_9_2 s_10_2 s_3_3 s_4_3 s_1_4 s_2_4 s_4_4 s_6_4 s_7_4 s_8_4 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_8_6 s_10_6 s_1_7 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_3_8 s_4_8 s_5_8 s_6_8 s_10_8 s_2_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_6_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_1_3 s_2_3 s_4_3 s_5_3 s_7_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_6_6 s_7_6 s_9_6 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_3_8 s_5_8 s_8_8 s_9_8 s_1_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_4_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_8_1 s_9_1 s_1_2 s_3_2 s_4_2 s_5_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_7_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_1_6 s_3_6 s_4_6 s_7_6 s_9_6 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_3_8 s_5_8 s_9_8 s_1_9 s_6_9 s_7_9 s_8_9 s_1_10 s_4_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_5_1 s_7_1 s_8_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_7_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_7_6 s_8_6 s_9_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_3_8 s_5_8 s_6_8 s_9_8 s_5_9 s_6_9 s_7_9 s_8_9 s_1_10 s_2_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_7_1 s_8_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_10_2 s_1_3 s_2_3 s_4_3 s_7_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_7_4 s_1_5 s_2_5 s_4_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_3_8 s_5_8 s_6_8 s_8_8 s_9_8 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_5_1 s_8_1 s_9_1 s_1_2 s_3_2 s_5_2 s_2_3 s_3_3 s_4_3 s_7_3 s_10_3 s_1_4 s_5_4 s_7_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_1_6 s_3_6 s_7_6 s_8_6 s_9_6 s_3_7 s_4_7 s_6_7 s_8_7 s_3_8 s_5_8 s_6_8 s_9_8 s_10_8 s_1_9 s_6_9 s_8_9 s_1_10 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_5_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_5_2 s_2_3 s_4_3 s_7_3 s_10_3 s_1_4 s_4_4 s_5_4 s_7_4 s_10_4 s_1_5 s_2_5 s_4_5 s_6_5 s_1_6 s_3_6 s_6_6 s_7_6 s_8_6 s_9_6 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_3_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_6_9 s_8_9 s_9_9 s_1_10 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_1_2 s_2_2 s_5_2 s_7_2 s_4_3 s_7_3 s_8_3 s_10_3 s_4_4 s_5_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_6_5 s_10_5 s_3_6 s_6_6 s_7_6 s_8_6 s_9_6 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_8_7 s_10_7 s_5_8 s_6_8 s_8_8 s_10_8 s_1_9 s_4_9 s_6_9 s_9_9 s_1_10 s_4_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_1_2 s_5_2 s_7_2 s_3_3 s_4_3 s_7_3 s_8_3 s_10_3 s_5_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_10_5 s_3_6 s_7_6 s_8_6 s_9_6 s_2_7 s_3_7 s_4_7 s_6_7 s_8_7 s_10_7 s_5_8 s_6_8 s_10_8 s_1_9 s_4_9 s_6_9 s_1_10 s_4_10 s_8_10 s_9_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_8_1 s_9_1 s_1_2 s_3_3 s_6_3 s_7_3 s_10_3 s_3_4 s_10_4 s_1_5 s_5_5 s_8_5 s_1_6 s_5_6 s_8_6 s_3_7 s_10_7 s_3_8 s_6_8 s_7_8 s_10_8 s_1_9 s_1_10 s_4_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_4_1 s_5_1 s_8_1 s_9_1 s_1_2 s_2_2 s_6_3 s_7_3 s_10_3 s_3_4 s_4_4 s_10_4 s_1_5 s_8_5 s_1_6 s_5_6 s_6_6 s_8_6 s_3_7 s_7_7 s_10_7 s_3_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_9_9 s_1_10 s_4_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_8_1 s_9_1 s_1_2 s_3_2 s_7_2 s_2_3 s_3_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_3_4 s_9_4 s_10_4 s_1_5 s_5_5 s_8_5 s_10_5 s_5_6 s_8_6 s_2_7 s_3_7 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_4_9 s_8_9 s_1_10 s_4_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_5_1 s_6_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_7_2 s_2_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_3_4 s_4_4 s_9_4 s_10_4 s_1_5 s_8_5 s_10_5 s_5_6 s_6_6 s_8_6 s_2_7 s_3_7 s_7_7 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_4_9 s_8_9 s_9_9 s_1_10 s_4_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_4_1 s_5_1 s_9_1 s_1_2 s_2_2 s_7_2 s_9_2 s_7_3 s_8_3 s_3_4 s_4_4 s_5_4 s_7_4 s_9_4 s_10_4 s_1_5 s_4_5 s_6_5 s_1_6 s_3_6 s_6_6 s_7_6 s_8_6 s_2_7 s_3_7 s_4_7 s_6_7 s_7_7 s_10_7 s_1_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_4_9 s_9_9 s_1_10 s_3_10 s_4_10 s_5_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_4_1 s_5_1 s_9_1 s_1_2 s_7_2 s_9_2 s_3_3 s_7_3 s_8_3 s_3_4 s_5_4 s_7_4 s_9_4 s_10_4 s_1_5 s_4_5 s_5_5 s_6_5 s_1_6 s_3_6 s_7_6 s_8_6 s_2_7 s_3_7 s_4_7 s_6_7 s_10_7 s_1_8 s_5_8 s_6_8 s_7_8 s_10_8 s_1_9 s_2_9 s_4_9 s_1_10 s_3_10 s_4_10 s_5_10 s_8_10 s_9_10 )))

(assert (not (and s_5_1 s_9_1 s_1_2 s_2_2 s_3_2 s_5_2 s_7_2 s_9_2 s_2_3 s_4_3 s_6_3 s_7_3 s_8_3 s_1_4 s_4_4 s_9_4 s_10_4 s_1_5 s_2_5 s_8_5 s_1_6 s_5_6 s_6_6 s_8_6 s_9_6 s_2_7 s_3_7 s_7_7 s_8_7 s_1_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_6_9 s_8_9 s_9_9 s_1_10 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_9_1 s_1_2 s_3_2 s_5_2 s_7_2 s_9_2 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_1_4 s_9_4 s_10_4 s_1_5 s_2_5 s_5_5 s_8_5 s_1_6 s_5_6 s_8_6 s_9_6 s_2_7 s_3_7 s_8_7 s_1_8 s_6_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_6_9 s_8_9 s_1_10 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_5_1 s_6_1 s_9_1 s_1_2 s_2_2 s_3_2 s_9_2 s_2_3 s_7_3 s_1_4 s_3_4 s_4_4 s_5_4 s_7_4 s_10_4 s_1_5 s_4_5 s_6_5 s_10_5 s_3_6 s_6_6 s_7_6 s_8_6 s_3_7 s_4_7 s_6_7 s_7_7 s_1_8 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_8_9 s_9_9 s_1_10 s_3_10 s_4_10 s_7_10 s_8_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_5_1 s_6_1 s_9_1 s_1_2 s_3_2 s_9_2 s_2_3 s_3_3 s_7_3 s_1_4 s_3_4 s_5_4 s_7_4 s_10_4 s_1_5 s_4_5 s_5_5 s_6_5 s_10_5 s_3_6 s_7_6 s_8_6 s_3_7 s_4_7 s_6_7 s_1_8 s_3_8 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_2_9 s_8_9 s_1_10 s_3_10 s_4_10 s_7_10 s_8_10 s_9_10 )))

(assert (not (and s_2_1 s_5_1 s_9_1 s_2_2 s_5_2 s_7_2 s_9_2 s_6_3 s_7_3 s_8_3 s_1_4 s_3_4 s_4_4 s_5_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_6_5 s_8_5 s_1_6 s_6_6 s_7_6 s_8_6 s_9_6 s_2_7 s_3_7 s_6_7 s_7_7 s_1_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_2_9 s_4_9 s_6_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_9_1 s_5_2 s_7_2 s_9_2 s_3_3 s_6_3 s_7_3 s_8_3 s_1_4 s_3_4 s_5_4 s_9_4 s_10_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_8_5 s_1_6 s_7_6 s_8_6 s_9_6 s_2_7 s_3_7 s_6_7 s_1_8 s_6_8 s_7_8 s_10_8 s_1_9 s_2_9 s_4_9 s_6_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_8_1 s_9_1 s_2_2 s_5_2 s_7_3 s_10_3 s_1_4 s_3_4 s_4_4 s_7_4 s_10_4 s_1_5 s_2_5 s_1_6 s_3_6 s_5_6 s_6_6 s_8_6 s_9_6 s_3_7 s_4_7 s_7_7 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_10_8 s_1_9 s_6_9 s_9_9 s_10_9 s_1_10 s_4_10 s_5_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_8_1 s_9_1 s_5_2 s_3_3 s_7_3 s_10_3 s_1_4 s_3_4 s_7_4 s_10_4 s_1_5 s_2_5 s_5_5 s_1_6 s_3_6 s_5_6 s_8_6 s_9_6 s_3_7 s_4_7 s_3_8 s_5_8 s_6_8 s_7_8 s_10_8 s_1_9 s_6_9 s_10_9 s_1_10 s_4_10 s_5_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_9_1 s_2_2 s_9_2 s_4_3 s_7_3 s_1_4 s_4_4 s_7_4 s_10_4 s_1_5 s_10_5 s_3_6 s_5_6 s_6_6 s_8_6 s_3_7 s_4_7 s_7_7 s_8_7 s_1_8 s_3_8 s_5_8 s_6_8 s_8_8 s_10_8 s_1_9 s_2_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_9_1 s_9_2 s_3_3 s_4_3 s_7_3 s_1_4 s_7_4 s_10_4 s_1_5 s_5_5 s_10_5 s_3_6 s_5_6 s_8_6 s_3_7 s_4_7 s_8_7 s_1_8 s_3_8 s_5_8 s_6_8 s_10_8 s_1_9 s_2_9 s_10_9 s_1_10 s_3_10 s_4_10 s_7_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_5_1 s_6_1 s_8_1 s_9_1 s_7_2 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_5_4 s_9_4 s_10_4 s_1_5 s_4_5 s_5_5 s_6_5 s_8_5 s_10_5 s_7_6 s_8_6 s_2_7 s_3_7 s_6_7 s_8_7 s_6_8 s_10_8 s_1_9 s_4_9 s_10_9 s_1_10 s_4_10 s_7_10 s_8_10 )))

(assert (not (and s_2_1 s_5_1 s_6_1 s_8_1 s_9_1 s_2_2 s_7_2 s_4_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_4_4 s_5_4 s_9_4 s_10_4 s_1_5 s_4_5 s_6_5 s_8_5 s_10_5 s_6_6 s_7_6 s_8_6 s_2_7 s_3_7 s_6_7 s_7_7 s_8_7 s_6_8 s_8_8 s_10_8 s_1_9 s_4_9 s_9_9 s_10_9 s_1_10 s_4_10 s_7_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_3_2 s_5_2 s_7_2 s_2_3 s_3_3 s_7_3 s_8_3 s_10_3 s_3_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_5_5 s_10_5 s_3_6 s_5_6 s_8_6 s_9_6 s_2_7 s_3_7 s_4_7 s_10_7 s_5_8 s_6_8 s_7_8 s_9_8 s_10_8 s_1_9 s_4_9 s_6_9 s_8_9 s_10_9 s_1_10 s_4_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_2_2 s_3_2 s_5_2 s_7_2 s_2_3 s_7_3 s_8_3 s_10_3 s_3_4 s_4_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_10_5 s_3_6 s_5_6 s_6_6 s_8_6 s_9_6 s_2_7 s_3_7 s_4_7 s_7_7 s_10_7 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_10_8 s_1_9 s_4_9 s_6_9 s_8_9 s_9_9 s_10_9 s_1_10 s_4_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_9_1 s_3_2 s_7_2 s_9_2 s_2_3 s_3_3 s_4_3 s_7_3 s_8_3 s_7_4 s_9_4 s_10_4 s_1_5 s_5_5 s_1_6 s_3_6 s_5_6 s_8_6 s_2_7 s_3_7 s_4_7 s_8_7 s_10_7 s_1_8 s_5_8 s_6_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_8_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_8_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_9_1 s_2_2 s_3_2 s_7_2 s_9_2 s_2_3 s_4_3 s_7_3 s_8_3 s_4_4 s_7_4 s_9_4 s_10_4 s_1_5 s_1_6 s_3_6 s_5_6 s_6_6 s_8_6 s_2_7 s_3_7 s_4_7 s_7_7 s_8_7 s_10_7 s_1_8 s_5_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_2_9 s_4_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_2_1 s_4_1 s_5_1 s_8_1 s_9_1 s_2_2 s_3_2 s_2_3 s_4_3 s_6_3 s_7_3 s_10_3 s_4_4 s_5_4 s_10_4 s_1_5 s_4_5 s_6_5 s_8_5 s_1_6 s_6_6 s_7_6 s_8_6 s_3_7 s_6_7 s_7_7 s_8_7 s_10_7 s_3_8 s_6_8 s_8_8 s_9_8 s_10_8 s_1_9 s_8_9 s_9_9 s_10_9 s_1_10 s_4_10 s_5_10 s_8_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_4_1 s_5_1 s_8_1 s_9_1 s_3_2 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_10_3 s_5_4 s_10_4 s_1_5 s_4_5 s_5_5 s_6_5 s_8_5 s_1_6 s_7_6 s_8_6 s_3_7 s_6_7 s_8_7 s_10_7 s_3_8 s_6_8 s_9_8 s_10_8 s_1_9 s_8_9 s_10_9 s_1_10 s_4_10 s_5_10 s_8_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_7_1 s_8_1 s_2_2 s_4_2 s_5_2 s_6_2 s_10_2 s_1_3 s_7_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_7_4 s_1_5 s_2_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_1_7 s_3_7 s_4_7 s_5_7 s_9_7 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_5_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_7_1 s_8_1 s_4_2 s_5_2 s_6_2 s_10_2 s_1_3 s_3_3 s_7_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_1_5 s_2_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_8_6 s_9_6 s_1_7 s_3_7 s_4_7 s_5_7 s_7_7 s_9_7 s_3_8 s_5_8 s_6_8 s_7_8 s_5_9 s_6_9 s_7_9 s_10_9 s_1_10 s_2_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_8_1 s_9_1 s_4_2 s_5_2 s_1_3 s_3_3 s_5_3 s_7_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_7_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_9_6 s_3_7 s_4_7 s_5_7 s_9_7 s_3_8 s_5_8 s_7_8 s_1_9 s_6_9 s_7_9 s_10_9 s_1_10 s_4_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_8_1 s_9_1 s_2_2 s_4_2 s_5_2 s_1_3 s_5_3 s_7_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_10_4 s_1_5 s_2_5 s_3_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_9_6 s_3_7 s_4_7 s_5_7 s_7_7 s_9_7 s_3_8 s_5_8 s_7_8 s_8_8 s_1_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_4_10 s_5_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_8_1 s_9_1 s_2_2 s_4_2 s_7_2 s_1_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_6_6 s_7_6 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_8_8 s_1_9 s_4_9 s_7_9 s_9_9 s_10_9 s_1_10 s_4_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_8_1 s_9_1 s_4_2 s_7_2 s_1_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_9_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_10_5 s_4_6 s_7_6 s_2_7 s_3_7 s_5_7 s_6_7 s_8_7 s_9_7 s_1_9 s_4_9 s_7_9 s_10_9 s_1_10 s_4_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_4_2 s_6_2 s_7_2 s_10_2 s_1_3 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_4_4 s_5_4 s_6_4 s_9_4 s_1_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_7_6 s_8_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_6_8 s_4_9 s_5_9 s_7_9 s_10_9 s_1_10 s_2_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_2_2 s_4_2 s_6_2 s_7_2 s_10_2 s_1_3 s_4_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_5_4 s_6_4 s_9_4 s_1_5 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_6_6 s_7_6 s_8_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_8_7 s_9_7 s_6_8 s_8_8 s_4_9 s_5_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_3_2 s_4_2 s_5_2 s_7_2 s_1_3 s_2_3 s_3_3 s_5_3 s_7_3 s_8_3 s_10_3 s_2_4 s_3_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_9_6 s_2_7 s_3_7 s_4_7 s_5_7 s_9_7 s_10_7 s_5_8 s_7_8 s_9_8 s_1_9 s_4_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_4_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_2_2 s_3_2 s_4_2 s_5_2 s_7_2 s_1_3 s_2_3 s_5_3 s_7_3 s_8_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_9_6 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_9_7 s_10_7 s_5_8 s_7_8 s_8_8 s_9_8 s_1_9 s_4_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_4_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_10_2 s_1_3 s_2_3 s_3_3 s_7_3 s_8_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_7_4 s_9_4 s_1_5 s_2_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_9_7 s_10_7 s_5_8 s_6_8 s_7_8 s_9_8 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_10_2 s_1_3 s_2_3 s_7_3 s_8_3 s_10_3 s_2_4 s_3_4 s_6_4 s_7_4 s_9_4 s_1_5 s_2_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_9_7 s_10_7 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_8_1 s_9_1 s_2_2 s_3_2 s_4_2 s_1_3 s_2_3 s_4_3 s_5_3 s_6_3 s_7_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_10_4 s_1_5 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_7_6 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_3_8 s_8_8 s_9_8 s_1_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_4_10 s_5_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_8_1 s_9_1 s_3_2 s_4_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_6_3 s_7_3 s_10_3 s_2_4 s_5_4 s_6_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_7_6 s_3_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_3_8 s_9_8 s_1_9 s_7_9 s_8_9 s_10_9 s_1_10 s_4_10 s_5_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_3_2 s_4_2 s_6_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_1_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_7_6 s_8_6 s_1_7 s_3_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_3_8 s_6_8 s_9_8 s_5_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_5_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_2_2 s_3_2 s_4_2 s_6_2 s_10_2 s_1_3 s_2_3 s_4_3 s_6_3 s_7_3 s_10_3 s_2_4 s_5_4 s_6_4 s_1_5 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_7_6 s_8_6 s_1_7 s_3_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_3_8 s_6_8 s_8_8 s_9_8 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_5_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_9_1 s_2_2 s_4_2 s_5_2 s_7_2 s_9_2 s_1_3 s_5_3 s_6_3 s_7_3 s_8_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_6_6 s_7_6 s_9_6 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_7_8 s_8_8 s_1_9 s_2_9 s_4_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_9_1 s_4_2 s_5_2 s_7_2 s_9_2 s_1_3 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_1_6 s_4_6 s_7_6 s_9_6 s_2_7 s_3_7 s_5_7 s_6_7 s_9_7 s_1_8 s_7_8 s_1_9 s_2_9 s_4_9 s_6_9 s_7_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_9_1 s_2_2 s_4_2 s_9_2 s_1_3 s_4_3 s_5_3 s_7_3 s_1_4 s_2_4 s_4_4 s_6_4 s_7_4 s_10_4 s_1_5 s_3_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_6_6 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_3_8 s_5_8 s_8_8 s_1_9 s_2_9 s_7_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_9_1 s_4_2 s_9_2 s_1_3 s_3_3 s_4_3 s_5_3 s_7_3 s_1_4 s_2_4 s_6_4 s_7_4 s_10_4 s_1_5 s_3_5 s_5_5 s_7_5 s_10_5 s_3_6 s_4_6 s_5_6 s_3_7 s_4_7 s_5_7 s_8_7 s_9_7 s_1_8 s_3_8 s_5_8 s_1_9 s_2_9 s_7_9 s_10_9 s_1_10 s_3_10 s_4_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_9_1 s_2_2 s_3_2 s_4_2 s_7_2 s_9_2 s_1_3 s_2_3 s_4_3 s_5_3 s_7_3 s_8_3 s_2_4 s_4_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_3_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_6_6 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_5_8 s_8_8 s_9_8 s_1_9 s_2_9 s_4_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_9_1 s_3_2 s_4_2 s_7_2 s_9_2 s_1_3 s_2_3 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_2_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_3_5 s_5_5 s_7_5 s_1_6 s_3_6 s_4_6 s_5_6 s_2_7 s_3_7 s_4_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_5_8 s_9_8 s_1_9 s_2_9 s_4_9 s_7_9 s_8_9 s_10_9 s_1_10 s_3_10 s_4_10 s_5_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_4_2 s_6_2 s_9_2 s_10_2 s_1_3 s_3_3 s_4_3 s_7_3 s_1_4 s_2_4 s_4_4 s_6_4 s_7_4 s_1_5 s_5_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_8_6 s_1_7 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_3_8 s_5_8 s_6_8 s_2_9 s_5_9 s_7_9 s_10_9 s_1_10 s_2_10 s_3_10 s_7_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_6_1 s_7_1 s_2_2 s_4_2 s_6_2 s_9_2 s_10_2 s_1_3 s_4_3 s_7_3 s_1_4 s_2_4 s_6_4 s_7_4 s_1_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_1_7 s_3_7 s_4_7 s_5_7 s_8_7 s_9_7 s_1_8 s_3_8 s_5_8 s_6_8 s_8_8 s_2_9 s_5_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_7_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_5_1 s_7_1 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_6_3 s_7_3 s_8_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_9_4 s_1_5 s_2_5 s_4_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_9_7 s_1_8 s_6_8 s_7_8 s_8_8 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_7_10 )))

(assert (not (and s_2_1 s_3_1 s_5_1 s_7_1 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_3_3 s_6_3 s_7_3 s_8_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_9_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_6_8 s_7_8 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_7_10 s_10_10 )))

(assert (not (and s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_3_2 s_4_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_7_3 s_8_3 s_2_4 s_4_4 s_6_4 s_7_4 s_9_4 s_1_5 s_5_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_8_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_7_7 s_8_7 s_9_7 s_10_7 s_1_8 s_5_8 s_6_8 s_9_8 s_2_9 s_4_9 s_5_9 s_7_9 s_8_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 s_10_10 )))

(assert (not (and s_1_1 s_2_1 s_3_1 s_4_1 s_5_1 s_7_1 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_7_3 s_8_3 s_2_4 s_6_4 s_7_4 s_9_4 s_1_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_5_6 s_6_6 s_8_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_8_7 s_9_7 s_10_7 s_1_8 s_5_8 s_6_8 s_8_8 s_9_8 s_2_9 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_10_9 s_1_10 s_2_10 s_3_10 s_5_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_7_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_9_2 s_10_2 s_1_3 s_2_3 s_7_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_1_5 s_4_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_9_7 s_1_8 s_3_8 s_5_8 s_6_8 s_7_8 s_8_8 s_9_8 s_2_9 s_5_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_7_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_1_2 s_2_2 s_3_2 s_4_2 s_6_2 s_7_2 s_10_2 s_1_3 s_2_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_9_4 s_1_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_6_6 s_8_6 s_1_7 s_2_7 s_3_7 s_5_7 s_9_7 s_6_8 s_7_8 s_8_8 s_9_8 s_4_9 s_5_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_7_1 s_1_2 s_3_2 s_4_2 s_6_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_7_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_1_5 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_7_6 s_8_6 s_1_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_3_8 s_5_8 s_6_8 s_7_8 s_9_8 s_2_9 s_5_9 s_7_9 s_8_9 s_1_10 s_2_10 s_3_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_7_1 s_8_1 s_1_2 s_3_2 s_4_2 s_6_2 s_7_2 s_10_2 s_1_3 s_2_3 s_3_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_9_4 s_1_5 s_5_5 s_7_5 s_8_5 s_9_5 s_10_5 s_2_6 s_4_6 s_5_6 s_8_6 s_1_7 s_2_7 s_3_7 s_5_7 s_7_7 s_9_7 s_6_8 s_7_8 s_9_8 s_4_9 s_5_9 s_7_9 s_8_9 s_1_10 s_2_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_7_1 s_1_2 s_2_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_4_3 s_6_3 s_7_3 s_8_3 s_1_4 s_2_4 s_6_4 s_9_4 s_1_5 s_2_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_5_7 s_8_7 s_9_7 s_1_8 s_6_8 s_8_8 s_9_8 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_9_9 s_1_10 s_2_10 s_3_10 s_5_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_7_1 s_1_2 s_3_2 s_4_2 s_5_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_2_3 s_3_3 s_4_3 s_6_3 s_7_3 s_8_3 s_1_4 s_2_4 s_4_4 s_6_4 s_9_4 s_1_5 s_2_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_5_7 s_7_7 s_8_7 s_9_7 s_1_8 s_6_8 s_9_8 s_2_9 s_4_9 s_5_9 s_6_9 s_7_9 s_8_9 s_1_10 s_2_10 s_3_10 s_5_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_1_2 s_4_2 s_5_2 s_6_2 s_7_2 s_10_2 s_1_3 s_3_3 s_4_3 s_7_3 s_8_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_1_5 s_2_5 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_5_8 s_6_8 s_4_9 s_5_9 s_6_9 s_7_9 s_1_10 s_2_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_7_1 s_8_1 s_1_2 s_2_2 s_4_2 s_5_2 s_6_2 s_7_2 s_10_2 s_1_3 s_4_3 s_7_3 s_8_3 s_10_3 s_2_4 s_5_4 s_6_4 s_7_4 s_9_4 s_1_5 s_2_5 s_4_5 s_6_5 s_7_5 s_9_5 s_10_5 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_9_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_5_8 s_6_8 s_8_8 s_4_9 s_5_9 s_6_9 s_7_9 s_9_9 s_1_10 s_2_10 s_9_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_7_1 s_1_2 s_2_2 s_4_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_7_3 s_8_3 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_9_4 s_1_5 s_4_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_6_6 s_7_6 s_8_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_9_7 s_10_7 s_1_8 s_5_8 s_6_8 s_7_8 s_8_8 s_2_9 s_4_9 s_5_9 s_7_9 s_9_9 s_1_10 s_2_10 s_3_10 s_5_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_7_1 s_1_2 s_4_2 s_6_2 s_7_2 s_9_2 s_10_2 s_1_3 s_3_3 s_7_3 s_8_3 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_1_5 s_4_5 s_5_5 s_6_5 s_7_5 s_9_5 s_1_6 s_2_6 s_3_6 s_4_6 s_7_6 s_8_6 s_1_7 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_10_7 s_1_8 s_5_8 s_6_8 s_7_8 s_2_9 s_4_9 s_5_9 s_7_9 s_1_10 s_2_10 s_3_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_1_2 s_4_2 s_6_2 s_10_2 s_1_3 s_3_3 s_6_3 s_7_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_1_5 s_5_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_8_6 s_1_7 s_3_7 s_5_7 s_7_7 s_9_7 s_10_7 s_3_8 s_6_8 s_7_8 s_5_9 s_7_9 s_1_10 s_2_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_7_1 s_8_1 s_1_2 s_2_2 s_4_2 s_6_2 s_10_2 s_1_3 s_6_3 s_7_3 s_10_3 s_2_4 s_3_4 s_6_4 s_1_5 s_7_5 s_8_5 s_9_5 s_1_6 s_2_6 s_4_6 s_5_6 s_6_6 s_8_6 s_1_7 s_3_7 s_5_7 s_9_7 s_10_7 s_3_8 s_6_8 s_7_8 s_8_8 s_5_9 s_7_9 s_9_9 s_1_10 s_2_10 s_5_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_8_1 s_9_1 s_1_2 s_4_2 s_1_3 s_3_3 s_5_3 s_6_3 s_7_3 s_10_3 s_2_4 s_3_4 s_6_4 s_10_4 s_1_5 s_3_5 s_5_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_3_7 s_5_7 s_9_7 s_10_7 s_3_8 s_7_8 s_1_9 s_7_9 s_1_10 s_4_10 s_5_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_8_1 s_9_1 s_1_2 s_2_2 s_4_2 s_1_3 s_5_3 s_6_3 s_7_3 s_10_3 s_2_4 s_3_4 s_4_4 s_6_4 s_10_4 s_1_5 s_3_5 s_7_5 s_8_5 s_1_6 s_4_6 s_5_6 s_6_6 s_3_7 s_5_7 s_7_7 s_9_7 s_10_7 s_3_8 s_7_8 s_8_8 s_1_9 s_7_9 s_9_9 s_1_10 s_4_10 s_5_10 s_9_10 )))

(assert (not (and s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_1_2 s_4_2 s_5_2 s_7_2 s_1_3 s_3_3 s_4_3 s_5_3 s_7_3 s_8_3 s_10_3 s_2_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_7_6 s_9_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_8_7 s_9_7 s_10_7 s_5_8 s_1_9 s_4_9 s_6_9 s_7_9 s_1_10 s_4_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_4_1 s_5_1 s_6_1 s_8_1 s_9_1 s_1_2 s_2_2 s_4_2 s_5_2 s_7_2 s_1_3 s_4_3 s_5_3 s_7_3 s_8_3 s_10_3 s_2_4 s_4_4 s_5_4 s_6_4 s_7_4 s_9_4 s_10_4 s_1_5 s_2_5 s_3_5 s_4_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_7_6 s_9_6 s_2_7 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_8_7 s_9_7 s_10_7 s_5_8 s_8_8 s_1_9 s_4_9 s_6_9 s_7_9 s_9_9 s_1_10 s_4_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_9_1 s_1_2 s_3_2 s_4_2 s_9_2 s_1_3 s_2_3 s_3_3 s_5_3 s_7_3 s_1_4 s_2_4 s_3_4 s_5_4 s_6_4 s_7_4 s_10_4 s_1_5 s_3_5 s_4_5 s_5_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_7_6 s_3_7 s_4_7 s_5_7 s_6_7 s_9_7 s_1_8 s_3_8 s_5_8 s_7_8 s_9_8 s_1_9 s_2_9 s_7_9 s_8_9 s_1_10 s_3_10 s_4_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_9_2 s_1_3 s_2_3 s_5_3 s_7_3 s_1_4 s_2_4 s_3_4 s_4_4 s_5_4 s_6_4 s_7_4 s_10_4 s_1_5 s_3_5 s_4_5 s_6_5 s_7_5 s_10_5 s_3_6 s_4_6 s_6_6 s_7_6 s_3_7 s_4_7 s_5_7 s_6_7 s_7_7 s_9_7 s_1_8 s_3_8 s_5_8 s_7_8 s_8_8 s_9_8 s_1_9 s_2_9 s_7_9 s_8_9 s_9_9 s_1_10 s_3_10 s_4_10 s_7_10 s_9_10 )))

(assert (not (and s_3_1 s_5_1 s_6_1 s_8_1 s_9_1 s_1_2 s_3_2 s_4_2 s_7_2 s_1_3 s_2_3 s_3_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_6_4 s_9_4 s_10_4 s_1_5 s_3_5 s_5_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_2_7 s_3_7 s_5_7 s_9_7 s_7_8 s_9_8 s_1_9 s_4_9 s_7_9 s_8_9 s_1_10 s_4_10 s_7_10 s_9_10 s_10_10 )))

(assert (not (and s_1_1 s_3_1 s_5_1 s_6_1 s_8_1 s_9_1 s_1_2 s_2_2 s_3_2 s_4_2 s_7_2 s_1_3 s_2_3 s_5_3 s_6_3 s_7_3 s_8_3 s_10_3 s_1_4 s_2_4 s_3_4 s_4_4 s_6_4 s_9_4 s_10_4 s_1_5 s_3_5 s_7_5 s_8_5 s_10_5 s_4_6 s_5_6 s_6_6 s_2_7 s_3_7 s_5_7 s_7_7 s_9_7 s_7_8 s_8_8 s_9_8 s_1_9 s_4_9 s_7_9 s_8_9 s_9_9 s_1_10 s_4_10 s_7_10 s_9_10 )))

(check-sat)
(get-model)

