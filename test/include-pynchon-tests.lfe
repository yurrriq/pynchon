(defmodule include-pynchon-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "eunit/include/eunit.hrl")
(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "pynchon/include/pynchon.lfe")


(deftest diamond
  (is-equal (-<> (car (list 1))) 1)

  ;; (is-equal (-<> 0
  ;;                (* <> 5)
  ;;                (list 1 2 <> 3 4))
  ;;           (list 1 2 0 3 4))

  ;; (is-equal (-<> (list 1 2 3)
  ;;                (list (list -1 0) <> (list 4 5)
  ;;                      (-<> 10
  ;;                           (list 7 8 9 <> 11 12)
  ;;                           (cons 6 <>)))
  ;;                (lists:append))
  ;;           (lists:seq -1 12))


  ;; (is-equal (list 1 2 3 10 4 5)
  ;;           (-<> 10 (list 1 2 3 <> 4 5)))

  ;; (is-equal (-<> 0 (list 1 2 3))
  ;;           (list 0 1 2 3))

  )
