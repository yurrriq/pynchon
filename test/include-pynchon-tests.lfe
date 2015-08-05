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
  )
