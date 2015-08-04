(defmodule swiss-arrows-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "eunit/include/eunit.hrl")
(include-lib "ltest/include/ltest-macros.lfe")
;; (include-lib "swiss-arrows/include/swiss-arrows.lfe")
(include-lib "include/swiss-arrows.lfe")

(deftest diamond
  (is-equal (-<> (car '(1))) 1)
  ;; (is-equal (-<> 0
  ;;                (* <> 5)
  ;;                (list 1 2 <> 3 4))
  ;;           '(1 2 0 3 4))
  )
