(use setup-api)
(compile tests.scm -o test1)
(compile test-jar-diff.scm -o test2)
(and (run (./test1))
     (run (./test2)))
