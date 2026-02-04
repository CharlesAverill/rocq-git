(lang dune 3.21)
(using rocq 0.11)

(name rocq-git)
(version 0.1)

(generate_opam_files true)

(source
 (github CharlesAverill/rocq-git))

(authors
 "Charles Averill <charles@utdallas.edu>")

(maintainers
 "Charles Averill <charles@utdallas.edu>")

(license GPL-3.0)

(documentation https://github.com/CharlesAverill/rocq-git)

(package
 (name rocq-git)
 (synopsis "Git implemented in extractable Rocq")
 (description "A rudimentary implementation of git in Rocq, with rudimentary filesystem modeling")
 (depends
   (dune (>= 3.21.0))
   (rocq (>= 9.0.0))
   (camlzip (>= 1.13))))
