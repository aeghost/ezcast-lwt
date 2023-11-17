(lang dune 2.8)

(generate_opam_files true)

(name ezcast-lwt)

(package
 (name ezcast-lwt)
 (version 0.1.0)
 (synopsis "A simple way to cast infos on networks")
 (description
  "Synchronise informations using Multicast or Broadcast through network")
 (license "MIT")
 (authors "Matthieu GOSSET matthieu.gosset.dev@outlook.com")
 (maintainers "Matthieu GOSSET matthieu.gosset.dev@outlook.com")
 (homepage "https://github.com/aeghost/ezcast-lwt")
 (source
  (github "aeghost/ezcast-lwt"))
 (bug_reports "https://github.com/aeghost/ezcast-lwt/issues")
 (depends
  (lwt
   (>= 5.0.0))
  (lwt_ppx
   (>= 2.0.0))
  (fileutils
   (>= 0.6.0))))
