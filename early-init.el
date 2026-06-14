(setq my-cpu-architecture "bonnell")

(setq native-comp-compiler-options
     `("-O2"
       ,(format "-mtune=%s" my-cpu-architecture)
       ,(format "-march=%s" my-cpu-architecture)
       "-g0"
       "-fno-omit-frame-pointer"
       "-fno-finite-math-only"))

(setq native-comp-driver-options
     '("-Wl,-z,pack-relative-relocs"
       "-Wl,-O2"
       "-Wl,--as-needed"))
