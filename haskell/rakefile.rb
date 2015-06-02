
desc "run haskell version"
task :haskell do
  sh "time cabal run linkcheck"
end

desc "profile haskell version"
task :profile_haskell do
  sh "time cabal run profiling-linkcheck --ghc-options=RTS -p -s -h -i0.1"
  sh "hp2ps -e8in -c profiling-linkcheck.hp"
end

