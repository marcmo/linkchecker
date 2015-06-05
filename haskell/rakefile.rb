package_db = "$HOME/.stackage/sandboxes/ghc-7.8.4/nightly-2015-05-04/x86_64-osx-ghc-7.8.4-packages.conf.d"

desc "run haskell version"
task :haskell do
  sh "cabal build linkcheck"
  sh "time cabal run linkcheck"
end

desc "profile haskell version"
task :profile_haskell do
  sh 'time cabal run profiling-linkcheck --ghc-options="RTS -p -s -h -i0.1"'
  sh "hp2ps -e8in -c profiling-linkcheck.hp"
end

desc "runghc simpleclient.hs"
task :simple do
  sh "runghc -- -package-db --ghc-arg=#{package_db} simpleclient.hs"
end

desc "ghci in sandbox"
task :ghci do
  sh"ghci -no-user-package-db -package-db #{package_db}"
end
