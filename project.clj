(defproject funcool/struct "1.1.0"
  :description "A structural validation library for Clojure(Script)"
  :url "https://github.com/funcool/struct"
  :license {:name "Public Domain" :url "http://unlicense.org/"}
  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [org.clojure/clojurescript "1.9.946" :scope "provided"]
                 [funcool/cuerdas "2.0.5"]]
  :source-paths ["src"]
  :test-paths ["test"]
  :codeina {:sources ["src"]
            :reader :clojure
            :target "doc/dist/latest/api"}
  :plugins [[funcool/codeina "0.5.0"]
            [lein-ancient "0.6.15" :exclusions [org.clojure/tools.reader]]])
