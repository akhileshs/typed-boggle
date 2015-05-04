(defproject typed-boggle "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.typed "0.2.84"]]
  :plugins [[lein-typed "0.3.5"]]
  :core.typed {:check [typed_boggle.board typed_boggle.dictionaryFile typed_boggle.solve]})
