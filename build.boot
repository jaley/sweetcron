(def project 'sweetcron)
(def version "0.1.0-SNAPSHOT")

(set-env! :resource-paths #{"resources" "src"}
          :source-paths   #{"test"}
          :dependencies   '[[org.clojure/clojure "1.8.0"]
                            [clj-time "0.13.0"]

                            [expectations "2.2.0-beta1" :scope "test"]
                            [seancorfield/boot-expectations "1.0.11" :scope "test"]])

(task-options!
 aot {:namespace   #{'sweetcron.core}}
 pom {:project     project
      :version     version
      :description "Scheduling, like cron, but sweet."
      :scm         {:url "https://github.com/jaley/sweetcron"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}}
 jar {:main        'sweetcron.core
      :file        (str "sweetcron-" version "-standalone.jar")})

(deftask build
  "Build the project locally as a JAR."
  [d dir PATH #{str} "the set of directories to write to (target)."]
  (let [dir (if (seq dir) dir #{"target"})]
    (comp (aot) (pom) (uber) (jar) (target :dir dir))))

(deftask run
  "Run the project."
  [a args ARG [str] "the arguments for the application."]
  (require '[sweetcron.core :as app])
  (apply (resolve 'app/-main) args))

(require '[seancorfield.boot-expectations :refer :all])
