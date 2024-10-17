(defpackage :social-shopping-system (:use :cl :asdf))
(in-package :social-shopping-system)

(defsystem social-shopping
  :name "Social-Shopping"
  :version "1.1"
  :author "Kevin Raison <first name @ offersavvy dot com>"
  :description "Social-Shopping"
  :depends-on (:puri
               :drakma
               :cl-who
               :cl-json
               :hunchentoot
               :clouchdb
               :parenscript
               :log4cl
               :bordeaux-threads
               :ironclad
               :uuid
               :secure-random
               :iterate
               :cl-smtp
               :view-server
               :local-time
               :cl-memcached
               :cl-utilities
               :sb-concurrency
               :html-entities
               :cl-geocode
               :py-configparser
               :linkshare
               :imago
               :cl-csv
               :parse-number
               :cl-elasticsearch
               :graph-db
               :cl-store
               :porter-stemmer
               :soundex
               :trivial-shell)
  :components ((:file "package")
               (:file "json" :depends-on ("package"))
               (:file "globals" :depends-on ("package"))
               (:file "utilities" :depends-on ("json"))
               (:file "cache" :depends-on ("utilities"))
               (:file "configuration" :depends-on ("globals" "utilities" "cache"))
               (:file "country" :depends-on ("configuration"))
               (:file "currency" :depends-on ("configuration"))
               (:file "types" :depends-on ("country" "currency"))
               (:file "schema" :depends-on ("types"))
               (:file "graph-views" :depends-on ("schema"))
               (:file "geo" :depends-on ("schema"))
               (:file "couchdb" :depends-on ("utilities" "configuration"))
               (:file "couchdb-views" :depends-on ("couchdb"))
               (:file "api-user" :depends-on ("schema"))
               (:file "var" :depends-on ("api-user"))
               (:file "api" :depends-on ("couchdb" "geo" "var"))
               (:file "api-doc" :depends-on ("api"))
               (:file "api-client" :depends-on ("api-doc"))
               (:file "api-misc" :depends-on ("api-doc"))
               (:file "gateway-client" :depends-on ("api-client" "api-misc"))
               (:file "stop-words" :depends-on ("globals"))
               (:file "setup-search" :depends-on ("geo" "stop-words"))
               (:file "search-index" :depends-on ("setup-search"))
               (:file "accounting" :depends-on ("couchdb" "gateway-client"))
               (:file "queue" :depends-on ("couchdb"))
               (:file "email-notifier" :depends-on ("queue"))
               (:file "messaging" :depends-on ("api-misc"))
               (:file "api-messaging" :depends-on ("messaging"))
               (:file "customer" :depends-on ("search-index"))
               (:file "customer-preferences" :depends-on ("customer"))
               (:file "bank-account" :depends-on ("customer"))
               (:file "credit-card" :depends-on ("customer"))
               (:file "circles" :depends-on ("customer"))
               (:file "points" :depends-on ("customer" "couchdb-views"))
               (:file "amplification" :depends-on ("customer" "points"))
               (:file "non-profits" :depends-on ("customer" "points" "accounting"))
               (:file "merchant" :depends-on ("var"))
               (:file "linkshare-merchant" :depends-on ("merchant"))
               (:file "location" :depends-on ("merchant"))
               (:file "product" :depends-on ("merchant"))
               (:file "sale" :depends-on ("product"))
               (:file "bookmarklet" :depends-on ("merchant"))
               (:file "potty-mouth" :depends-on ("globals"))
               (:file "product-images" :depends-on ("product"))
               (:file "collectables" :depends-on ("product" "bookmarklet"))
               (:file "offer" :depends-on ("amplification" "collectables"))
               (:file "trending" :depends-on ("offer"))
               (:file "activity-feed" :depends-on ("offer" "couchdb-views"))
               (:file "offer-wall" :depends-on ("offer" "trending"))
               (:file "want-list" :depends-on ("customer-preferences" "offer-wall" "circles"))
               (:file "collections" :depends-on ("want-list"))
               (:file "tracking" :depends-on ("offer" "customer" "product"))
               (:file "hashtag" :depends-on ("product" "potty-mouth" "tracking"))
               (:file "keyword-tag" :depends-on ("product" "potty-mouth" "tracking"))
               (:file "redemption" :depends-on
                      ("couchdb" "api-misc" "merchant" "location" "offer"
                                 "var" "customer"))
               (:file "linkshare" :depends-on ("customer" "offer" "merchant" "redemption"))
               (:file "api-var" :depends-on ("var" "api-messaging" "bank-account"))
               (:file "api-merchant" :depends-on ("merchant" "api-messaging"))
               (:file "recommender" :depends-on ("search-index" "offer-wall"))
               (:file "api-collectable" :depends-on ("api-misc" "collectables" "recommender" "tracking" "hashtag"))
               (:file "api-product" :depends-on ("api-collectable"))
               (:file "api-hashtag" :depends-on ("api" "collections" "api-misc" "api-collectable" "api-customer" "api-collections" "hashtag"))
               (:file "api-keyword-tag" :depends-on ("api" "collections" "api-misc" "api-collectable" "api-customer" "keyword-tag"))
               (:file "api-non-profits" :depends-on ("non-profits"))
               (:file "api-customer"
                      :depends-on ("customer" "api-messaging" "linkshare"
                                              "api-collectable" "want-list" "non-profits"
                                              "customer-preferences" "accounting"
                                              "bank-account" "credit-card"))
               (:file "api-circles" :depends-on ("api-customer" "circles"))
               (:file "api-bookmarklet" :depends-on ("api-customer" "api-merchant" "bookmarklet"))
               (:file "api-want-list" :depends-on ("api-customer" "api-circles"))
               (:file "api-collections" :depends-on ("collections" "api-customer" "api-circles" "api-want-list"))
               (:file "api-feed" :depends-on ("activity-feed" "api-customer" "api-collectable"
                                                              "api-merchant" "api-want-list"))
               (:file "api-track" :depends-on ("tracking" "api-customer"))
               (:file "api-points" :depends-on ("api-track"))
               (:file "api-sale" :depends-on ("api-product" "sale"))
               (:file "product-publisher" :depends-on ("queue" "product"))
               (:file "server" :depends-on
                      ("api-user" "api-var" "api-merchant" "api-customer"
                                  "api-track" "setup-search" "offer-wall"))
               ))
