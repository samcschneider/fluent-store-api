(ns catalog-api.core
  (:require [io.pedestal.http :as http]
            [io.pedestal.http.cors :as cors]
            [io.pedestal.http.route :as route]
            [io.pedestal.test :as test]
            [clojure.pprint :as pprint]
            [cheshire.core :refer :all]
            [io.pedestal.http.body-params :as bp]
            [catalog-api.network :as net])
  (:gen-class))

(defn response [status body & {:as headers}]
  {:status status :body body :headers headers})

(def ok       (partial response 200))
(def created  (partial response 201))
(def accepted (partial response 202))

(defonce database (atom {}))

(defn find-site-by-id [dbval db-id]
  (get dbval db-id))

(def db-interceptor
  {:name :database-interceptor
   :enter
         (fn [context]
           (update context :request assoc :database @database))
   :leave
         (fn [context]
           (if-let [[op & args] (:tx-data context)]
             (do
               (apply swap! database op args)
               (assoc-in context [:request :database] @database))
               context
             ))})

(def entity-render
  ;generate-string item ... doesn't seem to eliminate the symbol keys :(
  {:name :entity-render
   :leave
         (fn [context]
           (if-let [item (:result context)]
             (assoc context :response (ok item))
             context))})

(defn make-site [source-site]
  "source-site cannot specify categories or products as they have refs
  Site has a name, currency, language?. Category has id, name, parent.
  Product has an array of parent categories, id, name, sku, parent product,
  url, thumbnail, price"
  (assoc source-site :categories {} :products {} :locations {} :inventory []
                     :name (or (:name source-site) "Unnamed Site")))

(defn key-fn[p] (str (gensym p)))
(defn key-fn2[p] (str p (java.util.UUID/randomUUID)))

(defn db-key [prefix]
  (key-fn prefix))

(def site-create
  {:name :site-create
   :enter
         (fn [context]
           (let [site (get-in context [:request :json-params])
                 db-id    (db-key "s")
                 url      (route/url-for :site-view :params {:site-id db-id})]
             (let [new-site (make-site site)]
             (assoc context
               :response (created new-site "Location" url)
               :tx-data [assoc db-id  new-site]))))})

(def site-view
  {:name :site-view
   :enter
         (fn [context]
           (if-let [db-id (get-in context [:request :path-params :site-id])]
             (if-let [site (find-site-by-id (get-in context [:request :database]) db-id)]
               (assoc context :result site)
               context)
             context))})

(def site-delete
  {:name :site-delete
   :enter
         (fn [context]
           (if-let [db-id (get-in context [:request :path-params :site-id])]
             (if-let [site (find-site-by-id (get-in context [:request :database]) db-id)]
               (assoc context
                 :tx-data [dissoc db-id]
                 :response (accepted {:deleted db-id}))
               context
               )
             context
             )
           )})

(def all-sites-view
  {:name :all-sites-view
   :enter
         (fn [context]
           (let [db (get-in context [:request :database])
                 sites (keys db)
                 sites-list (reduce (fn [all-sites site-id]
                                      (conj all-sites {:id site-id :name (get-in db [site-id :name]) :url (route/url-for :site-view :params {:site-id site-id})})
                                      ) [] sites)
                 ]
             (assoc context :result {:sites sites-list})
             ))
   }
  )

(defn find-category-by-ref [site-id ref]
  (filter #(= (:ref %) ref) (vals (get-in @database [site-id :categories])))
  )

(defn make-category [db category category-id site-id]
  (let [parent (:parent category)]
    (if parent
      (if (empty? (find-category-by-ref site-id parent))
        (assoc (dissoc :parent category) :id category-id) ;TODO throw error instead?
        (assoc category :id category-id)
        )
      (assoc category :id category-id)
      )
    ))

(def category-create
  {:name :category-create
   :enter
         (fn [context]
           (let [category (get-in context [:request :json-params])
                 db (get-in context [:request :database])
                 site-id (get-in context [:request :path-params :site-id])
                 db-id (db-key "c")
                 new-category (make-category db category db-id site-id)
                 url (route/url-for :category-view :params {:category-id db-id})]
             (assoc context
               :response (created new-category "Location" url)
               :tx-data [assoc-in [site-id :categories db-id] new-category])))})

;expecting a structure like this for customer:
; {
;  :name {:first :last}
;  :email "foo@bar.com"
;  :saved-payment {:payment fields ... }
;  :saved-address {:address fields ... }
;  :attributes { :loyalty-level "Platinum" }
; }
(def customer-create
  {:name :customer-create
   :enter
         (fn [context]
           (let [category (get-in context [:request :json-params])
                 db (get-in context [:request :database])
                 site-id (get-in context [:request :path-params :site-id])
                 db-id (db-key "cx")
                 new-customer (assoc category :id db-id)
                 url (route/url-for :customer-view :params {:customer-id db-id})]
             (assoc context
               :response (created new-customer "Location" url)
               :tx-data [assoc-in [site-id :customers db-id] new-customer])))})

(def location-create
  {:name :location-create
   :enter
         (fn [context]
           (let [location (get-in context [:request :json-params])
                 db (get-in context [:request :database])
                 site-id (get-in context [:request :path-params :site-id])
                 db-id (db-key "l")
                 url (route/url-for :location-view :params {:location-id db-id})]
             (assoc context
               :response (created location "Location" url)
               :tx-data [assoc-in [site-id :locations db-id] location])))})

(defn find-category-by-id [db site-id category-id]
  (get-in db [site-id :categories category-id]))

(defn find-location-by-id [db site-id location-id]
  (get-in db [site-id :locations location-id]))

(defn delete-category-by-id [db site-id category-id]
  (dissoc (get-in db [site-id :categories]) category-id)
  )

;TODO rationalize how we access the database... param or direct reference?
(defn find-all-categories [site-id & [db]]
  (vals (get-in (or db @database) [site-id :categories]))
  )

(defn find-all-products [site-id & [db]]
  (vals (get-in (or db @database) [site-id :products]))
  )

(defn find-all-locations [site-id & [db]]
  (vals (get-in (or db @database) [site-id :locations]))
  )

(defn find-all-inventory [site-id & [db]]
  (get-in (or db @database) [site-id :inventory])
  )

(defn find-customer-by-id [db site-id customer-id]
  (get-in db [site-id :customers customer-id]))

(def location-view
  {:name :location-view
   :enter
         (fn [context]
           (if-let [db-id (get-in context [:request :path-params :site-id])]
             (if-let [location-id (get-in context [:request :path-params :location-id])]
               (if-let [location (find-location-by-id (get-in context [:request :database]) db-id location-id)]
                 (assoc context :result location))
               context)
             context))})

(def customer-view
  {:name :customer-view
   :enter
         (fn [context]
           (if-let [site-id (get-in context [:request :path-params :site-id])]
             (if-let [customer-id (get-in context [:request :path-params :customer-id])]
               (if-let [customer (find-customer-by-id (get-in context [:request :database]) site-id customer-id)]
                 (assoc context :result customer))
               context)
             context))})

(def category-delete
  {:name :category-delete
   :enter
         (fn [context]
           (if-let [site-id (get-in context [:request :path-params :site-id])]
             (if-let [category-id (get-in context [:request :path-params :category-id])]
               (if-let [categories (delete-category-by-id (get-in context [:request :database]) site-id category-id )]
               (assoc context
                 :tx-data [assoc-in [site-id :categories] categories]
                 :response (accepted {:deleted category-id}))
               context
               )
             context
             ))
           )})

(defn find-product-by-id [db site-id product-id]
  (get-in db [site-id :products product-id]))

(defn find-product-by-ref [site-id product-ref & [db]]
  (let [source-db (or db @database)]
    (first (filter #(= (:ref %) product-ref) (vals (get-in source-db [site-id :products]))))
    )
  )

(defn delete-product-by-id [db site-id product-id]
  ;Need to remove site -> Products -> product-id
  (let [products (get-in db [site-id :products])]
    (dissoc products product-id)
    )
  )
;TODO function to grab any needed params from context instead of multiple if-let...

(def category-view
  {:name :category-view
   :enter
         (fn [context]
           (if-let [db-id (get-in context [:request :path-params :site-id])]
             (if-let [category-id (get-in context [:request :path-params :category-id])]
               (if-let [category (find-category-by-id (get-in context [:request :database]) db-id category-id)]
               (assoc context :result category))
               context)
             context))})

(def product-create
  {:name :product-create
   :enter
         (fn [context]
           (let [product (get-in context [:request :json-params])
                 product-id    (db-key "p")
                 site-id (get-in context [:request :path-params :site-id])
                 url      (route/url-for :product-view :params {:product-id product-id :site-id site-id})]
             (assoc context
               :response (created product "Location" url)
               :tx-data [assoc-in [site-id :products product-id] product]))
           )
   })

(def product-view
  {:name :product-view
   :enter
         (fn [context]
           (if-let [site-id (get-in context [:request :path-params :site-id])]
             (if-let [product-id (get-in context [:request :path-params :product-id])]
               (if-let [product (find-product-by-id (get-in context [:request :database]) site-id product-id )]
                 (assoc context :result product))
               context)
             context))})

(def product-delete
  {:name :product-delete
   :enter
         (fn [context]
           (if-let [site-id (get-in context [:request :path-params :site-id])]
             (if-let [product-id (get-in context [:request :path-params :product-id])]
               (if-let [products (delete-product-by-id (get-in context [:request :database]) site-id product-id )]
                 (assoc context
                   :response (accepted {:deleted product-id})
                   :tx-data [assoc-in [site-id :products] products]))
               context)
             context))})


(defn db-to-string [database]
  (pr database)
  )

(defn persist-db [location database]
  (with-open [w (clojure.java.io/writer location)] (binding [*out* w] (db-to-string database)))
  )

(def db-checkpoint-create
  {
   :name :db-checkpoint-create
   :enter
         (fn [context]
           (let [{:keys [:location :stream]} (get-in context [:request :json-params])
                 db (get-in context [:request :database])
                 ]
             ;TODO add stream of db if :stream param
             ;Test stream first and stream db if that param is present
             (if location
               (persist-db location db)
               (println "Location not specified - unable to persist db")
               )
             )
           )
   }
  )

;TODO remove need to access db atom directly?
(defn restore-db [location]
  (let [db (with-open [r (java.io.PushbackReader. (clojure.java.io/reader location))] (binding [*read-eval* false] (read r)))]
    (reset! database db)
    )
  )

(def db-restore
  {
   :name :db-restore
   :enter
         (fn [context]
           (let [{:keys [:location] } (get-in context [:request :json-params])
                 ]
             ;TODO allow upload of db
             ;Test stream first and stream db if that param is present
             (if location
               (restore-db location)
               (println "Location not specified - unable to restore db")
               )
             )
           )
   }
  )

(def echo
  {:name :echo
   :enter
         (fn [context]
           (let [request (:request context)
                 response (ok context)]
             (assoc context :response response)))})

(def receive-order
  {
   :name :receive-order
   :enter
         (fn [context]
           (let [order (get-in context [:request :json-params])]
             (println order)
             (assoc context
               :response (ok {:order order})
               :tx-data [assoc-in [:orders] order]))
           )
   })

(def process-ship-compliant
  {
  :name :process-ship-compliant
  :enter
  (fn [context]
    (let [order-details (get-in context [:request :json-params])]
      ; do something with the order details
      (println order-details)
      (assoc context
        :response (ok {:order order-details})
        :tx-data [assoc-in [:ship-compliant] order-details])
      )
    )
  }
  )

(def sample-msg
  {:entitySubtype  "HD" :entityRef 791522
   :rootEntityType "ORDER", :retailerId 1
   :rootEntityRef  791522 :name "CREATE"
   :type           "NORMAL" :id "c8ee6a23-6e12-48a3-addb-4d1ec5940119"
   :flexType       "ORDER::HD" :trailLogs []
   :attributes
                   {:rootEntityRef 791522, :rootEntityType "ORDER"
                    :rootEntityId 81},
   :accountId      "DEMONA" :rootEntityId 81 :entityId 81
   :entityType     "ORDER" :entityStatus "BOOKED" :flexVersion 4}
  )

;need fluent.account DEMONA header
(defn pass-ship-compliant [source msg]

  (let [event (select-keys source[:accountId :entityStatus :retailerId :entityId :entitySubtype :entityType])
        attributes {:message {:compliance-status "passed"
                              :value             msg}}
        account (:accountId source)
        event-data (-> event
                       (assoc :attributes attributes)
                       (assoc :name "COMPLIANCE_SUCCESS")
                       )
        ]
    (println event-data)
    (net/create-event event-data account)
    )
  )



(defn pass-failed-ship-compliant [source msg]
  (let [event (select-keys source [:accountId :retailerId :entityId :entitySubtype :entityType])
        attributes {:message {:compliance-status "passed"
                              :value             msg}}
        account (:accountId source)
        event-data (-> event
                       (assoc :attributes attributes)

                       (assoc :name "COMPLIANCE_ADDRESSED")
                       )
        ]
    (println event-data)
    (net/create-event event-data account)
    )
  )
;NON_COMPLIANT_COMMS


(defn sf-case[source]
  (let [auth "00D6A000002iQem!ARMAQN61URqT7XwDkQka5KJWu8EyXS9JkbS2J6P85y1xS1vdwpgEnsdEyY7XZlTsU_BOqnH6IAYzERZGQOE0Wsy.yPJCTvJY"
        order-id (:rootEntityRef source)
        order-number (:rootEntityId source)
        subject (str "Order " order-id " has Failed")
        description (str "https://demo.sandbox.console.fluentretail.com/#/order/" order-number)
        payload {:Subject subject :Description description :Origin "Fluent" :ContactId "0036A00000QelCRQAZ"
                 :AccountId "0016A00000Ne6qyQAB" :Priority "High" :Type "General" :RecordTypeId "0126A000000EmOU"
                 :EntitlementId "5506A000000d5nyQAA" :OwnerId "00G6A000001rg8o" :Order_Number__c order-id
                 :reason "Order Compliance Issue"
                 }
        ]
    (net/sf-post "https://twewin18.my.salesforce.com/services/data/v41.0/sobjects/Case" payload {} auth)
    )
  )

(defn email-sf [source]
  (let [event (select-keys source [:accountId :entityStatus :retailerId :entityId :entitySubtype :entityType])
        account (:accountId source)
        event-data (-> event

                       (assoc :name "NON_COMPLIANT_COMMS")
                       )
        ]
    (println event-data)
    (net/create-event (-> event-data
                          (assoc :entity-status "NON-COMPLIANT")
                          ) account)
    )
  )

(defn fail-ship-compliant [source msg]
     (let [event (select-keys source [:accountId :entityStatus :retailerId :entityId :entitySubtype :entityType])
           attributes {:message {:name "compliance-message" :value msg }}
           account (:accountId source)
           event-data (-> event
                          (assoc :attributes attributes)
                          (assoc :name "COMPLIANCE_FAILED")
                          )
           ]
       (println event-data)
       (net/create-event event-data account)
       (sf-case source)
       ;(net/create-event (-> event-data
       ;                      (assoc :name "EmailSFSC")
       ;                      (assoc :entity-status "NON-COMPLIANT")
       ;                      ) account)
       )
     )

(def sample-event
  {
   "accountId" "DEMONA"
              "entityStatus" "BOOKED"
   "entitySubtype" "HD"
   "retailerId" "1"
   "name" "COMPLIANCE_SUCCESS"
   "entityType" "ORDER"
   "entityId" "81"
   "attributes"
   {
    "message" {"compliance-status" "passed"
               "value" "Compliance check successful. Shipment OK."
               }
    }
   }
  )

(def routes
  (route/expand-routes
    #{
      ["/site"                    :post   [db-interceptor (bp/body-params) site-create] :route-name :site-create]
      ["/site"                    :get    [entity-render db-interceptor all-sites-view] :route-name :all-sites]
      ["/site/:site-id"           :get    [entity-render db-interceptor site-view] :route-name :site-view]
      ["/site/:site-id"           :delete [entity-render  db-interceptor site-delete] :route-name :site-del]
      ["/site/:site-id/category"  :post   [db-interceptor (bp/body-params) category-create] :route-name :category-create]
      ["/site/:site-id/category/:category-id"  :get   [entity-render db-interceptor category-view] :route-name :category-view]
      ["/site/:site-id/category/:category-id"  :delete   [entity-render db-interceptor category-delete] :route-name :category-delete]
      ["/site/:site-id/location"  :post   [db-interceptor (bp/body-params) location-create] :route-name :location-create]
      ["/site/:site-id/location/:location-id"  :get   [entity-render db-interceptor location-view] :route-name :location-view]
      ["/site/:site-id/product"   :post   [db-interceptor (bp/body-params) product-create] :route-name :product-create]
      ["/site/:site-id/product/:product-id" :get [entity-render db-interceptor product-view] :route-name :product-view]
      ["/site/:site-id/product/:product-id" :delete [entity-render db-interceptor product-delete] :route-name :product-delete]
      ["/db-checkpoint"           :post   [db-interceptor (bp/body-params) db-checkpoint-create] :route-name :db-checkpoint-create]
      ["/db-restore"              :post   [db-interceptor (bp/body-params) db-restore] :route-name :db-restore]
      ["/order"                   :post [db-interceptor (bp/body-params) receive-order] :route-name :receive-order]
      ["/ship-compliant-check"    :post [entity-render db-interceptor (bp/body-params) process-ship-compliant] :route-name :process-ship-compliant]
      ["/site/:site-id/customer"  :post   [db-interceptor (bp/body-params) customer-create] :route-name :customer-create]
      ["/site/:site-id/customer/:customer-id"  :get   [entity-render db-interceptor customer-view] :route-name :customer-view]
      }))

;TODO add all categories view...

(def service-map
  {::http/routes routes
   ::http/type   :jetty
   ::http/allowed-origins ["http://localhost:3449" "chrome-extension://fhbjgbiflinjbdggehcddcbncdddomop"]
   ::http/port   8890})

(defn start [args]
  (http/start (http/create-server (merge service-map args))))

;; For interactive development
(defonce server (atom nil))

(defn start-dev []
  (reset! server
          (http/start (http/create-server
                        (assoc service-map
                          ::http/join? false)))))

(defn stop-dev []
  (http/stop @server))

(defn restart []
  (stop-dev)
  (start-dev))

(defn test-request [verb url]
  (io.pedestal.test/response-for (::http/service-fn @server) verb url))

(defn -main
  [& args]
  (let [argmap (apply hash-map args)]
    (if-let [port (get argmap "-port")]
      (start (merge argmap {::http/port (Integer/parseInt port)}))
      (start argmap)
      )
    )
 )