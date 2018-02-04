(ns catalog-api.core
  (:require [io.pedestal.http :as http]
            [io.pedestal.http.cors :as cors]
            [io.pedestal.http.route :as route]
            [io.pedestal.test :as test]
            [clojure.pprint :as pprint]
            [cheshire.core :refer :all]
            [io.pedestal.http.body-params :as bp]))

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
             )
           )
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
      }))

;TODO add all categories view...

(def service-map
  {::http/routes routes
   ::http/type   :jetty
   ::http/allowed-origins ["http://localhost:3449" "chrome-extension://fhbjgbiflinjbdggehcddcbncdddomop"]
   ::http/port   8890})

(defn start []
  (http/start (http/create-server service-map)))

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

(def temp-cat [
               {:description "Stylish t-shirt that is sure to impress your friends",
                :ref "AP-SH01",
                :name "Fancy t-shirt",
                :variant-selector "ap-001",
                :original 50,
                :supercategories #{"AP-001" "AP-002"},
                :thumbnail "img/shirt-black.png",
                :image "img/shirt-black.png",
                :base true,
                :price 40}
               {:sku "SKU002",
                :image "img/shirt-black.png",
                :thumbnail "img/shirt-black.png",
                :parent "AP-SH01",
                :variant-cats #{"ss1" "cblk1"}}
               {:sku "SKU003",
                :image "img/shirt-black.png",
                :thumbnail "img/shirt-black.png",
                :parent "AP-SH01",
                :variant-cats #{"sm1" "cblk1"}}
               {:sku "SKU004",
                :image "img/shirt-black.png",
                :thumbnail "img/shirt-black.png",
                :parent "AP-SH01",
                :variant-cats #{"sl1" "cblk1"}}
               {:sku "SKU005",
                :image "img/shirt-black.png",
                :thumbnail "img/shirt-black.png",
                :parent "AP-SH01",
                :variant-cats #{"sxl1" "cblk1"}}
               {:sku "SKU006",
                :image "img/shirt-green.png",
                :thumbnail "img/shirt-green.png",
                :parent "AP-SH01",
                :variant-cats #{"cgrn1" "ss1"}}
               {:sku "SKU007",
                :image "img/shirt-green.png",
                :thumbnail "img/shirt-green.png",
                :parent "AP-SH01",
                :variant-cats #{"sm1" "cgrn1"}}
               {:sku "SKU008",
                :image "img/shirt-green.png",
                :thumbnail "img/shirt-green.png",
                :parent "AP-SH01",
                :variant-cats #{"cgrn1" "sl1"}}
               {:sku "SKU009",
                :image "img/shirt-green.png",
                :thumbnail "img/shirt-green.png",
                :parent "AP-SH01",
                :variant-cats #{"sxl1" "cgrn1"}}
               {:parent "AP-SH01",
                :name "Fancy t-shirt (premium dye)",
                :original 90,
                :sku "SKU010",
                :thumbnail "img/shirt-red.png",
                :variant-cats #{"cred1" "ss1"},
                :image "img/shirt-red.png",
                :price 70}
               {:parent "AP-SH01",
                :name "Fancy t-shirt (premium dye)",
                :original 90,
                :sku "SKU011",
                :thumbnail "img/shirt-red.png",
                :variant-cats #{"cred1" "sl1"},
                :image "img/shirt-red.png",
                :price 70}
               {:sku "SKU012",
                :image "img/shirt.png",
                :thumbnail "img/shirt.png",
                :parent "AP-SH01",
                :variant-cats #{"sm1" "cprp1"}}
               {:sku "SKU013",
                :image "img/shirt.png",
                :thumbnail "img/shirt.png",
                :parent "AP-SH01",
                :variant-cats #{"sxl1" "cprp1"}}
               {:description "Tasty Braeburn apple",
                :ref "GP-001",
                :name "Organic Apples",
                :original 2.99,
                :sku "SKU902",
                :thumbnail "img/apple-braeburn.jpg",
                :image "img/apple-braeburn.jpg",
                :price 2}
               {:description
                                  "Featuring an adjustable closure and diamond accent, the Alina Choker Necklace in Neutral Gemstone Mix and 14k Rose Gold shines with stunning semi-precious gemstones. Wear this artful piece to perfect your everyday look or layer with other styles for a simple statement.",
                :ref "JN-001",
                :name
                                  "Alina Choker Necklace In Multi Gemstone Mix And 14k Yellow Gold",
                :variant-selector "ksj-001",
                :supercategories #{"KS-002" "KS-001"},
                :thumbnail
                                  "img/ks/kendra-scott-alina-choker-necklace-in-yellow-gold_00_default_lg.jpg",
                :image
                                  "img/ks/kendra-scott-alina-choker-necklace-in-yellow-gold_00_default_lg.jpg",
                :base true,
                :price 795}
               {:parent "JN-001",
                :ref "JN-001-V1",
                :default true,
                :name
                "Alina Choker Necklace In Multi Gemstone Mix And 14k Yellow Gold",
                :sku "KSJAN001",
                :thumbnail
                "img/ks/kendra-scott-alina-choker-necklace-in-yellow-gold_00_default_lg.jpg",
                :variant-cats #{"msyg"},
                :image
                "img/ks/kendra-scott-alina-choker-necklace-in-yellow-gold_00_default_lg.jpg",
                :price 795}
               {:parent "JN-001",
                :ref "JN-001-V2",
                :name
                        "Alina Choker Necklace In Neutral Gemstone Mix And 14k Rose Gold",
                :sku "KSJAN002",
                :thumbnail
                        "img/ks/kendra-scott-alina-choker-necklace-in-rose-gold_00_default_lg.jpg",
                :variant-cats #{"msrg"},
                :image
                "img/ks/kendra-scott-alina-choker-necklace-in-rose-gold_00_default_lg.jpg",
                :price 750}
               {:description
                                  "A dainty stone and delicate metallic chain combine to create the Elisa Pendant Necklace in Iridescent Drusy, your new favorite wear-anywhere accessory.",
                :ref "JN-002",
                :name "Elisa Pendant Necklace In Iridescent Drusy",
                :variant-selector "ksj-002",
                :supercategories #{"KS-002" "KS-001"},
                :thumbnail "img/ks/iridescent-drusy-842177092088_01_default_lg.jpg",
                :image "img/ks/iridescent-drusy-842177092088_01_default_lg.jpg",
                :base true,
                :price 65}
               {:description
                              "A dainty stone and delicate metallic chain combine to create the Elisa Pendant Necklace in Iridescent Drusy, your new favorite wear-anywhere accessory.",
                :parent "JN-002",
                :ref "JN-002-V1",
                :default true,
                :name "Elisa Pendant Necklace In Iridescent Drusy",
                :sku "KSJEP001",
                :thumbnail "img/ks/iridescent-drusy-842177092088_01_default_lg.jpg",
                :variant-cats #{"jsid"},
                :image "img/ks/iridescent-drusy-842177092088_01_default_lg.jpg",
                :base false,
                :price 65}
               {:description
                              "A dainty stone and delicate metallic chain combine to create the Elisa Pendant Necklace in Plum Drusy, your new favorite wear-anywhere accessory.",
                :parent "JN-002",
                :ref "JN-002-V2",
                :name "Elisa Pendant Necklace In Iridescent Plum",
                :sku "KSJEP002",
                :thumbnail
                              "img/ks/kendra-scott-elisa-gold-pendant-necklace-in-plum-drusy_01_default_lg.jpg",
                :variant-cats #{"jspd"},
                :image
                              "img/ks/kendra-scott-elisa-gold-pendant-necklace-in-plum-drusy_01_default_lg.jpg",
                :base false,
                :price 65}
               {:description
                              "A dainty stone and delicate metallic chain combine to create the Elisa Pendant Necklace in Slate, your new favorite wear-anywhere accessory.",
                :parent "JN-002",
                :ref "JN-002-V3",
                :name "Elisa Pendant Necklace In Slate",
                :sku "KSJEP003",
                :thumbnail "img/ks/slate-842177114513_01_default_lg.jpg",
                :variant-cats #{"jssce"},
                :image "img/ks/slate-842177114513_01_default_lg.jpg",
                :base false,
                :price 50}
               {:description
                              "A dainty stone and delicate metallic chain combine to create the Elisa Pendant Necklace in Cat's Eye Emerald, your new favorite wear-anywhere accessory.",
                :parent "JN-002",
                :ref "JN-002-V4",
                :name "Elisa Pendant Necklace In Cat's Eye Emerald",
                :sku "KSJEP004",
                :thumbnail
                              "img/ks/kendra-scott-elisa-gold-pendant-necklace-in-emerald_01_default_lg.jpg",
                :variant-cats #{"jscee"},
                :image
                              "img/ks/kendra-scott-elisa-gold-pendant-necklace-in-emerald_01_default_lg.jpg",
                :base false,
                :price 60}
               {:description
                              "A dainty stone and delicate metallic chain combine to create the Elisa Pendant Necklace in Bronze Veined Turquoise, your new favorite wear-anywhere accessory.",
                :parent "JN-002",
                :ref "JN-002-V5",
                :name "Elisa Pendant Necklace In Bronze Veined Turquoise",
                :sku "KSJEP005",
                :thumbnail "img/ks/veined-turquoise-842177147900_01_default_lg.jpg",
                :variant-cats #{"jsbvt"},
                :image "img/ks/veined-turquoise-842177147900_01_default_lg.jpg",
                :base false,
                :price 65}
               {:description
                                  "With cascades of crystals and fringed ends, the Arlo Lariat Necklace in Silver is guaranteed to make a statement.",
                :ref "JN-003",
                :name "Arlo Lariat Necklace In Silver",
                :variant-selector "ksj-003",
                :supercategories #{"KS-002" "KS-001"},
                :thumbnail
                                  "img/ks/kendra-scott-arlo-lariat-necklace-in-silver_00_default_lg.jpg",
                :image
                                  "img/ks/kendra-scott-arlo-lariat-necklace-in-silver_00_default_lg.jpg",
                :base true,
                :price 195}
               {:description
                              "With cascades of crystals and fringed ends, the Arlo Lariat Necklace in Silver is guaranteed to make a statement.",
                :parent "JN-003",
                :ref "JN-003-V1",
                :default true,
                :name "Arlo Lariat Necklace In Silver",
                :sku "KSJAL001",
                :thumbnail
                              "img/ks/kendra-scott-arlo-lariat-necklace-in-silver_00_default_lg.jpg",
                :variant-cats #{"ms2s"},
                :image
                              "img/ks/kendra-scott-arlo-lariat-necklace-in-silver_00_default_lg.jpg",
                :base false}
               {:description
                              "With cascades of crystals and fringed ends, the Arlo Lariat Necklace in Gold is guaranteed to make a statement.",
                :parent "JN-003",
                :ref "JN-003-V2",
                :default true,
                :name "Arlo Lariat Necklace In Gold",
                :sku "KSJAL002",
                :thumbnail
                              "img/ks/kendra-scott-arlo-lariat-necklace-in-gold_00_default_lg.jpg",
                :variant-cats #{"ms2g"},
                :image
                              "img/ks/kendra-scott-arlo-lariat-necklace-in-gold_00_default_lg.jpg",
                :base false}
               {:description
                              "With cascades of crystals and fringed ends, the Arlo Lariat Necklace in Rose Gold is guaranteed to make a statement.",
                :parent "JN-003",
                :ref "JN-003-V3",
                :default true,
                :name "Arlo Lariat Necklace In Rose Gold",
                :sku "KSJAL003",
                :thumbnail
                              "img/ks/kendra-scott-arlo-lariat-necklace-in-rose-gold_00_default_lg.jpg",
                :variant-cats #{"ms2rg"},
                :image
                              "img/ks/kendra-scott-arlo-lariat-necklace-in-rose-gold_00_default_lg.jpg",
                :base false}
               {:description
                                 "Modern elegance radiates from the Taylor Stud Earrings in Blue Drusy, a fool-proof accessory to elevate any look.",
                :ref "JE-001",
                :name "Taylor Stud Earrings In Blue Drusy",
                :supercategories #{"KS-003" "KS-001"},
                :sku "KSJTS001",
                :thumbnail
                                 "img/ks/kendra-scott-taylor-gold-stud-earrings-in-blue-drusy_00_default_lg.jpg",
                :image
                                 "img/ks/kendra-scott-taylor-gold-stud-earrings-in-blue-drusy_00_default_lg.jpg",
                :price 80}
               {:description
                                  "For a stunning update to a simple silhouette, try the Alice Statement Earrings in Gold for its elongated shape and sophisticated studs.",
                :ref "JE-002",
                :name "Alice Statement Earrings In Gold",
                :variant-selector "ksj-003",
                :supercategories #{"KS-003" "KS-001"},
                :thumbnail
                                  "img/ks/kendra-scott-alice-statement-earrings-in-gold_00_default_lg.jpg",
                :image
                                  "img/ks/kendra-scott-alice-statement-earrings-in-gold_00_default_lg.jpg",
                :base true,
                :price 85}
               {:description
                              "For a stunning update to a simple silhouette, try the Alice Statement Earrings in Antique Silver for its elongated shape and sophisticated studs.",
                :parent "JE-002",
                :ref "JE-002-V1",
                :default true,
                :name "Alice Statement Earrings In Antique Silver",
                :sku "KSJAS001",
                :thumbnail
                              "img/ks/kendra-scott-alice-statement-earrings-in-antique-silver_00_default_lg.jpg",
                :variant-cats #{"ms2s"},
                :image
                              "img/ks/kendra-scott-alice-statement-earrings-in-antique-silver_00_default_lg.jpg",
                :base false}
               {:description
                              "For a stunning update to a simple silhouette, try the Alice Statement Earrings in Gold for its elongated shape and sophisticated studs.",
                :parent "JE-002",
                :ref "JE-002-V2",
                :name "Alice Statement Earrings In Gold",
                :sku "KSJAS002",
                :thumbnail
                              "img/ks/kendra-scott-alice-statement-earrings-in-gold_00_default_lg.jpg",
                :variant-cats #{"ms2g"},
                :image
                              "img/ks/kendra-scott-alice-statement-earrings-in-gold_00_default_lg.jpg",
                :base false}
               {:description
                              "For a stunning update to a simple silhouette, try the Alice Statement Earrings in Rose Gold for its elongated shape and sophisticated studs.",
                :parent "JE-002",
                :ref "JE-002-V3",
                :name "Alice Statement Earrings In Rose Gold",
                :sku "KSJAS003",
                :thumbnail
                              "img/ks/kendra-scott-alice-statement-earrings-in-rose-gold_00_default_lg.jpg",
                :variant-cats #{"ms2rg"},
                :image
                              "img/ks/kendra-scott-alice-statement-earrings-in-rose-gold_00_default_lg.jpg",
                :base false}
               {:description
                                  "Cascades of stunning crystals make the Olympia Statement Earrings in Gold your shimmering staple for every occasion.",
                :ref "JE-003",
                :name "Olympia Statement Earrings In Gold",
                :variant-selector "ksj-003",
                :supercategories #{"KS-003" "KS-001"},
                :thumbnail
                                  "img/ks/kendra-scott-olympia-tassel-statement-earrings-in-gold_00_default_lg.jpg",
                :image
                                  "img/ks/kendra-scott-olympia-tassel-statement-earrings-in-gold_00_default_lg.jpg",
                :base true,
                :price 150}
               {:description
                              "Cascades of stunning crystals make the Olympia Statement Earrings in Gold your shimmering staple for every occasion.",
                :parent "JE-003",
                :ref "JE-003-V1",
                :default true,
                :name "Olympia Statement Earrings In Gold",
                :sku "KSJOS001",
                :thumbnail
                              "img/ks/kendra-scott-olympia-tassel-statement-earrings-in-gold_00_default_lg.jpg",
                :variant-cats #{"ms2g"},
                :image
                              "img/ks/kendra-scott-olympia-tassel-statement-earrings-in-gold_00_default_lg.jpg",
                :base false}
               {:description
                              "Cascades of stunning crystals make the Olympia Statement Earrings in Silver your shimmering staple for every occasion.",
                :parent "JE-003",
                :ref "JE-003-V2",
                :name "Olympia Statement Earrings In Silver",
                :sku "KSJOS002",
                :thumbnail
                              "img/ks/kendra-scott-olympia-tassel-statement-earrings-in-silver_00_default_lg.jpg",
                :variant-cats #{"ms2s"},
                :image
                              "img/ks/kendra-scott-olympia-tassel-statement-earrings-in-silver_00_default_lg.jpg",
                :base false}
               {:description
                              "Cascades of stunning crystals make the Olympia Statement Earrings in Rose Gold your shimmering staple for every occasion.",
                :parent "JE-003",
                :name "Olympia Statement Earrings In Rose Gold",
                :rev "JE-003-V3",
                :sku "KSJOS003",
                :thumbnail
                              "img/ks/kendra-scott-olympia-tassel-statement-earrings-in-rose-gold_00_default_lg.jpg",
                :variant-cats #{"ms2rg"},
                :image
                              "img/ks/kendra-scott-olympia-tassel-statement-earrings-in-rose-gold_00_default_lg.jpg",
                :base false}
               {:description
                                  "The romantic gold details and delicate stones featured in our Deb Adjustable Bracelet add visual interest and classic shine to your favorite bracelet stack.",
                :ref "JB-001",
                :name "Deb Adjustable Chain Bracelet In Gold",
                :variant-selector "ksj-003",
                :supercategories #{"KS-004" "KS-001"},
                :thumbnail "img/ks/deb_gold_842177153659_00_default_lg.jpg",
                :image "img/ks/deb_gold_842177153659_00_default_lg.jpg",
                :base true,
                :price 60}
               {:description
                              "The romantic gold details and delicate stones featured in our Deb Adjustable Bracelet add visual interest and classic shine to your favorite bracelet stack.",
                :parent "JB-001",
                :ref "JB-001-V1",
                :default true,
                :name "Deb Adjustable Chain Bracelet In Gold",
                :sku "KSJDB001",
                :thumbnail "img/ks/deb_gold_842177153659_00_default_lg.jpg",
                :variant-cats #{"ms2g"},
                :image "img/ks/deb_gold_842177153659_00_default_lg.jpg",
                :base false}
               {:description
                              "The romantic silver details and delicate stones featured in our Deb Adjustable Bracelet add visual interest and classic shine to your favorite bracelet stack.",
                :parent "JB-001",
                :ref "JB-001-V2",
                :name "Deb Adjustable Chain Bracelet In Silver",
                :sku "KSJDB002",
                :thumbnail "img/ks/deb_silver_842177153673_00_default_lg.jpg",
                :variant-cats #{"ms2s"},
                :image "img/ks/deb_silver_842177153673_00_default_lg.jpg",
                :base false}
               {:description
                              "The romantic rose gold details and delicate stones featured in our Deb Adjustable Bracelet add visual interest and classic shine to your favorite bracelet stack.",
                :parent "JB-001",
                :ref "JB-001-V3",
                :name "Deb Adjustable Chain Bracelet In Rose Gold",
                :sku "KSJDB003",
                :thumbnail "img/ks/deb_rose_gold_842177153666_00_default_lg.jpg",
                :variant-cats #{"ms2rg"},
                :image "img/ks/deb_rose_gold_842177153666_00_default_lg.jpg",
                :base false}
               {:description
                                 "Two dainty stones bookend the Teddy Pinch Bracelet in Blue Drusy for an elegant addition to any stack.",
                :ref "JB-002",
                :name "Teddy Pinch Bracelet In Blue Drusy",
                :supercategories #{"KS-004" "KS-001"},
                :sku "KSJTB001",
                :thumbnail
                                 "img/ks/kendra-scott-teddy-gold-pinch-bracelet-in-blue-drusy_00_default_lg.jpg",
                :image
                                 "img/ks/kendra-scott-teddy-gold-pinch-bracelet-in-blue-drusy_00_default_lg.jpg",
                :price 80}
               {:description
                                  "Adjust to a size that suits you with our Delphine Pinch Bracelet Set in Gold, featuring crown inspired details and stunning white CZ accents.",
                :ref "JB-003",
                :name "Delphine Pinch Bracelet Set In Gold",
                :variant-selector "ksj-003",
                :supercategories #{"KS-004" "KS-001"},
                :thumbnail
                                  "img/ks/kendra-scott-delphine-gold-pinch-bracelet-set_00_default_lg.jpg",
                :image
                                  "img/ks/kendra-scott-delphine-gold-pinch-bracelet-set_00_default_lg.jpg",
                :base true,
                :price 120}
               {:description
                              "Adjust to a size that suits you with our Delphine Pinch Bracelet Set in Gold, featuring crown inspired details and stunning white CZ accents.",
                :parent "JB-003",
                :ref "JB-003-V1",
                :default true,
                :name "Deb Adjustable Chain Bracelet In Gold",
                :sku "KSJDP001",
                :thumbnail
                              "img/ks/kendra-scott-delphine-gold-pinch-bracelet-set_00_default_lg.jpg",
                :variant-cats #{"ms2g"},
                :image
                              "img/ks/kendra-scott-delphine-gold-pinch-bracelet-set_00_default_lg.jpg",
                :base false}
               {:description
                              "Adjust to a size that suits you with our Delphine Pinch Bracelet Set in Antique Sliver, featuring crown inspired details and stunning white CZ accents.",
                :parent "JB-003",
                :ref "JB-003-V2",
                :name "Delphine Pinch Bracelet Set In Antique Silver",
                :sku "KSJDP002",
                :thumbnail
                              "img/ks/kendra-scott-delphine-antique-silver-pinch-bracelet-set_00_default_lg.jpg",
                :variant-cats #{"ms2s"},
                :image
                              "img/ks/kendra-scott-delphine-antique-silver-pinch-bracelet-set_00_default_lg.jpg",
                :base false}
               {:description
                              "Adjust to a size that suits you with our Delphine Pinch Bracelet Set in Rose Gold, featuring crown inspired details and stunning white CZ accents.",
                :parent "JB-003",
                :ref "JB-003-V3",
                :name "Delphine Pinch Bracelet Set In Rose Gold",
                :sku "KSJDP003",
                :thumbnail
                              "img/ks/kendra-scott-delphine-rose-gold-pinch-bracelet-set_00_default_lg.jpg",
                :variant-cats #{"ms2rg"},
                :image
                              "img/ks/kendra-scott-delphine-rose-gold-pinch-bracelet-set_00_default_lg.jpg",
                :base false}
               ]
  )