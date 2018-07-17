(ns catalog-api.network
  (:require
    [clojure.string :as str]
    [clj-http.client :as http]
    [cheshire.core :refer :all]
    )
  )

(def base-uri "https://sandbox.api.fluentretail.com/")
(def base-api (str base-uri "api/v4.1/"))

(def default-env {:client-id     "xxx"
                  :client-secret "yyy"
                  :username "zzz"
                  :password "aaa"
                  :retailer-id 1})

(def current-env (atom default-env))

;(def token-url (str base-uri "oauth/token"))
(def order-endpoint (str base-api "order"))
(def location-endpoint (str base-api "location"))
(def fulfillment-options-endpoint (str base-api "fulfilmentOptions"))
(def inventory-endpoint (str base-api "inventory"))
(def category-endpoint (str base-api "category"))
(def product-endpoint (str base-api "product"))
(def sku-search-endpoint (str base-api "sku"))
(def job-endpoint (str base-api "job"))
(def event-endpoint (str base-api "event"))
(def create-event-endpoint (str base-api "event/sync"))

(def bearer (atom "<no auth>"))

(defn auth-header[] {"Authorization"  (str "Bearer " @bearer)})

(defn default-config [headers]
  {:headers (merge headers (auth-header)) :with-credentials? false :throw-exceptions false :as :json}
  )

;TODO put in a better logger :)
(defn log [msg]
  (println(str msg))
  )

(defn token-url[env]
  (str "https://sandbox.api.fluentretail.com/oauth/token?username="
       (:username env) "&password="
       (:password env) "&scope=api&client_id="
       (:client-id env) "&client_secret="
       (:client-secret env) "&grant_type=password")
  )

(defn renew-token![]
   (let [response (http/post (token-url @current-env)
                              {:form-params       (merge @current-env {:grant_type "password" :scope "api"})
                               :with-credentials? false
                               :as                :json
                               :throw-exceptions false
                               })
          body (:body response)
          token (:access_token body)]
        (reset! bearer token)
        )
  )

(defn request

  ([request-fn]
   (request request-fn 0)
    )

  ([request-fn tries]
   (let [response (request-fn)]
     (if (and (>= (:status response) 200) (< (:status response) 400)) ;allow 2xx and 3xx response
       response
       (if (or (= (:status response) 401) (= (:status response 503)))
         (do
           (log "Received a non-200 response... renewing token...")
           (let [try-count (inc tries)]
             (if (< try-count 3)
               (do
                 (renew-token!)
                 (request request-fn try-count))
               (do
                 (log (str "Tries exceeded. Error response or unrecoverable status from server." response))
                 response
                 ))
             ))
         (do
           (log (str "Error response or unrecoverable status from server" response))
           response
           )
         )
       )

     )
    )

  )

(defn fr-get
  ([url headers]
   (fr-get url headers {})
    )
  ([url headers params]
   (request (fn []
              (do
                (log (str "GET request to " url))
                (let [config (default-config headers)]
                  (log (str "Using config " config "\n with params " params))
                  (http/get url (merge {:query-params params} config))
                  ))
              ))))

(defn fr-post
  ([url data headers ]
   (fr-post url data headers {})
    )
  ([url data headers params]
   ; find out why this doesn't seem to auto convert map to JSON?? :as :json-string-keys
   (println (str "Posting to url " url " with data: " data))
   (request (fn []
              (http/post url (merge {:query-params params :body (generate-string data)} (default-config (merge headers {"Content-Type" "application/json"}))))))
    )
  )

(defn sf-auth-header[auth] {"Authorization"  (str "Bearer " auth)})

(defn sf-default-config [headers auth]
  {:headers (merge headers (sf-auth-header auth)) :with-credentials? false :throw-exceptions false :as :json}
  )

(defn sf-post [url data headers auth]
 ;  (println (str "Posting to url " url " with data: " data))
   (request (fn []
              (http/post url (merge {:body (generate-string data)} (sf-default-config (merge headers {"Content-Type" "application/json"}) auth)))))
    )

(defn fr-put
  ([url data headers ]
   (fr-put url data headers {})
    )
  ([url data headers params]
    ; find out why this doesn't seem to auto convert map to JSON?? :as :json-string-keys
   (println (str "PUT to url " url " with data: " data))
   (request (fn []
              (http/put url (merge {:query-params params :body (generate-string data)} (default-config (merge headers {"Content-Type" "application/json"}))))))
    )
  )
;?agentStatuses=ACTIVE&count=20 /get request parameters
(def location-get-params {:agentStatuses "ACTIVE" :count 20})

(defn get-orders []
  (fr-get order-endpoint {} )
  )

(defn place-order [order]
  (log (str "Placing order " order))
  (fr-post order-endpoint order {})
  )

;assume skus -> [ { :skuRef "thesku" :requestedQuantity xx } ]
(defn get-fulfillment-options[locationRef items retailerId]
  (log (str "Getting fulfillment options for " locationRef items))
  (fr-post fulfillment-options-endpoint { :limit 10 :locationRef locationRef :items items :retailerId retailerId :orderType "CC"} {})
  )

;fetch locations using default params (grab first 20 ACTIVE locations only)
(defn get-all-locations []
  (fr-get location-endpoint {} location-get-params))

(defn get-locations [options]
  "Options such as query={somelocationref} are support as well as
  {:agentStatuses \"ACTIVE\" :count 20} :start etc.
  retailerId (string, compulsory),\nzoom (integer, optional),\nstart (integer, optional),\ncount (integer, optional),\nsort (string, optional),\nquery (string, optional),\nagentNetwork (array of string, optional),\nagentNetworkTypes (array of string, optional) = 'CP', 'CC', 'HD',\nagentStatuses (array of string, optional) 'ACTIVE', 'INACTIVE',\nsouthWestLat (double, optional),\nsouthWestLon (double, optional),\nnorthEastLat (double, optional),\nnorthEastLon (double, optional)"
  (fr-get location-endpoint {} options)
  )

(defn get-inventory[options]
  (fr-post inventory-endpoint options {})
  )

(defn get-categories
  ([]
   (get-categories {}))
  ([{:keys [:start :count :categoryRef]}]
   (fr-get category-endpoint {}
           (merge {} (when (some? start) {:start start}) (when (some? count) {:count count}) (when (some? categoryRef) {:categoryRef categoryRef})))
    )
  )

(defn get-products
  ([]
   (get-products {}))
  ([{:keys [:start :count :productRef]}]
   (fr-get product-endpoint {}
           (merge {} (when (some? start) {:start start}) (when (some? count) {:count count}) (when (some? productRef) {:productRef productRef})))
    )
  )

(defn get-skus
  ([]
   (get-skus {}))
  ([{:keys [:start :count :skuRef]}]
   (fr-get sku-search-endpoint {}
           (merge {} (when (some? start) {:start start}) (when (some? count) {:count count}) (when (some? skuRef) {:skuRef skuRef})))
    )
  )

(defn create-category[category]
     (fr-post category-endpoint category {})
     )

(defn update-category[category fluent-category-id]
  (fr-put (str category-endpoint "/" fluent-category-id) category {})
  )

(defn create-product[product]
  (fr-post product-endpoint product {})
  )

(defn update-product[product fluent-product-id]
  (fr-put (str product-endpoint "/" fluent-product-id) product {})
  )

(defn create-sku[sku fluent-product-id]
  (fr-post (str product-endpoint "/" fluent-product-id "/sku") sku {})
  )

(defn update-sku[sku fluent-product-id fluent-sku-id]
  (fr-put (str product-endpoint "/" fluent-product-id "/sku/" fluent-sku-id ) sku {})
  )

(defn create-location [location]
  (fr-post location-endpoint location {})
  )

(defn update-location [location fluent-location-id]
  (fr-put (str location-endpoint "/" fluent-location-id) location {})
  )

(defn create-job [job]
  "Requires :name and :retailerId minimally"
  (fr-post job-endpoint job {})
  )

(defn create-batch [batch job-id]
  (fr-post (str job-endpoint "/" job-id "/batch") batch {})
  )

(defn create-event[event fluent-account]
  (fr-post create-event-endpoint event {"fluent.account" fluent-account})
  )

