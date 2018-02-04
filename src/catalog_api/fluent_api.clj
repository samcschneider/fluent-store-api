(ns catalog-api.fluent-api
  (:require
    [catalog-api.network :as net]
    [catalog-api.core :as core]
    [cheshire.core :refer :all]
    )
  )

(defn get-inventory [location-type, location-refs, latitude, longitude, sku-qty, collection-type]
  ;"location-type: STORE, WAREHOUSE"
  ;"collection-type: CUSTOMER"
  ;"location-refs: array of string location refs"
  ;"sku-qty: map of sku + requestedQuantity" - e.g.  {"SKU002" 1000 "SKU001" 200}
  (net/get-inventory {
                  :locationType location-type
                  :collectionType collection-type
                  :locationRefs location-refs
                  :locationGeo {:longitude longitude :latitude latitude}
                  :orderItems (reduce #(conj %1 {:skuRef (first %2) :requestedQty (second %2)}) [] sku-qty)
                  })

  )

(defn find-by-ref [fr-categories ref]

  (first (filter #(= (:categoryRef %) ref) fr-categories))

  )

(defn get-all-categories[]
  ;TODO fix this to page through the results - temp hack
  (get-in (net/get-categories {:start 1 :total 2000}) [:body :results])
  )

(defn sync-categories [site-id retailer-id]

  (let [fr-categories (get-all-categories)

        ; another simple temp hack - order by categories without parents and process
        ; those first - this will obviously have a potential to fail on multi-level cats
        categories (sort-by :parent (core/find-all-categories site-id))
        fluent-id-map (atom {})                             ;ref => fluent id
        ]

    (doseq [cat categories]
      (let [fr-cat (:id (find-by-ref fr-categories (:ref cat)))
            parent-ref (:parent cat)
            fr-parent-ref (when parent-ref (or (:id (find-by-ref fr-categories parent-ref)) (get @fluent-id-map parent-ref)))]
        (println (str "processing... " cat))
        (println (str "parent-ref: " parent-ref " parent: " fr-parent-ref " fr-cat: " fr-cat))

        (let [{:keys [:name :ref]} cat
              category-data (merge {:name name :categoryRef ref :retailerId retailer-id}
                                   (when fr-parent-ref {:parentCategoryId fr-parent-ref}))
              ]
          (println (str "Category data:" category-data))
          (if (nil? fr-cat)
            (do
              (println (str cat " will be created"))
              (let [resp (net/create-category category-data)]
                (println (str "Response from category create: " resp))
                (if-let [fluent-id (get-in resp [:body :id])]
                  (swap! fluent-id-map assoc (:ref cat) fluent-id)
                  )
                ))
            (do
              (println (str cat " will be updated"))
              (let [resp (net/update-category category-data fr-cat)]
                (println (str "Response from category update: " resp))
                )
              )
            )
          )
        )
      )
    )
  )

(defn get-fluent-category-id[category-ref]
  (get-in (net/get-categories {:categoryRef category-ref}) [:body :results 0 :id])
  )

(defn synthetic-ref[product]
  (:ref product))

;TODO fix currency...
(defn make-price[product]
  [{:type "ORIGINAL" :value (:price product) :currency "USD"}]
  )

(defn get-fluent-product-by-ref [fluent-product-ref]
  (println (str "get-fluent-product-by-ref :: product by ref " fluent-product-ref))
  (get-in (net/get-products {:productRef fluent-product-ref}) [:body :results 0])
  )

(defn get-fluent-sku-by-ref [fluent-sku-ref]
  (println (str "get-fluent-sku-by-ref :: sku by ref " fluent-sku-ref))
  (get-in (net/get-skus {:skuRef fluent-sku-ref}) [:body :results 0])
  )

;TODO embed retailer into site config?
(defn sync-products [site-id retailer-id]

  (let [fr-categories (get-all-categories)
        ;partition products into base and "not base" (variant or products with no variants)
        products (group-by #(if (:base %) true false) (core/find-all-products site-id))
        fluent-id-map (atom {})                             ;ref => fluent id
        ]

    (println (str "Syncing " (count (vals products))))

    ;process base products first, then all variants and non-base/variant products
    (doseq [product (concat (get products true) (get products false))]

      (println (str "processing... " product))
      ;if base product -> update a fluent product
      ;if variant, update a sku
      ;if product without variants, update a pseudo-base, update a sku
      (let [synthetic-base (and (not (:base product)) (not (:parent product)))
            fluent-product-ref (:ref product)
            fluent-sku-ref (:sku product)
            is-variant (not (nil? (:parent product)))
            fr-product (if is-variant
                         (get-fluent-sku-by-ref fluent-sku-ref)
                         (get-fluent-product-by-ref fluent-product-ref))
            fr-base-product (when (or is-variant synthetic-base)
                              (get-fluent-product-by-ref (if is-variant (:parent product) (synthetic-ref product))))
            fluent-base-product-id (:productId fr-base-product)
            ;use fluent category ids for products (refs for skus... /sigh)
            categories-by-fluent-id (map #(get-fluent-category-id %) (:supercategories product))
            product-or-sku-template
            {:name       (subs (:name product) 0 (min 40 (count (:name product))))
             :retailerId retailer-id
             :prices      (make-price product)
             :status     "ACTIVE"
             :categories categories-by-fluent-id
             }
            ;use category ids for category links according to docs
            product-data (merge product-or-sku-template
                                {
                                 :productRef fluent-product-ref
                                 })
            ;use category refs for category links according to docs for skus
            sku-data (merge product-or-sku-template
                            ; use parent's category assignment if variant
                            {;:categories (:supercategories (core/find-product-by-ref site-id (:parent product)))
                             ;:categories (map #(get-fluent-category-id %) (:supercategories (core/find-product-by-ref site-id (:parent product))))
                             :skuRef     (or (:sku product) (:ref product));fluent-product-ref
                             ;:attributes [{:name "SIZE_DESC" :value "medium" :type "STRING"}]
                             ;:references [{:type "BARCODE" :value (str (gensym "bar-"))}]
                             :imageUrlRef (:thumbnail product)
                             :productRef (if is-variant (:parent product) fluent-product-ref)})
            ]
        (println (str "Is variant: " is-variant))
        (println (str "Fluent product-ref: " fluent-product-ref))
        (println (str "Fr-product " fr-product))
        (println (str "Fr-base-product " fr-base-product))
        (println (str "Fluent base-product-id " fluent-base-product-id))
        (println (str "Categories by fluent id " categories-by-fluent-id))
        (println (str "Product data: " product-data))
        (println (str "Sku data: " sku-data))
        (println (str "Synthetic base: " synthetic-base))
        (println (str "Product's parent: " (:parent product)))
        (println (str "Parent's super cats: " (core/find-product-by-ref site-id (:parent product))))

    ;    (if is-variant
          ;need to update the fluent sku with the current variant
          (if is-variant
            (if fr-product
              (let [fluent-sku-id (:skuId fr-product)]
                (println (str "Updating SKU: " sku-data " with fluent sku id " fluent-sku-id
                              " and fluent product id " fluent-base-product-id))
                ;use category refs for category links according to docs
                (println "******>> Sku data begin")
                (println (generate-string sku-data))
                (println "******>> Sku data end")
                (net/update-sku sku-data fluent-base-product-id fluent-sku-id)
                )
              (do
                (println (str "Creating SKU: " sku-data " for fluent product id " fluent-base-product-id))
                (net/create-sku sku-data fluent-base-product-id)
                )
              )
            ;synthetic product or actual base product may be created/updated here
            (do
              (if fr-product
                (do
                  (println (str "Updating product: " product-data " with fluent id " (:id fr-product)))
                  ;use category ids for category links according to docs
                  (println "******>> product data begin")
                  (println (generate-string product-data))
                  (println "******>> product data end")
                  (net/update-product product-data (:productId fr-product))
                  )
                (do
                  (println (str "Creating product: " product-data))
                  (let [new-product-id (get-in (net/create-product product-data) [:body :id])]
                    (println (str "New product created: " new-product-id " for ref " (:ref product)))
                    (swap! fluent-id-map assoc (:ref product) new-product-id)
                    )
                  )
                )
              )
            )

          ;after we have created or updated the base product in fluent, need to create
          ;or update a sku if we have a synthetic-base product
          (when synthetic-base
            (println (str "Synthetic base product - create/update corresponding sku"))
            ;the fluent sku is the analog to the non-variant products
            (let [fluent-sku (get-fluent-sku-by-ref (or (:sku product) (:ref product)))
                  fluent-product (get-fluent-product-by-ref fluent-product-ref)
                  fluent-product-id (:productId fluent-product)
                  ;synthetic-sku (merge sku-data
                  ;                     { ;:categories (:supercategories product)
                  ;                      :skuRef (:ref product)})
                  ]
              (if fluent-sku
                ;sku fluent-product-id fluent-sku-id
                (let [fluent-sku-id (:skuId fluent-sku)]
                  (println (str "Updating fluent sku id " fluent-sku-id " for synthetic base: "
                                fluent-product-ref " with product id " fluent-product-id
                                " data: \n " sku-data))
                  (net/update-sku sku-data fluent-product-id fluent-sku-id)
                  )
                (do
                  (println (str "Creating fluent sku for synthetic base: "
                                fluent-product-ref " with product id " fluent-product-id
                                " data: \n " sku-data))
                  (net/create-sku sku-data fluent-product-id)
                  )
                )
              )
            )
          ;)
        )

      )

    )
  )

(defn sync-locations[site-id]
  (let [locations (core/find-all-locations site-id)]
    (doseq [location locations]
      (let [fr-location (get-in (net/get-locations {:query (:locationRef location)}) [:body :results 0])]

        (if fr-location
          (let [fr-location-id (:locationId fr-location)]
            (println (str "Updating location " fr-location-id))
            (net/update-location location fr-location-id)
            )
          (do
            (println (str "Creating location " (:locationRef location)))
            (net/create-location location)
            )
          )
        )
      )
    )
  )

(defn sync-inventory

  ([site-id]
   (sync-inventory site-id (core/find-all-inventory site-id))
    )

  ([site-id inventory]
    ;TODO how to handle retailer id - defaulting it to 1 in places... store in site?

   (let [entities (vec (map #(merge % {:retailerId 1}) inventory))
         job (net/create-job {:name (str (gensym "i-")) :retailerId 1})
         job-id (get-in job [:body :id])
         ]

     (println (str "Created job " job " with id " job-id " and entities " entities))

     (net/create-batch {:action     "UPSERT"
                        :entityType "INVENTORY"
                        :entities   entities} job-id)
     )
    )

  )

(defn generate-random-inventory-rec [sku location cap]
  {:skuRef sku :locationRef location :qty (int (rand cap))}
  )

(defn generate-cartesian-inventory[cap site-id & [location]]
  (for [sku (filter #(not (:base %)) (core/find-all-products site-id))
        location (or [location] (core/find-all-locations site-id))]
    (generate-random-inventory-rec (:sku sku) (:locationRef location) cap))
  )

(defn generate-full-cartesian-inventory[cap site-id]
  (for [sku (filter #(not (:base %)) (core/find-all-products site-id))
        location (core/find-all-locations site-id)]
    (generate-random-inventory-rec (or (:sku sku) (:ref sku)) (:locationRef location) cap))
  )
;{
; "locationType":"STORE",
;               "collectionType":"CUSTOMER",
; "locationRefs":[
;
;                 ],
;               "locationGeo":{
;                              "longitude": -118.248614,
;                                         "latitude": 34.06407
;                              },
; "orderItems":[
;               {
;                "skuRef":"SKU002",
;                        "requestedQty":30000
;                }
;               ]
; }

