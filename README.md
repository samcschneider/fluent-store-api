# Fluent Store Catalog API

Provides settings for the Fluent Store allowing for the maintenance of multiple
product catalogs.

## Usage

Optional backend server for the Fluent Store sample:
https://github.com/samcschneider/fluent-store

In order to call the fluent APIs you will need to modify:
/src/catalog_api/network.clj
  -> default-env :client-id     "{{your client id}}"
                       :client-secret "{{your client secret}}"
                       :username "{{your api user name}}"
                       :password "{{your api password}}"})

## License

Copyright © 2017

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
