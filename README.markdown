# Cl-Airtable

## Installation

```
cd ~/quicklisp/local-projects/
git clone https://github.com/qubit55/cl-airtable.git .
```

## Usage
```
(ql:quickload '(cl-airtable arrow-macros))
(use-package :cl-airtable)
(:import-from :arrow-macros #:->)

;; Define your airtable base
(defparameter *airtable*
  (-> (airtable :key "airtable-key")
    (base "base-id-or-name"))
    "This is the airtable base.")

;; Define the table
(defparameter *some-table*
  (-> *airtable*
    (table "table-id-or-name"))
  "This table holds some info.")

;; Query *some-table*
(-> *some-table*
   (select :fields #("field-1" "field-2" "field-3")
           :max-records 10
           :sort #(("field-1" "asc") ("field-2" "desc"))
           :filter-by-formula (format nil "FIND(\"~A\" , {field-1})" "abc")
           :page-size 18
           :offset nil))
```

## Author

* Anton Lobach (antonlobach@uri.com)

## Copyright

Copyright (c) 2024 Anton Lobach (antonlobach@uri.com)

## License

Licensed under the MIT License.
