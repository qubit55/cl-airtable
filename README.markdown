# cl-airtable

## Installation

```
cd ~/quicklisp/local-projects/
git clone https://github.com/qubit55/cl-airtable.git .
```

## Usage
```
(ql:quickload '(:cl-airtable
		:serapeum
		:arrow-macros
		:shasht))

(defpackage :airtable-test
  (:use :cl :cl-airtable)
  (:import-from :serapeum
		#:dict
		#:toggle-pretty-print-hash-table)
  (:import-from :arrow-macros
		#:->)
  (:import-from :shasht
		#:read-json))

(in-package :airtable-test)

(toggle-pretty-print-hash-table)

;; Database
(defparameter *airtable*
  (-> (airtable :key "secret-key")
      (base "base-name-or-id")))

;; Table 
(defparameter *select-test*
  (-> *airtable*
    (table "table-name-or-id")))

;; All parameters except the table are optional
(-> *select-test*
   (select :fields #("field-1" "field-2" "field-3" "field-4")
      	   :max-records 20
	   :sort #(("field-4" "asc"))
	   :filter-by-formula (format nil "FIND(\"~A\" , {field-2})" "abc")
	   :page-size 18
	   :offset nil
	   :cell-format "string"
	   :time-zone "America/Indiana/Knox"
	   :user-locale "en-gb"
	   :return-fields-by-field-id t
	   :record-metadata #("commentCount"))
	   (read-json))

```

## Author

* Anton Lobach (antonlobach@uri.com)

## Copyright

Copyright (c) 2024 Anton Lobach (antonlobach@uri.com)

## License

Licensed under the MIT License.
