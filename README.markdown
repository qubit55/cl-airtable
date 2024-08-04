# cl-airtable

## Installation

```
cd ~/quicklisp/local-projects/ && git clone https://github.com/qubit55/cl-airtable.git .
```


## Usage
```lisp
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
(defparameter *test-table*
  (-> *airtable*
    (table "table-name-or-id")))

;; Create records https://airtable.com/developers/web/api/create-records
(create
 *test-table*
 :records (vector (dict "field-1" "a"
			"field-2" "abc"
			"field-3" "test@gmail.com"
			"field-4" 1)
		  (dict "field-1" "a"
			"field-2" "abc"
			"field-3" "test@gmail.com"
			"field-4" 1)
		  (dict "field-1" "a"
			"field-2" "abc"
			"field-3" "test@gmail.com"
			"field-4" 1)))

;; List records https://airtable.com/developers/web/api/list-records
(-> *test-table*
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

## API
```lisp
(defun select
  (table &key fields
	      sort
	      filter-by-formula
	      max-records
	      page-size
	      offset
	      view
	      cell-format
	      time-zone
	      user-locale
	      return-fields-by-field-id
	      record-metadata))

(defun create
  (table &key records
	      return-fields-by-field-id
	      typecast)
```

## Author

* Anton Lobach (antonlobach@uri.com)

## Copyright

Copyright (c) 2024 Anton Lobach (antonlobach@uri.com)

## License

Licensed under the MIT License.
