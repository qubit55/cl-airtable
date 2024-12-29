# cl-airtable

## Installation

```
cd ~/quicklisp/local-projects/ && git clone https://github.com/qubit55/cl-airtable.git
```

## Summary
cl-airtable is a Common Lisp client for working with airtable. Currently, it only supports listing and creation of records. More functionality is planned to be added later.

The client supports synchronous requests. Asyncronous non-blocking requests based on the Blackbird promises are also supported so the client can be used with Clack with the Wookie backend (see below for an example).

## Usage

### Synchronous - blocking requests
```lisp
(ql:quickload '(:cl-airtable
		:serapeum
		:arrow-macros))

(defpackage :airtable-test
  (:use :cl :cl-airtable)
  (:import-from :serapeum
		#:dict
		#:toggle-pretty-print-hash-table)
  (:import-from :arrow-macros
		#:->))

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

;; Creating records 
(-> *test-table*
   (create :records (vector (dict "field-1" "a"
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
			          "field-4" 1))))

;; Selecting records
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

### Asyncronous - non-blocking requests
For async requests all you need is to set the async flag to t. 

#### Creating records
```lisp
(ql:quickload '(:cl-airtable
		:blackbird
		:cl-async))
```

```lisp
(defun test-async-create
    ()
  (create
   *test-table*
   :async t
   :records (vector (dict "field-1" "a"
			  "field-2" "abc"
			  "field-3" "test@gmail.com"
			  "field-4" 2)
		    (dict "field-1" "a"
			  "field-2" "abc"
			  "field-3" "test@gmail.com"
			  "field-4" 2)
		    (dict "field-1" "a"
			  "field-2" "abc"
			  "field-3" "test@gmail.com"
			  "field-4" 2))))

(as:start-event-loop (lambda () (test-async-create)) :catch-app-errors t)
```

#### Selecting records
```lisp
(defun test-async-select
    ()
  (blackbird:catcher
   (blackbird:alet* ((response
		      (select *test-table*
			:fields #("field-1" "field-2" "field-3" "field-4")
			:async t))
		     (response-string (write-json response nil)))
     (print response-string))
   (error (e) (format t "Error in test-async-select: ~a~%" e))))

(as:start-event-loop (lambda () (test-async-select)) :catch-app-errors t)
```

## Ussage with Clack, Ningle and Wookie
Below is an example of using async requests with Clack.

```lisp
(ql:quickload '(:cl-airtable
		:blackbird
		:cl-async
		:clack
		:ningle))
		
(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/")
      "Welcome to ningle!!!")

;; (setf (ningle:route *app* "/test" :method :GET)
;;       #'(lambda (params)
;; 	  (lambda (responder)
;; 	    (blackbird:alet* ((body (drakma-async:http-request "http://google.com"))
;; 			      (body-capitalized (string-upcase body)))
;; 	      (funcall responder `(200 (:content-type "text/html")
;; 				       (,body-capitalized)))))))

(setf (ningle:route *app* "/airtable-async-select" :method :GET)
      #'(lambda (params)
	  (declare (ignore params))
	  (lambda (responder)
	    (blackbird:catcher
	     (blackbird:alet* ((records
				(-> *test-table*
				    (select :fields #("field-1" "field-2" "field-3" "field-4")
				      :max-records 20
				      :async t)
				    ))
			       (records-string (write-json records nil)))
	       (print "Called /airtable-async-select")
	       (funcall responder `(200 (:content-type "application/json")
					(,records-string))))
	     (error (e) (format t ""Error calling airtable-async-select ~a~%" e))))))

(setf (ningle:route *app* "/airtable-async-create" :method :GET)
      #'(lambda (params)
	  (declare (ignore params))
	  (lambda (responder)
	    (blackbird:catcher
	     (blackbird:alet* ((response
				(create
				 *test-table*
				 :async t
				 :records (vector (dict "field-1" "a"
							"field-2" "abc"
							"field-3" "test@gmail.com"
							"field-4" 2)
						  (dict "field-1" "a"
							"field-2" "abc"
							"field-3" "test@gmail.com"
							"field-4" 2)
						  (dict "field-1" "a"
							"field-2" "abc"
							"field-3" "test@gmail.com"
							"field-4" 2))))
			       (response-str (write-json response nil)))
	       (print "Called /airtable-async-create")
	       (funcall responder `(200 (:content-type "application/json")
					(,response-str))))
	     (error (e) (format t "Error calling airtable-async-create: ~a~%" e))))))

(defparameter *server*
  (clack:clackup *app* :port 5030
		       :server :wookie
		       :catch-app-errors t)))

(clack:stop *server*)
```

## API
### List records
https://airtable.com/developers/web/api/list-records
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
	      record-metadata
	      (async nil)))
```
When async is set to t, returns blackbird promise otherwise serapeum dict.

### Create records
https://airtable.com/developers/web/api/create-records
```lisp
(defun create
  (table &key records
	      return-fields-by-field-id
	      typecast
	      (async nil))	      
```
When async is set to t, returns blackbird promise otherwise serapeum dict.


## Author

* Anton Lobach (antonlobach@uri.com)

## Copyright

Copyright (c) 2024 Anton Lobach (antonlobach@uri.com)

## License

Licensed under the MIT License.
