(defpackage cl-airtable
  (:use :cl)
  (:import-from :access
		#:access)
  (:import-from :arrow-macros
		#:-> #:->>)
  (:import-from :shasht
		#:read-json #:write-json* #:write-json)
  (:import-from :metabang-bind
		#:bind)
  (:import-from :serapeum
		#:dict)
  (:import-from :alexandria
		#:copy-hash-table)
  (:export
   #:airtable
   #:base
   #:table
   #:extract-records
   #:extract-offset
   #:extract-fields
   #:select
   #:async-select
   #:create))

(in-package :cl-airtable)
(named-readtables:in-readtable :interpol-syntax)

(defun extract-records
    (data)
  (access data "records"))

(defun extract-offset
    (data)
  (access data "offset"))

(defun extract-fields
    (records)
  (->> records
    (map 'vector (lambda (rec) (access rec "fields")))))

(defun fetch-airtable-fields
    (url &key bearer-auth (verbose t))
  (-> url
      (dex:get :bearer-auth bearer-auth :verbose verbose)
      read-json
      extract-records
      extract-fields))

(defun get-field
    (fields name)
  (map 'vector (lambda (field) (access field name)) fields))

(defstruct (airtable-struct
            (:constructor airtable (&key key)))
  (key "" :read-only t))

(defstruct base-struct
  (key "" :read-only t)
  (base-id "" :read-only t))

(defstruct table-struct
  (key "" :read-only t)
  (base-id "" :read-only t)
  (table-id-or-name "" :read-only t))

(defun base
    (airtable base-id)
  (make-base-struct :key (airtable-struct-key airtable)
                    :base-id base-id))

(defun table
    (base table-id-or-name)
  (make-table-struct :key (base-struct-key base)
                     :base-id (base-struct-base-id base)
                     :table-id-or-name table-id-or-name))

(defun add-to-content
    (content parameter value)
  "Add parameter and its value to the content of the request"
  (cond ((null value) content)
        ((search parameter (format nil "~A" content)) content)
        (t (append content `((,parameter . ,value))))))

(defun add-max-records (content n)
  (add-to-content content "maxRecords" n))

(defun add-sort (content fields)
  (add-to-content content "sort" fields))

(defun add-fields (content fields)
  (add-to-content content "fields" fields))

(defun add-formula (content formula)
  (add-to-content content "filterByFormula" formula))

(defun add-page-size
    (content page-size)
  (add-to-content content "pageSize" page-size))

(defun add-offset
    (content offset)
  (add-to-content content "offset" offset))

(defun add-view
    (content view)
  (add-to-content content "view" view))

(defun add-cell-format
    (content cell-format)
  (add-to-content content "cellFormat" cell-format))

(defun add-time-zone
    (content time-zone)
  (add-to-content content "timeZone" time-zone))

(defun add-user-locale
    (content user-locale)
  (add-to-content content "userLocale" user-locale))

(defun add-return-fields-by-field-id
    (content return-fields-by-field-id)
  (add-to-content content "returnFieldsByFieldId" return-fields-by-field-id))

(defun add-record-metadata
    (content record-metadata)
  (add-to-content content "recordMetadata" record-metadata))

(defun format-sort-fields
    (xs)
  (map 'vector
       (lambda (x) (list (cons "field" (car x)) (cons "direction" (car (cdr x)))))
       xs))

(defun build-select-content
    (&key fields
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
       record-metadata)
  (write-json*
   (-> nil
       (add-max-records max-records)
       (add-sort sort)
       (add-fields fields)
       (add-formula filter-by-formula)
       (add-page-size page-size)
       (add-offset offset)
       (add-view view)
       (add-time-zone time-zone)
       (add-user-locale user-locale)
       (add-return-fields-by-field-id return-fields-by-field-id)
       (add-record-metadata record-metadata))
   :stream nil
   :alist-as-object t))

(defun send-content
    (url content key)
  (-> url
      (drakma:http-request :method :post
			   :keep-alive nil
			   :close t
			   :content content
			   :content-type "application/json"
			   :additional-headers `(("Authorization" . ,#?"Bearer ${key}")))
      (lambda (body)
	(if (stringp body)
	    body
	    (babel:octets-to-string body)))))

(defun async-send-content (url content key)
  (blackbird:catcher
   (blackbird:multiple-promise-bind
       (body)
       (das:http-request url
			 :method :post
			 :keep-alive nil
			 :close t
			 :content content
			 :content-type "application/json"
			 :additional-headers `(("Authorization" . ,#?"Bearer ${key}")))
     (if (stringp body)
	 body
	 (babel:octets-to-string body)))
   (error (e)
	  (format t "Error in async-send-content: ~a~%" e))))

(defun select
    (table &key
	     fields
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
	     (async nil))
  (bind ((key (table-struct-key table))
	  (base-id (table-struct-base-id table))
	  (table-id-or-name (table-struct-table-id-or-name table))
	  (url #?"https://api.airtable.com/v0/${base-id}/${table-id-or-name}/listRecords")
	  (sort-fields (format-sort-fields sort))
	  (content
	   (build-select-content
	    :fields fields
	    :sort sort-fields
	    :filter-by-formula filter-by-formula
	    :max-records max-records
	    :page-size page-size
	    :offset offset
	    :view view
	    :cell-format cell-format
	    :time-zone time-zone
	    :user-locale user-locale
	    :return-fields-by-field-id return-fields-by-field-id
	    :record-metadata record-metadata)))
    (if async
	;; Send the request async
	(blackbird:catcher
	 (blackbird:alet ((result-string (async-send-content url content key)))
	   (read-json result-string))
	 (error (e) (format t "Error in select: ~a~%" e)))
	;; Send the request sync
	(-> url
	    (send-content content key)
	    (read-json)))))

(defun add-to-create-content
    (content parameter value)
  (bind ((content-copy (copy-hash-table content)))
    (cond ((null value) content)
          (t (setf (gethash parameter content-copy) value)))
    content-copy))

(defun build-create-content
    (&key records return-fields-by-field-id typecast)
  (write-json*
   (-> (dict)
       (add-to-create-content "records"
	(cond ((null records) nil)
	      (t (map 'vector (lambda (x) (dict "fields" x)) records))))
       (add-to-create-content "returnFieldsByFieldId" return-fields-by-field-id)
       (add-to-create-content "typecast" typecast))
   :stream nil))

(defun create
    (table &key records
	     return-fields-by-field-id
	     typecast)
  "Function to insert a record into an airtable table"
  (bind ((key (table-struct-key table))
	  (base-id (table-struct-base-id table))
	  (table-id-or-name (table-struct-table-id-or-name table))
	  (url #?"https://api.airtable.com/v0/${base-id}/${table-id-or-name}")
	  (content (build-create-content
		    :records records
		    :return-fields-by-field-id return-fields-by-field-id
		    :typecast typecast)))
    (dex:post url
	      :bearer-auth key
	      :headers '(("content-type" . "application/json"))
	      :content content)))
