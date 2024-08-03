(defpackage cl-airtable
  (:use :cl)
  (:import-from :access
		#:access)
  (:import-from :arrow-macros
		#:-> #:->>)
  (:import-from :shasht
		#:read-json #:write-json*)
  (:import-from :metabang-bind
		#:bind)
  (:import-from :serapeum
		#:dict)
  (:export
   #:airtable
   #:base
   #:table
   #:extract-records
   #:extract-offset
   #:extract-fields
   #:select
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
  (cond ((null n) content)
        ((search parameter (format nil "~A" content)) content)
        (t (append content `((,parameter . ,n))))))

(defun add-max-records (content n)
  (add-to-content content "maxRecords" n))
  ;; (cond ((null n) content)
  ;;       ((search "maxRecords" (format nil "~A" content)) content)
  ;;       (t (append content `(("maxRecords" . ,n))))))

(defun add-sort (content fields)
  (add-to-content content "sort" fields))
  ;; (cond ((null fields) content)
  ;;       ((search "sort" (format nil "~A" content)) content)
  ;;       (t (append content `(("sort" . ,fields))))))

(defun add-fields (content fields)
  (add-to-content content "fields" fields))
  ;; (cond ((null fields) content)
  ;;       ((search "fields" (format nil "~A" content)) content)
  ;;       (t (append content `(("fields" . ,fields))))))

(defun add-formula (content formula)
  (add-to-content content "filterByFormula" formula))
  ;; (cond ((null formula) content)
  ;;       ((search "filterByFormula" (format nil "~A" content)) content)
  ;;       (t (append content `(("filterByFormula" . ,formula))))))

(defun add-page-size
    (content page-size)
  (add-to-content content "pageSize" page-size))
  ;; (cond ((null page-size) content)
  ;;       ((search "pageSize" (format nil "~A" content)) content)
  ;;       (t (append content `(("pageSize" . ,page-size))))))

(defun add-offset
    (content offset)
  (add-to-content content "offset" offset))
  ;; (cond ((null offset) content)
  ;;       ((search "offset" (format nil "~A" content)) content)
;;       (t (append content `(("offset" . ,offset))))))

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

(defun format-sort-fields
    (xs)
  (map 'vector
       (lambda (x) (list (cons "field" (car x)) (cons "direction" (car (cdr x)))))
       xs))

(defun select
    (table
     &key
       fields
       sort
       filter-by-formula
       max-records
       page-size
       offset
       view
       cell-format
       time-zone
       user-locale)
  (bind ((key (table-struct-key table))
         (base-id (table-struct-base-id table))
         (table-id-or-name (table-struct-table-id-or-name table))
         (url #?"https://api.airtable.com/v0/${base-id}/${table-id-or-name}/listRecords")
         (sort-fields (format-sort-fields sort))
         (content
          (-> nil
              (add-max-records max-records)
              (add-sort sort-fields)
              (add-fields fields)
              (add-formula filter-by-formula)
              (add-page-size page-size)
              (add-offset offset)
	      (add-view view)
	      (add-time-zone time-zone)
	      (add-user-locale user-locale))))
    (dex:post url
	      :bearer-auth key
	      :headers '(("content-type" . "application/json"))
	      :content (write-json* content
				    :stream nil
				    :alist-as-object t))))

(defun create
    (table &key records)
  "Function to insert a record into an airtable table"
  (bind ((key (table-struct-key table))
         (base-id (table-struct-base-id table))
         (table-id-or-name (table-struct-table-id-or-name table))
         (url #?"https://api.airtable.com/v0/${base-id}/${table-id-or-name}")
	 (content
	  (write-json*
	   (dict "records"
		 (map 'vector (lambda (x) (dict "fields" x)) records))
	   :stream nil)))
    (dex:post url
	      :bearer-auth key
	      :headers '(("content-type" . "application/json"))
	      :content content)))

