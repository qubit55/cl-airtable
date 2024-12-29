# 🚀 **cl-airtable**

## 📚 **Overview**

**cl-airtable** is a **Common Lisp** client for interacting with **Airtable**.  

**Current Features:**  
- **List Records**  
- **Create Records**  

**Planned Features:** More functionality will be added in future releases.  

Supports both:  
- ⚡ **Synchronous Requests** *(Blocking Calls)*  
- 🌐 **Asynchronous Requests** *(Non-blocking with Blackbird Promises, ideal for Clack/Wookie setups)*  

---

## 📥 **Installation**

Install `cl-airtable`:

```sh
cd ~/quicklisp/local-projects/ && git clone https://github.com/qubit55/cl-airtable.git
```

Make sure dependencies are loaded before usage.

---

## 💻 **Usage**

### Setup a package for this tutorial.

```lisp
(ql:quickload '(:cl-airtable :clack :ningle :shasht :serapeum :cl-dotenv))

(defpackage cl-airtable-tutorial
  (:use :cl)
  (:import-from :cl-airtable
	        #:airtable
		#:base
		#:table
		#:select
		#:create)
  (:import-from :arrow-macros #:->)
  (:import-from :shasht #:write-json)
  (:import-from :serapeum
		#:dict
		#:toggle-pretty-print-hash-table)
  (:local-nicknames (:bb :blackbird)))

(in-package :cl-airtable-tutorial)
(toggle-pretty-print-hash-table)
```

**Setup Database and Table:**

```lisp
(defparameter *airtable*
  (-> (airtable :key "secret-key")
      (base "base-name-or-id")))

(defparameter *test-table*
  (-> *airtable*
      (table "table-name-or-id")))
```

### 🔒 **Synchronous (Blocking) Requests**

Best for simple operations where blocking behavior is acceptable.

**Create Records:**

```lisp
(-> *test-table*
    (create :records (vector (dict "field-1" "a"
                                   "field-2" "abc"
                                   "field-3" "test@gmail.com"
                                   "field-4" 1))))
```

**Select Records:**

```lisp
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
            :record-metadata #( "commentCount")))
```

---

### 🌐 **Asynchronous (Non-blocking) Requests**

Perfect for web applications or event-driven systems. Set `:async t` to enable non-blocking behavior.

#### **Creating Records (Async)**

```lisp
(defun test-async-create ()
  (create *test-table*
          :async t
          :records (vector (dict "field-1" "a"
                                 "field-2" "abc"
                                 "field-3" "test@gmail.com"
                                 "field-4" 2))))
(as:start-event-loop (lambda () (test-async-create)) :catch-app-errors t)
```

#### **Selecting Records (Async)**

```lisp
(defun test-async-select ()
  (bb:catcher
   (bb:alet* ((response
                 (select *test-table*
                         :fields #("field-1" "field-2" "field-3" "field-4")
                         :async t))
              (response-string (write-json response nil)))
     (print response-string))
   (error (e) (format t "Error in test-async-select: ~a~%" e))))
(as:start-event-loop (lambda () (test-async-select)) :catch-app-errors t)
```

---

## 🛠️ **Integration with Clack, Ningle, and Wookie**

**cl-airtable** seamlessly integrates with web frameworks like **Clack**, **Ningle**, and uses **Wookie** as the backend.


**Web Routes Setup:**

```lisp
(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/") "Welcome to Ningle!!!")
```

**Async Select Route:**

```lisp
(setf (ningle:route *app* "/airtable-async-select" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (lambda (responder)
          (bb:catcher
           (bb:alet* ((records
                        (-> *test-table*
                            (select :fields #("field-1" "field-2" "field-3" "field-4")
                                    :max-records 20
                                    :async t)))
                      (records-string (shasht:write-json records nil)))
             (funcall responder `(200 (:content-type "application/json") (,records-string))))
           (error (e) (format t "Error calling airtable-async-select: ~a~%" e))))))
```

**Start Server:**

```lisp
(defparameter *server*
  (clack:clackup *app* :port 5030 :server :wookie :catch-app-errors t))
;; (clack:stop *server*) ;; To stop the server
```

---

## 📖 **API Reference**

### **List Records**
🔗 [API Documentation](https://airtable.com/developers/web/api/list-records)

```lisp
(defun select (table &key fields sort filter-by-formula max-records page-size
                     offset view cell-format time-zone user-locale
                     return-fields-by-field-id record-metadata (async nil)))
```

### **Create Records**
🔗 [API Documentation](https://airtable.com/developers/web/api/create-records)

```lisp
(defun create (table &key records return-fields-by-field-id typecast (async nil)))
```

---

## 👤 **Author**

- **Anton Lobach** 📧 [antonlobach@uri.com](mailto:antonlobach@uri.com)

---

## 📄 **License**

Licensed under the **MIT License**.  
©️ 2024 **Anton Lobach**
