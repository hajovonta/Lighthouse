(ql:quickload "gtfl")
;(ql:quickload "vecto")

(defpackage :cl-beacon
  (:use :cl :gtfl :hunchentoot :cl-who :ht-simple-ajax))
(in-package :cl-beacon)

(import 'gtfl::*requests*)
(import 'gtfl::*ajax-processor*)

(start-gtfl)

(defparameter max-lines 50)
(defparameter all-lines nil)
(defparameter text-id nil)
(defparameter interface-id nil)
(defparameter cursor 0)
(defparameter cl-b-version "0.1")
(defparameter platform-type (lisp-implementation-type))
(defparameter platform-version (lisp-implementation-version))
(defparameter hunchentoot-version (asdf:component-version
				   (asdf:find-system :hunchentoot)))
(defparameter gtfl-version (asdf:component-version
			    (asdf:find-system :gtfl)))
(defparameter cl-b-symbols (make-hash-table))
(defparameter refresh-interval 200)
(defparameter history-counter 0)
(defparameter list-of-f (make-hash-table :test 'equal))

(defmacro eval-js (&rest expr)
  "evals javascript on client side"
  `(progn (push (who2s (:eval-js (:expr (str (concatenate 'string "
" ,@expr "
"))))) *requests*) nil))

(define-js 'request-handling "
// handles the response from lisp and then polls the next requests after 200 ms
function requestsCallBack (result) {
  for (i=0;i<result.firstChild.firstChild.childNodes.length;i++) 
  {
    var request = document.importNode(result.firstChild.firstChild.childNodes[i],true);
    switch (request.nodeName) 
    {
    case 'reset': case 'RESET':
      var content = document.getElementById('content');
      while (content.firstChild) {
        content.removeChild(content.firstChild);
      }
      break;
    case 'add-element': case 'ADD-ELEMENT':
      while (request.firstChild) {
        document.getElementById('content').appendChild(request.firstChild);
      }
      window.scrollTo(0,100000000);
      break;
    case 'replace-element-content': case 'REPLACE-ELEMENT-CONTENT':
      var id = request.getElementsByTagName('id')[0].firstChild.nodeValue;
      var content = request.getElementsByTagName('content')[0];
      var node = document.getElementById(id);
      while (node.firstChild) { node.removeChild(node.firstChild); }
      while (content.firstChild) { node.appendChild (content.firstChild) };
      break;
    case 'append-to-element': case 'APPEND-TO-ELEMENT':
      var id = request.getElementsByTagName('id')[0].firstChild.nodeValue;
      var content = request.getElementsByTagName('content')[0];
      var node = document.getElementById(id);
      while (content.firstChild) { node.appendChild (content.firstChild) };
      break;
    case 'eval-js': case 'EVAL-JS':
      try {
        var id = eval.apply(window, [request.getElementsByTagName('expr')[0].firstChild.nodeValue]);
      } catch(e) { alert(e); }
      break;
    default:
      alert('unhandled request: ' + request.nodeName);
      break;
    }
  }

  window.setTimeout(getRequests,200);
}
    
// asynchronously polls the content of *requests* on the lisp side
function getRequests () {
  ajax_get_requests(requestsCallBack);
}
")

(defun gtfl-display-lines ()
  "Displays history lines and editor to mimic the lisp REPL"
  (replace-element-content text-id "")
  (loop
    with linecount = (min max-lines (length all-lines))
    for i from (- linecount 1) downto 0 do
      (let ((line (nth i all-lines)))
	(case (first line)
	  (output (append-to-element text-id (html-pprint (second line) :input nil)))
	  (input (append-to-element text-id (html-pprint
					     (concatenate 'string
							  (third line)
							  "> "
							  (second line)) :input t)))
	  (otherwise (append-to-element text-id (str "Destination unknown") (:br))))))
  (append-to-element text-id
                     (:textarea :id "typehere")
                     (:br))
  (eval-js "var editor = CodeMirror.fromTextArea(document.getElementById('typehere'), {autofocus: true, matchBrackets: true});
editor.on('keypress', function(instance, event) {
  enterhandler(instance, event);
});
window.scrollTo(0, document.body.scrollHeight);"))

(defun gtfl-init ()
;  (reset)
  (gtfl-out (:p (:span :id (setf interface-id (make-id-string)))))
  (gtfl-out (:p (:span :id (setf text-id (make-id-string)))))
  (gtfl-display-lines))

(defun gtfl-print (direction text)
  (push (list direction text (package-name *package*)) all-lines)
  (gtfl-display-lines))

(defun cl-b-reset ()
  (setf all-lines nil)
  (setf history-counter 0)
  (clrhash cl-b-symbols)
  (clrhash list-of-f)
  (gtfl-init)
  (gtfl-print 'output (intern (concatenate 'string
					   "*** CL-Beacon "
					   cl-b-version " *** "
					   platform-type " "
					   platform-version
					   " *** Hunchentoot "
					   hunchentoot-version
					   " *** GTFL "
					   gtfl-version " ***"))))

(defun-ajax my-repl (param) (*ajax-processor*)
  (setf history-counter 0)
  (gtfl-print 'input param)
  (gtfl-print 'output
	      (handler-case
		  (progn
		    ;; The following two lines are dangerous in production use
                    (let ((incoming (read-from-string param)))
		      (eval incoming)))
		(error (my-error)
		  my-error))))

(defun-ajax text-input (source param) (*ajax-processor*)
  (funcall (gethash source list-of-f) param))

(defun element-property (id property)
  (concatenate 'string
               "document.getElementById('" id "')." property))

(defun generate-keypress-func (id)
  (concatenate 'string
	       "if (event.keyCode == 13) {ajax_text_input('" id "', " (element-property id "value") ");}"))

(defun gtfl-textbox (id value f-to-execute)
  (append-to-element interface-id (:input :type "text"
					  :id id
					  :onkeypress (generate-keypress-func id)
					  :value value))
  (setf (gethash id list-of-f) f-to-execute)
  t)

(defun gtfl-span (id value)
  (append-to-element interface-id (:span :id id (str value))))

(defun cl-b-canvas (context-name width height)
  (let ((canvas-id (replace-all "-" (make-id-string) "")))
    (append-to-element interface-id (:canvas :id canvas-id :width width :height height))
    (eval-js "var " canvas-id " = document.getElementById('" canvas-id "');")
    (eval-js "var " context-name " = " canvas-id ".getContext('2d');")))
  
;(eval-js "
;function ajax_my_repl(param, callback) {
;  ajax_call('MY_REPL', callback, [param]);
;}
;")

(define-js 'enterhandler "
function enterhandler(obj, evt) {
  if (evt.keyCode == 13) {
    if (evt.ctrlKey) {
      ajax_my_repl(editor.getValue());
      return false;
    }
  }
  if (evt.keyCode == 38) {
    if (evt.ctrlKey) {
      ajax_history_prev();      
      return false;
    }
  }
  if (evt.keyCode == 40) {
    if (evt.ctrlKey) {
      ajax_history_next();
      return false;
    }
  }
}
")

;; following is a handy tool for dynamically loading foreign scripts,
;; one can use it to load KineticJS, NodeJS, AngularJS or other libraries
;; http://stackoverflow.com/questions/950087/how-to-include-a-javascript-file-in-another-javascript-file

(define-js 'loadscript
  "function loadScript(url, callback)
{
  var head = document.getElementsByTagName('head')[0];
  var script = document.createElement('script');
  script.type = 'text/javascript';
  script.src = url;
  script.onreadystatechange = callback;
  script.onload = callback;
  head.appendChild(script);
}")

(defun history-lines ()
  (remove-if-not #'(lambda (x) (eq (first x) 'input)) all-lines))

(defun replace-all (part string replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos))) 

(defun safe-chars (str)
  (let ((out str))
    (setf out (replace-all "'" out "\\'"))
    (setf out (replace-all "
" out "\\n"))
    out))

(defun-ajax history-prev () (*ajax-processor*)
  (let ((h (history-lines)))
    (setf history-counter (min
                           (length h)
                           (incf history-counter)))
    (eval-js "editor.setValue('" (safe-chars
				  (second
				   (nth
				    (- history-counter 1) h))) "');")))

(defun-ajax history-next () (*ajax-processor*)
  (let ((h (history-lines)))
    (setf history-counter (max
			   1
			   (decf history-counter)))
    (eval-js "editor.setValue('" (safe-chars
				  (second
				   (nth
				    (- history-counter 1) h))) "');")))

(push (create-static-file-dispatcher-and-handler
       "/codemirror.js" "c:/lispbox2/codemirror.js")
      *dispatch-table*)

(push (create-static-file-dispatcher-and-handler
       "/commonlisp.js" "c:/lispbox2/commonlisp.js")
      *dispatch-table*)

(push (create-static-file-dispatcher-and-handler
       "/codemirror.css" "c:/lispbox2/codemirror.css")
      *dispatch-table*)

(push (create-static-file-dispatcher-and-handler
       "/matchbrackets.js" "c:/lispbox2/matchbrackets.js")
      *dispatch-table*)

;; redefine this function from gtfl so that it displays Beacon REPL after reset
(defun reset-gtfl ()
  "Clears the content area of the client page and resets things"
  (mapcar #'funcall *reset-functions*)
  (push "<reset> </reset>" *requests*)
  (cl-b-reset)
  nil)

(let ((file "c:/lispbox2/beacon-logo.png")
      (img-id (make-id-string)))
  (defun dynamic-image-init ()
    (append-to-element interface-id (:p (:span :id img-id)))
    (append-to-element img-id (:img :src "/logo.png"))
    (gtfl-textbox (make-id-string) "" #'dynamic-image))
  
  (defun dynamic-image (text)
    (write-on-image file text)
    (replace-element-content img-id (:img :src
					  (concatenate 'string
						       "/logo.png?"
						       (make-id-string))))))

;(defun write-on-image (file text)
;  (vecto:with-canvas (:width 600 :height 50)
;    (vecto:set-rgb-fill 1.0 0.65 0.3)
;    (vecto:rectangle 0 0 600 50)
;    (vecto:fill-path)
;    (vecto:set-rgb-fill 0 0 0)
;    (vecto:set-font (vecto:get-font "times.ttf") 30)
;    (vecto:draw-string 10 10 text)
;    (vecto:save-png file)))

(push (create-static-file-dispatcher-and-handler
       "/logo.png" "c:/lispbox2/beacon-logo.png")
      *dispatch-table*)

(format t "***** started cl-beacon *****~%")
