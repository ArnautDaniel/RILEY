;;;Latex constants---------------------------------------------------

(defparameter header-name "\\dotfill \\textsc{Ordered by:} ")
(defparameter header-set "\\dotfill \\textsc{Set:} ")
(defparameter header-show "\\dotfill \\textsc{Show:} ")
(defparameter date-header "\\\\ {\\bf Pickup Date:} \\today \\dotfill")
(defparameter rental-period "{\\bf {Rental Period:}")
(defparameter begin-table "\\begin{invoiceTable}")
(defparameter end-table "\\end{invoiceTable}")
(defparameter unitrow "\\unitrow{")
(defparameter begin-img-table "\\figureSeriesFloat{}{")
(defparameter end-img-table "}")
(defparameter end-document " \\end{document}")
(defparameter begin-document "\\begin{document}")
;;;Takes an invoice and root-dir
(defparameter tail-conf "
\\par
{\\scriptsize \\begin{singlespace} It is agreed that \\textsc{Lessee} assumes all liability and responsibility for the item(s) listed above.  When item(s) have been accepted by \\textsc{Lessee} (as evidenced by \\textsc{Lessee’s} signature below), and while in the custody and possession of \\textsc{Lessee}, its employees, agents, owners, directors and officers and any other person or entity to whom the item(s) is entrusted by \\textsc{Lessee}, or assigns.  Liability extends to the full replacement cost or repair, in Lessor’s sole discretion.  Further, Lessee assumes full responsibility for any liability arising because of the use of the item(s) during the term of the lease and until item(s) is returned to the custody and control of Lessor (as evidenced by the written acknowledgement by Lessor). Further, to the extent permitted by law, the \\textsc{Lessee} agrees to protect, indemnify, defend and hold harmless Lessor, its directors, officers, agents, shareholders, and employees, against all claims or damages to people or property and costs (including reasonable attorney’s fees), up to Lessor’s pro-rata share of the item(s), arising out of or connected with the operation of \\textsc{Lessee’s} activities with respect to the item(s), including damage caused by inadequate maintenance of the item(s), use of the item(s), or a part thereof, by any customer, any guest, invitee, or by any agent of the \\textsc{Lessee}. Replacement value of all items (unless otherwise noted) equals (10X) first week rate. \\end{singlespace}}

{\\color{red} \\textsc{Signature}}\\hspace{0.5cm} \\makebox[3in]{\\hrulefill} \\hspace{0.5cm} \\textsc{Date}\\hspace{0.5cm} \\makebox[1in]{\\hrulefill} \\\\
\\textsc{Print}\\hspace{1.25cm} \\makebox[3in]{\\hrulefill}
  ")

(defparameter document-conf "\\documentclass{invoice} % Use the custom invoice class (invoice.cls)

\\def \\tab {\\hspace*{3ex}} % Define \\tab to create some horizontal white space

\\usepackage{color}
\\usepackage{courier}
\\usepackage{setspace}
\\usepackage{graphicx}
\\usepackage{pgffor}
\\usepackage{caption}
\\usepackage{expl3}
")

(defparameter heading-conf "
\\hfil{\\huge\\color{red}{\\textsc{Checkout Sheet}}}\\hfil
% \\bigskip\\break % Whitespace
\\break
\\hrule % Horizontal line

000 E Atlanta Drive \\hfill \\emph{Mobile:} (000) 000-0000 \\\\
Ste. 000 \\hfill{ \\emph{Office:} (000) 000-0000} \\\\
% Your address and contact information
Norfolk, Georgia 00000 \\hfill anon@anon.com
\\\\ \\\\
{\\bf Invoice To:} \\\\ ")

;;;Latex Functions----------------------------------------------
(defun generate-invoice-single-header (invoice func)
  (concatenate 'string  func invoice " "))

(defun generate-latex (invoice root-dir)
  (ensure-directories-exist (concatenate 'string root-dir
					 "pdf/"))
  (let* ((pdfname (concatenate 'string "pdf/" (directory-safe (show-name invoice))
			       "-" (directory-safe (invoice-set-name invoice))
			       ".tex"))
	 (ironic-tex-name (concatenate 'string "pdf/" (directory-safe (show-name invoice))
				       "-" (directory-safe (invoice-set-name invoice))
				       ".pdf"))
	 (complete-stream (concatenate 'string root-dir
				       pdfname))
	 (completer-stream (concatenate 'string root-dir
					ironic-tex-name)))
    (with-open-file (s complete-stream :direction :output
		       :if-exists :supersede)
      
      (princ document-conf s)
      (princ (concatenate 'string
			  "\\graphicspath{ {"
			  (invoice-root-dir invoice)
			  "webimg/}}") s)
      (princ begin-document s)
      (fresh-line s)
      (princ heading-conf s)
      (princ (generate-invoice-single-header
	      (show-name invoice) header-show) s)
      (princ (generate-invoice-single-header
	      (invoice-set-name invoice) header-set) s)
      (princ (generate-invoice-single-header
	      (invoice-contact-name invoice) header-name) s)
      
      (fresh-line s)
      (fresh-line s)
      
      (princ date-header s)
      (princ rental-period s)
      (fresh-line s)
      (princ begin-table s)
      (fresh-line s)
      (mapc (lambda (b) (princ (format-description b) s)
		    (fresh-line s)) (invoice-item-list invoice))
      (princ end-table s)
      (fresh-line s)
      (princ tail-conf s)
      (princ begin-img-table s)
      (mapc (lambda (b) (princ (format-picture b) s)
		    (fresh-line s)
		    (fresh-line s))
	    (picture-list (invoice-item-list invoice)))
      (princ end-img-table s)
      (princ end-document s))

    (trivial-shell:shell-command (concatenate 'string "pdflatex "
					      "-interaction=nonstopmode -output-directory="
					      root-dir "pdf/"
					      " " complete-stream))
    (setf (invoice-pdf-location invoice)
	  (subseq completer-stream 70))))

(defun format-description (b)
  (concatenate 'string
	       unitrow
	       (item-description b)
	       "}{"
	       (item-quantity b) "}{"
	       (item-price b) "}{"
	       (if (equal (item-notes b) '())
		   ""
		   (funcall (first (item-notes b)) :display))
	       "}{"
	       (item-returned-on b)
	       "}"))

;;;This kills the latex
(defun format-notes (notes)
  (concat-notes (mapcar #'(lambda (x) (funcall x :display))
			notes)))

(defun concat-notes (notes)
  (if (null notes)
      '()
      (concatenate 'string (car notes) "\\\\"
		   (concat-notes (cdr notes)))))

(defun subseq-img (b)
  (if (null b)
      ""
      (subseq (item-picture b)
	      (+ (search "webimg" (item-picture b)) 7))))

(defun subseq-img-path (b)
  (if (null b)
      ""
      (subseq b
	      (+ (search "webimg" b) 7))))
;;;I know this is ugly as sin.  I'm tired
;;;I'll fix it in a little bit.

(defun format-picture (b)
  (concatenate 'string
	       "\\figureSeriesRow{ "
	       (picrow-format (first b))
	       (picrow-format (second b))
	       (picrow-format (third b))
	       "}"))

(defun picrow-format (a)
  (if (null a)
      ""
      (concatenate 'string "\\figureSeriesElement{"
		   (item-description a)
		   "}{\\resizebox{0.3\\linewidth}{5cm}{\\includegraphics{"
		   (subseq-img a)
		   "}}}")))

(defun picture-list (b)
  (cond
    ((null b) nil)
    (t
     (cons (list (car b) (cadr b) (third b)) (picture-list (cdddr b))))))
