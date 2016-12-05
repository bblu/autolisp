;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	Copied from Autodesk Discussion Group
;	Date: 1/10/06
;
;	This Program was designed to copy text from a dwg
;	to notepad
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:tx (/ elist en fn fname i ss txt)
(setvar "cmdecho" 0)
(prompt "\n* Text file written to directory of current drawing *")
(if (setq ss (ssget (list (cons 0 "TEXT"))))
	(progn 
	(setq fname (getstring "\n* Enter text file name: "))
	(if (= fname "")
	(setq fname (substr (getvar "dwgname") 1 (- (strlen (getvar 
	"dwgname")) 4))))
	(setq fn (open (strcat (getvar "dwgprefix") fname ".csv") "w"))
	(setq i -1)
	(repeat (sslength ss)
		(setq i (1+ i))
		(setq en (ssname ss i)
		elist (entget en)
		txt (cdr (assoc 1 elist))
		p_list (cdr (assoc 10 elist)))
		(setq plc (cdr(assoc 62 ent_list)))
		(if (= nil plc) (setq plc 256))
		(write-line (strcat txt (rtos (car p_list)) "," (rtos (cadr p_list)) "," (itoa plc)) fn)
	)
	(close fn))
)
(princ (strcat "\n* Text file " (getvar "dwgprefix") fname " has been created *"))
(setvar "cmdecho" 1)

(setq fn (strcat (getvar "dwgprefix") fname ".csv"))
(startapp (strcat "Notepad " (chr 34) fn (chr 34))) 
(princ))
