;---------------------------------------------------------------------------
;多段管线导出
;(rtos number [mode [precision]])
;---------------------------------------------------------------------------
(defun c:zwgx()
(setvar "osmode" 0)
(setvar "modemacro" " luwenbo ")
(prompt ">选择导出的管线对象:")
(princ)
(setq ent_set (ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "POLYLINE") (-4 . "OR>"))))
(setq dc_file (getfiled "选择点坐标文件" ((substr (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4)) "管线信息") "csv" 1))
(setq ofs (open dc_file "w"))
(setq set_length (sslength ent_set))
(prompt "\n wait...\n")
(princ)
(write-line "句柄,序号,起点坐标,终点坐标,长度,管径,颜色" ofs)
(setq lnum 10000)
(while (> set_length 0)
	(setq ent_list (entget (setq ent_pln (ssname ent_set (- set_length 1)))))
	(cond
		((= (cdr (assoc 0 ent_list)) "LWPOLYLINE")
			;;(write-line "LWP begin:" ofs)
			(setq pnum 1)
			(setq pt_last "last")
			(setq pt_list "list")
			(setq handle (strcat "\t" (cdr (assoc 5 ent_list))))
			(setq plc (cdr(assoc 62 ent_list)))
			;(prompt "s\n")
			(cond ( (= nil plc) (setq plc "随层"))
				( (= 0 plc) (setq plc "随块"))
				( (= 1 plc) (setq plc "红色"))
				( (= 2 plc) (setq plc "黄色"))
				( (= 3 plc) (setq plc "绿色"))
				( (= 4 plc) (setq plc "青色"))
				( (= 5 plc) (setq plc "蓝色"))
				( (= 6 plc) (setq plc "洋红"))
				( (= 7 plc) (setq plc "白色"))
				( (= 256 plc) (setq plc "随层"))
				(t (setq plc (strcat "色值" (itoa plc))))
			);end cond
			;;(prompt plc)
			;;(command "text" org 20 0 (strcat "\""  note "\"")) 
			;;(write-line handle ofs)
			(while (setq sub_list (car ent_list))
				(if (= (setq p_code (car sub_list)) 10)
				(progn
					(setq w0 (cdr (assoc 40 ent_list)))
					(setq w1 (cdr (assoc 41 ent_list)))
					(setq id (+ lnum pnum))
					(if (= pnum 1)(progn
						(setq p_last (cdr sub_list))
						(setq pt_last (strcat handle "," (itoa pnum) ",\"[" (rtos (car p_last) 2 4) "," (rtos (cadr p_last) 2 4) "]\""))
					)(progn
						(setq p_list (cdr sub_list))
						(setq pt_list (strcat ",\"[" (rtos (car p_list) 2 4) "," (rtos (cadr p_list) 2 4) "]\""))
						(write-line (strcat pt_last pt_list "," (rtos (distance p_last p_list) 2 2) "," (rtos (* 1000 w0) 2 0) ) ofs);"," plc
						(setq p_last p_list)
						(setq pt_last (strcat handle "," (itoa pnum) pt_list))
					))
					(setq pnum (+ pnum 1))
				)
				)
			(setq ent_list (cdr ent_list))			
			)
		)
		((= (cdr (assoc 0 ent_list)) "POLYLINE")
		(progn
			(setq sub_ent_name (entnext ent_pln))
			(write-line sub_ent_name ofs)
		  	(write-line "P begin:" ofs)
			(while (and (/= sub_ent_name nil) (/= (cdr (assoc 0 (entget sub_ent_name))) "SEQEND"))
				(setq p_list (cdr (assoc 10 (entget sub_ent_name))))
				(setq pt_list (strcat (rtos (+ (* 1000 lnum) pnum)) ";[" (rtos (car p_list) 2 4) "," (rtos (cadr p_list) 2 4) "];"))
				(write-line pt_list ofs)
				(setq sub_ent_name (entnext sub_ent_name))
			)
			)
		)
	)
  	(setq lnum (+ lnum 10000))
	;;(write-line "end" ofs)
	(setq set_length (- set_length 1))
)
(close ofs)
)
;========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	Copied from Autodesk Discussion Group
;	Date: 1/10/06
;
;	This Program was designed to copy text from a dwg
;	to notepad
;	导标注
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:zwbz (/ elist en fn fname i ss txt)
(setvar "cmdecho" 0)
(prompt "\n* Text file written to directory of current drawing *")
(if (setq ss (ssget (list (cons 0 "TEXT"))))
	(progn 
	;(setq fname (getstring "\n* Enter text file name: "))
	;(if (= fname "")
	(setq fname (strcat (substr (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4)) "_标注"))
	;)
	(setq fn (open (strcat (getvar "dwgprefix") fname ".csv") "w"))
	(setq i -1)
	(repeat (sslength ss)
		(setq i (1+ i))
		(setq en (ssname ss i)
		elist (entget en)
		txt (cdr (assoc 1 elist))
		p_list (cdr (assoc 10 elist)))
		(setq plc (cdr(assoc 62 ent_list)))
		;(princ (itoa plc))
		(cond ( (= nil plc) (setq plc "随层"))
				( (= 0 plc) (setq plc "随块"))
				( (= 1 plc) (setq plc "红色"))
				( (= 2 plc) (setq plc "黄色"))
				( (= 3 plc) (setq plc "绿色"))
				( (= 4 plc) (setq plc "青色"))
				( (= 5 plc) (setq plc "蓝色"))
				( (= 6 plc) (setq plc "洋红"))
				( (= 7 plc) (setq plc "白色"))
				( (= 256 plc) (setq plc "随层"))
				(t (setq plc (strcat "色值" (itoa plc)))))
		(write-line (strcat txt ",\"[" (rtos (car p_list)) "," (rtos (cadr p_list)) "]\"," plc) fn)
		;(if (= nil plc) (setq plc 256))
		;(write-line (strcat txt ",\"[" (rtos (car p_list)) "," (rtos (cadr p_list)) "]\"," (itoa plc)) fn)


	)
	(close fn))
)
(princ (strcat "\n* Text file " (getvar "dwgprefix") fname " has been created *"))
(setvar "cmdecho" 1)

(setq fn (strcat (getvar "dwgprefix") fname ".csv"))
;(startapp (strcat "Notepad " (chr 34) fn (chr 34))) 
(princ))
;--------------------------------------
(defun c:hdls (/ ss n ed sz)
  (setq sz (getvar 'TextSize))
  (prompt "对象集句柄: ")
  (if (and (setq ss (ssget)) (setq n (sslength ss)))
    (while (>= (setq n (1- n)) 0)
      (setq ed (entget (ssname ss n)))
      (prompt  (strcat (cdr (assoc 5 ed)) ", "))
    )
  )
  (princ)
)
;---
(defun c:hdl (/ en ed newtxt)
  (if (setq en (entsel "对象句柄: ")) ;Ask user to select object
    (progn
      (setq ed (entget (car en)))    ;Get data of object
	  ;; Create new text object's data
      (prompt  (cdr (assoc 5 ed))) ;_ end of setq
    ) ;_ end of progn
    (princ "Canceled")
  ) ;_ end of if
  (princ)
)
