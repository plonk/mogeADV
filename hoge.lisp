
;;"フォント名 サイズ"
(defun make-font-string ()
  (concatenate 'string *font-name* " " *font-size*))

;;フォントサイズ変更ウィンドウ
(defun change-font-size (b1)
  (setf *font-window* t)
  (let* ((t0 (make-instance 'toplevel  :title "フォント設定"
				       :on-close t)) ;;ウィンドウ
	 (m0 (make-instance 'message :master t0 :width 200
				     :text "フォントサイズ" :font "cica 18"))
	 (spin1 (make-instance 'spinbox :master t0 :increment 1 :from 6 :to 72
					:text *font-size*))
	 (f0 (make-instance 'frame :master t0))
	 (ok-btn (make-instance 'button :master f0 :text "OK"))
	 (can-btn (make-instance 'button :master f0 :text "キャンセル")))
     (set-geometry t0 200 100 (+ 100 (window-x *tk*)) (+ 100 (window-y *tk*)))
     ;;(on-close t0 (lambda () (setf *font-window* nil)))
    (setf (command ok-btn)
          (lambda ()
            (when (parse-integer (text spin1) :junk-allowed t)
              (setf *font-size* (text spin1))
	      (configure b1 :font (make-font-string)))
	    (destroy t0)))
    (setf (command can-btn) ;;キャンセルボタン
	  (lambda () (destroy t0)))
    (bind t0 "<Destroy>" ;;on-closeをtにしないと動かない
    	  (lambda (e)
    	    (declare (ignore e))
	    ;;(setf (text b1) "にょ！")
    	    (setf *font-window* nil)))
    
    (pack (list m0 spin1 f0))
    (pack (list ok-btn can-btn) :side :left)))

;;シーン切り替わったときテキスト更新
(defun text-set (p)
  (let* ((s1 (cadr (assoc (player-scene p) *text*)))
	 (mp (cadr (assoc 'mogep s1))))
    (when mp
      (incf (player-mogep p) mp))
    (setf (player-text p)      (cadr (assoc 'text s1))
	  (player-ask p)       (cadr (assoc 'ask s1))
	  (player-ask_skill p) (cadr (assoc 'skill s1))
	  (player-tp p)        (cadr (assoc 'tp s1))
	  (player-next p)      (cadr (assoc 'next s1)))))


;;サイコロ
(defun dice-100 (f dice me skill-p)
  (let* ((num 1)
	 (f2 (make-instance 'frame :master f))
	 (l3 (make-instance 'label :master f2 :font *font22*)))
    (pack f2)
    (loop for i from 0 below dice
	  do (let ((l1 (make-instance 'label :master f2 :font *font22*))
		   (l2 (make-instance 'label :master f2 :text (format nil "目標値:~d" skill-p)
					     :font *font22*)))
	       (pack (list l2 l1) :pady 5)
	       (loop for i from 0 to 20
		     do (setf num (1+ (random me)))
			(setf (text l1) (write-to-string num)) 
			(ltk:process-events)
			(sleep (* 0.1)))))
    (if (>= skill-p num)
	(setf (text l3) "成功！")
	(setf (text l3) "失敗！"))
    (pack (list f2 l3) :side :top)
    (ltk:after 2000 (lambda () (destroy f2)))
    (>= skill-p num)))
    ;;(let ((ok-btn (make-instance 'button :text "OK" :master f)))
    ;;  (pack (list f2 ok-btn)))))

(defun show-text-set-next (p b1)
  (setf (text b1) (car (player-text p))
	(player-text p) (cdr (player-text p))))


;;普通の選択肢
(defun make-button (p b1 f btn)
  (setf (player-select_window p) t)
  (let ((f1 (make-instance 'frame :master f)))
    (pack f1)
    (loop for b in btn
	  do (let ((btn1 (make-instance 'button :master f1 :text (car b))))
	       (setf (command btn1)
		     (lambda ()
		       (setf (player-select_window p) nil)
		       (setf (player-scene p) (cadr (assoc (text btn1) btn :test #'equal)))
		       (text-set p)
		       (show-text-set-next p b1)
		       (destroy f1)))
	       (pack btn1)))))

;;技能選択ボタン
(defun make-skill-button (p l1 f btn)
  (setf (player-select_window p) t)
  (let* ((f1 (make-instance 'frame :master f)))
    (pack f1)
    (loop for b in btn
	  do (let* ((skill (car b))
		    (skill-p (cadr (assoc skill (player-skill p) :test #'equal))) ;;目標値
		    ;;(n-text (cadr b))
		    (succ (caadr b)) ;;成功シーン
		    (fail (cadadr b)) ;;失敗シーン
		    (btn1 (make-instance 'button :master f1 
						 :text (format nil "~a:~d" skill skill-p))))
	       (setf (command btn1)
		     (lambda ()
		       (destroy f1)
		       (if (dice-100 f 1 100 skill-p)
			   (setf (player-scene p) succ)
			   (setf (player-scene p) fail))
		       ;;(setf (player-scene p) (cadr (assoc (text btn1) btn :test #'equal)))
		       (text-set p)
		       (setf (player-select_window p) nil)
		       ;;(setf (text b1) (car (player-text p)))
		       ))
	       (pack btn1)))))

(defun moge-point-p (p)
  (let* ((num (car (player-tp p))))
    (if (>= (player-mogep p) num)
	(setf (player-scene p) (caadr (player-tp p)))
	(setf (player-scene p) (cadadr (player-tp p))))))

;;ゲーム本編
(defun game-start (p f)
  (let* ((f1 (make-instance 'frame :master f))
	 (lf1 (make-instance 'labelframe :master f1 :text "選択肢"))
	 (lf2 (make-instance 'frame :master f1 :relief :raised
			     :borderwidth 4)) ;;:text "テキスト"))
	 
	 (m0 (make-menubar))
	 (l1 (make-instance 'label :master lf2 :width 640
				   :wraplength *window-w* :anchor :nw
				   :font (make-font-string))))
    (make-menubutton m0 "font" (lambda ()
				 (when (null *font-window*)
				   (change-font-size l1))))
    (text-set p)
    (show-text-set-next p l1)
    (pack (list lf1));;:fill :both :expand t)
    (pack (list f1 lf2) :fill :both :expand t)
    (pack (list l1) :padx 5 :pady 5 :fill :both :expand t)
    (bind *tk* "<Configure>" ;;ウィンドウサイズ変更
	  (lambda (e)
	    (declare (ignore e))
	    (setf *window-w* (window-width *tk*)
		  *window-h* (window-height *tk*))
	    (configure l1 :wraplength (1- *window-w*))))
    (bind l1 "<Button-1>" ;;クリック
	  (lambda (e)
	    (declare (ignore e))
	    (cond
	      ((null (player-text p)) ;;表示するテキストがなくなった
	       (when (null (player-select_window p)) ;;選択肢ウィンドウが開いてないとき
		 (cond
		   ((player-ask p) ;;普通の選択肢
		    (make-button p l1 lf1 (player-ask p)))
		   ((player-ask_skill p) ;;技能選択肢
		    (make-skill-button p l1 lf1 (player-ask_skill p)))
		   ((player-tp p) ;;モゲポイントによる分岐
		    (moge-point-p p)
		    (text-set p)
		    (show-text-set-next p l1))
		   (t ;;次のシーンへ
		    (setf (player-scene p) (player-next p))
		    (text-set p)
		    (show-text-set-next p l1)))))
	      (t
	       (show-text-set-next p l1)))))))

;;スピンボックス内の技能ポイント初期化
(defun init-skill-num (p lst)
  (loop for skill in (player-skill p)
	for num in lst
	do (setf (text num) (write-to-string (cadr skill)))))

;;技能ポイントセット
(defun set-skill-point (p lst)
  (loop for skill in (player-skill p)
	for num in lst
	do (setf (cadr skill) (parse-integer (text num)))))

;;技能設定
(defun init-skill (p f)
  (let* ((btn nil) (ok! t) (defo-num (loop for x in (player-skill p)
					   sum (cadr x)))
	 (point 200)
	 (f1 (make-instance 'frame :master f))
	 (f2 (make-instance 'frame :master f1))
	 (l1 (make-instance 'label :master f1
				   :text (format nil "好きな技能に初期値〜99の間で200ポイント分振り分けてください~%(初期値未満にはできません)")
				   :font *font20* :wraplength *window-w*))
	 (l2 (make-instance 'label :master f1 :text (format nil "残り~dポイント" point)
				   :font *font20*))
	 (ok-btn (make-instance 'button :master f1 :text "OK")))
    (pack (list f1 l1 l2 f2) :pady  10)
    (loop for s in (player-skill p)
	  for i from 0
	  do (let ((l1 (make-instance 'label :text (car s) :master f2 :font *font18*))
		   (t1 (make-instance 'spinbox :from (cadr s) :to 99 :increment 1 :font *font18*
					       :text (format nil "~d" (cadr s))
					       :master f2 :font *font-size* :width 3)))
	       (push t1 btn)
	       (multiple-value-bind (r c)
		   (floor i 3)
		 (grid l1 r (* c 2))
		 (grid t1 r (1+ (* c 2))))))

    
    (setf (command ok-btn)
	  (lambda ()
	    (let ((num-list (reverse (mapcar #'(lambda (x) (parse-integer (text x) :junk-allowed t)) btn))))
	      (if (member nil num-list)
		  (progn (setf (text l1) "数字を入力してください")
			 (init-skill-num p (reverse btn)))
		  (let ((hoge nil) (old-sum 0) (new-sum 0))
		    (loop for skill in (player-skill p)
			  for new-p in num-list
			  do (if (or (> (cadr skill) new-p)
				     (>= new-p 100))
				 (progn (setf hoge (car skill))
					(return))
				 (progn (incf old-sum (cadr skill))
					(incf new-sum new-p))))
		    (if hoge
		        (setf (text l1) (format nil "[~a]が初期値未満or100以上になってますた" hoge))
			(if (> (- new-sum old-sum) 200)
			    (setf (text l1) "200ポイント以上振り分けています！")
			    (progn (setf ok! nil)
				   (set-skill-point p (reverse btn))
				   (destroy f1)
				   (game-start p f)))))))))
		  
    (pack ok-btn)
    (loop while ok!
	  do ;; (setf (text l1)
	     ;; 	   (format nil "~s" (mapcar #'(lambda (x) (text x)) btn)))
	     (if (not (or (find "" btn :test #'equal :key #'(lambda (x) (text x)))
			  (member nil (mapcar #'(lambda (x) (parse-integer (text x) :junk-allowed t)) btn))))
		 (setf (text l2) (format nil "残り~dポイント" (- 200 (- (apply #'+ (mapcar #'(lambda (x) (parse-integer (text x) :junk-allowed t)) btn)) defo-num)))))
	     (sleep 0.1)
	     (process-events))))

;;スタート画面
(defun start-gamen (p f) 
  (let* ((f1 (make-instance 'frame :master f))
	 (b1 (make-instance 'button :master f1 :text "スタート"))
	 (b2 (make-instance 'button :master f1 :text "終わる")))
    (pack (list f1 b1 b2) :pady 10)
    (setf (command b1)
	  (lambda ()
	    (destroy f1)
	    (init-skill p f))) ;;技能設定
    (setf (command b2)
	  (lambda ()
	    (setf *exit-mainloop* t)))))

(defun hoge ()
  (with-ltk ()
    (wm-title *tk* "ほげ！")
    (bind *tk* "<q>" (lambda (e) (declare (ignore e))
		       (setf *select-window* nil)
		       (return-from hoge)))
    (set-geometry *tk* *window-w* *window-h* 100 100)
    (let* ((p (make-player))
	   ;; (f (make-instance 'labelframe :text "ボタン"))
	   ;; ;;(l1 (make-instance 'label :master f :text "てすと"))
	   ;; (f0 (make-instance 'labelframe :text "test"))
	   (f (make-instance 'frame)))
	   ;; (b1 (make-instance 'label :master f0 :width 640
	   ;; 			     :wraplength 640 :anchor :nw
	   ;; 			     :font (make-font-string)
	   ;; 			     :text "ほげのげ")))
      ;; (make-menubutton m0 "font" (lambda ()
      ;; 				   (when (null *font-window*)
      ;; 				     (change-font-size b1))))
      ;;(text-set p)
      (pack f :fill :both :expand t)
      ;;(format t  "hoge~%")
      (start-gamen p f))))
      
      ;;(ltk:process-events))))
;;(init-skill p f)
      
      ;; (pack f0 :fill :both :expand t)
      ;; (pack (list b1) :fill :both :expand 1)
      ;; (bind *tk* "<Configure>" ;;ウィンドウサイズ変更
      ;; 	    (lambda (e)
      ;; 	      (declare (ignore e))
      ;; 	      (setf *window-w* (window-width *tk*)
      ;; 		    *window-h* (window-height *tk*))
      ;; 	      (configure b1 :wraplength *window-w*)))
      ;; (bind b1 "<Button-1>" ;;クリック
      ;; 	    (lambda (e)
      ;; 	      (declare (ignore e))
      ;; 	      (cond
      ;; 		((null (player-text p)) ;;表示するテキストがなくなった
      ;; 		 (when (null (player-select_window p))
      ;; 		   (cond
      ;; 		     ((player-ask p)
      ;; 		      (make-button p b1 f (player-ask p)))
      ;; 		     ((player-ask_skill p)
      ;; 		      (make-skill-button p b1 f (player-ask_skill p)))
      ;; 		     (t
      ;; 		      (setf (player-scene p) (player-next p))
      ;; 		      (text-set p)
      ;; 		      (show-text-set-next p b1)))))
      ;; 		(t
      ;; 		 (show-text-set-next p b1))))))))

