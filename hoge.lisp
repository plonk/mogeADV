
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
    	    (setf *font-window* nil)))

    (pack (list m0 spin1 f0))
    (pack (list ok-btn can-btn) :side :left)))

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
			(sleep 0.1))))
    (if (>= skill-p num)
	(setf (text l3) "成功！")
	(setf (text l3) "失敗！"))
    (pack (list f2 l3) :side :top)
    (sleep 2)
    (destroy f2)
    ;(ltk:after 2000 (lambda () (destroy f2)))
    (>= skill-p num)))

;;技能選択ボタン
(defun make-skill-button (p f skills jump-func)
  (let* ((f1 (make-instance 'frame :master f)))
    (pack f1)
    (loop for s in skills
	  do
          (destructuring-bind
              (skill succ-label fail-label)
              s

            (let* ((skill-p (cadr (assoc skill (player-skill p) :test #'equal))) ;;目標値
                   (btn (make-instance 'button :master f1
                                        :text (format nil "~a:~d" skill skill-p))))

	       (setf (command btn)
		     (lambda ()
		       (destroy f1)
		       (if (dice-100 f 1 100 skill-p)
			   (funcall jump-func succ-label)
			   (funcall jump-func fail-label))
                       (destroy f)))

	       (pack btn))))))

(defun moge-point-p (p)
  (let* ((num (car (player-tp p))))
    (if (>= (player-mogep p) num)
	(setf (player-scene p) (caadr (player-tp p)))
	(setf (player-scene p) (cadadr (player-tp p))))))

;;ゲーム本編
(defun game-start (p f)
  (let* ((f1 (make-instance 'frame :master f))
	 (lf2 (make-instance 'frame :master f1 :relief :raised
			     :borderwidth 4))

	 (m0 (make-menubar))
	 (l1 (make-instance 'label :master lf2 :width 640
				   :wraplength *window-w*
                                   :anchor :nw
				   :font (make-font-string)))
         (ip 0)
         (curr-op nil)
         (curr-args nil))

    (make-menubutton m0 "Font" (lambda ()
				 (when (null *font-window*)
				   (change-font-size l1))))

    (pack (list f1 lf2) :fill :both :expand t)
    (pack (list l1) :padx 5 :pady 5 :fill :both :expand t)

    (bind *tk* "<Configure>" ;;ウィンドウサイズ変更
	  (lambda (e)
	    (declare (ignore e))
	    (setf *window-w* (window-width *tk*)
		  *window-h* (window-height *tk*))
	    (configure l1 :wraplength (window-width l1))))

    (labels ((execute ()
                      (let ((inst (nth ip *text*)))
                        (cond
                         ((integerp inst) (goto-next))
                         ((null inst) (error "null instruction"))
                         ((listp inst)
                          (setf curr-op (car inst) curr-args (cdr inst))
                          (case curr-op
                            (text
                             (show-next-line))
                            (ask
                             (show-choices))
                            (skill
                             (show-skills))
                            (mogep
                             (update-mogep (car curr-args)))
                            (next
                             (jump (car curr-args)))
                            (tp
                             (destructuring-bind
                                 (threshold not-less-label less-label) curr-args
                               (if (>= (player-mogep p) threshold)
                                   (jump not-less-label)
                                 (jump less-label))))
                            (stop)
                            (t
                             (error "unknown op"))))
                         (t
                          (error "illegal instruction")))))
             (show-next-line ()
                             (when (not (eq 'text curr-op)) (error "not executing text"))
                             (if (null curr-args)
                                 (goto-next)
                               (setf (text l1) (car curr-args)
                                     curr-args (cdr curr-args))))

             (show-choices ()
                           (when (not (eq 'ask curr-op)) (error "not executing ask"))
                           (let ((btn-frame (make-instance 'labelframe :master f :text "選択肢")))
                             (pack btn-frame :before lf2)
                             (loop for pair in curr-args ;; (string label)
                                   do
                                   (let ((btn (make-instance 'button :master btn-frame :text (car pair)))
                                         (label (cadr pair)))
                                     (setf (command btn)
                                           (lambda ()
                                             (jump label)
                                             (destroy btn-frame)))
                                     (pack btn)))))

             (show-skills ()
                          (when (not (eq 'skill curr-op)) (error "not executing skill"))
                          (let ((btn-frame (make-instance 'frame :master f)))
                            (pack btn-frame :before lf2)
                            (make-skill-button p btn-frame curr-args #'jump)))

             (update-mogep (diff)
                           (format t "mogep old: ~S~%" (player-mogep p))
                           (incf (player-mogep p) diff)
                           (format t "mogep new: ~S~%" (player-mogep p))
                           (goto-next))

             (jump (label)
                   (let ((index (position label *text*)))
                     (when (not index)
                       (error "jump to nonexistent label"))
                     (setf ip index)
                     (execute)))

             (goto-next ()
                        (incf ip)
                        (execute)))
      (bind l1 "<Button-1>"
            (lambda (e) (declare (ignore e))
              (case curr-op
                (text
                 (show-next-line))
                (t nil))))

      (execute))))

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
	  do
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
		       (return-from hoge)))
    (set-geometry *tk* *window-w* *window-h* 100 100)
    (let* ((p (make-player))
	   (f (make-instance 'frame)))
      (pack f :fill :both :expand t)
      (start-gamen p f))))
