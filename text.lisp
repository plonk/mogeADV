;;シナリオ

;;村にいく　どんな村
;;村探索
;;村でなんか起きる
;;エンディング条件

;;text 表示する文章
;;ask 選択肢 ((文章 次のシーン) ...)
;;mogep モゲポイント (mogep 値)
;;tp モゲポイントによる分かれ道 (tp (目標値 (目標以上シーン 目標未満シーン)))
;;skill 技能選択肢 ((技能名 (成功シーン 失敗シーン)) ...)

;;4 空き
(defparameter *text*
  '(
    0
    (text "大学２年の夏休み、あなたは親友のもげぞうからお宝探しに誘われた。"
          "とある限界集落に、古くから伝わるお宝がある、と言う"
          "そのお宝はなにか特殊な宝石でできた置物だ、と言う"
          "ただしそのお宝を探しに行った者は２度と帰ってこない、と言う")
    (ask ("興味がある。行ってみよう" 1)
         ("絶対に行かない！" 500))

    1 ;;0 準備
    (text "その限界集落の名は火赤巣村というらしい。"
          "もげぞうは村の名前とある程度の場所しか知らないと言う。"
          "詳しい村の情報もなく、険しい道を進んでいくことになりそうだ。")
    (ask ("図書館へ行って村について調べる" 2)
         ("ホームセンターへ行って買い物をする" 7)
         ("とりあえず軽い気持ちで行ってみる" 10))

    ;;---------------------------------------------------------------------------------------

    2 ;;1 図書館
    (text "図書館に着いたあなたは村の詳しい情報を調べようとした。")
    (skill ("図書館" 3 6)
           ("オカルト" 3 6))

    3 ;;2  図書館orオカルト成功
    (text "村の位置が記載されている地図と、むかし村の近郊で起きた事件が書かれている新聞を発見した。"
          "なんでも30年ほど前に火赤巣村付近を通りかかった旅行者3名が行方不明になったそうだ。"
          "一人は発見されたが残りの二名は発見できなかったらしい。")
    (mogep 1)
    (next 19)

    6 ;;2 図書館で技能失敗
    (text "図書館へ来たが詳しい情報は得られなかった。")
    (next 10)
    
    19 ;;3  出発
    (text "村の正確な位置もわかったので準備を整え出発した。"
          "図書館で見つけた地図を見ながら行くとすんなりと村に着いた。")
    (next 20)
    
    ;;---------------------------------------------------------------------------------------

    7 ;;1 ホームセンター
    (text "あなたはホームセンターへ行き必要そうな道具を買うことにした。")
    (skill ("目星" 8 9) ("アイデア" 8 9))

    8 ;;7 目星成功
    (text "山に入るかもしれないのでしっかりした装備を整えた。")
    (mogep 1)
    (next 10)

    9 ;;7 技能失敗
    (text "あなたはライトなど最低限必要なものを買うことにした。")
    (next 10)
    
    10 ;;1 6 8 9  出発
    (text "探索の準備を終えたあなたは火赤巣村へ向け出発した。"
          "しかしもげぞうは、急な腹痛のため行けなくなったので、仕方なく一人で出発することになった。")
    (next 11)

    11 ;;10
    (text "もげぞうに教えてもらったルートできたが、この先車で行けそうもない。")
    (ask ("歩いて探してみる" 12) ("歩くのは嫌だ。もう帰る" 16))

    12 ;;11
    (text "獣道を黙々と歩いていく。だんだんこの道であっているのか不安になってくる")
    (tp 1 13 14)

    13 ;;12 mogep成功
    (text "装備をしっかり整えてきたあなたは水や食料の心配もなく黙々と進んでいく。"
          "しばらく行くと舗装された道路に出た。"
          "あたりを見回すと遠くの方に集落が見えた。例の村だろう。")
    (next 20)

    14 ;;12 mogep失敗
    (text "軽い装備しかしてこなかったあなたはこのまま進んでいいのか悩む。")
    (ask ("まだ行ける！" 15) ("迷ったら危険だ。引き返そう" 16))

    15 ;;14
    (text "ここまで来て引き返せないと意気込み探索を再開する")
    (skill ("目星" 17 18) ("ナビゲート" 17 18) ("幸運" 17 18))

    16 ;;11 14
    (text "探索を諦めたあなたは帰路についた。")
    (next 500)

    17 ;;15 技能成功
    (text "注意深くあたりを見回すと人が通った跡のようなものがある"
          "跡を辿っていくと舗装された道路に出た。"
          "あたりを見回すと遠くの方に集落が見えた。例の村だろう。")
    (next 20)

    18 ;;15 技能失敗
    (text "あなたは草木が生い茂った林の中を黙々と進んでいった。"
          "ひたすら歩き回った挙句ようやく舗装された道路に出た"
          "あたりを見回すと遠くの方に集落が見えた。例の村だろう。")
    (mogep -1)
    (next 20)

    ;;---------------------------------------------------------------------------------------
    ;;村到着

    20
    (text "あなたは村に到着した。"
          "よくある寂れた農村という雰囲気だが本当にお宝があるのだろうか。")
    (ask ("村長を探す" 101)
         ("遠くに見える祠らしき所へ向かう" 25))

    101 ;;20
    (text "あなたは畑仕事をしていた村人に村長の居場所を聞きだそうとした")
    (skill ("信用" 102 104) ("言いくるめ" 103 104))

    102 ;;101 技能成功
    (text "笑顔で挨拶をして、このあたりを旅行しているものだと自己紹介してから村長の居場所を聞くと"
          "村人は快く村長の居場所を教えてくれた。")
    (mogep 1)
    (next 21)

    103 ;;101 言いくるめ成功
    (text "笑顔で挨拶をし、このあたりを旅行しているものだと自己紹介してから村長の居場所を聞くと"
          "村人は快く村長の居場所を教えてくれた。"
          "ついでにこの村のお宝について聞いてみると、村人は少し悩んだあとに、何かの像が隠されていると聞いたことがあると答えた。")
    (mogep 2)
    (next 21)

    104 ;;101 技能失敗
    (text "村人に軽く挨拶をし村長の居場所を聞くと、怪訝な顔をされたが一応居場所は教えてくれた。")
    (next 21)
     
    21 ;;20
    (text "村人に教えてもらった家を尋ねると一人の老人が出迎えてくれた。"
          "この村への旅行者が珍しいのか村長はすこし驚いた顔をしている。")
    (ask ("お宝について聞く" 22) ("30年前の事件について聞く" 23))
    
    22 ;;21 105
    (text "お宝という言葉に村長は反応したようだ。少し考えたあと、この村には言い伝えがあると言う"
          "「たぬきと和解せよ」"
          "これが言い伝えだという。"
          "しかし、この意味がわからないらしく、そのうち誰も気にしなくなったという。"
          "続けて村長は、村の祠にたぬきの像があるがいろいろ調べても何もわからず諦めたと説明した。")
    (ask ("祠へ行く" 25))

    23 ;;21
    (text "あの旅行者が行方不明になった事件ですか、と村長の顔が曇る"
          "この村にも警察の捜索が入ったが結局3人のうちの2人は見つからなかったそうだ。")
    (skill ("心理学" 105 107))

    105 ;;23 心理学成功
    (text "事件について話す村長の様子がどことなくおかしい。"
          "なにか隠していることがあるのだろうか。")
    (ask ("村長さん、なにか隠していませんか？" 106) ("そんなことよりもお宝について聞く" 22))

    106 ;;105
    (text "村長の表情が一瞬固まった。が、隠し事などなにもないと言う。"
          "あの雰囲気は怪しい。何かあるに違いないが、目的はお宝だ。")
    (mogep 1)
    (next 24)

    107 ;;23 心理学失敗
    (text "事件のことも気になるが、今はお宝が目的だ。")
    (next 24)

    24 ;;23 106 107
    (ask ("お宝について聞く" 22))

    ;;---------------------------------------------------------------------------------------

    25 ;;20 22 祠
    (text "祠には、たぬきと狐と熊と犬の像が並んでおり奥には木の板があった"
          "木の板には「たくたたまたのぞたうたなたでたろ」と書いてあった"
          "なにかの暗号だろうか")
    (ask ("たぬきの像を調べる" 26) ("狐の像を調べる" 27)
         ("熊の像を調べる" 28) ("犬の像を調べる" 29))

    28 ;;25
    (text "あなたはクマの像を調べようとペタペタ触ってみた。"
          "ちょうど腹のあたりを撫でたとき、地面の一部が動き穴ができた。"
          "その穴を覗いてみると木箱があった。"
          "あなたは木箱を手に取り開けてみた。"
          "なんと箱の中にはきれいな半透明な緑色した竜の像が入っていた。")
    (mogep 1)
    (ask ("こっそり持ち帰る" 30) ("竜の像を観察する" 108) ("村長に報告する" 33))

    27 ;;25
    (text "狐の像を調べたが特に何もなかった。")
    (mogep -1)
    (next 25)

    26 ;;25
    (text "たぬきの像を調べたが特に何もなかった。")
    (mogep -1)
    (next 25)

    29 ;;25
    (text "犬の像を調べたが特に何もなかった。")
    (mogep -1)
    (next 25)

    108 ;;28
    (text "あなたは竜の像を手に取りよく見た。")
    (skill ("博物学" 109 111) ("地質学" 110 111))

    109 ;; 108 博物学成功
    (text "あなたはこの竜の像があまり精巧な作りでなく、重さもなんとなく軽いなと感じた。")
    (ask ("こっそり持ち帰る" 30) ("村長に報告する" 33))

    110 ;;108 地質学精巧
    (text "あなたは竜の像についてよくわからなかったが、埋めてあった穴はここ1〜2年で作られた穴ではないかと感じた")
    (ask ("こっそり持ち帰る" 30) ("村長に報告する" 33))

    111 ;;108 失敗
    (text "よくわからんけど多分お宝だろう。")
    (ask ("こっそり持ち帰る" 30))
    

    30 ;;28
    (text "あなたはもう一度クマの像の腹を撫で地面を元に戻し、木箱をリュックに入れそそくさと車に乗り込んだ。"
          "お宝を手に入れたあなたは、ウッキウキで帰路についた。")
    (tp 1 31 32)

    31 ;;30
    (text "無事に家に着いたあなたはさっそく竜の像を取り出し眺めた。"
          "なんの材質でできているかわからないがとても美しい。")
    (next 501)

    32 ;;30
    (text "早く帰ろうとするあなたですが、旅の疲れとなれない土地のせいか同じ場所を何度も通っているきがした。"
          "霧も出てきて視界が悪くなりどこを走っているかますますわからなくなった。"
          "不安になり車を止め外に出てみると、あの村の前であった"
          "霧の中に人の影が何十もうごめいている。"
          "怖くなり慌てて車に乗り込むがエンジンがかからない。"
          "顔を上げるとすでに村人たちに囲まれていた.....")
    (next 502)

    33 ;;村長へ報告
    (text "あなたは竜の像を持って村長のもとを訪れた。"
          "竜の像を見た村長はニヤリと笑い、「見つかってしもうたか、その竜の像はわしが作って埋めておいたものじゃ」と言いニコニコと笑っている。")
    (ask ("どういうことだ！お宝はないのか！？" 34))

    34 ;;33
    (text "あなたが詰め寄ると村長は首を横に振り"
          "「お宝はある！そなたの秘宝を求めるロマンあふれる探究心が本物のお宝じゃ・・・」"
          "と、目を細めながら満足気に語った")
    (ask ("そうだったのか！ありがとう村長さん！" 35)
         ("ふざけんな！" 36))

    35 ;; 34
    (text "本物のお宝を手に入れたあなたは心が洗われた様なスッキリとした気持ちで帰路についた。")
    (next 503)

    36 ;;33
    (text "あなたは夏休みの貴重な時間を無駄に過ごしてしまったと後悔と怒りでもやもやとした気持ちで帰路についた。")
    (next 504)

    ;;---------------------------------------------------------------------------------------
    ;;エンディング

    500 ;;0 
    (text "あなたは夏休みを満喫した。"
          "NORMAL END")
    (stop)

    501
    (text "お宝を手に入れたあなたは満足気だが、本当に貴重なものかどうかはまだわからない...."
          "GOOD END")
    (stop)

    502
    (text "その後、あなたの姿を見たものは誰もいない"
          "BAD END")
    (stop)

    503 ;;35
    (text "なにか騙されてる気がするがいい探索の旅をした。"
          "TRUE END")
    (stop)

    504 ;;36
    (text "もげぞうのホラ話にはもう付き合わないでおこう"
          "NORMAL END")
    (stop)

    ;;test
    100
    (text "どーする")
    (skill ("目星" 103 101) ("応急手当" 104 105))

    101
    (text "なんもない！")
    (mogep -1)
    (next 107)

    102
    (text "BAD END")
    (stop)

    103
    (text "なんかあった！")
    (mogep 1)
    (next 107)

    104
    (text "応急手当した！")
    (next 106)

    105
    (text "傷口に塩を塗り込んだ")
    (next 102)

    106
    (text "GOOD END")
    (stop)

    107
    (text "モゲチェック")
    (tp 0 106 102)))
