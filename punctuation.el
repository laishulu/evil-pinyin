(defconst evil-pinyin--punctuation-alist
  '((?,  . "[，,]")
    (?.  . "[。.]")
    (??  . "[？?]")
    (?:  . "[：:]")
    (?!  . "[！!]")
    (?-  . "[—-]")
    (?~  . "[～~]")
    (?$  . "[￥$]")
    (?*  . "[×*＊·・]")
    (?   . "[　 ]")
    (?{  . "[『〖｛{]")
    (?}  . "[』〗｝}]")
    (?<  . "[《<]")
    (?>  . "[》>]")
    (?\[ . "[\[「【［]")
    (?\] . "[\]」】］]")
    (?\; . "[；;]")
    (?\\ . "[、\\]")
    (?\( . "[（(]")
    (?\) . "[）)]")
    (?\' . "[‘’']")
    (?\" . "[“”\"]"))
  "English to Chinese punctuations map.")
