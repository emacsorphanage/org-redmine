(setq json-object-type 'hash-table)
(setq json-array-type 'list)

(setq fixture-issue-json
      (json-read-from-string "{\"issue\":{
\"updated_on\":\"2011/07/06 21:27:04 +0900\",
\"category\":{\"name\":\"バージョン 0.3\",\"id\":1},
\"status\":{\"name\":\"新規\",\"id\":1},
\"subject\":\"軌跡検知\",
\"project\":{\"name\":\"肉体言語 Tython\",\"id\":1},
\"tracker\":{\"name\":\"機能\",\"id\":2},
\"spent_hours\":0.0,
\"assigned_to\":{\"name\":\"Wataru MIYAGUNI\",\"id\":3},
\"start_date\":\"2011/07/06\",
\"created_on\":\"2011/07/06 21:22:01 +0900\",
\"done_ratio\":0,
\"description\":\"軌跡検知を実装する\",
\"author\":{\"name\":\"Wataru MIYAGUNI\",\"id\":3},
\"id\":1,
\"priority\":{\"name\":\"通常\",\"id\":4}
}}"))

(setq fixture-issue-all-json
      (json-read-from-string "\
{\"offset\":0,\"total_count\":3,\"limit\":25,
 \"issues\":[
   {
     \"updated_on\":\"2011/07/07 23:01:48 +0900\",
     \"status\":{\"name\":\"進行中\",\"id\":2},
     \"subject\":\"サマーソルトキックを認識\",
     \"project\":{\"name\":\"肉体言語 Tython\",\"id\":1},
     \"tracker\":{\"name\":\"機能\",\"id\":2},
     \"assigned_to\":{\"name\":\"Wataru MIYAGUNI\",\"id\":3},
     \"start_date\":\"2011/07/07\",
     \"created_on\":\"2011/07/07 22:57:00 +0900\",
     \"done_ratio\":40,
     \"description\":\"めんどくさい\",
     \"due_date\":\"2011/07/20\",
     \"author\":{\"name\":\"Wataru MIYAGUNI\",\"id\":3},
     \"id\":3,
     \"priority\":{\"name\":\"\u901a\u5e38\",\"id\":4}
   },
   {
     \"updated_on\":\"2011/07/07 22:55:15 +0900\",
     \"status\":{\"name\":\"新規\",\"id\":1},
     \"subject\":\"走る\",
     \"project\":{\"name\":\"Gongo Kinect Diet\",\"id\":2},
     \"tracker\":{\"name\":\"\u904b\u52d5\",\"id\":4},
     \"start_date\":\"2011/07/07\",
     \"created_on\":\"2011/07/07 22:54:07 +0900\",
     \"done_ratio\":0,
     \"description\":\"走れ\",
     \"custom_fields\":[{\"value\":\"320\",\"name\":\"\u6d88\u8cbb\u30ab\u30ed\u30ea\u30fc\",\"id\":1}],
     \"fixed_version\":{\"name\":\"90kg代\",\"id\":1},
     \"author\":{\"name\":\"Wataru MIYAGUNI\",\"id\":3},
     \"id\":2,
     \"priority\":{\"name\":\"\u901a\u5e38\",\"id\":4}
   },
   {
     \"updated_on\":\"2011/07/07 23:01:28 +0900\",
     \"category\":{\"name\":\"0.3\",\"id\":1},
     \"status\":{\"name\":\"解決\",\"id\":3},
     \"subject\":\"軌跡検知\",
     \"project\":{\"name\":\"肉体言語 Tython\",\"id\":1},
     \"tracker\":{\"name\":\"機能\",\"id\":2},
     \"assigned_to\":{\"name\":\"Wataru MIYAGUNI\",\"id\":3},
     \"start_date\":\"2011/07/06\",
     \"created_on\":\"2011/07/06 21:22:01 +0900\",
     \"done_ratio\":100,
     \"description\":\"軌跡検知を実装する\",
     \"author\":{\"name\":\"Wataru MIYAGUNI\",\"id\":3},
     \"id\":1,
     \"priority\":{\"name\":\"\u901a\u5e38\",\"id\":4}
   }
 ]
}"))

(setq hash-json
      (json-read-from-string "{\"a\":3, \"b\":{ \"c\":\"12\", \"d\":{ \"e\":\"31\" } } }"))

(setq fixture-issue (gethash "issue" fixture-issue-json))
(setq fixture-issue-all (gethash "issues" fixture-issue-all-json))

(provide 'org-redmine-test-fixture)
