# elm levenstein
* levenstein distanceを算出するサイト  
https://takumi34.github.io/elm-levenstein/


* アルゴリズム本体
```elm
leven: String -> String -> Int
leven s1 s2 =
    if String.length s1 == 0 then
            String.length s2
    else if String.length s2 == 0 then
            String.length s1
    else 
          if right 1 s1 == right 1 s2 then
            leven (dropRight 1 s1) (dropRight 1 s2) -- 右端の文字が同じなら切り捨て
          else
            Maybe.withDefault 0
            (List.minimum [leven (dropRight 1 s1) s2,
                  leven s1 (dropRight 1 s2),
                  leven (dropRight 1 s1) (dropRight 1 s2)]) + 1
```
