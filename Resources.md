----------------
### Ресурсы 

Electronic versions with newly typeset mathematics and figures:
[Link](https://sicpebook.wordpress.com/ebook/)

- [HTML5](http://sarabander.github.io/sicp/), [EPUB3](https://www.dropbox.com/s/y9jt5kmabt02yg8/sicp.epub?dl=0), [repo](https://github.com/sarabander/sicp)

- [PDF](https://github.com/sarabander/sicp-pdf/blob/master/sicp.pdf?raw=true), [repo](https://github.com/sarabander/sicp-pdf)

- [Pocket format PDFs](https://github.com/sarabander/sicp-pocket)


Дополнительные материалы:

- [Полная спецификация языка R5RS](https://bmstu-iu9.github.io/scheme-labs/r5rs.pdf)
- [Абельсон, Сассман «Структура и интерпретация компьютерных программ» /RUS /PDF](https://bmstu-iu9.github.io/scheme-labs/sicp.pdf)

Отличные форматы для чтения на наладошниках.

#### Ссылки на Scheme-сайты, всякие papers и прочее.

##### Библиотека SICP для Racket

   Репа с пакетом, реализующим язык sicp: (https://github.com/sicp-lang/sicp.git)
   Описание пакета (https://docs.racket-lang.org/sicp-manual/index.html)

```
;;Пример отрисовки Энштейна.
#lang sicp
    (#%require sicp-pict)
    (paint einstein)
```

Установка через сайт https://planet-compats.racket-lang.org/ не получилась. Виснет, не качает. Похоже, proxy как-то через нужно обойти.

Т.к. raco может брать пакеты не только с сайта, но и из папок, архивов и  CVS-систем.
Пакет склонировал  git-репо и установил так:
raco pkg install  https://github.com/sicp-lang/sicp.git

Пакет установил служебные файлы в %USERHOME%\AppData\Roaming\Racket
Теперь он виден всем установкам Racket на компьютере.
Причём там только ссылки и оформление пакета. А сами файлы лежат в директории, куда я их склонировал. 

Официальный индекс (https://pkgs.racket-lang.org/) сам пакет SICP знает и находит, но единственная указанная ссылка - на git-репо. 
По ссылке ( https://planet-compats.racket-lang.org), которая указана везде, как источник для поиска пакетов, пакета SICP нет. Поэтому raco и висит при попытке установить sicp через централь:
"raco pkg install sicp". 


###### *Чтобы не забыть:*
 * [Почти официальные community.schemewiki.org! Полный комплект:)](http://community.schemewiki.org/?sicp-solutions)
 * [Eli Bendersky's website](http://eli.thegreenplace.net/tag/sicp)
 * [SICP по-русски](http://sicp.sergeykhenkin.com/sicp-exercise-solutions/)

###### *Глянуть оформление репы и тесты:*
 * [SICP exercises Barry Allison (последнее Exercise 4.76 /23rd July, 2018)](https://wizardbook.wordpress.com/) 
 * [Последнее 3.82 23/08/2016](https://github.com/sarabander/p2pu-sicp)
 * [Interactive SICP n. Interactive Structure and Interpretation of Computer Programs.](https://xuanji.appspot.com/isicp/index.html) - Online version of SICP with a built-in scheme interpreter to allow readers to edit and run the code embedded in SICP.
###### *Доп:*
 * [Ivan Ivanov's Weblog (На Главе 3 остановился)](https://ivanovivan.wordpress.com/category/project-sicp) 
 * [Ken Dyck (2.13 последнее)](https://kendyck.com/2007/10/24/solution-to-sicp-exercise-213/)

##### Scheme implementations
- [MIT/GNU Scheme](http://groups.csail.mit.edu/mac/projects/scheme/)
- [Racket: a Scheme superset](http://www.racket-lang.org/)
- [SISC with online REPL](http://sisc-scheme.org/)
- [Chez Scheme](http://www.scheme.com/)
- [Gambit](http://dynamo.iro.umontreal.ca/~gambit/wiki/index.php/Main_Page)
- [Chicken](http://www.call-cc.org/)                                                                 
