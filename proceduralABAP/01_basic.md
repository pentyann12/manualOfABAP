# Basic <!-- omit in toc -->

## Table of contents <!-- omit in toc -->

- [一般構文](#一般構文)

## 一般構文

まずはABAPでプログラミングする中で汎用的に必要な要素を覚えましょう。

- コメント  
  `*`から始まる行はコメントになります。
  また、行中の`"`以降はコメントになります。

- テキストリテラル  
  テキストリテラルは`'`で囲みます。

- 命令の終了  
  各命令文は`.`で終了します。

- チェーン命令  
  `:`を使用すると同一命令文を一つにまとめることができます。
  コロンの後に続く各命令は`,`で区切ります。

<details><summary>参考</summary>

***

``` ABAP   画面出力の例
* 地道な記述
WRITE 'Hello'.
WRITE 'World!!'.

* チェーン命令による記述
WRITE : 'Hello',          " <-ピリオドではなくカンマで区切ります
        'World!!'.
```

***
</details>

## 3. データ型とデータオブジェクト（変数）

ABAPには通常のプログラム言語と同様に「型」と「変数」が存在します。型は標準で用意されているものと、開発者が独自に定義できるものがあります。

### 3.1. 事前定義データ型

ABAPに事前定義されているデータ型です。

- `C` : 文字列
- `N` : 数値のみの文字列 (Numeric)
- `D` : 日付型 (YYYYMMDD)
- `T` : 時刻型 (HHMMSS)
- `X` : バイト (16進)
- `I` : 整数型
- `P` : パック型
- `F` : 浮動小数点数値型
- `STRING` : 可変長文字列型

<!-- パック型の説明をどこかに設けたい -->

### 3.2. ディクショナリデータ型

ABAPでは特定のデータ型を事前登録することが出来ます。たとえば商品データテーブルMARAの「品目コード」項目であるMATNRは`MATNR`というディクショナリデータ型を参照しています。この`MATNR`は18桁のCHAR型としてSAPのデータベースに事前登録されています。

> 詳しくは「ABAPディクショナリ」の研修で扱います。

### 3.3. 変数の定義

変数は型を参照してプログラム中で定義されます。変数には基本データ、構造、内部テーブルがあります。

- 基本データ  
  単一のデータを格納する変数です。

  ``` ABAP
  DATA 基本データ名 TYPE 型.
  ```

- 構造  
  複数の項目をひとまとめにして扱う変数です。ただしJavaにおける配列とは異なり、CやRustにおける__構造体__と近いものです。

  ``` ABAP
  DATA 構造名 TYPE 構造型名.
  ```

  <details><summary>参考</summary>

  ***

  商品データテーブル`mara`には

  - `matnr` : 品目コード
  - `ersda` : 登録日
  - `mtart` : 品目タイプ
  - `meins` : 基本数量単位

  などが定義されています。  
  この`mara`を参照して定義された構造型変数は、メンバ（要素）としてこれらの項目を持ちます。

  > maraを参照した構造の**st_material**
  > | matnr | ersda | mtart | meins | ... |
  > |------|-------|-------|-------|-----|

  各メンバへのアクセスは「-」（ハイフン）を使用します。

  ``` ABAP
  st_material-matnr = 'ITEM001'.
  st_material-ersda = '20040101'.
  ```

  ***
  </details>

- 内部テーブル  
  二重配列に近いイメージです。構造変数を配列にした形であり、複数の項目と複数の行を持ちます。メモリ上でデータベーステーブルのように扱うことができます。

  <details><summary>参考</summary>

  ***

  商品データテーブルと同じ構造を持つ内部テーブルを宣言します

  ``` ABAP
  DATA:
    it_material TYPE STANDARD TABLE OF mara.
  ```

  これにより以下のような内部テーブルが作成されます。

  | matnr | ersda | mtart | meins |...|
  |-------|-------|-------|-------|---|
  | ITEM001 | 20040101 | HALB | PC |...|
  | ITEM002 | 20040401 | ROH  | G  |...|
  | ITEM003 | 20040501 | HAWA | PC |...|
  | ITEM004 | 20040601 | HALB | PC |...|

  ***

  </details>

  内部テーブルは用途に応じて３種類のタイプを使い分けることが可能です。

  - 標準テーブル  
    `STANDARD TABLE`として宣言します。通常は標準テーブルを使用します。アクセスにはインデックスもしくはキーを使用します。

  - ソートテーブル  
    `SORTED TABLE`として宣言します。指定されたキーに対してソートされていることを保証する内部テーブルです。アクセスにはインデックスもしくはキーを使用します。

  - ハッシュテーブル  
    `HASHED TABLE`として宣言します。  アクセスには内部的にハッシュキーを用います。一意検索に対してパフォーマンスを発揮します。

## 4. 宣言

### 4.1. TYPES 型宣言

型は`TYPES`によって宣言します。

- 基本データ型  

  ``` ABAP
  * 構文
  TYPES (データ型名) TYPE (データ型).

  * 例
  TYPES t_matnr TYPE mara-matnr.  "商品データの品目コードと同じ型を宣言
  ```

- 構造データ型  

  ``` ABAP
  * 構文
  TYPES :
    BEGIN OF (構造データ型名),
      (データ型名) TYPE (データ型), 
      (データ型名) TYPE (データ型), 
    END   OF (構造データ型名).

  * 例
  TYPES :
    BEGIN OF tst_materials ,
      matnr TYPE mara-matnr,  " 商品データテーブルの品目コード
      werks TYPE marc-werks,  " 品目プラントテーブルのプラント
      maktx TYPE makt-maktx,  " 品目名称テーブルの品名
    END OF tst_materials.
  ```

- テーブルデータ型  

  ``` ABAP
  * 構文
  TYPES (テーブルデータ型名) TYPE STANDARD TABLE OF (構造データ型名).

  * 例
  TYPES tit_materials TYPE STANDARD TABLE OF tst_materials.
  ```

### 4.2. DATA 変数宣言

変数は`DATA`によって宣言します。

``` ABAP
* 構文
DATA (変数名) TYPE (データ型).

* 例
DATA:
  v_text            TYPE string,                  " 事前定義データ型で定義
  v_material_name   TYPE material_name,           " すでに宣言されている変数を参照する場合
  st_material       TYPE mara,                    " テーブルをTYPEで参照すると構造型になる
  it_material       TYPE STANDARD TABLE OF mara,  " 商品データテーブルを参照した内部テーブルの宣言
  st_materials      TYPE tst_materials,           " ローカルで定義した構造型を参照する場合
  it_materlals      TYPE tit_materials.           " ローカルで定義した内部テーブル型を参照する場合
```

### 4.3. CONSTANTS 定数宣言

プログラムで使用される固定の値に別名をつけるイメージです。定数は`CONSTANTS`で宣言します。

``` ABAP
* 構文
CONSTANTS (定数名) TYPE (データ型) VALUE (初期値).

* 例
CONSTANTS cns_tax_rate TYPE f VALUE '0.1'. 消費税率を意味する定数宣言
```

定数を利用するメリット

1. 可読性の向上  
ソースコード内で"0.1"と書かれているよりも、cns_tax_rateと書かれている方が消費税率の値であることが把握できます。
2. 保守性の向上  
たとえば消費税率が10%から15%に変わった場合に、消費税の計算する箇所で"0.1"と書いていると、ソースコードを一通り目を通して一つずつ"0.15"に置き換える必要があり大変です。このときに定数を利用していると、消費税率の定数の値を変更するだけで済みます。  
なお後述しますが、リテラルについてはメリットの有無に関わらず、開発規約によって定数化することを指定されることが多いです。

### 4.4. RANGE レンジ型変数の宣言

レンジ型変数は`RANGE OF`を用いて宣言します。

``` ABAP
* 構文
DATA (レンジテーブル名) TYPE RANGE OF (データ型).

* 例
DATA r_ersda TYPE RANGE OF mara-ersda. 商品データテーブルの登録日付
```

レンジ型変数とは、値の”範囲”を持たせることができる内部テーブルのことです。レンジ型変数を宣言すると以下の4項目を持つ変数が作成されます。

- SIGN  
  `LOW`と`HIGH`の値を含めるか、除外するかの設定をします。

  - I：含める(INCLUDE)
  - E：除外する(EXCLUDE)
- OPTION  
  範囲の指定方法を設定します。以下はその一部です。

  - BT：`LOW`から`HIGH`の値を指定
  - EQ：`LOW`の値のみを指定
- LOW  
  下限値を指定します。
- HIGH  
  上限値を指定します。OPTIONが`BT`、`NB`のときのみ使用されます。

  <details><summary>参考</summary>

  ***

  | SIGN | OPTION | LOW | HIGH |
  |------|--------|-----|------|
  | I    | BT     | 1   | 10   |
  | I    | EQ     | 15  |      |
  | E    | BT     | 2   | 5    |

  - １行目：1以上10以下
  - ２行目：15
  - ３行目：2より大きく5未満

  ***

  </details>

> - 特にコーディング規約などで指定が無い場合は、チーム内で統一された分かりやすい接頭辞(prefix)をつけることをお勧めします。
> - 構造型には使用する項目のみを含めるようにすると無駄なメモリの使用が避けられ、パフォーマンスが向上します。可能な限りローカルで型を定義し、それを参照してください。
> - プログラム内ではなるべくリテラルを直接使わずに、定数を宣言して使用してください。

## 5. 命令文 その１（画面出力とデータ操作）

- REPORT  
  プログラムの最初に記述し、実行可能プログラムとして定義します。謂わばフレームワークのような物であり、後述の選択画面・出力画面が提供されます。また、画面サイズ等のオプションを追加することも可能です。

  ``` ABAP
  * 構文
  REPORT  プログラム名.

  * 例 画面サイズ170*58、プログラム表題を非出力として実行可能プログラムを定義
  REPORT  Y_SAMPLE001
          NO STANDARD PAGE HEADING
          LINE-SIZE   170
          LINE-COUNT  58.
  ```

### 5.1. 選択画面

- PARAMETERS  
  選択画面の入力項目を作成します。必須入力や初期値などのオプションを追加することも可能です。

  ``` ABAP
  * 構文
  PARAMETERS 項目名 TYPE データ型.

  * 例 maraテーブルのmatnrを参照した型で入力項目を作成
  PARAMETERS:
    p_matnr TYPE mara-matnr.
  ```

  ![](20220727115803.png)

  ![](20220727115856.png)

  また、ラジオボタンの作成もPARAMETERSで行います。

  ``` ABAP
  * 構文
  PARAMETERS ボタン名 RADIOBUTTON GROUP ボタングループ.

  * 例 選択するとXが入るボタン
  PARAMETERS:
    bt1 RADIOBUTTON GROUP rad1 DEFAULT 'X',
    bt2 RADIOBUTTON GROUP rad1.
  ```

  ![](20220727115940.png)

- SELECT-OPTIONS  
  選択画面の範囲選択項目を定義します。入力する項目を表す変数を事前に定義する必要があります。

  ``` ABAP
  * 構文
  SELECT-OPTIONS 項目名 FOR 変数.

  * 例 maraテーブルのmatnrを参照した型で、範囲選択を作成します。
  DATA:
    v_matnr TYPE mara-matnr.

  SELECT-OPTIONS:
    s_matnr FOR  v_matnr.
  ```

### 5.2. データ操作

- COMPUTE  
  算術演算を行います。ただしこの命令語は省略可能なので、実際に記述することはあまりありません。

  ``` ABAP
  * 構文
  COMPUTE 算術演算式.

  * 例 b + c を aへ代入します。
  COMPUTE a = b + c.
  ```

- MOVE  
  値をコピーします。この命令語は代入演算子で代用することができます。

  ``` ABAP
  * 構文
  MOVE コピー元 TO コピー先.

  * 例
  ```

- MOVE-CORRESPONDING  
  構造型変数の値をコピーします。このとき、各要素に対応する値が転送されます。なお「安全のために絶対書け」派と「処理を軽量化するために絶対書くな」派に分かれるようです。開発規約に従ってください。

  ``` ABAP
  * 構文
  MOVE-CORRESPONDING コピー元の構造 TO コピー先の構造.

  * 例
  ```

- CLEAR  
  変数や構造型変数を初期化します。

  ``` ABAP
  * 構文
  CLEAR 変数.

  * 例
  ```

- CONCATENATE  
  文字列を結合します。

  ``` ABAP
  * 構文 文字列a,bを結合して、cへ代入します。
  CONCATENATE 文字列１ 文字列２ INTO 代入先.

  * 例
  CONCATENATE 'HELLO' 'WORLD' INTO NEWMESSAGE.
  ```

### 5.3. 画面出力

- WRITE  
  画面への出力を行います。

  ``` ABAP
  * 構文
  WRITE 出力するもの.

  * 例
  WRITE 'ABCDEFG'. 文字列を出力します。
  WRITE / . 改行します。
  WRITE 10 'ABCDEFG'. 出力位置を指定します。
  WRITE sy-vline. 縦罫線を出力します。
  WRITE sy-uline. 横罫線を出力します。
  ※ 詳しい構文はABAP HELPを参照してください。
  ABAPの構文に関するHELPはソースコード上の構文にカーソルを当て「F1」キーで確認できます。

## 6. 演習１ 選択画面操作と結果表示

- 問題  
  1. 選択画面でInt型の必須パラメータを２つ作り、それらのパラメータを足した数字を表示する
      - [ ] 2つのパラメータの初期値をそれぞれ、`4`、`5`とする
      - [ ] 2つのパラメータは必須入力とする
  2. 選択画面でCHAR型の必須パラメータを２つ作り、2つの文字列`FirstName`、`FamilyiName`を記入し、その2つの文字列をつなげて表
      - [ ] 2つの文字の間には、半角スペースを入れる
- ヒント  
  - 選択画面の設定は、`PARAMETERS`を使用する。
  - 結果の表示は、`WRITE`を使用する。
  - 文字をつなげるには、`CONCATENATE`を使用する。

- 結果  
  1. 選択画面
     結果画面
  2. 選択画面
     結果画面

<details><summary>解説</summary>

***

1. コード例

  ``` abap
  *&---------------------------------------------------------------------*
  *&     変数定義
  *&---------------------------------------------------------------------*
  DATA:
    lv_sum  TYPE i.

  *&---------------------------------------------------------------------*
  *&      選択画面定義
  *&---------------------------------------------------------------------*
  PARAMETERS:
    p_num1  TYPE i DEFAULT 4 OBLIGATORY,
    p_num2  TYPE i DEFAULT 5 OBLIGATORY.

  *&=====================================================================*
  *&      メイン処理
  *&=====================================================================*
  COMPUTE
    lv_sum = p_num1 + p_num2.

  WRITE:
    lv_sum.
  ```

2. コード例

``` abap
*&---------------------------------------------------------------------*
*&     変数定義
*&---------------------------------------------------------------------*
DATA:
  lv_fullname   TYPE c LENGTH 40.

*&---------------------------------------------------------------------*
*&      選択画面定義
*&---------------------------------------------------------------------*
PARAMETERS:
  p_firstname   TYPE c LENGTH 20,
  p_familyname  TYPE c LENGTH 20.

*&=====================================================================*
*&      メイン処理
*&=====================================================================*
CONCATENATE
  p_firstname
  p_familyname
  INTO lv_fullname
  IN CHARACTER MODE SEPARATED BY ' '.

WRITE:
  '氏名：',
  lv_fullname.
```

</details>

## 6. テキストエレメント

プログラム内で使用するテキスト（メッセージ、表題）を一元管理することができます。また、ログオン言語に応じて言語を変えることも可能です。

![](20220727130947.png)

- 選択テキスト  
  選択画面上に表示される項目の表示名を設定します。

  ![](20220727131437.png)
  ![](20220727131535.png)
  ![](20220727131517.png)
- テキストシンボル  
  プログラム内で使用する文字列を設定します。プログラム中では`text-xxx`として使用します。

  ![](20220727131617.png)

  ``` ABAP
  WRITE : text-001.
  ```

  ![](20220727131702.png)

## 7. 演習２ テキストシンボルと選択テキスト

- 問題  
  1. 演習１で作成したプログラムの選択画面に以下の変更を加えてください。
      - [ ] 項目名の表示をそれぞれ、`みょうじ`、`おなまえ`と表示する。
      - [ ] 結果データ表示の初めに`氏名：`と表示する。

- 結果  
  1. 選択画面
  ![](20220727132127.png)
  2. 結果画面
  ![](20220727132054.png)

## 8. 命令文 その２ （制御文）

- CASE  
  変数の値に応じて分岐を行います。

  ``` ABAP
  CASE 変数.
    WHEN 値1.
      処理1.
    WHEN 値2.
      処理2.
    WHEN OTHERS.
      処理3.
  ENDCASE.
  ```

  > ※CASE文にはできる限りOTHERSを実装します。

- IF  
  条件式の評価に応じて分岐を行います。

  ``` ABAP
  IF 条件式1.
    処理1
  ELSEIF 条件式2.
    処理2
  ELSE .
    処理3
  ENDIF.
  ```

  > IF文にはできる限りELSEを実装します。
- DO  
  同じ処理を繰り返します。

  ``` ABAP
  DO.
    IF a = 10.
      EXIT.
    ENDIF.
    a = a + 1.
  ENDDO.
  DO 10 TIMES. ループが10回回ります。
  :
  ENDDO.
  ```

- WHILE  
  条件式が成り立つ間ループします。

  ``` ABAP
  WHILE 条件式 .
    処理. a = a + 1.
  ENDWHILE.
  ```

  > ※ 無限ループには十分注意してください。

> ※ ここに挙げた構文は一例です。使用の際はABAP HELPを参照してください。

## 9. 演習３ 選択画面操作と分岐

- 問題  
  演習１の選択画面にラジオボタンを4つ付け足し、ラジオボタンの選択により和、差、積、商の計算結果を表示するようにプログラムを変更してください。
  なお商の場合のみ、小数点３桁まで表示します。

- ヒント  
  -ラジオボタンは、PARAMETERSのオプションRADIOBUTTERNを使用する
  - 小数点３桁はパック型で宣言し、DECIMALオプションを使用する
- 結果  

## 10. OPEN SQL

ABAPからデータベースへ発行されるクエリをOPEN SQLといいます。
OPEN SQLはシステムによってデータベース固有のSQLへと変換されるので、データベース製品を意識せずにクエリを発行することができます。そのため、ABAPへ直接ネイティブSQLを記述することができますが、通常は行いません。
> 詳細は HELP ネイティブSQL を参照

``` ABAP
* 基本構文 概ね標準SQLに沿ったものとなっています。
  SELECT  項目１ 項目名２‥
    INTO  変数名
    FROM  テーブル名
    WHERE  条件式
GROUP BY  グループ項目
  HAVING  集計項目への条件式
ORDER BY  ソート項目.

* 例
TYPES:
  BEGIN OF tst_materials ,            " 取得する項目を纏めた構造型
    matnr TYPE mara-matnr,              " 品目コード
    werks TYPE marc-werks,              " プラント
    maktx TYPE makt-maktx,              " 品目テキスト
  END OF tst_materials.

DATA:
  st_materials TYPE tst_materials.    " 上記で定義した構造型の構造変数

SELECT
  mara~matnr                          " 取得する項目
  marc~werks                          " 取得元のテーブルが複数ある場合は
  makt~maktx                          " 「データベース名~項目名」で記述
INTO CORRESPONDING FIELDS OF
  st_materials                        " 取得した値を受け取る変数
FROM
  mara  JOIN  marc                    " 取得元テーブル
        ON    mara~matnr = marc~matnr " テーブルの結合条件
        LEFT  OUTER JOIN makt         " 左外部結合
        ON    mara~matnr = makt~matnr
              AND makt~spras = sy-langu "sy-languはログオン言語
WHERE
  marc~werks = '1000'                 " プラント1000のデータのみ
.
WRITE :/ st_materials-matnr, "取得データを一件ずつ出力
  st_materials-werks,
  st_materials-maktx.
ENDSELECT. "結果セットに対してループされる
```

- SELECT SINGLE
  1件のみ取得したい場合に使用します。

  ``` ABAP
  * 構文 ※ ENDSELECT.は必要ありません
  SELECT SINGLE 項目
           INTO 変数
           FROM テーブル
          WHERE 条件.
  ```

  > WHERE句によって一意に絞り込まれない場合、どのデータが取得されるかは保証されません。

- SELECT INTO TABLE
  複数行のデータを取得して、内部テーブルへ代入する際に使用します。一旦内部テーブルにデータを格納すると、メモリ上で高速にデータ処理を行うことが可能になります。

  ``` ABAP
  * 構文 ※ ENDSELECT.は必要ありません
  SELECT 項目
    INTO CORRESPONDING FIELDS OF TABLE 内部テーブル
    FROM テーブル
   WHERE 条件.
  ```

- FOR ALL ENTRIES
  データベーステーブルと内部テーブルによる集合演算を行います

  ``` ABAP
  * 構文
  SELECT 項目１ 項目２
  INTO CORRESPONDING FIELDS OF TABLE 内部テーブル１
  FROM テーブル１
  FOR ALL ENTRIES IN 内部テーブル２
  WHERE 項目１ = 内部テーブル２-項目１ .
  テーブル１ ∩ 内部テーブル２
  ```

  > 内部テーブル２のデータが０件の場合、データは全件取得となり集合演算が狂います。
  > 内部テーブル２にデータが無い場合はSELECTを行わないようなコーディングが必要となります。
  > また、FOR ALL ENTRIES使用時、そのSELECT文は自動的にDISTINCTされます。

> SQLのパフォーマンスに関する一般的なセオリーは基本的に通用します。索引を効率よく使用するSQLを心がけてください。
> 操作手順は、「11_ABAP開発クイックスタートガイド_操作.xlsx」を参照ください。

## 11. リターンコード

各命令文やSQL文が成功したか失敗したかの実行結果が”0”や”4”といったコードで表されます。この実行結果をリターンコードといい、プログラムで自動生成される変数sy-subrcに格納されています。

``` ABAP
* 使用例
DATA : material_code TYPE mara-matnr .
SELECT SINGLE matnr
INTO material_code
FROM mara
WHERE matnr = 'ITEM001'.
IF sy-subrc = 0.
"正常に取得できた場合
ELSEIF sy-subrc = 4 .
"データが存在しない場合
ELSE .
"それ以外の異常な何かの場合
ENDIF.
```

> 各命令文のHELPにリターンコードのパターンが記載されています。

## 12. 演習４ 単表から１件のデータ取得と表示

- 問題
  CSKSより、１件の原価センタデータを取得して表示してください。
  - 選択パラメータ（全て必須項目とする）
    - `KOKRS` ：管理領域
    - `KOSTL` ：原価センタ
    - `DATE`  ：有効日
  - 表示項目
    - `KOKRS` ：管理領域
    - `KOSTL` ：原価センタ
    - `DATAB` ：有効開始日
    - `DATBI` ：有効終了日
    - `BUKRS` ：会社コード
    - `GSBER` ：事業領域
    - `PRCTR` ：利益センタ

    有効日は、その日に有効な原価センタを取得するために使用
    よって、SELECT文の有効日の条件は 有効開始日≦有効日≦有効終了日 とする
- ヒント
  - SELECT SINGLEで取得する
  - １行取得、１行表示のため、構造の宣言のみ（内部テーブル宣言は不要）
  - 表示はWRITEで行う
- 結果
  ＜選択画面＞
  ＜結果表示＞
- テストのポイント
  - [ ] 必須を入力しない場合、エラーとなり、メッセージが表示されるか？
  - [ ] 日付でない数値（2014/12/99など）を入力した場合、エラーとなりメッセージが表示されるか？
  - [ ] 指定された表示項目が表示されているか？
  - [ ] 指定した条件に見合ったデータが出力されているか？
  > 特に有効日（P_DATE）については、条件の閾値前後のデータが仕様通りに選択されるかを確認

## 13. 命令文 その３ 内部テーブル操作

- APPEND
  構造や内部テーブルの値を内部テーブルの末尾へ追加します。内部テーブルと構造の項目は同じである必要があります。

  ``` ABAP
  * 構文 構造の追加
  APPEND 構造 TO 追加先内部テーブル.

  * 内部テーブルの追加
  APPEND LINES OF 内部テーブル TO 追加先内部テーブル.
  ```

- INSERT
  条件式に従って、内部テーブルへデータを追加します

  ``` ABAP
  * 構文
  INSERT 構造 INTO 追加先内部テーブル 条件式.
  ```

- READ
  条件式に従って、内部テーブルから構造へデータを読み出します

  ``` ABAP
  * 構文
  READ TABLE 内部テーブル INTO 構造 条件式.
  ```

- READ WITH KEY
  内部テーブルの項目に条件指定することで必要なデータのみ読み出すことができます。

  ``` ABAP
  READ TABLE it_a INTO st_a WITH KEY a1 = 'A002'.
  内部テーブルの項目a1が'A002'であるデータを
  st_aへコピーする。
  一意検索で無い場合は最初に見つかったデータが
  コピーされます。
  ```

- READ WITH KEY BINARY SEARCH
  READ命令のオプションとしてBINARY SEARCHというものがあります。
  条件に合致したデータを二分検索するもので、WITH KEYだけよりも高速に検索が可能となります。

  ``` ABAP
  例 SORT it_a BY a1.
READ TABLE it_a INTO st_a WITH KEY a1 = 'A002' BINARY SEARCH.
読込む内部テーブルは後述するSORTにより、
WITH KEYで指定した条件によって
ソートされている必要があります。
指定条件通りにソートされていない場合、
読込エラーとなるので注意が必要です。
■ MODIFY 内部テーブルの行を編集します
MODIFY TABLE it_a <condition> . 条件式にマッチする行を変更します。
■ DELETE 内部テーブルの行を削除します
DELETE it_a <condition> . 条件式にマッチする行を削除します。
■ COLLECT 集計データを作成できます
■ SORT 内部テーブルを並び替えます
例 SORT it_a BY a1. 内部テーブルのデータをa1でソートします。
メモリ内で行われるため、高速です。
例 SORT it_a BY a1 ASCENDING ASCENDINGで昇順、DESCENDINGで降順を指定できます。
 a2 DESCENDING. 左記の場合a1を昇順、a2を降順でソートします。
（何も記載がない場合暗黙的に昇順となります。）
■ LOOP 内部テーブルのデータを一件ずつ処理します
LOOP AT it_a INTO st_a. １レコードずつst_aへコピーし、ループ内で処理します。
: 内部テーブルへデータが100件あれば、ループは100回
: 回ることになります。
ENDLOOP.
■ LOOP WHERE 内部テーブルのループへ条件式を適用できます
例 LOOP AT it_a INTO st_a WHERE a2 = 'XXX'.
: 内部テーブルの項目a2の値が'XXX'であるデータのみが
: ループします。
ENDLOOP.
■ CLEAR 内部テーブル/変数等のデータを初期化します
CLEAR it_a. 内部テーブルのデータをクリアします。
■ REFRESH 内部テーブルのデータを初期化します
REFRESH it_a. 内部テーブルのデータをクリアします。
SAPのバージョンアップにより廃止されつつあります。
基本的には上記CLEARを使用してください。
■ FREE 内部テーブルを破棄します
FREE it_a . 内部テーブルのメモリ割当を開放します。
■ INDEX
内部テーブルには索引(INDEX)と呼ばれる領域が設けられています。
it_a
index a1 a2 a3
1 A001 .. ..
2 A002 .. ..
3 A003 .. ..
索引を使って内部テーブルへアクセスすることが可能です。
例 READ TABLE it_a INTO st_a INDEX 1. 内部テーブルit_aのINDEX1の行をst_aへコピーする。
