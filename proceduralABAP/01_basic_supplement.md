# サブルーチンについて補足資料

サブルーチンの引数の扱い方、特にCHANGING変数が良く分からないと云う人が多かったため補足資料
内容は他言語との対応表。2022現在研修で用いているJavaのほか、事前に学習してる人が多そうな言語を羅列していますが、FORTRANを知っていればFORTRANを見るのが一番早いでしょう。

- ABAP

  ``` ABAP
  * any statement
  PERFORM add
      USING
        g_num1
        g_num2
      CHANGING
        g_num3.

  FORM add
      USING
        u_num1 TYPE i
        u_num2 TYPE i
      CHANGING
        c_num  TYPE i.

    c_sum = u_num1 + u_num2.
  ENDFORM.
  ```

- Java

  ``` Java
  public static void main(String[] args) {
    // any statement
    g_num3 = add(g_num1, g_num2);
  }

  static int add(int u_num1, int u_num2) {
    return u_num1 + u_num2;
  }
  ```

- Python

  ``` Python
  def main() -> None:
      # any statement
      g_num3 = add(g_num1, g_num2)


  def add(u_num1: int, u_num2: int) -> int:
      return u_num1 + u_num2
  # any statement
  ```

- Rust

  ``` Rust
  fn main() {
    // any statement
    g_num3 = add(g_num1, g_num2);
  }

  fn add(u_num1: i32, u_num2: i32) -> i32 {
    u_num1 + u_num2
  }
  ```

- FORTRAN

  ``` FORTRAN
  C any statement
        implicit none
  C any statement
        call add(g_num1, g_num2, g_num3)

        contains
          subroutine add(u_num1, u_num2, c_num)
            integer,intent(in) :: u_num1, u_num2
            integer,intent(inout) :: c_num

            c_num = u_num1 + u_num2
          end subroutine
  C any statement
  ```

おそらく理解しにくい理由は、他の言語であれば

``` txt
代入先の変数 ＝ 代入する値を返す関数
```

という形が基本形だが、ABAPの場合には代入先の変数も同時にサブルーチンへ渡し、サブルーチンの中で代入を行うという形であることによるものかと思われます。
