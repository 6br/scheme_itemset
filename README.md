# scheme_itemset
Item-set mining implement in scheme

## Language
* Scheme

## Environment
* Gauche scheme shell, version 0.9.4 [utf-8,pthreads], x86\_64-unknown-linux-gnu

		一部Gaucheでしか実装されていない機能を使っているので、Guileでは動作しない。

## File
* itemset.scm

## Usage
```bash
#gosh itemset.scm <input_file> <theta> <option> 
gosh itemset.scm retail_1based_500.txt 10  
gosh itemset.scm retail_1based_1000.txt 10  
gosh itemset.scm retail_1based_1000.txt 10 1
```

If option is 1, this program uses occ. You don't have to set option 0.(default: null)

## Optimization Techniques
### Recursive call to loop
Schemeでは、末尾再帰で関数を定義することで、末尾再帰の最適化と呼ばれるスタックを積まないループに変換してくれる実装が処理系に対して要請されている。

プログラムの実装においても、関数を再帰的に呼ぶのではなく、末尾再帰形で呼んでいるので、for文と同等の実行になっていると考えられる。

### Occurrence deliver
OCCを実装した。optionに1をつけると動作するが、出力される結果は頻出する値の組が入力した行単位で返されれるようになっているので、比較的データ点が少なく疎なデータに対しては必要ない要素も多く含まれる。
