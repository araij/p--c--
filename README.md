P--C-- Compiler
===============

疑似コードからフローチャートを生成します。

入力は P--C-- という言語で与えます。P--C-- の意味は Pseudo Code であり、P~~AS~~C~~AL~~でもあります（今はむしろ Ruby に似ていますが）。 出力は Dot 言語なので、Graphviz を使って画像化してください。

ビルド
------

### 必要なもの
- Glasgow Haskell Compiler（7.6.3 で動作確認）
- Parsec3（1.0.0.5 で動作確認）
- その他利用者の環境によっては追加のライブラリが必要になります

### ビルド方法

    $ make

使用例
------

例えば以下のような P--C-- ソースを `dijkstra.p-c` という名前で作成します。（コードは[ダイクストラ法 - Wikipedia](http://ja.wikipedia.org/wiki/%E3%83%80%E3%82%A4%E3%82%AF%E3%82%B9%E3%83%88%E3%83%A9%E6%B3%95)を元にしています）

    グラフ G = (V, E)，スタートとなる頂点 s，および各辺 e の長さ length(e) を入力として受け取る
    H ← ∅
    d(s) ← 0．s 以外の各 v ∈ V に対し，d(v) ← ∞
    各 v ∈ V に対し，w[v] ←「無し」
    while V - H ≠ ∅ do
	u ← d(v) が最少となる頂点
	(u, w[u]) を出力
	H ← H ∪ {u}
	V' ← u からの辺がある各 v ∈ V - H
	while V' にまだ訪ねていない頂点 v がある do
	    if d(v) > d(u) + length(u, v) then
		d(v) ← d(u) + length(u, v)
		w[v] ← u
	    end
	end
    end

make でできた実行可能ファイル `p-c` を使って Dot 言語ファイル dijkstra.dot へ変換します。

    ./p-c dijkstra.p-c

Graphviz で画像ファイル dijkstra.png に変換します。

    dot -Tpng -o dijkstra.png dijkstra.dot


