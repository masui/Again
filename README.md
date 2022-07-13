<h1>again.el</h1>

<ul>
  <li><code>again.el</code>はEmacs上のさまざまな操作を再実行するシステムです</li>
  <li><a href="https://scrapbox.io/masui/Dynamic_Macro">Dynamic Macro</a>(<code>ndmacro.el</code>)と組み合わせて使います</li>
</ul>

<h2>使い方</h2>

<ul>
  <li>Emacs上で <img src="again.png" height=20> を押すと、最近実行したキー操作が再実行されます</li>
  <li>「最近実行されたキー操作」とは、2秒間キー操作が無かった時点から現在までのキー操作です</li>
  <li>2秒間キー操作をしなかった後で「abc」と入力し、すかさず <img src="again.png" height=20> を押すと再度「abc」が入力されます</li>
  <li>2秒待ちたくない場合はCtrl-Lを入力します</li>
  <li>同じ操作を二度繰り返した後で <img src="again.png" height=20> を押すと、繰り返されたキー操作が再実行されます (DynamicMacro機能)</li>
</ul>

<h2>利用例</h2>

<ul>
  <li>単純操作を繰り返す例と、Search&amp;Replaceを繰り返す例です</li>
</ul>

<a href="https://s3-ap-northeast-1.amazonaws.com/masui.org/8/5/85a450002fe3d72f26378b84a9dce40d.mp4">音声入り解説動画</a>
<br>
<img src=again.gif>


<h2>インストール</h2>

<ul>
  <li><code>again.el</code>と<code>ndmacro.el</code>をEmacsのライブラリのパスに置き、
    以下を<code>~/.emacs</code>に記述します</li>
  <li><code>ndmacro.el</code>は<a href="https://github.com/snj14/ndmacro.el">snj14さんのGitHub</a>のものです</li>
  <li>これはCtrl-Tを <img src="again.png" height=20> に指定した例です</li>
  <pre>
   (require 'ndmacro)
   (require 'again)
   (defconst *again-key* "\C-t" "再実行キー") ;; Ctrl-tを再実行キーにする場合
   (global-set-key *again-key* 'exec-again)  </pre>
</ul>

<h2>原理</h2>

<ul>
  <li>しばらくユーザ操作が無かったとき、その時点でのキー操作履歴を<code>*old-history*</code>という変数に格納します</li>
  <li>ユーザが何かを操作してから<code>*again-key*</code>を押すと、そのときの操作履歴を<code>*recent-history*</code>という変数に格納します</li>
  <li><code>*recent-history*</code>と<code>*old-history*</code>の差分が新しい操作だと考えられるので、これをキーボードマクロとして登録して実行します</li>
  <li>再度<code>*again-key*</code>が押されたときは、以前登録したマクロを再実行します</li>
  <li>同じ操作が二度繰り返されていたときはDynamic Macroと同じ動作をします</li>
</ul>

<h2>考察</h2>

<ul>
  <li>動的にマクロを生成するという意味では<code>again.el</code>もDynamicMacroの一種といえるかもしれない</li>
  <li>しかし繰り返し操作にもとづくものではないので、どう扱うべきなのだろう...</li>
  <li>Ctrl-Lを押すのは面倒だろうか? 慣れれば問題ないか? </li>
</li>
