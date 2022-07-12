<h1>Again</h1>

<ul>
  <li><code>again.el</code>はEmacs上の操作を再実行するシステムです</li>
  <li>Dynamic Macroと異なり、同じ操作を二度繰り返す必要がありません</li>
</ul>

<h2>インストール</h2>

<ul>
  <li><code>again.el</code>をEmacsのライブラリのパスに置き、
    以下を<code>~/.emacs</code>に記述します</li>
  <pre>
   (require 'again)
   (defconst *again-key* "\C-l" "再実行指定キー") ;; Ctrl-Lを再実行キーにする場合
   (global-set-key *again-key* 'exec-again)  </pre>
</ul>

<h2>原理</h2>

<ul>
  <li>しばらく操作が無かった場合、それまでのキー操作履歴を<code>*old-recent*</code>という変数に格納します</li>
  <li>ユーザが何かを操作してから<code>*again-key*</code>を押すと、そのときの操作履歴を<code>*new-recent*</code>という変数に格納します</li>
  <li><code>*new-recent*</code>と<code>*old-recent*</code>の差分が新しい操作だと考えられるので、これをキーボードマクロとして登録して実行します</li>
  <li>再度<code>*again-key*</code>が押されたときは、以前登録したマクロを再実行します</li>
  <li></li>
  <li></li>
  <li></li>
</ul>




