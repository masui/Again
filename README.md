<h1>Again</h1>

<ul>
  <li><code>again.el</code>はEmacs上の操作を再実行するシステムです</li>
  <li>Dynamic Macroと異なり、同じ操作を二度繰り返す必要がありません</li>
</ul>

<h2>インストール</h2>

<ul>
  <li>`again.el`をEmacsのパスに置き、以下を`~/.emacs`に記述します</li>
  <pre>
   (require 'again)
   (defconst *again-key* "\C-l" "再実行指定キー")
   (global-set-key *again-key* 'exec-again)
  </pre>
</ul>

<h2>原理</h2>





