<h3>What can't you do with macid?</h3>


<p>Keeping everything in macid limits you to how much RAM you can afford.
   Even if you have a business model where you can afford a lot (16GB in the amazon cloud costs \$576/month) 
   there's no guarantee that you won't
   max that out if your application has a lot of data, if you are limited to one computer.</p>
   
<p>(See the <a href=/tutorial/macid-stress-test>stress test</a> chapter for more caveats.)
   The Happstack core developers have promised Happstack features
   that will make it easy to share application state
   across many computers, making scaling to ebay-sized proportions relatively straightforward: you 
   just add more computers to your amazon EC2 cloud.</p>


<p>My take is that, as currently implemented, Macid may be impractical for an app with tens of thousands of 
   concurrent sessions, especially if real acid transactionality is required. (EG, an accounting application.)
   The situation improves if you have large numbers of users but don't require transactionality. 
   (E.G, facebook, reddit, message boards.)
   This is true for web apps with a database backend as well, for more or less similar reasons.

<p>A realistic way to use Happstack with macid is to write an alpha version of an application 
   using macid (no database), 
   and then add some other type of persistent hard drive storage (probably database)
   outside of macid only if it becomes necessary. 

<p>This raises the question: if you are eventually going to have to put in a database back end, why use 
   macid at all?


<p>The -- perhaps slightly depressing answer -- is that you probably won't have to put in a database back end, 
   because your app won't be so successful that this is required. If your state needs transcend what
   macid can deliver with reasonable performance, you will have
   to rewrite a lot of your state code, but it will be worth it, because venture capitalists will be 
   knocking down your door. (You're the next facebook, ebay, etc.) 

<p>In the best case scenario, you can use Multimaster to spread your application across multiple machines and
   not even have to worry about a state rewrite.

 
<p>Since macid is available and macid is more straightforward to use than a database layer,
   coding in a database from a start is a form of insidious premature optimization, if you buy
   the argument that using a database from the start introduces significant maintenance overhead.

<!--
<p>Having made this lengthy argument for macid... I can see myself rewriting a future tutorial chapter
   that describes using HAppS with takusen and postresql as a backend rather than macid. But that hasn't 
   happened yet. If it needs to, it will. &nbsp; :) &nbsp;
-->

<hr>

<p>Well folks, that more or less wraps up the <a href="http://tutorial.happstack.com">happstack tutorial</a>.

<p>You can now go out and use happstack, using happs-tutorial as a template to get you started if appropriate.

<p>The remaining chapters are of an appendix nature. Nice to have, but not fundamental.

<p>If you want to keep reading, the next chapter is about using <a href=/tutorial/foreignchars>utf8</a> in Happstack.</p>
