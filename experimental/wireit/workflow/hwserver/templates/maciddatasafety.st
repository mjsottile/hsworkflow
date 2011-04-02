<h3>Keeping your MACID data safe</h3>

<p>If you are using php, ruby on rails, or one of the other popular web frameworks, your user data is likely
   in a mysql database. If you have outsourced your server hosting,
   maybe you have a database administrator that takes backups for you on a regular basis.
   That probably helps you sleep at night, assuming that you can really trust that your dba is doing their job.</p>

<p>As we learned in the previous lesson, if you are using Happstack with MACID, your data is 
   right there on your filesystem, by default in the directory called <i>_local</i>.

<p>~/happs-tutorial>ls _local/happs-tutorial_state/
<br>current-0000000000 events-0000000000 events-0000000001 events-0000000002        

<p>If there is money on the line, you are going to want to be careful with this directory.

<p>When <a href="/tutorial/macid-migration">migrating MACID data</a> to a new schema, you are also going to want to be extra cautious.

<p>For now, since you don't have any valuable data, the following procedure is probably enough
   to remind yourself to be careful while learning about Happstack in the tutorial sandbox.

<ul>
  <li>Stop Happstack by doing ctrl-c if you are running the ./happs-tutorial app from a shell
      or ctrl-c and completely exiting ghci if you are doing runInGhci within ghci.
  <li>~/happs-tutorial> mv _local _local.20081001-0917am.bak
  <li>Start the Happstack application again. All users, profiles, jobs, and sessions should be gone,
      and a new _local directory with nothing in it should have been created.
      A fresh start.
  <li>If you want your old data back, backup your existing _local directory somewhere safe
      (or just rm -rf if you want to get rid of it)
      , and rename the .bak directory back to _local
</ul>

The above procedure raises some questions.

<p>Q: Do you have to shut down the Happstack server every time you migrate data to a new schema?

<p>A: No, but online migrations are a topic that will be covered in a future chapter.</p>

<p>Q: Is MACID safe? Could I wake up one day with corrupted data under _local and no way to recover from it?
<p>A: Let's be realistic. 
      Compared to, say, mysql, Happstack hasn't been stress-tested much in critical high-volume web sites. 
      On the other hand, stress testing is on the docket for the Happstack team and when more data is known
      I'll be including it in this tutorial. </p>
<p> That said, the unix filesystem is pretty good at not losing your data -- 
    a point <a href="http://www.paulgraham.com/vwfaq.html">famously</a> made by startup guru Paul Graham,
    who created viaweb (now yahoo stores) with all the application state in flat files.</p>

<p>If you use Windows or Mac, you probably believe these filesystem are pretty reliable too.</p>

<p>Taking a closer look at what is under _local...</p>
<p>thartman@thartman-laptop:~/happs-tutorial/_local/happs-tutorial_state>ls -lth
<br>total 12K
<br>-rw-r--r-- 1 thartman thartman   0 Oct  1 13:55 events-0000000003
<br>-rw-r--r-- 1 thartman thartman   0 Oct  1 11:55 events-0000000002
<br>-rw-r--r-- 1 thartman thartman 792 Oct  1 11:04 events-0000000001
<br>-rw-r--r-- 1 thartman thartman 491 Oct  1 11:00 events-0000000000
<br>-rw-r--r-- 1 thartman thartman  25 Oct  1 10:59 current-0000000000
<br>thartman@thartman-laptop:~/happs-tutorial/_local/happs-tutorial_state></p>


<p> MACID serialization works by writing state change event data
    one file at a time. At server startup, Happstack "replays" all the information here
    in the order specified by the file names.
    This is similar to the <a href="http://en.wikipedia.org/wiki/Transaction_log">database transaction log</a>
    used by many rdbms systems.</p>
<p> So, if I woke up one morning with my Happstack application in a corrupt, non-startable state and 
    my inbox full of angry customer email, probably what I would do is move files, one at a time,
    out of the serialization directory, last-file created first, and keep trying to restart Happstack.</p>

<p> Q: What if my hard drive dies and I can't get my data back?</p>
<p>    A: Like with any other data storage system, if there's valuable data, you need to be making backups.
       In the case of Happstack data stored under _local, I would probably be <a href="http://www.rsync.net">rsyncing</a>
       the _local directory to a remote server, or maybe multiple remote servers for extra safety.
       For now I am not worried
       about securing data, but when that day comes I'm pretty confident I'll be ok.</p>
      
<p> Let's now populate our web application with <a href="/tutorial/macid-dummy-data">dummy data</a>.</p>