<h3>Macid stress test</h3>

<p>Happstack is a new, relatively unproven technology.

<p>So I asked myself this question.
   Will Happstack allow me to scale the toy job board I created for happs-tutorial 
   into a high-volume, high concurrency,
   large-user count type job board?
   You know, the kind that <a href=http://www.monster.com>might make money</a>?

$! <p>Like all the world's great businessmen, I then pulled some numbers out of my ass. !$
<p>What will make money in the real world is impossible to say until you try it, 
   but I attempted to come up with some not-completely-ridiculous estimates anyway. 
   I figure for a typical ad-supported web 2.0 type project, each user is worth \$0.50 cent/year.
   So if you have 100,000 users, you are probably not rolling in dough, but hopefully you can at least pay your rent.

<p>Therefore, once I had the basic state functionality working I decided to see if I could scale my toy job board
   up to 100,000 users. To make things more realistic, each user would have 200 jobs.
   So, this would be the equivalent of 20,100,000 data records if we were using an rdbms: 
   100,000 users and 20 million jobs.

<p>I failed to achieve this goal. 

$! Okay, so seriously, should this part actually stay in or not? !$

<p>Or, more optimistically,
   I am declaring defeat for the time being so I can put a version 5 release up on hackage
   and share what I learned with a wider audience. In a future release, I still intend  to show how you can scale 
   a Happstack app up to large numbers of users! :)

<p>Sidenote: as (I think) 37 Signals said, "you don't have a scaling problem." I certainly don't have a scaling problem,
   and if I did I would probably be jumping for joy even while I was tearing my hair out trying to figure out
   how to accomodate more users. My current plans are for a membership site with paying user count, optimistically,
   in the high thousands -- so I'm okay using macid even with all the caveats. I'm mainly working on the 
   user scaling problem because I find it interesting and am learning a lot; secondarily because I have ideas
   in the works that might actually run into the RAM limit. 

<p>That said, the most important lesson I learned is that putting all application state into macid won't work if you 
   have a lot of data -- not even if you have one of those <a href="http://aws.amazon.com/ec2/instance-types/">heavy iron</a>
   boxen from amazon web services, or for that matter a whole virtual data center full of them.
   16GB of RAM gets used up surprisingly fast if you are using ram for everything.
   The cloud is no substitute for
   <a href="http://highscalability.com/">thinking your application's transactional requirements through</a>
   and coding to them.
  
<p>So, if I make good on the promise I made above about scaling this toy application up to 20,000,000 data records, you 
   will be getting some future version of this tutorial where macid is being used
   in conjunction with disk based storage: either an rdbms or haskell data values serialized to disk, 
   I haven't decided yet.
   Assuming I can get this working, my intuition is that this solution will be 
   scale up to a lot more than my original goal of 100K users,
   and do so in a cloud computing-friendly way, so stay tuned.
<p>

<p>Another lesson learned is that the macid data structure can have a dramatic effect on application scaleability.
   In version 4 of happs-tutorial, I was using sets to store users and a linked list for jobs. Lookup of user/job values 
   required finding the first value in the set where the appropriate record field matched the desired criteria.
   I also had some quirk where modifying a job value resulted in writing the entire user (with all attached jobs)
   to state, which was resulting in huge serialization files. (Thanks to LemmiH on the happs group for diagnosing this.)
   When I switched to using maps for macid state,
   with UserName and JobName values as keys, and was more careful about serialization, I had dramatic performance gains.
   Not dramatic enough to get me up to 100,000 users, true -- but initially I was seeing my server slow 
   to a crawl after inserting a couple  hundred
   users, and now I can handle 10,000 users comfortably on an 8GB box. I suspect that the macid state I am using now 
   can be optimized even further -- though at some point, as I said, algorithmic finesse won't help anymore and 
   part of the state will have to be moved out of RAM and into hard disk.
   
<p>Finally, I created what appears to be the first macid stress test in the public domain. I actually created 3 tests, all of which are activated by clicking a link with an integer argument in the url, that specifies how many users to insert. In all cases, there are 200 jobs inserted for each user, the equivalent of 4200 records in a tradiational rdbms.

<p><font color=orange>
   Before running any of these stres tests yourself, I recommend limiting the amount of swap your happs server
   is allowed to use via the ulimit command. Otherwise stressing Happstack will result in your 
   computer grinding to a crawl.
   But if you limit your swap, the process just gets killed, which is a lot less annoying. 
   Since every macid action is serialized, you will still see the data your stress test
   inserted up to the point it was killed when you reestart Happstack. I have 

   <p>ulimit -v 262144 # (256M of swap)
 
   <p>set in .bashrc.</font>

 <p>If you click on <a href=/tutorial/stresstest/atomicinserts/20>stresstest/atomicinserts/20</a> the result is that 20 users 
    get inserted, each with 200 jobs: about 4000 macid transactions total.
    Each job insertion is its own event, so macid winds up having to do a lot of work.
    This is useful for seeing how effectively happs deals with getting hammered by insert transactions,
    but it's impractical for inserting large amounts of data.
 
 <p>If you click on <a href=/tutorial/stresstest/onebiginsert/20>stresstest/onebiginsert/20</a> 
    the effect is the same, 20 users and 4000 jobs get inserted, but the insertion is performed with one big
    macid transaction. Onebiginsert is faster than atomicinserts for small number of users, but it won't work 
    with 1000 users, all I got was a churning hard disk that eventually had to be kill -9ed.
    From this I conclude that large maps don't play well with serialization.

 <p>I found that what worked best in practice for inserting large quantities of data
    was <a href=/tutorial/stresstest/atomicinsertsalljobs/20>stresstest/atomicinsertsalljobs/20</a>.
    Here, there is one insert per user, but all jobs get inserted at once. 
    Using this method of insertion, I was able to insert 1000 users at a time on my junky old laptop,
    and 4000 users at a time on a heftier server with 8G of ram. On the hefty server I was able to get up to 
    20,000 users (4 million jobs) before performance degraded because of the amount of data being kept in ram.
    (With 10,000 users it ran without breaking a sweat, 20,000 was doable but went into swap.)

<p> Obviously this chapter of the happs-tutorial is in progress and, to some extent, in flux. There is a 
    thread about the stress tests in the <a href="http://groups.google.com/group/HAppS/browse_thread/thread/e5e8458edd34cde7/a3f0655bb66ecce2?lnk=gst&q=stress+testing#a3f0655bb66ecce2">
    happstack googlegroup</a> that you can have a look at
    for even more information. I also say a few more words about what macid isn't good for in 
    <a href="/tutorial/macid-limits">macid limits</a>.
