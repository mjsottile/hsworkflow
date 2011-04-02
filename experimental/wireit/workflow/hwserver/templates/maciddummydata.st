<h3>Populate your job board with dummy data</h3>

<p>Assumng you are following along with the tutorial and running locally, you now have a job board with one user
   ("testuser") and no jobs,  unless you took the time to create additional test data manually.

<p>
When quickly hacking together a data-driven website, populating your app with dummy data is a chore you
want to avoid wasting time on, so let's see how to do that. 

<p>Firstly, restart Happstack in a pristine state by either deleting or backing up your _local data directory
   as described earler.

<p>Now, click <a href="/tutorial/initializedummydata">this link</a>.

<p>If everything worked right, you should now have two <a href="/tutorial/consultants">users</a> and a
   couple hundred <a href="/tutorial/jobs">jobs</a> worth of test data
   displaying in your jobs site.</p>
<p>What have we done here?</p>
<p>We've used the code found in <a href="/src/ControllerStressTests.hs">ControllerStressTests.hs</a> to take action
on the tutorial application state, which is to be found in <a href="/src/StateVersions/AppState1.hs">AppState1.hs</a>.
Now that you've had a basic introduction of using Happstack.State, I recommend looking through these files to get a
better feel for more complicated examples.</p>
<p>Now we need to discuss what to do when you need to <a href="/tutorial/macid-migration">change</a>
your data types.</p>

