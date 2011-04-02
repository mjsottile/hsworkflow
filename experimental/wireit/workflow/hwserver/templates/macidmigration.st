<h3>Macid migration</h3>

<p>What happens when your data model changes?</p>
  
<p>This isn't a problem for tutorial.happstack.com, because typically there are only a 
   few dozen users and jobs, plus whatever dummy data I've entered myself. Who cares?
   So far, rather than migrating, I've just wiped the slate clean.</p>

<p>However, that isn't going to work for your latest facebook-killer.  Instead, you need to use Happstack's 
migration machinery.  Migrating is actually far simpler than one might expect.  There are really only two things
you need to understand to make it work:  the Migrate and Version type classes.</p>
<p>ghci>:m + Happstack.Data
<br>ghci>:i Migrate
<br>class Migrate a b where migrate :: a -> b
<br>  	-- Defined in Happstack.Data.Migrate
<br>ghci>:i Version
<br>class Version a where mode :: Mode a
<br>  	-- Defined in Happstack.Data.Serialize
<br>ghci>:i Mode
<br>data Mode a
<br>  = Primitive | Versioned (Happstack.Data.Serialize.VersionId a) (Maybe (Happstack.Data.Serialize.Previous a))
<br>  	-- Defined in Happstack.Data
</p>
<p>Now, you've already seen in previous Happstack.State examples that we've been relying on the default instance of
Version.  It defines the given type as being of version number 0 and having no previous version.  This is good enough
for your first version of application state.</p>
<p>The basic intuition is that if you want one data type to be converted to another in a new version, it needs to be
reflected in an instance of Migrate, with the actual conversion function between the two types, and in the Version instance
of the new state type.  You really don't need to do a lot more than this in order to have your migration working correctly.</p>
<p>Try taking a look at the code in <a href="/projectroot/src/migrationexample">src/migrationexample</a> in order to get a feel for how
to practically make migrations work.</p>
<ol>
<li>Read over the comments in the migrationexample files</li>
<li>runghc CreateState1.hs</li>
<li>runghc MigrateToState2.hs</li>
</ol>