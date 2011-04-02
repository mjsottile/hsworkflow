WireIt Workflow GUI

Contact: Matt Sottile (matt@cs.uoregon.edu)
         Dan Keith (dkeith@cs.uoregon.edu)
         Geoff Hulette (ghulette@cs.uoregon.edu)
         Daniel Mundra (dmundra@cs.uoregon.edu)
         
==============================================================================
                                   SUMMARY
==============================================================================

We are using WireIt to create two GUIs for WOOL.

* GUI1: Based on Dan Keith's WOOL Model - WOOL_README.txt
* GUI2: Based on Matt Sottile and Geoff Hulette's Stream Model 
	- STREAM_README.txt

The GUI generates haskell code that is then executed server-side with the 
output displayed in a popup.

**WARNING** This is a preview release. It is highly likely that you will find
bugs or unsupported features. If you do find bugs, please report them!

==============================================================================
                                 INSTALLATION
==============================================================================

Our GUI are a bunch script files that are executed by the WireIt Library.

Minimum Requirements:

Browser with javascript capabilities. This is enough to run WireIt and our
GUI to draw graphs and generate haskell code.

Typical Requirements:

To save drawn graphs, execute haskell code on the server you will need to have
Apache server, MySql, PHP5, Haskell GHC 6.10.1 or greater, Haskell Cabal

Haskell GHC:

Download and install from http://www.haskell.org/ghc/

Edit the ghc path in run.php and run-unix.php located in stream/ & workflow/

Haskell Cabal:

Download and install from http://www.haskell.org/cabal/

Use cabal to install packages that some of the haskell code requires. For
both Windows and Unix

> cabal update
> cabal install <package-name> --global

Apache, MySql, PHP5:

Windows: Install WAMP Server: http://www.wampserver.com/en/
Easiest way to get all three installed and configured. Place this gui in the
www/ folder for the wamp and run wamp server. To view the gui in the browser
navigate to http://localhost/navigate/to/gui. For unix run this command on 
the gui to allow for code generation and execution.

$ chmod -R 766 gui/

Unix/Linux: For most versions of Unix/Linux you can just run

$ sudo apt-get install apache2 php5-mysql libapache2-mod-php5 mysql-server

For Ubuntu versions you can do this instead to install the base version

$ sudo tasksel install lamp-server

You don't need to configure anything after install. To test that apache, php,
mysql are configure correctly create a file called test.php and put in this
line of code

<?php phpinfo(); ?>

Use your browser to navigate to the file and you can then search through the
file for php5 and mysql, if there are not there that means they weren't
installed correctly.

Above steps taken from: https://help.ubuntu.com/community/ApacheMySQLPHP

Database setup:

Create a user and database for the gui and navigate to gui/backend/php.
Execute mysql command on WiringEditor.sql to create the required tables

> mysql <dbname> -u <username> -p < WiringEditor.sql

Edit WiringEditor.php and fill in, dbhost, dbuser, dbpass, dbname to
the one you have setup.

WireIt supports other backend connections, refer to this guide:
http://javascript.neyric.com/wireit/guide.html#adapters

Maximum Requirements:

Using the debug features available in the WOOL GUI. Requires HAppstack
HAppstack-helpers. **WARNING** HAppstack-helpers currently works only
on unix system, it is possible to do it on windows with cygwin but we haven't
tested that. Right debug only works on unix systems.

Use cabal to install HAppstack and HAppstack-helpers. Cabal should install
all the dependencies required.

$ cabal install happstack --global
$ cabal install happstack-helpers --global

==============================================================================
                                    USAGE
==============================================================================

Open browser and point it to wireit/index.html.

Refer to:
WOOL_README.txt
STREAM_README.txt

==============================================================================
                                   WIREIT
==============================================================================

WireIt README

WireIt is an open-source javascript library to create web wirable interfaces 
for dataflow applications, visual programming languages, graphical modeling, 
or graph editors.

Project home page:
    http://javascript.neyric.com/wireit/

WireIt guide:
	http://javascript.neyric.com/wireit/guide.html

The code for Wireit is provided under a BSD license:
    http://javascript.neyric.com/wireit/license.txt
    
Source code:
    http://github.com/neyric/wireit

Issue tracker:
	 http://github.com/neyric/wireit/issues
	
Blog: 
    http://javascript.neyric.com/blog/category/wireit/

==============================================================================
                               UPDATING WIREIT
==============================================================================

WireIt README

Project home page:
    http://javascript.neyric.com/wireit/
    
Refer to WireIt Project home for updates. Download the updated WireIt and
copy over workflow/ stream and backend/php/WiringEditor.php to the new
WireIt directory. Follow any new database installation requirements from
WireIt. Refer to examples/jsBox/jsBox.* to see if there is any changes
made to the way a wireit instance is created and change workflow/workflow.*
and stream/stream.* accordingly.

==============================================================================
                                   LICENSE
==============================================================================

Refer to license.txt

 