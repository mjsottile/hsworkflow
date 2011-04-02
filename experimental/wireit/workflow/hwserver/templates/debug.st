<h3>Debug</h3>

<p>
    <form action="/tutorial/step" method="post">
    <table>
	<tr><td># Ticks:</td><td><input type="textfield" name="numticks"/></td></tr>

	<tr><td><input type="submit" name="step" value="Step"></td></tr>
    </table>
    </form>
    <font color=green>
      $ statusmsgStep $
    </font>
    <font color=red>
      $ errormsgStep $
    </font>
</p>


<p>
    <table>
	<tr><td>Ticks:</td><td>$ stateTick $</td></tr>
	<tr><td>Debug:</td><td>$ stateDebug $</td></tr>
	<tr><td>Output:</td><td>$ stateOutput $</td></tr>
    </table>

    <h4>Pipes</h4>
    $ statePipes $
    
    <h4>Activities</h4>
    $ stateActivities $

</p>
