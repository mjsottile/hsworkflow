<style>
.tbody {display: none;}
</style>

<script Language="JavaScript">
    var current = 1;

    function move( byWhat )
    {
	current += byWhat;
	if ( current < 1 ) current = 2;
	if ( current > 2 ) current = 1;
	show( current );
    }

    function show( nwhich )
    {
	current = nwhich;
	for ( var dnum = 1; dnum <= 2; ++dnum )
	{
	    var dv = document.getElementById("BODY"+dnum);
	    dv.style.display = ( dnum == nwhich ? 'block' : 'none' );
	}
    }
</script>

<DIV ID="BODY1" class="tbody" style="display: block;">
    <div id="login">
    <form action="/tutorial/actions/login" method="post">
      <table>
	<tr><td>Username:</td> <td><input type="textfield" name="username"/></td> </tr>
 	
	<tr> <td>Password:</td> <td><input type="password" name="password"/></td> </tr>

	<tr> <td> <input type="submit" name="create" value="Log in"> </td>
	     <td> <a href="#" onClick="move(1);">register</a></td>
	</tr>
      </table>
    </form>
    <font color=red>
      $ loginerrormsg $
    </font>    
    </div>
</DIV>

<DIV ID="BODY2" class="tbody">
    <form action="/tutorial/actions/newuser" method="post"><table>
	<tr><td>Username:</td><td><input type="textfield" name="username"/></td></tr>
	
	<tr><td>Password:</td><td><input type="password" name="password"/></td></tr>

	<tr><td>Verify Password:</td><td><input type="password" name="password2"/></td></tr>

	<tr><td><input type="submit" name="create" value="Create Account"></td>
            <td><a href="#" onClick="move(1);">login</a></td>
        </tr>
    </table></form>
    <font color=red>
      $ errormsgRegister $
    </font>
    
</DIV>





