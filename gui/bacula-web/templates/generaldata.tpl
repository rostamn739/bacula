<div class="box">
	<p class="title">General informations</p>
<!--
<table class=genmed cellspacing="0" cellpadding="0" border=1 align="center" width=100%>
<tr>
  <td align=center class=tbl_medium background="images/bg3.png">
	General information
  </td>
</tr>
</table>
-->

<table class=genmed width="100%" cellspacing="1" cellpadding="3" border="0" align="center">
<tr>	
	<td width=35%>
	{t}Total clients:{/t}
	</td>

	<td>
	<font color=red>{$clientes_totales}</font>
	</td>

	<td width=35%>
	{t}Total bytes stored{/t}:
	</td>
	
	<td>
	<font color=red>{$bytes_stored}</font>
	</td>
</tr>

<tr>
	<td width=35%>
	{t}Total files:{/t}
	</td>
	
	<td>
	<font color=red>{$files_totales}</font>
	</td>
	
	<td width=35%>
	{t}Database size{/t}:
	</td>
	
	<td>
	<font color=red>{$database_size}</font>
	</td>
</tr>

<tr>
	<td colspan=2 align=center>
		<a href="javascript:OpenWin('index.php?pop_graph1=yes','600','400')">{t}Last month, bytes transferred{/t}</a>
	</td>
	<td colspan=2 align=center>
		<a href="javascript:OpenWin('index.php?pop.graph2=yes','600','400')">{t}Last month, bytes transferred (pie){/t}</a>
	</td>
</tr>
</table>
</div> <!-- end div box -->