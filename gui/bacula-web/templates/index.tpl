<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" 
  "http://www.w3.org/TR/html4/loose.dtd">
<html lang="en">
<head>
<title>bacula-web</title>
<link rel="stylesheet" type="text/css" href="style/default.css">
{literal}
<script type="text/javascript">
	function OpenWin(URL,wid,hei) {
		window.open(URL,"window1","width="+wid+",height="+hei+",scrollbars=yes,menubar=no,location=no,resizable=no")
	}
</script>
{/literal}

</head>
<body>
{popup_init src='./js/overlib.js'}
{include file=header.tpl}

<div id="main_left">
{include file=volumes.tpl}
</div>

<div id="main_right">
  <!-- General information -->
  <div class="box">
	<p class="title">General informations</p>
	<table width="90%">
	  <tr>
	    <td class="label">{t}Clients{/t}</td> <td class="info">{$clientes_totales}</td>
	  </tr>
	  <tr>
		<td class="label">{t}Total bytes stored{/t}:</td> <td class="info">{$bytes_stored}</td>
	  </tr>
	  <tr>
		<td class="label">{t}Total files:{/t}</td> <td class="info">{$files_totales} file(s)</td>
	  </tr>
	  <tr>
		<td class="label">{t}Database size{/t}:</td> <td class="info">{$database_size}</td>
	  </tr>
	  <tr>
		<td colspan=2 align=center>
		  <a href="javascript:OpenWin('index.php?pop_graph1=yes','600','400')">{t}Last month, bytes transferred{/t}</a>
		</td>
	  </tr>
	  <tr>
		<td colspan=2 align=center>
		  <a href="javascript:OpenWin('index.php?pop.graph2=yes','600','400')">{t}Last month, bytes transferred (pie){/t}</a>
		</td>
	  </tr>
	</table>
  </div>
	
  {include file="$last_report"} 	
  
  <div class="box">
	<p class="title">General report</p>
	{if $server==""} 
	  <img src="stats.php?server={$server}&amp;tipo_dato=69&amp;title=General%20report&amp;modo_graph=bars&amp;sizex=420&amp;sizey=250&amp;MBottom=20&amp;legend=1" alt="" />
	{else}
	  <img src="stats.php?server={$server}&amp;tipo_dato=3&amp;title={$server}&amp;modo_graph=bars" alt="" />
	{/if}
  </div> <!-- end div box -->

</div> <!-- end div main_right -->

{include file="footer.tpl"}