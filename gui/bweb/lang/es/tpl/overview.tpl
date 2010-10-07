<style type="text/css">
table.sample {
	border-width: 1px 1px 1px 1px;
	border-spacing: 3px;
	border-style: solid solid solid solid;
	border-color: black black black black;
	border-collapse: collapse;
	background-color: white;
}
table.sample th {
	border-width: 1px 1px 1px 1px;
	padding: 4px 4px 4px 4px;
	border-style: solid solid solid solid;
	border-color: gray gray gray gray;
	background-color: white;
}
table.sample td {
	border-width: 1px 1px 1px 1px;
	padding: 4px 4px 4px 4px;
	border-style: solid solid solid solid;
	border-color: gray gray gray gray;
	background-color: white;
}
</style>
 <div class='titlediv'>
  <h1 class='newstitle'> Jobs overview (<TMPL_VAR label>)</h1>
 </div>
 <div class='bodydiv'>
  <table class='sample' id='report'>
   <tr id='days'><td/>
  </table>
 </div>



<script type="text/javascript" language="JavaScript">

var table = document.getElementById('report');
var nodate = {};
var tr; var td; var img; var infos;
var all = new Array();
var dates = {};
var dates_pos = {};

<TMPL_LOOP items>
infos = new Array();
 <TMPL_LOOP events>
 dates['<TMPL_VAR date>']='<TMPL_VAR num>';
 infos['<TMPL_VAR date>'] = new Array('<TMPL_VAR num>', '<TMPL_VAR status>', 
                                      '<TMPL_VAR joberrors>', '<TMPL_VAR title>');
 </TMPL_LOOP>
all.push({ name: "<TMPL_VAR name>", values: infos});
</TMPL_LOOP>

//infos = new Array();
//infos['2007-10-01'] = new Array(1, 'T', 8, '2007-10-01');
//infos['2007-10-02'] = new Array(2, 'T', 8, '2007-10-02');
//infos['2007-10-05'] = new Array(3, 'R', 8, '2007-10-05');
//
//all.push({ name: "zog", values: infos});

function init_tab() // initialize the table
{
    var i=0;
    var step = new Array();
    // common steps in milliseconds
    step['day'] = 86400000;
    step['week'] = 604800000;
    step['month'] = 2678400000;
    var last_date;
    var current_date;

    // javascript can't do foreach $j (sort keys %dates)
    var keys = new Array();
    for (var j in dates) {
        console.log(j);
        keys.push(j);
    }
    keys.sort();

    for (var j in keys) {
       j = keys[j];                     // get real key
       if (!last_date) {                // do it once
          last_date = new Date(j.substr(0, 10));
       }
       // current date
       var current_date = new Date(j.substr(0, 10));

       // Try to find days where we have nothing reported
       if (step['<TMPL_VAR type>']) {
          last_date.setTime( last_date.getTime() + step['<TMPL_VAR type>'] * 1.25);
          while ( last_date < current_date ) {
             // Insert gap between current_date and last_date
             last_date.setTime( last_date.getTime() + step['<TMPL_VAR type>']);
             var t=document.createElement("TD");
             t.setAttribute("id", "day" + i++); // should not be used
             t.appendChild(document.createTextNode('?'));
             document.getElementById("days").appendChild(t);
          }
       }
       last_date = current_date;
       var t=document.createElement("TD");
       t.setAttribute("id", "day" + i);
       nodate[j]=1;
       dates_pos[j]=i++;        // position in the tab
       document.getElementById("days").appendChild(t);
    }
}

function add_client(name, infos)
{
    tr=document.createElement("TR"); // client row
    table.appendChild(tr);

    td=document.createElement("TD"); // client name
    tr.appendChild(td);
    a=document.createElement("A");
    a.setAttribute("href", "?action=<TMPL_VAR action>" + name);
    a.appendChild(document.createTextNode(name));
    td.appendChild(a);
    var cur_pos=0;

    for (var j in infos) { // one img for each days
        while (cur_pos <= dates_pos[j]) { // create empty blocks
           td=document.createElement("TD"); 
           tr.appendChild(td);
           cur_pos++;
        }
        if (nodate[j] == 1) { // put the date in the first row if empty
           var t = document.getElementById("day" + dates_pos[j]);
	   t.appendChild(document.createTextNode(infos[j][0]));
	   nodate[j]=0;
        }
//	a=document.createElement("A"); // create a link to action=job
//	a.setAttribute('href', "?action=job;client_group=" + name);
        img=document.createElement("IMG");
        img.setAttribute("src", bweb_get_job_img(infos[j][1],infos[j][2], 'B'));
        img.setAttribute("title", infos[j][3]);
//	a.appendChild(img);
        td.appendChild(img);        
    }
}

init_tab();

for(var i=0; i<all.length; i++) {
   var elt = all[i];
   add_client(elt['name'], elt['values']);
}

</script>
