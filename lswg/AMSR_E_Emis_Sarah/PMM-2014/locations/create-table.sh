
# creat html table to show the 20 locations 

# single-column format
echo "<table>"

for loc in `cat loc_table.txt |awk '{print \$1}'`; do
  echo "<tr><td align=\"center\"><b> Location: $loc</b></td></tr>"
  echo "<tr><td><a href=\"locations/$loc-eval-fits-trim.png\"><img src=\"locations/$loc-eval-fits-trim-sm.png\"></a></td></tr>"

done


echo "</table>"


exit



# 4-col format
echo "<table>"

echo "<tr>"
ic=0

for loc in `cat loc_table.txt |awk '{print \$1}'`; do 
  echo "<td><img src=\"locations/$loc-eval-fits-trim-sm.png\"</td>"
  let ic=ic+1
  echo "ic=$ic"
  if ((ic%4 == 0)); then 
    ic=0
    echo "</tr> <tr>"
  fi

done


echo "</tr></table>" 




