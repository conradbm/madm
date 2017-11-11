<h2> Python Script to Automate Algorithms</h2>
<h3> Potential Execution </h3>
<hr>
<code> ./madm –f decisionmatrix.txt <–a topsis> <–v> <–s <-e variables_exaustive_0_100.txt | -ws variables_weights_by_range.txt>> </code>
<ul>
<li> <> Indicates optional </li>
<li> -f filename </li>
<li> -a algorithm (topsis or saw) (default topsis)</li>
<li> -v verbose (print whats happening)</li>
<li> -s sensitivity analysis
     <ul> 
          <li>-e exaustive, go through weights from 0 to 100 for each variable and flag when ranks change (dataset is excel ready for plots or plots included in a flag as a later feature) </li> 
          <li>-ws weight specific, there will be given a variable and its range, so V1 0 100 would look at the weight of V1 from 0 to 100 (by integers). This version assumes an equal split among the other variables.</li>
     </ul>
</li>

</ul>
