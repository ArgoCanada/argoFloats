# Figure 1: Flow Chart of the action taken when a user applies
# useAdjusted(which=ALL)

if (!interactive()) png("diagram.png")

library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      tab8 [label = '@@8']
      tab9 [label = '@@9']


      # edge definitions with the node IDs
      tab1 -> tab2;
      tab1 -> tab3;
      tab2 -> tab4 -> tab5
      tab4 -> tab6
      tab5 -> tab7
      tab6 -> tab8
      tab3 -> tab9
      }

      [1]: 'Is there an adjusted data field?'
      [2]: 'Yes'
      [3]: 'No'
      [4]: 'Are the parameters all NA Values?'
      [5]: 'Yes'
      [6]: 'No'
      [7]: '`[[` returns <param> values'
      [8]: '`[[` returns <param>Adjusted values'
      [9]: '`[[` returns <param> values'
      ")

if (!interactive()) dev.off()

