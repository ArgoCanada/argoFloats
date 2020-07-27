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
      tab10 [label = '@@10']
      tab11 [label = '@@11']


      # edge definitions with the node IDs
      tab1 -> tab2
      tab2 -> tab4 -> tab5
      tab1 -> tab3
      tab4 -> tab6
      tab5 -> tab7
      tab7 -> tab8
      tab7 -> tab9
      tab9 ->tab10
      tab8 -> tab11
      tab3 -> tab11
      tab6 -> tab10
      }

      [1]: 'Is there an adjusted data field?'
      [2]: 'Yes'
      [3]: 'No'
      [4]: 'Are the parameters all NA Values?'
      [5]: 'Yes'
      [6]: 'No'
      [7]: 'Is fallback=TRUE?'
      [8]: 'Yes'
      [9]: 'No'
      [10]: '`[[` returns <param>Adjusted values'
      [11]: '`[[` returns <param> values'
      ")

if (!interactive()) dev.off()

