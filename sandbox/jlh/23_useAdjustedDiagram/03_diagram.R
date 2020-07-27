# Figure 1: Flow Chart of the action taken when a user applies
# useAdjusted(which=ALL)

if (!interactive()) png("diagram.png")

library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, color=red]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab5 [label = '@@5']
      node [fontname = Helvetica, shape = rectangle, color=black]
      tab4 [label = '@@4']

      # edge definitions with the node IDs
      tab1 -> tab2 [label= 'Yes', fontname= Helvetic]
      tab2 -> tab3 [label= 'Yes', fontname= Helvetic]
      tab2 -> tab4 [label= 'No', fontname= Helvetic]
      tab3 -> tab4 [label= 'No', fontname= Helvetic]
      tab3 -> tab5 [label= 'Yes', fontname= Helvetic]
      tab1 -> tab5 [label= 'No', fontname= Helvetic]
    
      }

      [1]: 'Is there an adjusted data field?'
      [2]: 'Are the parameters all NA Values?'
      [3]: 'Is fallback=TRUE?'
      [4]: '`[[` returns <param>Adjusted values'
      [5]: '`[[` returns <param> values'
      ")

if (!interactive()) dev.off()

