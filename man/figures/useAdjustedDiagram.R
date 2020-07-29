# Figure 1: Flow Chart of the action taken when a user applies
# useAdjusted(which=ALL) or useAdjusted(which=<param>)

library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, color=black]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab5 [label = '@@5']
      tab4 [label = '@@4']

      # edge definitions with the node IDs
      tab1 -> tab2 [label= 'Yes', fontname= Helvetica]
      tab2 -> tab3 [label= 'Yes', fontname= Helvetica]
      tab2 -> tab4 [label= 'No', fontname= Helvetica]
      tab3 -> tab4 [label= 'No', fontname= Helvetica]
      tab3 -> tab5 [label= 'Yes', fontname= Helvetica]
      tab1 -> tab5 [label= 'No', fontname= Helvetica]
    
      }

      [1]: 'Is there an adjusted data field?'
      [2]: 'Are all the <param> values NA?'
      [3]: 'Is fallback=TRUE?'
      [4]: '`[[` returns <param>Adjusted values'
      [5]: '`[[` returns <param> values'
      ")
