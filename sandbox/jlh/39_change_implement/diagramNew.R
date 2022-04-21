# Figure 1: Flow Chart of the action taken when a user applies
# useAdjusted(which=ALL)

if (!interactive()) png("diagram.png")

library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, fontsize=40]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      node [fontname = Helvetica, shape = circle, fontsize=40]        
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      node [fontname = Helvetica, shape = rectangle, fontsize=40]        
      tab5 [label = '@@5']
      node [fontname = Helvetica, shape = circle, fontsize=40]        
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      node [fontname = Helvetica, shape = rectangle, fontsize=40]        
      tab8 [label = '@@8']
      node [fontname = Helvetica, shape = circle, fontsize=40]        
      tab9 [label = '@@9']
      tab10 [label = '@@10']
      node [fontname = Helvetica, shape = rectangle, fontsize=40]        
      tab11 [label = '@@11']
      node [fontname = Helvetica, shape = circle, fontsize=40]        
      tab12 [label = '@@12']
      tab13 [label = '@@13']
      node [fontname = Helvetica, shape = rectangle, fontsize=40]        
      tab14 [label = '@@14']
      node [fontname = Helvetica, shape = circle, fontsize=40]        
      tab15 [label = '@@15']
      tab16 [label = '@@16']
      node [fontname = Helvetica, shape = circle, style = bold, color= red, fontcolor=red]        
      tab17 [label = '@@17']
      tab18 [label = '@@18']

      # edge definitions with the node IDs
      tab1 -> tab2
      tab2 -> tab3
      tab2 -> tab4
      tab4 -> tab5
      tab5 -> tab6
      tab5 -> tab7
      tab6 -> tab8
      tab8 -> tab9
      tab8 -> tab10
      tab10 -> tab11
      tab11 -> tab12
      tab11 -> tab13
      tab12 -> tab14
      tab14 -> tab15
      tab14 -> tab16
      tab15 -> tab18
      
      tab16 -> tab17
      tab3 -> tab18
      tab7 -> tab17
      tab9 -> tab18
      tab13 -> tab17
      
     



      }

      [1]: 'Should the developers try to implement a requested change?'
      [2]: 'Does the request already exist within the package?'
      [3]: 'Yes'
      [4]: 'No'
      [5]: 'Is the request an identified bug?'
      [6]: 'No'
      [7]: 'Yes'
      [8]: 'Is the requested change desirable?'
      [9]: 'No'
      [10]: 'Yes'
      [11]: 'Have many users requested this change?'
      [12]: 'No'
      [13]: 'Yes'
      [14]: 'Does the cost outweigh the benefit?'
      [15]: 'No'
      [16]: 'Yes'
      [17]: 'Yes'
      [18]: 'No'
      ")

if (!interactive()) dev.off()

