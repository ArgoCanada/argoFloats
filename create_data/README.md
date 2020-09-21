This README.md provides the steps to insert a built in data set. To date (July 22, 2020), `argoFloats` has index, indexBgc, indexSynthetic, and indexDeep.

# Step one : Decide the area of interest 
In this step the creators of `argoFloats` aims to have a few hundred floats.

# Step two : Create create_index file
The create_index R file goes under the folder argoFloats/create_data. A good way to do this is to copy the text from a previous index R file, an change the appropriate parameters.

# Step three : Update Makefile
For `argoFloats`, the Makefile that needs adjustment can be found under argoFloats/create_data. This is a two step process:

Firstly, add the following text to the Makefile:

```
indexDeep:force
<TAB>Rscript create_indexBgc.R > create_indexBgc.out
```
and then add the following:

```
install_indexDeep:
<TAB>cp indexDeep.rda ../data
```
# Step four: Update AllClass.R
The next step is to update AllClass.R. The best approach is to follow the format of the other indices. 

# Step five: Update create_index.R file
This is a one time step. In this add `!` before the FALSE in `if (FALSE) {`, and run the code. 

# Step six: Make the .rda
To do this, in the terminal, if the name of the index was indexDeep, for example, the user would type ```make indexDeep```

# Step seven: Install the index
To do this, in the terminal do `make install_indexDeep` if the name of the index is indexDeep, for example.

# Step eight: Update create_index.R
Remove the `!` before the FALSE in `if (FALSE) {`, and save. 