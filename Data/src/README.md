#extract.cpp

This file requires the input of the name list of all folders in the csv folder, which can be obtained by running the following.

```
find csv/. -maxdepth 1 -type d | cut -c 7- > list.txt
```

Make sure the list is put in the same directory as extract.cpp.