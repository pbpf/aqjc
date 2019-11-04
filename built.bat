mkdir doc
cd doc
scribble   ../src/document/index.scrbl
cd ..
raco exe  --gui  -o aqjc.exe  --ico src/res/airplane.ico src/login.rkt
raco exe  --gui -o tool.exe --ico src/res/tools.ico src/init.rkt 
mkdir aqjc
raco distribute aqjc aqjc.exe tool.exe
del *.exe