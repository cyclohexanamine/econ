for /R ..\src %%f in (*.lhs) do runhaskell stripc.hs %%f | pandoc -f latex+lhs -o pdf/%%~nf.pdf --listings -H listings.tex
cd ..\src
haddock Grid.hs -o ..\doc\haddock --html
cd ..\doc