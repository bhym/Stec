#!/bin/sh
cp $1 input.tex

echo "Converting pdf images in pngs"
mogrify -format png imgs/*.pdf
echo "Adjusting the tex so that it points to png rather than pdfs"
sed -i  's/\.pdf/.png/g' input.tex

echo "Converting the tex document to a standalone output.docx"
pandoc input.tex --mathjax \
                 --bibliography=paper_integration.bib \
                 -o output.docx 

rm -f input.tex imgs/*.png
