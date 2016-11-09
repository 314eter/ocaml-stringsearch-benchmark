#!/usr/bin/env sh

curl "http://www.gutenberg.org/cache/epub/4078/pg4078.txt" | tr "[:space:]" " " | tr -s " " > doriangray.txt

curl "ftp://ftp.ncbi.nlm.nih.gov/genomes/Homo_sapiens/CHR_01/hs_ref_GRCh38.p7_chr1.fa.gz" | gunzip | tail -n +2 | tr -d '\n' > dna.txt

curl -d "&pages=$(cat wikipages.txt)" "https://en.wikipedia.org/w/index.php?title=Special:Export" | tr -cd "[:alnum:][:space:]" | tr "[:upper:]" "[:lower:]" | tr "[:space:]" " " | tr -s " " > wikipedia.txt
