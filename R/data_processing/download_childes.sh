#wget -r -np -nH --no-parent -e robots=off -R "index.html*" http://childes.talkbank.org/access/Eng-NA/
#find . -name "*.zip" | while read filename; do unzip -o -d "`dirname "$filename"`" "$filename"; done;
#find . -type f -name '*.zip' -delete

## this script automates the download of raw chat files from CHILDES database
## code adapted from example here: http://childes.talkbank.org/data.html

## first, create empty dir to store the files  
cd ../../data/
mkdir raw_chat_files
cd raw_chat_files

## get zip files from CHILDES using wget
wget -r -np -nH -nd --no-parent -e robots=off -R "index.html*" -A '*.zip' http://childes.talkbank.org/data/Eng-NA/

## unzip and remove the original zip files
find . -name "*.zip" | while read filename; do unzip -o -d "`dirname "$filename"`" "$filename"; done;
find . -type f -name '*.zip' -delete