#!/bin/sh


db="zeit.db"

comments_dir="txt"

ids=`(sqlite3 -batch $db <<EOF
select id from comments;
EOF
)`

for i in $ids; do
    echo "Writing comment $i to $comments_dir/$i.txt";
    #query="select comment from comment where comment_id == $id;"
    sqlite3 -batch $db <<EOF > $comments_dir/$i.txt
select text from comments where id = '$i';
EOF
done
