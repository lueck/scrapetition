#!/bin/sh

USAGE="$0 -- generate a html file from comments.
[ -h | --help		Show this help.
[ --database FILENAME ]	Specify database file.
[ -w | --where WHERE ] 	Where clause.
[ -H | --no-header ] 	Do not print meta data on comments.

WHERE must be a valid sqlite3 WHERE clause. Defaults to 1, which
selects all comments. The file repair.sed must be present in this
directory, too.


Examples:

$0 -w \"thread = 457\" > t457.html
will put all comments in thread 457 into the file t457.html.

$0 > all.html
will dump the whole database into file all.html.

$0 -w \"name LIKE '%Uwe%'\" > auUwe.html
will dump all comments by Uwe into the file auUwe.html.

$0 -w \"comment IN (\$(./subthread.sh 569720))\"
will dump all comments in the subthread started with comment 569720.
"

die()
{
    echo "$1" >&$2
    exit $3
}


# Default values
db="data.db"
table="comments"
column="comment"
where="1" ## "TRUE"
suffix="xml"
header="t"

# parse command line parameters
while true; do 
    case "$1" in
	-h | --help)
	    die "$USAGE" 1 0 ;;
	--database)
	    if test ! "$2"; then
		die "$USAGE" 2 1 ;
	    else		
		$db=$2
		shift
	    fi ;;
	-w | --where)
	    if test ! "$2"; then
		die "$USAGE" 2 1 ;
	    else
		where=$2
		shift
	    fi ;;
	-H | --no-header)
	    header="f" ;;
	-*)
	    die "$USAGE" 2 1 ;;
	*)
	    break ;;
    esac
    shift
done

ids=$(sqlite3 -batch $db <<EOF
-- recursively assign thread ID and calculate the height
WITH RECURSIVE
     dis (id, domain, text, title, user, name, date_informal, date, parent, thread, up_votes, down_votes, url, scrape_url, scraper, height) AS (
     SELECT id, domain, text, title, user, name, date_informal, date, parent, id, up_votes, down_votes, url, scrape_date, scraper, 0 FROM comments WHERE parent is NULL
     UNION
     SELECT comments.id, comments.domain, comments.text, comments.title, comments.user, comments.name, comments.date_informal, comments.date, comments.parent, dis.thread, comments.up_votes, comments.down_votes, comments.url, comments.scrape_date, comments.scraper, dis.height+1
     FROM comments, dis WHERE comments.parent = dis.id)
SELECT id FROM dis WHERE $where;
EOF
   );
echo "<html><head><meta charset=\"UTF-8\"></head><body><div class=\"container\">"
for i in $ids; do
    echo "<div id=\"$i\" class=\"comment\">";
    meta=$(sqlite3 -batch $db <<EOF
WITH RECURSIVE
     dis (id, domain, text, title, user, name, date_informal, date, parent, thread, up_votes, down_votes, url, scrape_url, scraper, height) AS (
     SELECT id, domain, text, title, user, name, date_informal, date, parent, id, up_votes, down_votes, url, scrape_date, scraper, 0 FROM comments WHERE parent is NULL
     UNION
     SELECT comments.id, comments.domain, comments.text, comments.title, comments.user, comments.name, comments.date_informal, comments.date, comments.parent, dis.thread, comments.up_votes, comments.down_votes, comments.url, comments.scrape_date, comments.scraper, dis.height+1
     FROM comments, dis WHERE comments.parent = dis.id)
SELECT * FROM dis WHERE id = '$i';
EOF
	);
    if [ "$header" = "t" ]; then
       echo "<div class=\"meta\">"
       echo "ID: $(echo $meta | awk -F '\\|' '{print $1}')<br/>";
       threadid=$(echo $meta | awk -F '\\|' '{print $10}')
       echo "Thread-ID: <a href=\"#$threadid\">$threadid</a><br/>";
       parentid=$(echo $meta | awk -F '\\|' '{print $9}')
       echo "Parent-ID: <a href=\"#$parentid\">$parentid</a><br/>";
       informaldate=$(echo $meta | awk -F '\\|' '{print $7}' );
       echo "Date: $informaldate<br/>";
       timestamp=$(echo $meta | awk -F '\\|' '{print $8}' );
       # date=$(date -d @$timestamp);
       # echo "Date: $date<br/>";
       echo "Author: $(echo $meta | awk -F '\\|' '{print $6}')<br/>";
       echo "Title: $(echo $meta | awk -F '\\|' '{print $4}')<br/>";
       echo "Up Votes: $(echo $meta | awk -F '\\|' '{print $11}')<br/>";
       echo "<br/>";
       echo "</div>";
    fi
    echo "<div class=\"text\">";
    sqlite3 -batch $db "select text from comments where id = '$i';" | ./repairxml.sed
    echo "</div>";
    echo "</div>";
    echo "<hr/>";
    echo "\n"
done
echo "</div></body></html>"
