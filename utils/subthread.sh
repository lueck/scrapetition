#!/usr/bin/env bash

# Default values
db="data.db"

USAGE="$0 [options] START_ID
[ -h | --help ]	     	 Show this help
[ --database FILENAME ]	 Specify database file. Default: $db
[ --delimiter | -d ]  	 Specify a delimiter. Default: ', '

$0 collects IDs of a sub-thread starting from START_ID.

Examples:
$0 -db petition.db 569720
$0 --database data.db 'cid-50377256'
"

die()
{
    echo "$1" >&$2
    exit $3
}

positional=()

delim=", "

# parse command line parameters
while [[ $# -gt 0 ]];
do
    case "$1" in
	-h | --help)
	    die "$USAGE" 1 0 ;;
	--database)
	    if test ! "$2"; then
		die "$USAGE" 2 1 ;
	    else		
		db=$2
		shift
		shift
	    fi ;;
	-d | --delimiter)
	    if test ! "$2"; then
		die "$USAGE" 2 1 ;
	    else
		delim=$2
		shift
		shift
	    fi;;
	-*)
	    die "$USAGE" 2 1 ;;
	*) # positonal argument
	    positional+=("$1")
	    shift
	    ;;
    esac
done

# start is the first positional argument
start=${positional[0]}

# recursive select using a CTE
ids=$(sqlite3 -batch $db <<EOF
WITH RECURSIVE ids (id) AS (
     VALUES ('$start')
     UNION
     SELECT comment.id FROM ids, comment
     WHERE comment.parent = ids.id)
SELECT * FROM ids;
EOF
   )

# interleave IDs with commas, so they can be used in an WHERE clause
# with comment_id IN ...
rc=""
for i in $ids; do
    if [[ "$rc" == "" ]]; then
	rc="'$i'";
    else
	rc="$rc$delim'$i'";
    fi
done

echo $rc
