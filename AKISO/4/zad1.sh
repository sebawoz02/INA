#!/bin/bash

readarray -t pids < <(sudo find /proc -maxdepth 1 -name '[0-9]*')

echo "PID---- | PPID---- | STATE---- | TTY---- | RSS---- | PGID---- | SID---- | COMM---- | FILES_OPENED----" > printfile.txt

for pr in ${pids[@]}; 
do	
	if [ -d "$pr" ]; then
		readarray -d " " -t process_info < $pr/stat
		separator="|"
		prinfo="${process_info[0]}$separator ${process_info[3]} $separator ${process_info[2]} $separator"
		prinfo="$prinfo ${process_info[6]} $separator ${process_info[23]} $separator ${process_info[4]} $separator"
		prinfo="$prinfo ${process_info[5]} $separator ${process_info[1]} $separator $(sudo ls ${pr}/fd/ | wc -l)"	
		echo "$prinfo" >> printfile.txt
	fi
done

column printfile.txt -t -s "|"
rm printfile.txt
