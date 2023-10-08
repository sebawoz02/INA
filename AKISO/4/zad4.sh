#!/bin/bash
directory=$1
cd $directory
readarray -d '|' -t filesarr < <(sudo find "$PWD" -type f  -printf '%s|\t%p|\n'| sort -nr)
len=${#filesarr[@]}
for ((i=1; i<$len-2; i=i+2));
do
	cmpid1=$(sha256sum ${filesarr[$i]} | awk '{print $1}')
	for ((j=$i+2; j<$len; j=j+2));
	do
                if [ ${filesarr[$i-1]} != ${filesarr[$j-1]} ]; then
                        break
                fi
		cmpid2=$(sha256sum ${filesarr[$j]} | awk '{print $1}')
		if [ $cmpid1 = $cmpid2 ]; then
			echo -e "Rozmiar: ${filesarr[$i-1]}\n f1: ${filesarr[$i]}\n f2: ${filesarr[$j]}"
		fi 
	done
done


