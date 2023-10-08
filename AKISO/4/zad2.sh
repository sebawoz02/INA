#!/bin/bash

#funkcja zamienajaca MHz na GHz
function mhzNaGhz {
	local mhz=$1
	local Ghz=$((mhz/10))
	local dGhz=$((Ghz/10%10))
	local sGhz=$((Ghz%10))
	Ghz=$((Ghz/100))
	printf '%d.%d%d GHZ' $Ghz $dGhz $sGhz
}

#funkcja przesuwa wartosci w tablicy przechowujacej ostatnio zapamietane pomiary. Usuwa ostatni (najstarszy pomiar) i dodaje na zerowym indexie najnowszy.
function przesunWykresy { 
	local val1=$1
	for (( i=$szerokoscWykresu-1; i>0; i-- )); do
		let j=i-1
		networkIn[$i]=${networkIn[$j]}
	done
	networkIn[0]=$val1
	local val2=$2
	for (( i=$szerokoscWykresu; i>0; i-- )); do
		let j=i-1
		networkOut[$i]=${networkOut[$j]}
	done
	networkOut[0]=$val2
}
#funkcja rysuje wykresy do aktualnej predkosci przesylania danych
function rysujWykresy {
	#rysuje wykres NetworkIN	
	local maxin=0
	for (( i=0; i<$szerokoscWykresu; i++ )); do
		if [ ${networkIn[$i]} -gt $maxin ]; then
			maxin=${networkIn[$i]}
		fi
	done
	if [ $maxin -eq 0 ]; then
		maxin=120
	fi
	local levelHeight=$((maxin/wysokoscWykresu))
	local ramka="┌"
	for (( i=1; i<$((szerokoscWykresu*3+1)); i++ )); do
		ramka+="─"
	done
	ramka+="┐"
	echo $ramka
	for (( i=$wysokoscWykresu; i>0; i-- )); do
		local poziom="│\e[35m"
		local bitylevel=$((levelHeight*i))
		for (( j=0; j<$szerokoscWykresu; j++ )); do
			local curbarHeight=${networkIn[$j]}
			if [ $curbarHeight -ge $bitylevel ]; then
				for (( k=0; k<3; k++ )); do
					poziom+="█"
				done
			else
				for (( k=0; k<3; k++ )); do
					poziom+=" "
				done
			fi
		done
		echo -e "$poziom\e[39m│$(zamianaNaBity $bitylevel)"
	done
	ramka="└"
	for (( i=1; i<$(($szerokoscWykresu*3+1)); i++ )); do
		ramka+="─"
	done
	ramka+="┘"
	echo $ramka		
	#rysuje wykres NetworkOut	
	local maxout=0
	for (( i=0; i<$szerokoscWykresu; i++ )); do
		if [ ${networkOut[$i]} -gt $maxout ]; then
			maxout=${networkOut[$i]}
		fi
	done
	if [ $maxout -eq 0 ]; then
		maxout=120
	fi
	levelHeight=$((maxout/wysokoscWykresu))
	ramka="┌"
	for (( i=1; i<$(($szerokoscWykresu*3+1)); i++ )); do
		ramka+="─"
	done
	ramka+="┐"
	echo $ramka

	for (( i=$wysokoscWykresu; i>0; i-- )); do
		poziom="│\e[33m"
		bitylevel=$((levelHeight*i))
		for (( j=0; j<$szerokoscWykresu; j++ )); do
			curbarHeight=${networkOut[$j]}
			if [ $curbarHeight -ge $bitylevel ]; then
				for (( k=0; k<3; k++ )); do
					poziom+="█"
				done
			else
				for (( k=0; k<3; k++ )); do
					poziom+=" "
				done
			fi
		done
	echo -e "$poziom\e[39m│$(zamianaNaBity $bitylevel)"
	done
	ramka="└"
	for (( i=1; i<$(($szerokoscWykresu*3+1)); i++ )); do
		ramka+="─"
	done
	ramka+="┘"
	echo $ramka
}
#funkcja przyjmuje jako argument bity i przelicza je na MB lub KB jezeli ilosc bitow jest wystarczajaco duza
function zamianaNaBity {
	local B=$1
	local KB=$((100*B/1024))
	local KBd=$((KB%100/10))
	local KBs=$((KB%10))
	local MB=$((KB/1024))
	local MBd=$((MB%100/10))
	local MBs=$((MB%10))
	MB=$((MB/100))
	if [ $MB -gt 0 ]; then
		printf '%d.%d%d MB' $MB $MBd $MBs
	elif [ $KB -gt 99 ]; then
		KB=$((KB/100))
		printf '%d.%d%d KB' $KB $KBd $KBs
	else
		printf '%d B' $B 
	fi
}
i=0
szerokoscWykresu=30
wysokoscWykresu=14
while [ $i -lt $szerokoscWykresu ]; do
	networkIn+=(0)
	networkOut+=(0)
	let i=i+1
done
licznik=0
avgBytesIn=0
avgBytesOut=0
while :
do
	sleep 1
	clear
	
	#Network
	
	readarray -t network < <(cat /proc/net/dev)
	tmp=(${network[3]})
	bytesIn=${tmp[1]}
	bytesOut=${tmp[9]}
	if [ -z $prevBytesIn ]; then
		sleep 1
		readarray -t network < <(cat /proc/net/dev)
		tmp=(${network[3]})
		prevBytesIn=$bytesIn
		prevBytesOut=$bytesOut
		bytesIn=${tmp[1]}
		bytesOut=${tmp[9]}	
	fi
	echo CURRENT NETWORK IN: "$(zamianaNaBity $((bytesIn-prevBytesIn)))"  CURRENT NETWORK OUT: "$(zamianaNaBity $((bytesOut-prevBytesOut)))"
	przesunWykresy $((bytesIn-prevBytesIn)) $((bytesOut-prevBytesOut))
	rysujWykresy	
	avgBytesIn=$((avgBytesIn*licznik+bytesIn-prevBytesIn))
	l1=$((licznik+1))
	avgBytesIn=$((avgBytesIn/l1))
	avgBytesOut=$((avgBytesOut*licznik+bytesOut-prevBytesOut))
	avgBytesOut=$((avgBytesOut/l1))
	licznik=$l1	
	echo AVERAGE IN: "$(zamianaNaBity $((avgBytesIn)))" AVERAGE OUT: "$(zamianaNaBity $((avgBytesOut)))"
	prevBytesIn=$bytesIn
	prevBytesOut=$bytesOut	
	echo
	
	#Uptime
	
	uptimeTotal=$(cat /proc/uptime | sed 's/\..*$//')
	uptimeSec=$((uptimeTotal%60))
	uptimeMin=$((uptimeTotal/60%60))
	uptimeHr=$((uptimeTotal/3600%24))
	uptimeDays=$((uptimeDays/8640))
	echo "UPTIME: $uptimeDays days $uptimeHr hours $uptimeMin minutes $uptimeSec seconds"
	echo
		
	#Battery %
	
	fullBattery=$(cat /sys/class/power_supply/BAT0/uevent | grep 'POWER_SUPPLY_ENERGY_FULL='| sed 's/POWER_SUPPLY_ENERGY_FULL=//')
	curBattery=$(cat /sys/class/power_supply/BAT0/uevent | grep 'POWER_SUPPLY_ENERGY_NOW='| sed 's/POWER_SUPPLY_ENERGY_NOW=//')
	echo "BATTERY: $(($curBattery*100/$fullBattery))%"
	echo
		
	#System Utilization
	
	readarray -d " " -t cpuUtil < <(cat /proc/loadavg)
	echo "SYS UTILIZATION: ${cpuUtil[0]} ${cpuUtil[1]} ${cpuUtil[2]}"
	echo
		
	#Memory Utilization
	
	readarray -t memUtil < /proc/meminfo
	memTotal=$(echo "${memUtil[0]}"| sed 's/MemTotal://g'| sed 's/kB//')
	memFree=$(echo "${memUtil[1]}" | sed 's/MemFree://g'| sed 's/kB//')
	memUsed=$((memTotal-memFree))
	memUsed=$((memUsed*100/memTotal))
	echo "MEMEORY UTILIZATION: $memUsed%"	
	echo	
	
	#Cores Utilization
	
	echo "CORES UTILIZATION:"
	hertz=($(cat /proc/cpuinfo | grep 'cpu MHz'))
	for ((i=0;i<8;i++));
	do
		idx=$((i*4+3))
		cpuHZ[$i]=$(mhzNaGhz $(echo ${hertz[$idx]} | cut -f1 -d"."))
	done	
	readarray -t coresUtil < <(cat /proc/stat)
	for ((i=0;i<8;i++));
	do
		coreStat=(${coresUtil[$i]})
		totaltime=0
		for ((j=1;j<9;j++));do
			time=${coreStat[j]}
			totaltime=$((totaltime+time))
		done
		idle=${coreStat[4]}
		iowait=${coreStat[5]}
		IDLE=$((idle+iowait))
		prevtot=${prevtotaltime[i]}
		totald=$((totaltime-prevtot))
		prevI=${previdle[i]}
		idled=$((IDLE-prevI))
		coreUsage=$((totald-idled))
		coreUsage=$((coreUsage*100/totald))
		graph=$(printf "╠\e[3%dm" $i)
		for ((j=0;j<coreUsage;j++));
		do
			graph+="▒"
		done
		for ((j=0;j<100-coreUsage;j++));
		do
			graph+="-"
		done
		graph+="╣"
		echo "cpu$i $graph $coreUsage% | ${cpuHZ[$i]}"
		prevtotaltime[i]=$totaltime
		previdle[i]=$IDLE
	done	
done
