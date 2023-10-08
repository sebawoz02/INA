#!/bin/bash

gr_suffix='.gr'
ss_suffix='.ss'
itr=1
readarray -t usa_graphs < <(ls ch9-1.1/inputs/USA-road-d/*.gr)
readarray -t rand_graphs < <(ls ch9-1.1/inputs/Random4-C/*.gr)
readarray -t square_graphs < <(ls ch9-1.1/inputs/Square-C/*.gr)
readarray -t long_graphs < <(ls ch9-1.1/inputs/Long-n/*.gr)

make dial
make radix
make
for ugr in "${usa_graphs[@]}";
do
    cpy=${ugr}
    ss=${ugr//$gr_suffix/$ss_suffix}
    echo "./dijkstra -d ${cpy} -ss ${ss} -res rand_data.txt"
    ./dijkstra -d "${cpy}" -ss "${ss}" -res usa_data.txt
    echo "./dial -d ${cpy} -ss ${ss} -res rand_data.txt"
    ./dial -d "${cpy}" -ss "${ss}" -res usa_data.txt
    echo "./radix -d ${cpy} -ss ${ss} -res rand_data.txt"
    ./radix -d "${cpy}" -ss "${ss}" -res usa_data.txt
    echo "USA ${itr}/${#usa_graphs[@]}"
    itr=$((itr+1))
done

itr=1
for rgr in "${rand_graphs[@]}";
do
    cpy=${rgr}
    ss=${rgr//$gr_suffix/$ss_suffix}
    echo "./dijkstra -d ${cpy} -ss ${ss} -res rand_data.txt"
    ./dijkstra -d "${cpy}" -ss "${ss}" -res rand_data.txt
    echo "./dial -d ${cpy} -ss ${ss} -res rand_data.txt"
    ./dial -d "${cpy}" -ss "${ss}" -res rand_data.txt
    echo "./radix -d ${cpy} -ss ${ss} -res rand_data.txt"
    ./radix -d "${cpy}" -ss "${ss}" -res rand_data.txt
    echo "RAND ${itr}/${#rand_graphs[@]}"
    itr=$((itr+1))
done

itr=1
for sgr in "${square_graphs[@]}";
do
    cpy=${sgr}
    ss=${sgr//$gr_suffix/$ss_suffix}
    echo "./dijkstra -d ${cpy} -ss ${ss} -res rand_data.txt"
    ./dijkstra -d "${cpy}" -ss "${ss}" -res usa_data.txt
    echo "./dial -d ${cpy} -ss ${ss} -res rand_data.txt"
    ./dial -d "${cpy}" -ss "${ss}" -res usa_data.txt
    echo "./radix -d ${cpy} -ss ${ss} -res rand_data.txt"
    ./radix -d "${cpy}" -ss "${ss}" -res usa_data.txt
    echo "SQUARE ${itr}/${#square_graphs[@]}"
    itr=$((itr+1))
done

itr=1
for lgr in "${long_graphs[@]}";
do
    cpy=${lgr}
    ss=${lgr//$gr_suffix/$ss_suffix}
    echo "./dijkstra -d ${cpy} -ss ${ss} -res rand_data.txt"
    ./dijkstra -d "${cpy}" -ss "${ss}" -res usa_data.txt
    echo "./dial -d ${cpy} -ss ${ss} -res rand_data.txt"
    ./dial -d "${cpy}" -ss "${ss}" -res usa_data.txt
    echo "./radix -d ${cpy} -ss ${ss} -res rand_data.txt"
    ./radix -d "${cpy}" -ss "${ss}" -res usa_data.txt
    echo "LONG ${itr}/${#long_graphs[@]}"
    itr=$((itr+1))
done


make clean
