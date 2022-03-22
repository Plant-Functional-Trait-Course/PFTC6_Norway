#!/bin/sh
cd ~/svalbard/PFTC4_Svalbard/traits/Rdatagathering/

xsane &

while true
do
  inotifywait -e create /home/pi/Desktop/Svalbard_leaves |  Rscript run_check_image.R
done 
