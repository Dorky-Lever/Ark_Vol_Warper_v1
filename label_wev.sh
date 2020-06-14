#!/bin/bash
#$ -N "test_kat_crop"
#$ -cwd
#$ -V
#$ -S /bin/bash
#$ -q "hugemem.q"
#$ -l h_vmem=80g,virtual_free=79.9g
#$ -pe threads 40
#$ -m e
#$ -M "kyle.drover@anu.edu.au"

cd wev1
for file in $(echo crop*)
do
        mkdir ${file%.mnc}
        python2 /home/kyle_arkell/seg_methods/mask.py -i $file -option otsu -tight -external -ndilate -o ${file%.mnc}
done
