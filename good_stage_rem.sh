#!/bin/bash
cd man_labelled/
for file in $(echo *.mnc);
do
    mincmath -clobber -mult -const 300 ../e7_stage_label.mnc labelled_math.mnc
    mincmath -clobber -add -const 1 labelled_math.mnc prepped_stage.mnc
    mincmath -clobber -div $file prepped_stage.mnc test_rem.mnc
    mincmath -clobber -clamp -const2 0 100000 test_rem.mnc rem_${file}
done
