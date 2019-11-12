#!/bin/bash
cd scans
for file in $(echo *.mnc); 
do  
   /usr/local/bin/antsRegistration --verbose 1 --dimensionality 3 --float 0 --collapse-output-transforms 0 \
        --output [synq,synqWarped.nii.gz,synqInverseWarped.nii.gz] --interpolation Linear \
        --use-histogram-matching 0 --winsorize-image-intensities [0.005,0.995] \
        --initial-moving-transform [../e3_rot_copy.mnc,$file,1] \
        --transform Rigid[0.1] --metric MI[../e3_rot_copy.mnc,$file,1,32,Regular,0.25] \
        --convergence [1000x500x250x0,1e-6,10] --shrink-factors 12x8x4x2 --smoothing-sigmas 4x3x2x1vox \
        --transform Affine[0.1] --metric MI[../e3_rot_copy.mnc,$file,1,32,Regular,0.25] \
        --convergence [1000x500x250x0,1e-6,10] --shrink-factors 12x8x4x2 --smoothing-sigmas 4x3x2x1vox --transform SyN[0.1,3,0] \
        --metric CC[../e3_rot_copy.mnc,$file,1,3] --convergence [100x100x70x50x0,1e-6,10] --shrink-factors 10x6x4x2x1 --smoothing-sigmas 5x3x2x1x0vox

    antsApplyTransforms -d 3 -i ../forebrain_label.mnc -r $file -t [synq0DerivedInitialMovingTranslation.mat,1] -t [synq1Rigid.mat,1] -t [synq2Affine.mat,1]  -t synq3InverseWarp.nii -o labelled_${file}
done
