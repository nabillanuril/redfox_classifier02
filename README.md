This repository is made as a correction to the first repository

Run order matters. Execute the scripts in the order below.

Disclaimer: the xeno-canto API changes from time to time; query_xc may break without notice.

Scripts (in order)

01.BL_convertmp32wav.R
Convert British Library MP3 recordings to WAV.


01.DR_fox_active_time.R
Filter Dartmoor long recordings to fox-active windows to speed up manual labelling.


01.XC_download_filter_redfox.R
Download Xeno-Canto recording


02.load_label_result.R
Import selection label to R


03.sig2noise.R
Compute signal-to-noise ratio (SNR) for recordings/clips.


04.slice_clips_test_augment.R
Curate test sets; prepare any augmented material needed for test data.


05.mix_sig2noise_test.ipynb.ipynb
Mix calls with background noise at target SNRs for test data.


06.slice_clips_test_clips&move.R
Cut/copy test clips and move them into the test folders.


07.slice_clips_train_dr.R
Curate and clip training data from Dartmoor (including augmented Dartmoor recordings).


07.slice_clips_train_xc.R
Curate and clip training data from Xeno-Canto recordings.


08.mix_sig2noise_train.ipynb.ipynb
Visualise sample distributions.


09.result_plot&stats.R
Visualise model performance comparisons.


10.load_evaluation metrics.R
Load/visualise evaluation metrics and run statistical tests.


10.map.R
visualise biophone deployment points

Additional\n
10.sample_distribution.R\n
visualise sample characteristic distribution


