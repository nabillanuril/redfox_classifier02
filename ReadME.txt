Run order matters. Execute the scripts in the order below.

Disclaimer: the xeno-canto API changes from time to time; query_xc may break without notice.

Scripts (in order)

1.BL_convermp32wav
Convert British Library MP3 recordings to WAV.

1.DR_fox_active_time
Filter Dartmoor long recordings to fox-active windows to speed up manual labelling.

1.XC_download_filter_redfox

2.sig2noise
Compute signal-to-noise ratio (SNR) for recordings/clips.

slice_clip_test_augment
Curate test sets; prepare any augmented material needed for test data.

mix_sig2noise_test
Mix calls with background noise at target SNRs for test data.

slice_clip_test&move
Cut/copy test clips and move them into the test folders.

slice_clip_train_dr
Curate and clip training data from Dartmoor (including augmented Dartmoor recordings).

slice_clip_train_xc
Curate and clip training data from Xeno-Canto recordings.

sample_distribution (optional)
Visualise sample distributions.

result&stats
Visualise model performance comparisons.

load_evaluation metrics
Load/visualise evaluation metrics and run statistical tests.

map
visualise biophone deployment points

