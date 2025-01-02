import rsgislib
import rsgislib.segmentation
from rsgislib.segmentation import shepherdseg
import sys
inputImage = sys.argv[1]
outputImage = sys.argv[2]
tmp_dir = sys.argv[3]
num_clusters = int(sys.argv[4])
min_n_pxls= int(sys.argv[5])
    
shepherdseg.run_shepherd_segmentation(inputImage,outputImage, None,tmp_dir= tmp_dir, num_clusters= num_clusters,
                                      min_n_pxls= min_n_pxls,gdalformat="HFA",no_delete=True,dist_thres=1000000,
                                      calc_stats=False, no_stretch=True)
#                                      calc_stats=True, no_stretch=False, no_delete=False)
