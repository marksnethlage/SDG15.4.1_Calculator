#!/bin/bash
#SBATCH --job-name=runO
#SBATCH --error=/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity/outfiles/runIO.err
#SBATCH --output=/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity/outfiles/runIO.out
#SBATCH --nodes=1
#SBATCH --time=48:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=90GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh

ml curl physics gdal udunits/2.2.26 netcdf R/3.6.1 proj geos;

cd $OAK/group_members/aminaly/mountain_biodiversity
Rscript ./analysis/Intersection_Official.R
