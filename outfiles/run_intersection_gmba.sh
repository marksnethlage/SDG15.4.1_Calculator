#!/bin/bash
#SBATCH --job-name=runGMBA
#SBATCH --error=/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity/outfiles/runIG.err
#SBATCH --output=/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity/outfiles/runIG.out
#SBATCH --nodes=1
#SBATCH --time=48:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=90GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh

ml curl physics gdal udunits netcdf proj geos R/4.1.2;

cd $OAK/group_members/aminaly/mountain_biodiversity
Rscript ./analysis/Intersection_GMBA.R
