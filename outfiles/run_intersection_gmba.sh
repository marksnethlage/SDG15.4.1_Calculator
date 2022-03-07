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

ml system math devel sqlite/3.18.0
ml physics proj/4.9.3 geos gdal/2.2.1 udunits/2.2.26 curl/7.54.0 netcdf/4.4.1.1 R/3.6.1;

cd $OAK/group_members/aminaly/mountain_biodiversity
Rscript ./analysis/Intersection_GMBA.R
