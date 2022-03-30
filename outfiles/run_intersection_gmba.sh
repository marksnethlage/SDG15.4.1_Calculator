#!/bin/bash
#SBATCH --job-name=runGMBA
#SBATCH --error=/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity/outfiles/runIG.err
#SBATCH --output=/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity/outfiles/runIG.out
#SBATCH --nodes=1
#SBATCH --time=48:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=120GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh

ml system math devel sqlite gcc
ml physics proj geos gdal udunits curl netcdf R/4.1.2;
ml gcc/9.1.0

cd $OAK/group_members/aminaly/mountain_biodiversity
Rscript ./analysis/Intersection_GMBA.R
