#!/bin/bash

# alias for sbatch and then printing the stdout

# Check if the first argument is provided
if [ -z "$1" ]
then
	    echo "No argument provided. Please provide an .sbatch script."
	        exit 1
fi

# Check if the file exists
if [ ! -f "$1" ]
then
	    echo "File not found. Please provide a valid .sbatch script."
	        exit 1
fi

# Submit the job and store the job id
jobid=$(sbatch $1 | awk '{print $4}')

# Check if the job id was successfully obtained
if [ -z "$jobid" ]
then
	    echo "Failed to obtain job id. Please check the sbatch command."
	        exit 1
fi

# Wait for the job output file to be created and not be empty
while [ ! -s "slurm-$jobid.out" ]
do
	    sleep 5
    done

# Tail the job output
tail -n 600 -f slurm-$jobid.out
