#!/bin/bash 
#
# 
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
# 
#   http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
# 
#


usage()
{
  echo "usage: process.sh <Qpid Version> <Test Type Queue/Topic/Latency..> <Volume of data %age> <broker details>"
  echo "These parameters are used to title and name the graphs:"
  echo 'Title = $<Qpid Version> $<Test Type> : $<Volume>% volume'
  echo 'File  = $<Qpid Version>-$<Broker Details>-$<Test Type>-$<Volume>'
  exit 1
}

processCMSGCFile()
{
 # Extract all the ParNew timings: Ignore any lines corrupted by concurrent CMS logging.
 grep -v CMS gc.log|grep ParNew > $work/parnew.gc.log

 # Calculate Full GC Data
 grep failure -B 1 gc.log | sed -e :a -e '$!N;s/\n.(concurrent/ /;ta' -e 'P;D' |grep -v ^\-  > $work/full.gc.log
 cut -d ':' -f 1 $work/full.gc.log > $work/full.time.log
 sed -e 's/failure)/\#/g' $work/full.gc.log |cut -d '#' -f 2- |awk '{print $3}' > $work/full.dur.log

 # Move to the working directory to further process the gathered data.
 pushd $work &> /dev/null

 # Extract the Heap and timing data.
 #8 is the full gc data
 #5 is the paranew gc data
 cat parnew.gc.log | awk '{print $8}' | cut -d '(' -f 2 | cut -d 'K' -f 1 > HEAP_MAX.log
 cat parnew.gc.log | awk '{print $8}' | cut -d 'K' -f 1 > HEAP_PRE_GC.log
 cat parnew.gc.log | awk '{print $8}' | cut -d 'K' -f 2 | cut -d '>' -f 2 > HEAP_POST_GC.log
 cat parnew.gc.log | awk '{print $1}' | cut -d ':' -f 1 > GC_TIME.log


 # Calculate ParNew GC Cumulative total 
 cat parnew.gc.log |awk '{sum=sum+$6; print $6, sum}' > GC_DUR.log
 # Give a count of GC occurances
 parNewGCCount=`wc -l GC_DUR.log| awk '{print $1}'`

 # Create the Heap data file
 paste GC_TIME.log  HEAP_POST_GC.log  HEAP_PRE_GC.log  HEAP_MAX.log > GC.Heap.data
 # Create ParNew GC Duration data file for graphing
 paste GC_TIME.log GC_DUR.log > GC.Dur.data
 # Create Full GC occurance log file for plotting
 paste full.time.log full.dur.log > GC.Full.data

 # Calculate All GC Timing and give a count of their occurance
 awk '{print $1}' GC_DUR.log > gc_dur.log
 paste GC_TIME.log gc_dur.log > gc_all.log
 cat GC.Full.data >> gc_all.log
 sort -n gc_all.log | awk '{sum=sum+$2;print $1 , sum}' > GC.Dur.All.data
 fullGCCount=`wc -l GC.Full.data| awk '{print $1}'`
}

processG1GCFile()
{
 cat gc.log | grep -e \^[0-9]*.[0-9]*\:  -e  \^.*[0-9]*.\-\> > $work/g1.gc.log
 
 # Move to the working directory to further process the gathered data.
 pushd $work &> /dev/null

 # Calculate Full GC Data
 cat g1.gc.log | sed -e :a -e '$!N;s/\n.\ *\[/ \[/;ta' -e 'P;D' > full.gc.log
 grep Full full.gc.log |awk '{print $1}'| tr ':' ' ' > full.time.log 
 grep Full full.gc.log |awk '{print $5}' > full.dur.log
 fullGCCount=`wc -l GC.Full.data| awk '{print $1}'`
 
 # Create Full GC occurance log file for plotting
 paste full.time.log full.dur.log > GC.Full.data

 # Extract the Heap and timing data.
 # Create G1 Young Duration data file for graphing
 grep "(young),"  full.gc.log |awk '{print $1}' | tr ':' ' ' > GC_TIME.log
 grep "(young),"  full.gc.log |awk '{print $5}' > young.gc.time.log
 # Calculate G1 young Cumulative total 
 cat young.gc.time.log |awk '{sum=sum+$1; print $1, sum}' > GC_DUR.log
 # Create G1 Young Duration data file for graphing
 paste GC_TIME.log GC_DUR.log > GC.Dur.data
 
 # Give a count of GC occurances
 youngGCCount=`wc -l GC_DUR.log| awk '{print $1}'`
 
 #
 # If we have no GCs then something is wrong
 if [ $youngGCCount == 0 ] ; then
   echo "Error no YoungGC log entries to proceses"
   return
 fi

 # Gather the Heap Size data

 grep "(young)," full.gc.log | awk '{print $8}' | cut -d '(' -f 2 | cut -d ')' -f 1 > HEAP_MAX.Sized.log
 grep "(young)," full.gc.log | awk '{print $8}' | cut -d '-' -f 1 > HEAP_PRE_GC.Sized.log
 grep "(young)," full.gc.log | awk '{print $8}' | cut -d '>' -f 2| cut -d '(' -f 1 > HEAP_POST_GC.Sized.log

 normaliseSizeMemFile HEAP_MAX.Sized.log HEAP_MAX.log
 normaliseSizeMemFile HEAP_PRE_GC.Sized.log HEAP_PRE_GC.log
 normaliseSizeMemFile HEAP_POST_GC.Sized.log HEAP_POST_GC.log	


 # Create the Heap data file
 paste GC_TIME.log  HEAP_POST_GC.log  HEAP_PRE_GC.log  HEAP_MAX.log > GC.Heap.data

 # Calculate All GC Timing and give a count of their occurance
 awk '{print $1}' GC_DUR.log > gc_dur.log
 paste GC_TIME.log gc_dur.log > gc_all.log
 cat GC.Full.data >> gc_all.log
 sort -n gc_all.log | awk '{sum=sum+$2;print $1 , sum}' > GC.Dur.All.data

}

#
# Take an input file ($1) of lines
# <value><K|M>
# and output file $2 of <value> in whole M
#
normaliseSizeMemFile()
{
rm -f $2
for i in `cat $1` ; do
 if [[ `echo $i | grep -c "K" ` == 1 ]] ; then
  kb=`echo $i|cut -d 'K' -f 1`
  echo $[ $kb / 1024 ] >> $2
 else
  echo $i|cut -d 'M' -f 1 >> $2
 fi
done


}


# Parse command line
# TODO more advanced processing
# Check we have enough parameters
if [ $# != 4 ] ; then
  # Take one arg to be a graph data file.
  if [ $# == 1 ] ; then
    textArray[0]="" # hold text
    length=0
    # read whole file in loop
    while read line
    do
      textArray[$length]=$line # store line
      length=$(expr $length + 1) # increase length by 1
    done < $1

    if [ $length != 2 ] ; then
      usage
    fi
  
    #Get Title and file name
    title=${textArray[0]}
    file=${textArray[1]}
    
    pushd `dirname $1`
    
  else
    usage
  fi
else  
  version=$1
  type=$2
  volume=$3
  brokerState=$4


  # Configure Graph Title and image file names
  title="$version $type : $volume% volume"
  file="$version-$brokerState-$type-$volume"
fi

work=work

mkdir -p $work

echo -n "Processing GC Usage Data : "
ParNew=`grep -c ParNew gc.log`

if [ $ParNew != 0 ] ; then 
 echo "CMS log file"
 processCMSGCFile 
 PLOT="\"GC.Dur.data\" with lines axis x1y1 ti \"ParNew GC Time ($parNewGCCount)\", "
else
 echo "G1 log file"
 processG1GCFile
 PLOT="\"GC.Dur.data\" with lines axis x1y1 ti \"G1 Young Time ($youngGCCount)\", "
fi



# Prepare the plot command
# If a Full GC occured during this test then plot those 
if [[ $fullGCCount > 0 ]] ; then
PLOT=$PLOT"\"GC.Dur.data\" using 1:3 with lines axis x1y2 ti \"Cumulative Total Time(ParNew)\", \
     \"GC.Dur.All.data\" with lines axis x1y2 ti \"Cumulative Total Time(All)\", \
     \"GC.Full.data\" with points ti \"Full GCs Time ($fullGCCount)\" "
else
PLOT=$PLOT"\"GC.Dur.data\" using 1:3 with lines axis x1y2 ti \"Cumulative Total Time(ParNew)\", \
     \"GC.Dur.All.data\" with lines axis x1y2 ti \"Cumulative Total Time(All)\" "
fi

if [ $ParNew != 0 ] ; then 
  gcs=$parNewGCCount
else
  gcs=$youngGCCount 
fi

# Call out to gnuplot to generate graphs
# Only do this if we have data
if [ $gcs != 0 ] ; then
  # Generate the Heap Graph and the GC Duration Graph
  gnuplot << EOGNUPLOT
set xlabel "Run Time(s)"

set title "$title : Heap Size"
set term png
set output "$file-Heap.png"
set ylabel "MB" +0.5,0
plot "GC.Heap.data" using 1:2 with lines axis x1y1 ti "GC Size Post",\
     "GC.Heap.data" using 1:4 with lines axis x1y1 ti "GC Size Max ", \
     "GC.Heap.data" using 1:3 with lines axis x1y1 ti "GC Size Pre "   

set y2tics nomirror
set ytics nomirror
set key top left
set title "$title GC Time"
set ylabel "Time(s)" +0.5,0
set y2label "Total Time(s)"
set output "$file-GCDuration.png"
plot $PLOT
EOGNUPLOT

else
  echo "Warning: No GC Data to graph"
fi

# pop back to further process for CPU usage
popd &> /dev/null

echo "Processing CPU Usage Data"

# CPU.data is just TimeStamp + %age
cat broker_cpu.log |awk '{print $1 "T" $2 " " $3}' > $work/CPU.data

# Move to work directory to run gnuplot
pushd $work &> /dev/null

# Call out to gnuplot to generate graphs
# Generate the Heap Graph and the GC Duration Graph
gnuplot << EOGNUPLOT
set term png
set title "$title : CPU"
set output "$file-CPU.png"
unset key
set xlabel "Real Time(h:m)"
set ylabel "Usage(%)"

set xdata time	
set timefmt "%Y-%m-%dT%H:%M:%S"
set format x "%H:%M"
plot "CPU.data" using 1:2 with lines 
EOGNUPLOT

popd &> /dev/null
