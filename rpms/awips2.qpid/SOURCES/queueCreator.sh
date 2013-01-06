#!/bin/sh

export PATH=/awips2/python/bin:$PATH
export LD_LIBRARY_PATH=/awips2/python/lib:$LD_LIBRARY_PATH
port=5672

#define 96 Meg persistence queues
QUEUES=('external.dropbox' 'Ingest.Text')
for queue in ${QUEUES[*]};
do
   echo "Creating queue $queue"
   qpid-config --broker-addr localhost:$port add queue $queue --durable --file-count 32 --file-size 48
done

#define 48 Meg persistence queues
QUEUES=('Ingest.Grib' 'Ingest.Radar' 'watchwarn' 'cpgsrvFiltering' 'Ingest.modelsounding' 'Ingest.Shef' 'Ingest.obs' 'Ingest.dpa')
for queue in ${QUEUES[*]};
do
   echo "Creating queue $queue"
   qpid-config --broker-addr localhost:$port add queue $queue --durable --file-count 32 --file-size 24
done

#define 24 Meg Dat queues
QUEUES=('ffmp' 'cwat' 'vil' 'qpf' 'preciprate' 'fssobs' 'fog')
for queue in ${QUEUES[*]};
do
   echo "Creating queue ${queue}Generate"
   qpid-config --broker-addr localhost:$port add queue ${queue}Generate --durable --file-count 16 --file-size 24
done

#define 24 Meg persistence queues for GFE
QUEUES=('activeTablePending' 'gfeSvcBackupOp' 'gfeIscDataReceive' 'edex.tpcWatch' 'edex.spcWatch')
for queue in ${QUEUES[*]};
do
   echo "Creating queue $queue"
   qpid-config --broker-addr localhost:$port add queue $queue --durable --file-count 16 --file-size 24
done

#define 24 Meg persistence queues for ingest
QUEUES=('vaa' 'textlightning' 'tcs' 'tcg' 'taf' 'svrwx' 'sfcobs' 'redbook' 'recco' 'q2' 'profiler' 'poessounding' 'pirep' 'lsr' 'loctables' 'ldadprofiler' 'ldadmesonet' 'ldadhydro' 'goessounding' 'cwa' 'ccfp' 'bufrua' 'bufrssmi' 'bufrsigwx' 'bufrquikscat' 'bufrncwf' 'bufrmthdw' 'bufrmos' 'bufrhdw' 'bufrascat' 'binlightning' 'airmet' 'airep' 'acars' 'Warning' 'ShefStaged' 'Satellite')
for queue in ${QUEUES[*]};
do
   echo "Creating queue Ingest.$queue"
   qpid-config --broker-addr localhost:$port add queue Ingest.$queue --durable --file-count 16 --file-size 24
done

#define 24 Meg persistence queues for HPE ingest
QUEUES=('Ingest.dhr' 'dhrProcess')
for queue in ${QUEUES[*]};
do
   echo "Creating queue $queue"
   qpid-config add queue $queue --durable --file-count 16 --file-size 24
done

#define 24 Meg persistence queues for HPE ingest
QUEUES=('Ingest.dhr' 'dhrProcess')
for queue in ${QUEUES[*]};
do
   echo "Creating queue $queue"
   qpid-config add queue $queue --durable --file-count 16 --file-size 24
done


