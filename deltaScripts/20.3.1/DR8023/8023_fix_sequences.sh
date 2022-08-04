#!/bin/bash

# Hibernate upgrade led to a change in how Hibernate interacts with sequences.
# Update database sequences to have the appropriate increment value.
# All increment values below are taken from the value of
# @SequenceGenerator.allocationSize in the corresponding entity class.
# A value of 50 is used if that parameter is not specified.
# 
#
# Run on dv1.
#
# Author: tgurney

echo "INFO: $0 started."

echo INFO: Updating sequences in metadata.
psql --user=awipsadmin --db=metadata << 'EOF'
begin;
alter sequence if exists awips.acarsseq increment 50;
alter sequence if exists awips.acarssoundingseq increment 50;
alter sequence if exists awips.aggregate_seq increment 50;
alter sequence if exists awips.airepseq increment 50;
alter sequence if exists awips.airmetseq increment 50;
alter sequence if exists awips.atcfseq increment 50;
alter sequence if exists awips.awwseq increment 50;
alter sequence if exists awips.backup_blobseq increment 1;
alter sequence if exists awips.backup_jobseq increment 1;
alter sequence if exists awips.bandwidth_seq increment 1;
alter sequence if exists awips.binlightningseq increment 50;
alter sequence if exists awips.bufrascatseq increment 50;
alter sequence if exists awips.bufrhdwseq increment 50;
alter sequence if exists awips.bufrmosAvnseq increment 50;
alter sequence if exists awips.bufrmosEtaseq increment 50;
alter sequence if exists awips.bufrmosGfsseq increment 50;
alter sequence if exists awips.bufrmosHpcseq increment 50;
alter sequence if exists awips.bufrmosLampseq increment 50;
alter sequence if exists awips.bufrmos_locationseq increment 1;
alter sequence if exists awips.bufrmosMrfseq increment 50;
alter sequence if exists awips.bufrmthdwseq increment 50;
alter sequence if exists awips.bufrncwfseq increment 50;
alter sequence if exists awips.bufrsigwxseq increment 50;
alter sequence if exists awips.bufrssmiseq increment 50;
alter sequence if exists awips.bufruaseq increment 50;
alter sequence if exists awips.ccfpseq increment 50;
alter sequence if exists awips.convsigmetseq increment 50;
alter sequence if exists awips.crimssseq increment 50;
alter sequence if exists awips.cwaseq increment 50;
alter sequence if exists awips.cwatseq increment 50;
alter sequence if exists awips.data_set_latency_seq increment 1;
alter sequence if exists awips.dmwseq increment 50;
alter sequence if exists awips.ffgseq increment 50;
alter sequence if exists awips.ffmpseq increment 50;
alter sequence if exists awips.fogseq increment 50;
alter sequence if exists awips.fssobsseq increment 50;
alter sequence if exists awips.geodataflattseq increment 50;
alter sequence if exists awips.geodataintattseq increment 50;
alter sequence if exists awips.geodatastattseq increment 50;
alter sequence if exists awips.geodbSeq increment 50;
alter sequence if exists awips.gfe_dbid_seq increment 50;
alter sequence if exists awips.gfe_gridlocation_seq increment 50;
alter sequence if exists awips.gfe_history_seq increment 1;
alter sequence if exists awips.gfe_lock_seq increment 50;
alter sequence if exists awips.gfe_parmid_seq increment 50;
alter sequence if exists awips.gfe_parminfo_seq increment 50;
alter sequence if exists awips.gfeseq increment 50;
alter sequence if exists awips.goessoundingseq increment 50;
alter sequence if exists awips.gpdseq increment 50;
alter sequence if exists awips.gridcoverage_seq increment 1;
alter sequence if exists awips.gridinfo_seq increment 1;
alter sequence if exists awips.gridseq increment 50;
alter sequence if exists awips.idftseq increment 50;
alter sequence if exists awips.intlsigmetseq increment 50;
alter sequence if exists awips.iscmosaicjobseq increment 50;
alter sequence if exists awips.ldadhydroseq increment 50;
alter sequence if exists awips.ldadmesonetseq increment 50;
alter sequence if exists awips.level_seq increment 1;
alter sequence if exists awips.lsrseq increment 50;
alter sequence if exists awips.madisseq increment 50;
alter sequence if exists awips.mcidasseq increment 50;
alter sequence if exists awips.modelsoundingseq increment 50;
alter sequence if exists awips.modisseq increment 50;
alter sequence if exists awips.mosaicseq increment 50;
alter sequence if exists awips.mpeprecipseq increment 50;
alter sequence if exists awips.mpingseq increment 50;
alter sequence if exists awips.ncpafmseq increment 50;
alter sequence if exists awips.ncscatseq increment 50;
alter sequence if exists awips.nctafseq increment 50;
alter sequence if exists awips.nctextseq increment 50;
alter sequence if exists awips.ncuairseq increment 50;
alter sequence if exists awips.nonconvsigmetseq increment 50;
alter sequence if exists awips.notification_seq increment 50;
alter sequence if exists awips.nswrcradialseq increment 50;
alter sequence if exists awips.ntransseq increment 50;
alter sequence if exists awips.nucapsseq increment 50;
alter sequence if exists awips.obsseq increment 50;
alter sequence if exists awips.pgenseq increment 50;
alter sequence if exists awips.pirepseq increment 50;
alter sequence if exists awips.poessoundingseq increment 50;
alter sequence if exists awips.pointsetseq increment 50;
alter sequence if exists awips.practicewarningseq increment 50;
alter sequence if exists awips.preciprateseq increment 50;
alter sequence if exists awips.probsevereseq increment 50;
alter sequence if exists awips.pshdataseq increment 50;
alter sequence if exists awips.qcseq increment 50;
alter sequence if exists awips.qpfseq increment 50;
alter sequence if exists awips.radarseq increment 50;
alter sequence if exists awips.redbookseq increment 50;
alter sequence if exists awips.replicationevent_seq increment 100;
alter sequence if exists awips.satelliteseq increment 50;
alter sequence if exists awips.satspatial_seq increment 1;
alter sequence if exists awips.scanseq increment 50;
alter sequence if exists awips.sfcobsseq increment 50;
alter sequence if exists awips.sgwhseq increment 50;
alter sequence if exists awips.sgwhvseq increment 50;
alter sequence if exists awips.sshaseq increment 50;
alter sequence if exists awips.stats_seq increment 50;
alter sequence if exists awips.stormtrackseq increment 50;
alter sequence if exists awips.stqseq increment 50;
alter sequence if exists awips.subscription_retrieval_seq increment 1;
alter sequence if exists awips.svrwxseq increment 50;
alter sequence if exists awips.tafseq increment 50;
alter sequence if exists awips.tcgseq increment 50;
alter sequence if exists awips.tcmseq increment 50;
alter sequence if exists awips.tcsseq increment 50;
alter sequence if exists awips.vaaseq increment 50;
alter sequence if exists awips.viirsseq increment 50;
alter sequence if exists awips.vilseq increment 50;
alter sequence if exists awips.warningseq increment 50;
alter sequence if exists awips.wcpseq increment 50;
alter sequence if exists ebxml.Map_sequence increment 50;
alter sequence if exists ebxml.ObjectRefList_sequence increment 50;
alter sequence if exists ebxml.RegistryObjectList_sequence increment 50;
alter sequence if exists ebxml.Value_sequence increment 50;
commit;
EOF
echo INFO: Finished updating sequences in metadata.

echo INFO: Updating sequences in fxatext.
psql --user=awipsadmin --db=fxatext << 'EOF'
begin;
alter sequence if exists public.replacementseq increment 50;
alter sequence if exists public.subscriptionseq increment 50;
commit;
EOF
echo INFO: Finished updating sequences in fxatext.

echo INFO: Updating sequences in bmh.
psql --user=awipsadmin --db=bmh << 'EOF'
begin;
alter sequence if exists public.area_seq increment 1;
alter sequence if exists public.broadcast_fragment_seq increment 50;
alter sequence if exists public.broadcast_msg_seq increment 50;
alter sequence if exists public.dac_seq increment 1;
alter sequence if exists public.input_msg_seq increment 50;
alter sequence if exists public.ldad_config_seq increment 1;
alter sequence if exists public.message_type_seq increment 1;
alter sequence if exists public.playlist_seq increment 50;
alter sequence if exists public.program_seq increment 1;
alter sequence if exists public.static_msg_type_seq increment 1;
alter sequence if exists public.suite_seq increment 1;
alter sequence if exists public.transmitter_group_seq increment 1;
alter sequence if exists public.transmitter_seq increment 1;
alter sequence if exists public.validated_msg_seq increment 50;
alter sequence if exists public.word_seq increment 1;
alter sequence if exists public.zone_seq increment 1;
commit;
EOF
echo INFO: Finished updating sequences in bmh.

echo INFO: Updating sequences in bmh_practice.
psql --user=awipsadmin --db=bmh_practice << 'EOF'
begin;
alter sequence if exists public.area_seq increment 1;
alter sequence if exists public.broadcast_fragment_seq increment 50;
alter sequence if exists public.broadcast_msg_seq increment 50;
alter sequence if exists public.dac_seq increment 1;
alter sequence if exists public.input_msg_seq increment 50;
alter sequence if exists public.ldad_config_seq increment 1;
alter sequence if exists public.message_type_seq increment 1;
alter sequence if exists public.playlist_seq increment 50;
alter sequence if exists public.program_seq increment 1;
alter sequence if exists public.static_msg_type_seq increment 1;
alter sequence if exists public.suite_seq increment 1;
alter sequence if exists public.transmitter_group_seq increment 1;
alter sequence if exists public.transmitter_seq increment 1;
alter sequence if exists public.validated_msg_seq increment 50;
alter sequence if exists public.word_seq increment 1;
alter sequence if exists public.zone_seq increment 1;
commit;
EOF
echo INFO: Finished updating sequences in bmh_practice.
echo "INFO: $0 finished."
