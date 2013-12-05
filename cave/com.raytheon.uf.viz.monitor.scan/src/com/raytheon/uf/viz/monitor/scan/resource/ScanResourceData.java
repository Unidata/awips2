/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.monitor.scan.resource;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.scan.ScanRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.monitor.scan.ScanMonitor;

/**
 * Provides the metadata and constructor for scan Radar
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2009            dhladky     Initial creation
 * Feb 28, 2013 1731       bsteffen    Optimize construction of scan resource.
 * Apr 18, 2013 1926       njensen     Reuse URIs in construction of resource
 * Jul 24, 2013 2218       mpduff      Changed method signature.
 * Aug 15, 2013 2143       mpduff      Add missing data check.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * 04 Dec 2013  #2592      lvenable    Added check to ensure the PluginDataObject
 *                                     array has at least one element.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "scanResourceData")
public class ScanResourceData extends AbstractRequestableResourceData {

    @XmlAttribute
    public String tableType;

    @XmlAttribute
    public String icao;

    public HashMap<DataTime, ScanRecord> dataObjectMap;

    public ScanMouseAdapter sma;

    public ScanMonitor monitor = null;

    /**
     * 
     */
    public ScanResourceData() {
        // initialize the scan monitor to be used throughout
        getScan();
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {

        if (objects.length > 0) {

            List<String> uris = getScan().getAvailableUris(
                    ScanTables.valueOf(tableType), icao);
            try {
                long t0 = System.currentTimeMillis();
                // Forces ScanMonitor to grab data back for one extra hour 1/2
                // past the first time.
                Calendar firstCal = ((ScanRecord) objects[0]).getDataTime()
                        .getRefTimeAsCalendar();
                firstCal.add(Calendar.MINUTE, -90);
                Date firstDate = firstCal.getTime();
                int count = 0;
                List<ScanRecord> recordsToLoad = new ArrayList<ScanRecord>(
                        uris.size());
                for (String uri : uris) {
                    ScanRecord record = new ScanRecord(uri);
                    if (record.getDataTime().getRefTime().after(firstDate)) {
                        recordsToLoad.add(record);
                    }
                }
                ScanRecord[] records = recordsToLoad.toArray(new ScanRecord[0]);

                populateRecords(records);
                for (ScanRecord record : records) {
                    if ((record.getTableData() != null)
                            && (record.getDataTime() != null)
                            && (record.getTableData().getVolScanTime() != null)) {

                        getScan().setTableData(icao, record.getTableData(),
                        /*
                         * TODO: This should be the volume scan time, but
                         * {Radar,Scan}Record.getVolScanTime is actually the
                         * radar product generation time.
                         */
                        record.getDataTime().getRefTime(), record.getTilt(),
                                record.getDataTime().getRefTime(), tableType);
                        count++;

                        if (record.getType().equals(ScanTables.DMD.name())) {
                            if (dataObjectMap == null) {
                                dataObjectMap = new HashMap<DataTime, ScanRecord>();
                            }
                            dataObjectMap.put(record.getDataTime(), record);
                        }
                    }
                }

                // populate the DMD data map
                if (tableType.equals(ScanTables.DMD.name())) {
                    if (dataObjectMap != null) {
                        for (ScanRecord record : dataObjectMap.values()) {
                            getScan().setTableData(icao, record.getTableData(),
                            /*
                             * TODO: This should be the volume scan time, but
                             * {Radar,Scan}Record.getVolScanTime is actually the
                             * radar product generation time.
                             */
                            record.getDataTime().getRefTime(),
                                    record.getTilt(),
                                    record.getDataTime().getRefTime(),
                                    record.getType());
                            getScan().setDmdTilt(record.getTilt(), icao);
                            getScan().addDmdScanRecord(record);
                        }
                    }
                }
                long t4 = System.currentTimeMillis();

                System.out.println("Loaded " + count + " out of " + uris.size()
                        + " objects in " + (t4 - t0) + "ms");
                // need to update the dialog here after the
                // scanResourceData has been fully populated
                getScan().setInstantiated(true);
                if ((getScan().getDialog(ScanTables.valueOf(tableType), icao) != null)
                        && !getScan()
                                .getDialog(ScanTables.valueOf(tableType), icao)
                                .getCurrentShell().isDisposed()) {
                    DataTime time = getScan().getMostRecent(tableType, icao);
                    if (time != null) {
                        getScan().updateDialog(
                                ScanTables.valueOf(tableType),
                                icao,
                                time.getRefTime(),
                                time.getRefTime(),
                                getScan().getTiltAngle(
                                        ScanTables.valueOf(tableType), icao));
                    }
                }
            } catch (Exception e) {
                getScan().closeDialog(icao);
            }
        }
        return new ScanResource(this, loadProperties);
    }

    /**
     * populate Scan Record
     * 
     * @param record
     */
    public ScanRecord populateRecord(ScanRecord record) throws VizException {
        populateRecords(new ScanRecord[] { record });
        return record;
    }

    public void populateRecords(ScanRecord[] records) throws VizException {
        Map<File, Set<ScanRecord>> fileMap = new HashMap<File, Set<ScanRecord>>();
        for (ScanRecord record : records) {
            File loc = HDF5Util.findHDF5Location(record);
            Set<ScanRecord> recordSet = fileMap.get(loc);
            if (recordSet == null) {
                recordSet = new HashSet<ScanRecord>();
                fileMap.put(loc, recordSet);
            }
            recordSet.add(record);
        }
        for (Entry<File, Set<ScanRecord>> fileEntry : fileMap.entrySet()) {
            IDataStore dataStore = DataStoreFactory.getDataStore(fileEntry
                    .getKey());

            String[] datasetGroupPath = new String[fileEntry.getValue().size()];
            ScanRecord[] scanRecords = new ScanRecord[datasetGroupPath.length];
            int i = 0;
            for (ScanRecord record : fileEntry.getValue()) {
                datasetGroupPath[i] = record.getDataURI()
                        + DataStoreFactory.DEF_SEPARATOR + record.getType();
                scanRecords[i] = record;
                i += 1;
            }
            try {
                IDataRecord[] dataRecords = dataStore.retrieveDatasets(
                        datasetGroupPath, Request.ALL);
                for (i = 0; i < dataRecords.length; i += 1) {
                    ByteDataRecord byteData = (ByteDataRecord) dataRecords[i];
                    scanRecords[i].setTableData(byteData);
                }
            } catch (Exception e) {
                throw new VizException(e);
            }
        }
    }

    // create the monitor instance
    public ScanMonitor getScan() {
        if (monitor == null) {
            monitor = ScanMonitor.getInstance();
        }
        return monitor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * getAvailableTimes()
     */
    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        if (tableType.equals(ScanTables.DMD.name())) {
            DataTime[] allTimes = super.getAvailableTimes();

            List<DataTime> dataList = new ArrayList<DataTime>();

            long[] times = monitor.getDMDMaxAngleTimes(icao);
            int index = (times.length - 1) < 0 ? 0 : times.length - 1;
            if ((times != null) && (times.length != 0)) {
                for (DataTime allTime : allTimes) {
                    if (allTime.getRefTime() != null) {
                        if (allTime.getRefTime().getTime() == times[index]) {
                            dataList.add(allTime);
                            index--;
                            if (index == -1) {
                                break;
                            }
                        }
                    }
                }
            }
            monitor.setDataUpdated(true);

            return dataList.toArray(new DataTime[dataList.size()]);
        } else {
            return super.getAvailableTimes();
        }
    }

}
