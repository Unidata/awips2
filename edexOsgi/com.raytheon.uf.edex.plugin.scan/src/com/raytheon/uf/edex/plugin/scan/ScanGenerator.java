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
 * Contractor Address:     6825 Pine Street, Suite 144
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.uf.edex.plugin.scan;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.uf.common.dataplugin.scan.ScanRecord;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.monitor.config.SCANRunSiteConfigurationManager;
import com.raytheon.uf.common.monitor.events.MonitorConfigEvent;
import com.raytheon.uf.common.monitor.events.MonitorConfigListener;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator;
import com.raytheon.uf.edex.dat.utils.DatMenuUtil;
import com.raytheon.uf.edex.dat.utils.ScanDataCache;

/**
 * Generator implementation for SCAN
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 25, 2013 1660        D. Hladky   Fixed SCAN configuration bug.
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract
 * May 12, 2014 3133        njensen     Remove unused field
 * Jul 10, 2014 2914        garmendariz Remove EnvProperties
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class ScanGenerator extends CompositeProductGenerator implements
        MonitorConfigListener {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScanGenerator.class);

    private static final String genName = "SCAN";

    private static final String productType = "scan";

    private int alarmMaxNumber = 0;

    private UFStatus.Priority alarmPriority = UFStatus.Priority.DEBUG;

    /** Set of icaos to filter for */
    private Set<String> icaos = null;

    /** run configuration manager **/
    public SCANRunSiteConfigurationManager srcm = null;

    /**
     * Public construction
     */
    public ScanGenerator() {
        super(genName, productType);
    }

    @Override
    protected void configureFilters() {

        statusHandler.handle(Priority.INFO, getGeneratorName()
                + " process Filter Config...");

        try {
            getRunConfig().readConfigXml();
        } catch (SerializationException e) {
            statusHandler.handle(Priority.ERROR,
                    "Couldn't read scan configuration!!!", e);
        }
        boolean configValid = getRunConfig().isPopulated();

        if (!configValid) {
            statusHandler.handle(Priority.WARN,
                    "Configuration for SCAN is invalid!!!");
            return;
        }

        icaos = new HashSet<String>(getRunConfig().getSiteNames());
    }

    @Override
    protected void createFilters() {
        // do more here if you wish
        ArrayList<URIFilter> tmp = new ArrayList<URIFilter>(icaos.size());
        Iterator<String> iter = icaos.iterator();

        while (iter.hasNext()) {
            String icao = iter.next();
            try {
                tmp.add(new ScanURIFilter(icao, this));
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Couldn't create SCAN URIFilter.." + icao
                                + " is not a known RADAR site.", e);
                iter.remove();
            }
        }
        filters = tmp.toArray(new ScanURIFilter[tmp.size()]);
    }

    /**
     * Slight difference in the way scan operates doesn't use the persistent
     * queue, ran into strange behavior.
     */
    @Override
    public void matchURIs(DataURINotificationMessage messages) {
        if (messages instanceof DataURINotificationMessage) {
            URIFilter[] filters = getFilters();
            if (filters != null) {
                for (URIFilter filter : filters) {
                    // only needed if scan becomes multi-threaded at topic level
                    synchronized (filter) {
                        if (filter.isMatched(messages)) {
                            generate(filter.createGenerateMessage());
                            filter.reset();
                        }
                    }
                }
            }
        }
    }

    @Override
    public void generateProduct(URIGenerateMessage genMessage) {
        try {
            ScanURIFilter sfilter = ((ScanURIGenerateMessage) genMessage)
                    .getFilter();
            @SuppressWarnings("rawtypes")
            HashMap<ScanTables, ScanTableData> tables = new HashMap<ScanTables, ScanTableData>();

            // new cell
            if (sfilter.cellIsNew()) {
                tables.put(ScanTables.CELL, sfilter.getCopy(ScanTables.CELL));
                statusHandler.handle(Priority.INFO, sfilter.getIcao()
                        + ": writing CELL record");
            }

            // new dmd
            if (sfilter.dmdIsNew()) {
                tables.put(ScanTables.DMD, sfilter.getCopy(ScanTables.DMD));
                statusHandler.handle(Priority.INFO, sfilter.getIcao()
                        + ": writing DMD record " + sfilter.getCellTilt());
            }

            // new tvs
            if (sfilter.tvsIsNew()) {
                // meaning a full cycle of TVS
                tables.put(ScanTables.TVS, sfilter.getCopy(ScanTables.TVS));
                statusHandler.handle(Priority.INFO, sfilter.getIcao()
                        + ": writing TVS record");
            }

            // new md
            if (sfilter.mdIsNew()) {
                // meaning a full cycle of MESO
                tables.put(ScanTables.MESO, sfilter.getCopy(ScanTables.MESO));
                statusHandler.handle(Priority.INFO, sfilter.getIcao()
                        + ": writing MESO record");
            }

            ScanRecord[] scanRecords = new ScanRecord[tables.size()];
            int i = 0;
            if (tables.size() > 0) {
                for (ScanTables table : tables.keySet()) {
                    String type = table.name();
                    ScanRecord scanRec = new ScanRecord();
                    scanRec.setType(type);
                    scanRec.setTilt(sfilter.getTilt(table));
                    scanRec.setIcao(sfilter.getIcao());
                    scanRec.setLastElevationAngle(tables.get(table)
                            .getLastElevationAngle());
                    // experimental code
                    scanRec.setDataTime(new DataTime(sfilter.getValidTime()));
                    scanRec.setTableData(tables.get(table));
                    scanRec.setVolScanTime(tables.get(table).getVolScanTime());

                    scanRec.constructDataURI();
                    scanRecords[i] = scanRec;
                    i++;
                }

                this.setPluginDao(new ScanDao(productType));
                this.setPluginDataObjects(scanRecords);
                statusHandler.handle(Priority.INFO, sfilter.getIcao()
                        + ": Wrote records. " + i);

            }
        } catch (Exception e) {
            e.printStackTrace();
            statusHandler.handle(Priority.ERROR, "Can not run SCAN.");
        }
    }

    @Override
    public boolean isRunning() {
        return getConfigManager().getScanState();
    }

    /**
     * run config manager
     * 
     * @return
     */
    public SCANRunSiteConfigurationManager getRunConfig() {
        if (srcm == null) {
            srcm = SCANRunSiteConfigurationManager.getInstance();
            srcm.addListener(this);
        }
        return srcm;
    }

    /**
     * Gets the cache of data
     * 
     * @return
     */
    public ScanDataCache getCache() {
        return ScanDataCache.getInstance();
    }

    /**
     * Sets the checks number of alarms
     * 
     * @param alarmMaxNumber
     */
    public void setAlarmMaxNumber(int alarmMaxNumber) {
        this.alarmMaxNumber = alarmMaxNumber;
    }

    /**
     * gets the maximum number of alarms
     * 
     * @return
     */
    public int getAlarmMaxNumber() {
        return alarmMaxNumber;
    }

    /**
     * Sets the max priority to be sent out
     * 
     * @param alarmPriority
     */
    public void setAlarmPriority(UFStatus.Priority alarmPriority) {
        this.alarmPriority = alarmPriority;
    }

    /**
     * Gets the max priority of the message
     * 
     * @return
     */
    public UFStatus.Priority getAlarmPriority() {
        return alarmPriority;
    }

    @Override
    public void configChanged(MonitorConfigEvent fce) {

        if (fce.getSource() instanceof SCANRunSiteConfigurationManager) {
            statusHandler.handle(Priority.INFO,
                    "Re-configuring SCAN URI filters...Run Site Config change");
            resetFilters();

            DatMenuUtil dmu = new DatMenuUtil();
            dmu.setDatSite(SiteUtil.getSite());
            dmu.setOverride(true);
            dmu.createMenus();
        }
    }
}
