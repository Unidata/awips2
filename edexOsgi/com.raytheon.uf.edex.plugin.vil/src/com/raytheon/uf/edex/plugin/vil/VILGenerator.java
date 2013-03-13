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
package com.raytheon.uf.edex.plugin.vil;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.vil.VILRecord;
import com.raytheon.uf.common.dataplugin.vil.VILRecord.DATA_TYPE;
import com.raytheon.uf.common.dataplugin.vil.dao.VILDao;
import com.raytheon.uf.common.monitor.config.SCANRunSiteConfigurationManager;
import com.raytheon.uf.common.monitor.events.MonitorConfigEvent;
import com.raytheon.uf.common.monitor.events.MonitorConfigListener;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator;
import com.raytheon.uf.edex.plugin.vil.common.VILConfig;

/**
 * VILGenerator Product
 * 
 * VIL files for use in EDEX.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/07/2009   2037       dhladky    Initial Creation.
 * 02/25/13     1660       D. Hladky   Fixed SCAN configuration bug.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class VILGenerator extends CompositeProductGenerator implements
        MonitorConfigListener {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(VILGenerator.class);

    private static final String genName = "VIL";

    private static final String productType = "vil";

    /** Set of icaos to filter for */
    private Set<String> icaos = null;
    
    /** run configuration manager **/
    public SCANRunSiteConfigurationManager srcm = null;

    /**
     * Public constructor for VILGenerator
     * 
     * @param name
     * @param compositeProductType
     */
    public VILGenerator() {
        super(genName, productType);
    }

    @Override
    protected void createFilters() {
        // do more here if you wish
        ArrayList<URIFilter> tmp = new ArrayList<URIFilter>(icaos.size());
        Iterator<String> iter = icaos.iterator();

        while (iter.hasNext()) {
            String icao = iter.next();
            try {
                tmp.add(new VILURIFilter(icao));
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Couldn't create VIL URIFilter.." + icao
                                + " is not a know RADAR site.", e);
                iter.remove();
            }
        }
        filters = tmp.toArray(new VILURIFilter[tmp.size()]);
    }

    @Override
    protected void configureFilters() {

        statusHandler.handle(Priority.INFO, getGeneratorName()
                + " process Filter Config...");

        try {
            getRunConfig().readConfigXml();
        } catch (SerializationException e) {
            statusHandler.handle(Priority.ERROR,
                    "Couldn't read VIL(scan) configuration!!!", e);
        }
        boolean configValid = getRunConfig().isPopulated();

        if (!configValid) {
            statusHandler.handle(Priority.WARN,
            "Configuration for vil(scan) is invalid!!!");
            return;
        }

        icaos = new HashSet<String>(getRunConfig().getSiteNames());
    }

    @Override
    public void generateProduct(URIGenerateMessage genMessage) {
        // returns Data URIS of vil, dvil, et, eet.
        VILConfig vil_config = null;

        try {
            vil_config = new VILConfig(genMessage, this);

            if (vil_config.getMode()) {
                VIL vil = new VIL(vil_config);
                vil.genVIL();
                this.setPluginDao(new VILDao(productType));
                VILRecord[] scanRecords = new VILRecord[DATA_TYPE.values().length];
                int i = 0;

                for (DATA_TYPE name : DATA_TYPE.values()) {
                    // create a record for each type
                    VILRecord vilRec = new VILRecord();
                    if (name.equals(DATA_TYPE.DVILD)
                            || name.equals(DATA_TYPE.EDVILD)) {
                        vilRec.setNx(ScanUtils.SCAN_GRID_DIM_HALFKM);
                        vilRec.setNy(ScanUtils.SCAN_GRID_DIM_HALFKM);
                        vilRec.setDx(ScanUtils.SCAN_GRID_HALFK_DIM_RESOLUTION);
                        vilRec.setDy(ScanUtils.SCAN_GRID_HALFK_DIM_RESOLUTION);
                    } else if (name.equals(DATA_TYPE.VILD)) {
                        vilRec.setNx(ScanUtils.SCAN_GRID_DIM);
                        vilRec.setNy(ScanUtils.SCAN_GRID_DIM);
                        vilRec.setDx(ScanUtils.SCAN_GRID_DIM_RESOLUTION / 2);
                        vilRec.setDy(ScanUtils.SCAN_GRID_DIM_RESOLUTION / 2);
                    }

                    vilRec.setPluginName(this.getCompositeProductType());
                    vilRec.setIcao(vil_config.getIcao());
                    vilRec.setDataTime(this.getProductTime());
                    vilRec.setSpatialInfo(vil_config.getSpatialInfo());
                    vilRec.setFieldName(name.name());
                    vilRec.setDataArray(vil.getFloatArrays().get(name.name()));
                    vilRec.constructDataURI();
                    scanRecords[i] = vilRec;
                    i++;
                }

                this.setPluginDataObjects(scanRecords);
                statusHandler.handle(Priority.INFO, vil_config.getIcao()
                        + ": Wrote VILRecords." + i);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Can not run VIL. " + e);
        }
    }

    @Override
    public boolean isRunning() {
        return getConfigManager().getVILState();
    }

    @Override
    public void configChanged(MonitorConfigEvent fce) {
        if (fce.getSource() instanceof SCANRunSiteConfigurationManager) {
            statusHandler.handle(Priority.INFO,
                    "Re-configuring VIL URI filters...Run Site Config change");
            resetFilters();
        }
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

}
