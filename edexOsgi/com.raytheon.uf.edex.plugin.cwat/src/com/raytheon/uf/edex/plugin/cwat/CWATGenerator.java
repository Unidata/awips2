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
package com.raytheon.uf.edex.plugin.cwat;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.cwat.CWATRecord;
import com.raytheon.uf.common.dataplugin.cwat.dao.CWATDao;
import com.raytheon.uf.common.monitor.config.SCANRunSiteConfigurationManager;
import com.raytheon.uf.common.monitor.events.MonitorConfigEvent;
import com.raytheon.uf.common.monitor.events.MonitorConfigListener;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator;
import com.raytheon.uf.edex.plugin.cwat.common.CWATConfig;

/**
 * CWATGenerator Product
 * 
 * CWAT files for use in EDEX.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/25/13     1660       D. Hladky Fixed configuration bug in scan.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class CWATGenerator extends CompositeProductGenerator implements
        MonitorConfigListener {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CWATGenerator.class);

    private static final String genName = "CWAT";

    private static final String productType = "cwat";

    /** Set of icaos to filter for */
    private Set<String> icaos = null;
    
    /** run configuration manager **/
    public SCANRunSiteConfigurationManager srcm = null;

    /**
     * Public CWAT constructor
     * 
     * @param name
     * @param compositeProductType
     */
    public CWATGenerator() {
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
                    "Couldn't read CWAT(scan) configuration!!!", e);
        }
        boolean configValid = getRunConfig().isPopulated();

        if (!configValid) {
            statusHandler.handle(Priority.WARN,
            "Configuration for CWAT(scan) is invalid!!!");
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
                tmp.add(new CWATURIFilter(icao));
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Couldn't create CWAT Filter.." + icao
                                + " is not a viable RADAR site.", e);
                iter.remove();
            }
        }
        filters = tmp.toArray(new CWATURIFilter[tmp.size()]);
    }

    @Override
    public void generateProduct(URIGenerateMessage genMessage) {
        try {
            CWATConfig cwa_config = null;
            try {
                cwa_config = new CWATConfig(genMessage, this);
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "CWAT Configuration parameters for run not met...",e);
                return;

            }

            CWAThreat cwa = new CWAThreat(cwa_config);
            cwa.genCWA();
            this.setPluginDao(new CWATDao(productType));

            // create a record for each type
            CWATRecord cwaRec = new CWATRecord();
            cwaRec.setNx(cwa_config.getVil().getNumBins());
            cwaRec.setNy(cwa_config.getVil().getNumRadials());

            if (cwa_config.getVil().getNumBins() == ScanUtils.SCAN_GRID_DIM) {
                cwaRec.setDx(ScanUtils.SCAN_GRID_DIM_RESOLUTION);
                cwaRec.setDy(ScanUtils.SCAN_GRID_DIM_RESOLUTION);
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Incorrect Resolution of SCAN Grid..."
                                + cwa_config.getVil().getNumBins());
            }

            cwaRec.setPluginName(cwa_config.getGenerator()
                    .getCompositeProductType());
            cwaRec.setIcao(cwa_config.getIcao());
            cwaRec.setDataTime(new DataTime(cwa_config.getCZ().getDataTime()
                    .getRefTime()));
            cwaRec.setSpatialInfo(cwa_config.getSpatialInfo());
            cwaRec.setFieldName(CWATRecord.DATA_TYPE.CWAT.name());
            cwaRec.setDataArray(cwa.getCWAThreat());
            cwaRec.setThreats(cwa.getThreatConditions());
            cwaRec.setMaxScti(cwa.getMaxSCTI());

            cwaRec.constructDataURI();

            this.setPluginDataObjects(new CWATRecord[] { cwaRec });

            statusHandler.handle(Priority.INFO, cwa_config.getIcao()
                    + ": CWAT generated. " + cwa.getMaxSCTI());

        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Couldn't process CWAT.."
                    + genMessage.getName(), e);
        }
    }

    @Override
    public boolean isRunning() {
        return getConfigManager().getCWATState();
    }

    @Override
    public void configChanged(MonitorConfigEvent fce) {
        if (fce.getSource() instanceof SCANRunSiteConfigurationManager) {
            statusHandler.handle(Priority.INFO,
                    "Re-configuring CWAT URI filters...Run Site Config change");
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
