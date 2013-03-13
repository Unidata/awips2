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
package com.raytheon.uf.edex.plugin.preciprate;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.preciprate.PrecipRateRecord;
import com.raytheon.uf.common.dataplugin.preciprate.dao.PrecipRateDao;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.DHRValues;
import com.raytheon.uf.common.monitor.config.SCANRunSiteConfigurationManager;
import com.raytheon.uf.common.monitor.events.MonitorConfigEvent;
import com.raytheon.uf.common.monitor.events.MonitorConfigListener;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator;
import com.raytheon.uf.edex.plugin.preciprate.common.PrecipRate;
import com.raytheon.uf.edex.plugin.preciprate.common.PrecipRateConfig;

/**
 * Generator implementation for Precipitation Rate
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 01/25/10      3796       D. Hladky   Initial release
 * 02/25/13     1660        D. Hladky   Fixed SCAN configuration bug.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class PrecipRateGenerator extends CompositeProductGenerator implements
MonitorConfigListener{
    
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PrecipRateGenerator.class);

    private static final String genName = "PrecipRate";

    private static final String productType = "preciprate";
    
    /** run configuration manager **/
    public SCANRunSiteConfigurationManager srcm = null;

    /** Set of icaos to filter for */
    private Set<String> icaos = null;

    public PrecipRateGenerator(String name, String compositeProductType) {
        super(genName, productType);
    }

    /** default thrift constructor **/
    public PrecipRateGenerator() {
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
                    "Couldn't read PrecipRate(scan) configuration!!!", e);
        }
        boolean configValid = getRunConfig().isPopulated();

        if (!configValid) {
            statusHandler.handle(Priority.WARN,
            "Configuration for PrecipRate(scan) is invalid!!!");
            return;
        }

        icaos = new HashSet<String>(getRunConfig().getSiteNames());
    }

    @Override
    protected void createFilters() {
        ArrayList<URIFilter> tmp = new ArrayList<URIFilter>(icaos.size());
        Iterator<String> iter = icaos.iterator();

        while (iter.hasNext()) {
            String icao = iter.next();
            try {
                tmp.add(new PrecipRateURIFilter(icao));
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Couldn't create PrecipRate URIFilter.." + icao
                                + " is not a know RADAR site.", e);
                iter.remove();
            }
        }
        filters = tmp.toArray(new PrecipRateURIFilter[tmp.size()]);
    }

    @Override
    public void generateProduct(URIGenerateMessage genMessage) {
        PrecipRateConfig preciprate_config = null;

        try {
            preciprate_config = new PrecipRateConfig(
                    (PrecipRateURIGenerateMessage) genMessage, this);

            if (preciprate_config.getMode()) {

                PrecipRateRecord precipRateRec = new PrecipRateRecord();
                this.setPluginDao(new PrecipRateDao(productType));
                precipRateRec.setPluginName(this.getCompositeProductType());
                precipRateRec.setDataTime(this.getProductTime());
                precipRateRec.setIcao(preciprate_config.getIcao());
                precipRateRec.setGateResolution(preciprate_config.getDHR()
                        .getGateResolution());
                precipRateRec.setNumBins(preciprate_config.getDHR()
                        .getNumBins());
                precipRateRec.setNumRadials(preciprate_config.getDHR()
                        .getNumRadials());
                precipRateRec.setAngleData(preciprate_config.getDHR()
                        .getAngleData());
                precipRateRec.setLocation(preciprate_config.getDHR()
                        .getLocation());
                precipRateRec.setMnemonic(preciprate_config.getDHR()
                        .getMnemonic());
                precipRateRec.setLatitude(preciprate_config.getDHR()
                        .getLatitude());
                precipRateRec.setLongitude(preciprate_config.getDHR()
                        .getLongitude());
                precipRateRec.setVolumeCoveragePattern(preciprate_config
                        .getDHR().getVolumeCoveragePattern());

                // populate object
                PrecipRate pr = new PrecipRate(preciprate_config, precipRateRec);
                precipRateRec = pr.getPrecipRate();
                precipRateRec.setDhrMap(pr.getDhrMap());

                precipRateRec.setAcoefficent(pr.getDhrMap().get(
                        DHRValues.ZRMULTCOEFF));
                precipRateRec.setBias(pr.getDhrMap().get(
                        DHRValues.BIAS));
                precipRateRec.setHailcap(pr.getDhrMap().get(
                        DHRValues.MAXPRECIPRATEALLOW));
                precipRateRec.setCoefficent(pr.getDhrMap().get(
                        DHRValues.ZRPOWERCOEFF));

                precipRateRec.constructDataURI();
                this.setPluginDataObjects(new PrecipRateRecord[] { precipRateRec });

                statusHandler.handle(Priority.INFO, precipRateRec.getIcao()
                        + " Successfully generated");
            }
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.ERROR, "Can not run PrecipRate. " + e);
        }
    }

    @Override
    public boolean isRunning() {
        return getConfigManager().getPrecipRateState();
    }

    @Override
    public void configChanged(MonitorConfigEvent fce) {
        if (fce.getSource() instanceof SCANRunSiteConfigurationManager) {
            statusHandler.handle(Priority.INFO,
                    "Re-configuring PrecipRate URI filters...Run Site Config change");
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
