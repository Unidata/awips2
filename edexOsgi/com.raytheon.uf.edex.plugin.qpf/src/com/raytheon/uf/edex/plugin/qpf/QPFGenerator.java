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
package com.raytheon.uf.edex.plugin.qpf;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.qpf.QPFRecord;
import com.raytheon.uf.common.dataplugin.qpf.QPFRecord.DATA_TYPE;
import com.raytheon.uf.common.monitor.config.SCANRunSiteConfigurationManager;
import com.raytheon.uf.common.monitor.events.MonitorConfigEvent;
import com.raytheon.uf.common.monitor.events.MonitorConfigListener;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator;
import com.raytheon.uf.edex.plugin.qpf.common.QPFConfig;

/**
 * QPFGenerator Product
 * 
 * QPF files for use in EDEX.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 07, 2009 1981       dhladky     Initial Creation.
 * Apr 27, 2012 562        dgilling    Accept getter and setter  renames in
 *                                     QPFRecord.
 * Feb 25, 2013 1660       D. Hladky Fixed configuration bug in scan.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class QPFGenerator extends CompositeProductGenerator implements
        MonitorConfigListener {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(QPFGenerator.class);

    private static final String genName = "QPF";

    private static final String productType = "qpf";

    /** run configuration manager **/
    public SCANRunSiteConfigurationManager srcm = null;

    /** Set of icaos to filter for */
    private Set<String> icaos = null;

    /**
     * Public constructor for QPFGenerator
     * 
     * @param name
     * @param compositeProductType
     */
    public QPFGenerator() {
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
                tmp.add(new QPFURIFilter(icao));
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Couldn't create QPF URIFilter.." + icao
                                + " is not a know RADAR site.", e);
                iter.remove();
            }
        }
        filters = tmp.toArray(new QPFURIFilter[tmp.size()]);
    }

    @Override
    protected void configureFilters() {

        statusHandler.handle(Priority.INFO, getGeneratorName()
                + " process Filter Config...");

        try {
            getRunConfig().readConfigXml();
        } catch (SerializationException e) {
            statusHandler.handle(Priority.ERROR,
                    "Couldn't read qpf(scan) configuration!!!", e);
        }
        boolean configValid = getRunConfig().isPopulated();

        if (!configValid) {
            statusHandler.handle(Priority.WARN,
                    "Configuration for qpf(scan) is invalid!!!");
            return;
        }

        icaos = new HashSet<String>(getRunConfig().getSiteNames());
    }

    @Override
    public void generateProduct(URIGenerateMessage genMessage) {
        // returns Data URIS of early,current for vil, cz, sti and any models.
        QPFConfig qpf_config = null;
        try {
            qpf_config = new QPFConfig((QPFURIGenerateMessage) genMessage, this);

            if (qpf_config.getMode()) {
                QPF qpf = new QPF(qpf_config);
                qpf.genQPF();
                this.setPluginDao(new QPFDao(productType));
                QPFRecord[] qpfRecords = new QPFRecord[DATA_TYPE.values().length];
                int i = 0;

                for (DATA_TYPE name : DATA_TYPE.values()) {
                    // create a record for each type
                    QPFRecord qpfRec = new QPFRecord();
                    qpfRec.setNx(qpf_config.getCurrentVil().getNumBins());
                    qpfRec.setNy(qpf_config.getCurrentVil().getNumRadials());
                    qpfRec.setEwvelocity(qpf.getWEVelocity());
                    qpfRec.setNsvelocity(qpf.getRNSVelocity());
                    qpfRec.setAvgspd(qpf.getAVGSpd());
                    qpfRec.setAvgdir(qpf.getAVGDir());

                    if (qpf_config.getCurrentVil().getNumBins() == ScanUtils.SCAN_GRID_DIM) {
                        qpfRec.setDx(ScanUtils.SCAN_GRID_DIM_RESOLUTION);
                        qpfRec.setDy(ScanUtils.SCAN_GRID_DIM_RESOLUTION);
                    } else {
                        statusHandler.handle(Priority.PROBLEM,
                                "Incorrect Resolution of QPF Grid..."
                                        + qpf_config.getCurrentVil()
                                                .getNumBins());
                    }

                    qpfRec.setIcao(qpf_config.getIcao());
                    qpfRec.setDataTime(this.getProductTime());
                    qpfRec.setSpatialInfo(qpf_config.getSpatialInfo());
                    qpfRec.setFieldName(name.name());
                    qpfRec.setDataArray(qpf.getFloatArrays().get(name.name()));
                    qpfRec.constructDataURI();
                    qpfRecords[i] = qpfRec;
                    i++;
                }

                this.setPluginDataObjects(qpfRecords);
                statusHandler.handle(Priority.INFO, qpf_config.getIcao()
                        + ": Wrote QPFRecords." + i);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Can not run QPF.", e);
        }
    }

    @Override
    public boolean isRunning() {
        return getConfigManager().getQPFState();
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

    @Override
    public void configChanged(MonitorConfigEvent fce) {
        if (fce.getSource() instanceof SCANRunSiteConfigurationManager) {
            statusHandler.handle(Priority.INFO,
                    "Re-configuring QPF URI filters...Run Site Config change");
            resetFilters();
        }
    }

}
