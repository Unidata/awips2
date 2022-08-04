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
package com.raytheon.uf.edex.plugin.ffmp;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.plugin.radar.dao.RadarStationDao;
import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataplugin.radar.util.RadarsInUseUtil;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.DataType;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.SourceIngestConfigXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * Creates and saves a default FFMPRunConfig.xml at the CONFIGURED localization
 * level. Code extracted from FFMPGenerator.java.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2018  6695      njensen     Initial creation
 * Jul 30, 2018  6720      njensen     Update for changed method names
 * Aug 14, 2018  6720      njensen     Use simplified enums
 *
 * </pre>
 *
 * @author njensen
 */

public class FFMPDefaultConfigCreator {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPDefaultConfigCreator.class);

    private FFMPDefaultConfigCreator() {
        // don't allow instantiation
    }

    public static DomainXML generateDefaultRunConfig(
            FFMPSourceConfigurationManager fscm,
            FFMPRunConfigurationManager frcm) {
        FFMPRunXML runner = new FFMPRunXML();
        List<ProductRunXML> products = new ArrayList<>();

        // these two are always there in default setups
        ProductRunXML hpeProduct = new ProductRunXML();
        hpeProduct.setProductName("DHRMOSAIC");
        hpeProduct.setProductKey("hpe");
        products.add(hpeProduct);

        ProductRunXML biasHpeProduct = new ProductRunXML();
        biasHpeProduct.setProductName("BDHRMOSAIC");
        biasHpeProduct.setProductKey("bhpe");
        products.add(biasHpeProduct);

        // TODO Did someone forget MRMS or is it intentionally left out?

        List<String> rfc = new ArrayList<>();
        List<String> sites = RadarsInUseUtil.getSite(null,
                RadarsInUseUtil.LOCAL_CONSTANT);
        if (sites.isEmpty()) {
            RadarStationDao dao = new RadarStationDao();
            List<RadarStation> stations = null;
            try {
                stations = dao.queryByWfo(SiteUtil.getSite());
            } catch (DataAccessLayerException e) {
                statusHandler.handle(Priority.ERROR,
                        "Unable to access data object for radar station table",
                        e);
            }

            for (RadarStation station : stations) {
                // this is just for a default
                ProductRunXML dhrProduct = new ProductRunXML();
                dhrProduct.setProductName("DHR");
                dhrProduct.setProductKey(station.getRdaId().toLowerCase());
                products.add(dhrProduct);

                String newRfc = FFMPUtils.getRFC(dhrProduct.getProductKey());
                if (!rfc.contains(newRfc)) {
                    rfc.add(newRfc);
                }

                sites.add(station.getRdaId().toLowerCase());
            }

        } else {
            for (String site : sites) {
                // this is just for a default
                ProductRunXML dhrProduct = new ProductRunXML();
                dhrProduct.setProductName("DHR");
                dhrProduct.setProductKey(site);
                products.add(dhrProduct);

                String newRfc = FFMPUtils.getRFC(dhrProduct.getProductKey());
                if (!rfc.contains(newRfc)) {
                    rfc.add(newRfc);
                }
            }
        }

        runner.setProducts(products);

        // Apply site list to all QPE types
        for (String sourceName : fscm.getQPESourceNames()) {
            SourceXML qpeSource = fscm.getSource(sourceName);
            // Radar Derived sources use the primary source site keys for
            // mosiac datakey
            // Auto Config for any Radar derived sources
            if (qpeSource.getDataType() == DataType.RADAR) {
                SourceIngestConfigXML sicm = new SourceIngestConfigXML();
                sicm.setSourceName(qpeSource.getSourceName());
                sicm.setUriSubLocation(3);

                for (String siteid : sites) {
                    sicm.addDataKey(siteid);
                }

                runner.addSourceIngest(sicm);
            }
        }

        // We have a list of available RFC's, now find mosaic
        // Apply this to all RFCFFG sources
        for (String sourceName : fscm.getGuidanceSourceNames()) {
            SourceXML guidSource = fscm.getSource(sourceName);

            // Auto config for RFC sources
            if (guidSource.isRfc()) {
                // add a source mosaic config to the Run Config
                SourceIngestConfigXML sicm = new SourceIngestConfigXML();
                sicm.setSourceName(guidSource.getSourceName());
                sicm.setUriSubLocation(3);

                for (String dataKey : rfc) {
                    sicm.addDataKey(dataKey);
                }

                runner.addSourceIngest(sicm);
            }
        }

        // Apply site list to all SCANQPF default
        for (String sourceName : fscm.getQPFSourceNames()) {
            SourceXML qpfSource = fscm.getSource(sourceName);
            // Radar Derived sources use the primary source site keys for
            // mosiac datakey
            // Auto Config for any Radar derived sources (QPFSCAN) for
            // example
            if ("QPFSCAN".equals(qpfSource.getSourceName())) {
                SourceIngestConfigXML sicm = new SourceIngestConfigXML();
                sicm.setSourceName(qpfSource.getSourceName());
                sicm.setUriSubLocation(3);

                for (String siteid : sites) {
                    sicm.addDataKey(siteid);
                }

                runner.addSourceIngest(sicm);
            }
        }

        DomainXML domain = new DomainXML();
        domain.setPrimary(true);
        domain.setCwa(EDEXUtil.getEdexSite());
        runner.addDomain(domain);

        frcm.setFFMPRunner(runner);
        frcm.saveConfigXml();
        frcm.setPopulated(true);

        return domain;
    }

}
