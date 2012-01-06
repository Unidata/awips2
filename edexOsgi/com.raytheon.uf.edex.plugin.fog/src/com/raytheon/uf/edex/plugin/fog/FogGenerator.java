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
package com.raytheon.uf.edex.plugin.fog;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.fog.FogRecord;
import com.raytheon.uf.common.dataplugin.fog.dao.FogDao;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.plugin.fog.common.FogConfig;

public class FogGenerator extends CompositeProductGenerator {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FogGenerator.class);

    private static final String genName = "FOG";

    private String cwa = null;

    private static final String productType = "fog";

    /**
     * Public constructor for FogGenerator
     * 
     * @param name
     * @param compositeProductType
     */
    public FogGenerator() {
        super(genName, productType);
    }

    @Override
    protected void configureFilters() {
        cwa = SiteUtil.getSite();
    }

    @Override
    protected void createFilters() {
        filters = new URIFilter[1];
        filters[0] = new FogURIFilter(cwa);
    }

    @Override
    public void generateProduct(URIGenerateMessage genMessage) {
        // returns Data URIS of vis, ir3_9, ir10_7
        FogConfig fog_config = null;
        try {
            fog_config = new FogConfig((FogURIGenerateMessage) genMessage, this);

            FogRecord fogRec = new FogRecord();
            this.setPluginDao(new FogDao(productType));
            fogRec.setPluginName(this.getCompositeProductType());
            fogRec.setDataTime(this.getProductTime());
            fogRec.setRefHour(TimeTools.roundToNearestHour(fogRec.getDataTime()
                    .getValidTime()));
            fogRec.setCwa(fog_config.getCwa());
            fogRec.setDx(fog_config.getDx());
            fogRec.setDy(fog_config.getDy());
            fogRec.setNx(fog_config.getNx());
            fogRec.setNy(fog_config.getNy());
            fogRec.setLat(fog_config.getCenter().y);
            fogRec.setLon(fog_config.getCenter().x);
            fogRec.setSatHeight(fog_config.getSatHeight());
            fogRec.setSatLon(fog_config.getSatPos().x);

            if (fog_config.getVis() != null) {
                fogRec.setVisArray(fog_config.getVis());
            }
            if (fog_config.getIR3_9() != null) {
                fogRec.setIR_3_9Array(fog_config.getIR3_9());
            }
            if (fog_config.getIR10_7() != null) {
                fogRec.setIR_10_7Array(fog_config.getIR10_7());
            }

            fogRec.constructDataURI();

            this.setPluginDataObjects(new FogRecord[] { fogRec });

            statusHandler.info(getGeneratorName() + " wrote FogRecord:  \n"
                    + fogRec.toString());
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    @Override
    public boolean isRunning() {
        return getConfigManager().getFogState();
    }

}
