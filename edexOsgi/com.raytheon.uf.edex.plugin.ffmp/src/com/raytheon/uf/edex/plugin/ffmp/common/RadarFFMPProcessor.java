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
package com.raytheon.uf.edex.plugin.ffmp.common;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasinMetaData;
import com.raytheon.uf.common.dataplugin.ffmp.SourceBin;
import com.raytheon.uf.common.dataplugin.ffmp.SourceBinEntry;
import com.raytheon.uf.common.dataplugin.ffmp.SourceBinList;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.DHRValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataInterrogator;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.plugin.ffmp.FFMPGenerator;
import org.locationtech.jts.geom.Coordinate;

/**
 * Processes radar data for FFMP. Logic extracted from FFMPProcessor.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 07, 2018 6560       njensen     Initial creation
 *
 * </pre>
 *
 * @author njensen
 */

public class RadarFFMPProcessor extends FFMPProcessor {

    protected RadarRecord radarRec = null;

    /** the DHR values */
    protected Map<DHRValues, Double> dhrMap = null;

    protected RadarDataInterrogator rdi = null;

    /**
     * Public constructor
     * 
     * @param config
     */
    public RadarFFMPProcessor(FFMPConfig config, FFMPGenerator generator,
            FFMPRecord ffmpRec, FFMPTemplates template) {
        super(config, generator, ffmpRec, template);
    }

    @Override
    protected Date initialSetup(Object data) throws Exception {
        Date recdate = null;

        try {
            radarRec = (RadarRecord) data;
            if ("DHR".equals(radarRec.getMnemonic())) {
                dhrMap = RadarRecordUtil.getDHRValues(radarRec);
                statusHandler.handle(Priority.INFO,
                        "DHR Bias: " + dhrMap.get(DHRValues.BIAS_TO_USE));
                statusHandler.handle(Priority.INFO, "DHR HailCap: "
                        + dhrMap.get(DHRValues.MAXPRECIPRATEALLOW));
            }

            recdate = getDataDate();
        } catch (Exception e) {
            logAndThrowConfigurationException(getDataType(), e);
        }

        statusHandler.handle(Priority.INFO,
                "Source Expiration: " + source.getExpirationMinutes(siteKey));

        return recdate;
    }

    @Override
    protected SourceBinList makeNewSBL(String cwa, LinkedHashMap<Long, ?> map) {
        SourceBinList newSBL = null;
        if (cwaGeometries == null || cwaGeometries.isEmpty()) {
            cwaGeometries = template.getRawGeometries(dataKey, cwa);
        }
        // DR15684
        try {
            newSBL = (new RadarSBLGenerator().generate(sourceId, map.keySet(),
                    cwaGeometries, radarRec));
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN,
                    "Error generating Source Bin List", e);
            if (!checkLockStatus()) {
                ClusterLockUtils.unlock(sourceBinTaskName, sourceId);
            }
        }
        if (newSBL != null) {
            generator.setSourceBinList(newSBL);
            existingSBL = true;
        }
        return newSBL;
    }

    /**
     * Overridden due to different logic.
     */
    @Override
    protected void processVirtualGageBasins() {
        /*
         * process each domain separately, but within the same URI/HDF5 named by
         * primary domain
         */
        for (DomainXML domain : template.getDomains()) {
            LinkedHashMap<String, FFMPVirtualGageBasinMetaData> vmap = template
                    .getVirtualGageBasins(siteKey, domain.getCwa());

            for (Entry<String, FFMPVirtualGageBasinMetaData> entry : vmap
                    .entrySet()) {
                try {
                    FFMPVirtualGageBasinMetaData fvgbmd = entry.getValue();

                    if (fvgbmd != null) {
                        FFMPVirtualGageBasin basin = getVirtualBasin(
                                fvgbmd.getLid(), fvgbmd.getLookupId());

                        Date date = getDataDate();
                        SourceBinEntry sbe = new SourceBinEntry();
                        sbe.setCoor(fvgbmd.getCoordinate());
                        sbe.setArea(1.0);
                        List<SourceBinEntry> sourceBins = new ArrayList<>();
                        sourceBins.add(sbe);
                        Float val = processRADAR(sourceBins);

                        // Missing doesn't work well with Virtual Gage's
                        if (val == FFMPUtils.MISSING) {
                            val = 0.0f;
                        }

                        if (date != null) {
                            basin.setValue(date, val);
                        }
                    }
                } catch (Exception e) {
                    ffmpRec = null;
                    statusHandler.error("Unable to process VGB", e);
                }
            }
        }
    }

    /**
     * Get average radar value for a given geometry
     * 
     * @param pfaf
     * @param geo
     * @return
     */
    private float processRADAR(Long pfaf) {
        List<SourceBinEntry> entries = null;
        float val = 0.0f;

        if (existingSBL) {
            SourceBin bin = sbl.getMap(pfaf);
            if (bin != null) {
                entries = bin.getEntries();
            }
        }

        if (entries != null) {
            val = processRADAR(entries);
        }

        return val;
    }

    /**
     * Get average radar value for a given coordinate
     * 
     * @param entries
     * @return
     */
    private float processRADAR(List<SourceBinEntry> entries) {
        double val = 0.0f;
        double area = 0.0f;

        if (rdi == null) {
            rdi = new RadarDataInterrogator(radarRec);
        }

        Coordinate[] coors = new Coordinate[entries.size()];
        double[] areas = new double[entries.size()];

        for (int i = 0; i < entries.size(); i++) {
            coors[i] = entries.get(i).getCoor();
            areas[i] = entries.get(i).getArea();
        }

        int[] dataVals = rdi.getDataValues(coors);

        if ("DHR".equals(radarRec.getMnemonic())) {
            for (int j = 0; j < dataVals.length; j++) {
                try {
                    // fval, DR 13083
                    val += ScanUtils.getZRvalue2(dataVals[j],
                            dhrMap.get(DHRValues.ZRMULTCOEFF),
                            dhrMap.get(DHRValues.MAXPRECIPRATEALLOW),
                            dhrMap.get(DHRValues.ZRPOWERCOEFF),
                            dhrMap.get(DHRValues.BIAS_TO_USE)) * areas[j];
                    area += areas[j];
                } catch (Exception e) {
                    statusHandler.error(
                            "Error getting DHR parameters, can't process", e);
                }
            }
        } else if ("DPR".equals(radarRec.getMnemonic())) {
            for (int j = 0; j < dataVals.length; j++) {
                val += ScanUtils.decodeDPRValue(dataVals[j]) * areas[j];
                area += areas[j];
            }
        }

        return (float) (val / area);
    }

    @Override
    protected void addExtraLogging(StringBuilder sb) {
        if (radarRec != null) {
            sb.append("Record: ").append(radarRec.getDataURI()).append("\n");
        }
    }

    @Override
    protected float processBasin(Long pfaf, String cwa) throws Exception {
        return processRADAR(pfaf);
    }

    @Override
    protected float processCoordinate(Coordinate coord, double area) {
        /*
         * unused by radar since this overrides processVirtualGageBasins()
         */
        return 0;
    }

    @Override
    protected void setupCache() throws Exception {
        // do nothing, no cache for radar
    }

    @Override
    protected Date getDataDate() {
        return radarRec.getDataTime().getRefTime();
    }

    @Override
    protected GridGeometry2D getGridGeometry() {
        /*
         * not supported and not used since this overrides
         * processVirtualGageBasins()
         */
        return null;
    }

}
