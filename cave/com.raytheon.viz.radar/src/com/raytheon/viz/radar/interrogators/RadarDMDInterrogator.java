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
package com.raytheon.viz.radar.interrogators;

import java.io.File;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.xml.bind.JAXB;

import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.DMDPacket.DMDAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.AreaComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.viz.radar.util.DmdAttribute;
import com.raytheon.viz.radar.util.DmdModifier;
import com.raytheon.viz.radar.util.GraphicDataUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Interrogator class for Radar DMD sampling.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 04, 2010            mnash     Initial creation
 * MAR 05, 2013 15313      kshresth  Added sampling for DMD
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarDMDInterrogator extends RadarGraphicInterrogator implements
        IRadarInterrogator {

    private Map<String, String> _dataMap = new HashMap<String, String>();

    private DmdModifier modifier;

    public RadarDMDInterrogator() {
        super();
        initializeStylePreferences();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.interrogators.IRadarInterrogator#sample(com.raytheon
     * .edex.plugin.radar.RadarRecord, com.vividsolutions.jts.geom.Coordinate,
     * com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public Map<String, String> sample(RadarRecord record, Coordinate latLon,
            ColorMapParameters params) {
        if (latLon == null) {
            return null;
        }
        double[] input = { latLon.x, latLon.y }; // rr
        double[] output = new double[2]; // rr
        try {
            MathTransform mt = CRSCache.getInstance().getTransformFromLatLon(
                    record.getCRS());

            mt.transform(input, 0, output, 0, 1);
            _dataMap.put("crsLocation", output == null ? "-1,-1" : output[0]
                    + "," + output[1]);
        } catch (Exception e) {
            return null;
        }

        _dataMap.put("ICAO", record.getIcao());
        _dataMap.put("Mnemonic", record.getMnemonic());
        addParameters(record, latLon, _dataMap);
        return _dataMap;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.interrogators.IRadarInterrogator#addParameters
     * (com.raytheon.uf.common.dataplugin.radar.RadarRecord,
     * com.vividsolutions.jts.geom.Coordinate, java.util.Map)
     */
    @Override
    public int addParameters(RadarRecord radarRecord, Coordinate latLon,
            Map<String, String> dataMap) {
        dataMap.put("Value", getDataValues(radarRecord, latLon));
        return 0;
    }

    /**
     * Initializes the DMD sampling style preferences. DmdModifier are created
     * from the xml via JaxB.
     */
    private void initializeStylePreferences() {

        PathManager pm = (PathManager) PathManagerFactory.getPathManager();

        LocalizationFile file = pm.getStaticLocalizationFile("styleRules"
                + File.separator + "dmdModifier.xml");

        if (file.exists()) {
            this.modifier = JAXB.unmarshal(file.getFile(), DmdModifier.class);
        }

    }

    private String getDataValues(RadarRecord radarRecord, Coordinate latLon) {
        StringBuffer rval = new StringBuffer();

        // Remove the attributes with an order of -1 and reorder.
        List<DmdAttribute> attributesCopy = new ArrayList<DmdAttribute>();
        if (modifier == null) {
            return rval.toString();
        }
        GraphicDataUtil.setupModifier(modifier);
        for (DmdAttribute att : modifier.getAttributes()) {
            if (att.getOrder() != -1) {
                attributesCopy.add(att);
            }
        }
        Collections.sort(attributesCopy, new DMDAttributeOrder());

        Coordinate c1 = new Coordinate(latLon.x + .025, latLon.y + .025);
        Coordinate c2 = new Coordinate(latLon.x - .025, latLon.y - .025);
        Envelope env = new Envelope(c1, c2);

        UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND
                .getConverterTo(NonSI.KNOT);

        if (radarRecord.getProductCode() == 149) {
            for (RadarDataKey key : radarRecord.getSymbologyData().keySet()) {
                Coordinate currStorm = new Coordinate(key.getLon(),
                        key.getLat());

                if (env.contains(currStorm)) {
                    // Get the data for the select feature
                    RadarDataPoint currPoint = radarRecord.getSymbologyData()
                            .get(key);

                    if (currPoint.isVisible()) {
                        AreaComponent currFeature;
                        HashMap<Integer, HashMap<Integer, GenericDataComponent>> currPointData = currPoint
                                .getDisplayGenericPointData();

                        for (Integer type : currPointData.keySet()) {
                            for (GenericDataComponent currComp : currPointData
                                    .get(type).values()) {
                                currFeature = (AreaComponent) currComp;

                                for (DmdAttribute attribute : attributesCopy) {
                                    DmdStyleAttribute attributeValue = DmdStyleAttribute
                                            .valueOf(attribute.getValue());
                                    NumberFormat formatter = new DecimalFormat(
                                            formatDecimal(attribute
                                                    .getDecimalPlaces()));
                                    switch (attributeValue) {
                                    case RADAR_NAME:
                                        rval.append(radarRecord.getIcao() + " ");
                                        break;
                                    case MESO_ID:
                                        // Feature ID
                                        String mid = currFeature
                                                .getValue(DMDAttributeIDs.MESO_ID
                                                        .toString());
                                        if (!mid.isEmpty()) {
                                            rval.append(mid + " ");
                                        }
                                        break;
                                    case LOCATION:
                                        // Range @ Azimuth
                                        double range = Double
                                                .parseDouble(currFeature
                                                        .getValue(DMDAttributeIDs.BASE_RANGE
                                                                .toString()));
                                        UnitConverter converter = SI.KILOMETER
                                                .getConverterTo(NonSI.NAUTICAL_MILE);
                                        int rangeNm = (int) Math
                                                .round(converter.convert(range));
                                        String rangeUnits = "nm";
                                        String azimuth = currFeature
                                                .getValue(DMDAttributeIDs.BASE_AZIMUTH
                                                        .toString());
                                        // Get rid of decimal point
                                        azimuth = azimuth.contains(".") ? azimuth
                                                .split("\\.")[0] : azimuth;
                                        rval.append(String.format(
                                                "%7.7s@%-3.3s", String.format(
                                                        "%d%s", rangeNm,
                                                        rangeUnits), azimuth)
                                                + " ");
                                        break;
                                    case BASE:
                                        // Base
                                        String baseOnLowestElev = currFeature
                                                .getValue(DMDAttributeIDs.BASE_ON_LOWEST_ELEV
                                                        .toString());

                                        String baseHeight = "";
                                        if (baseOnLowestElev
                                                .equalsIgnoreCase("Y")) {
                                            baseHeight += "<";
                                        }

                                        baseHeight += GraphicDataUtil.setupConverter(currFeature,
                                                DMDAttributeIDs.BASE_HEIGHT.toString(),
                                                1, true);
                                        rval.append(baseHeight+" ");
                                        break;
                                    case DEPTH:
                                        // Depth
                                        String depth = GraphicDataUtil
                                        .setupConverter(currFeature,
                                                DMDAttributeIDs.DEPTH
                                                        .toString(),
                                                1,true);
                                    rval.append(depth + " ");
                                        break;
                                    case RANK:
                                        // Rank
                                        String rank = currFeature
                                               .getValue(DMDAttributeIDs.STRENGTH_RANK
                                        		       .toString());
                                        rval.append("r" + rank + " ");
                                        break;
                                    case MSI:
                                        // MSI
                                    	String msi = GraphicDataUtil
                                               .setupConverter(
                                        	           currFeature,
                                                       DMDAttributeIDs.MSI.toString(),
                                                       1, true);
                                            rval.append(msi + " ");
                                        break;
                                    case RV:
                                        // llrotv
                                        String llrotv = GraphicDataUtil
                                                .setupConverter(
                                                        currFeature,
                                                        DMDAttributeIDs.BASE_ROTATIONAL_VEL
                                                                .toString(),
                                                        1, true);
                                            rval.append(llrotv + " ");
                                        break;
                                    case G2G:
                                        // llg2g
                                        String llg2g = GraphicDataUtil
                                                .setupConverter(
                                                        currFeature,
                                                        DMDAttributeIDs.BASE_GTG_VEL_DIFF
                                                                .toString(),
                                                        1, true);
                                           rval.append(llg2g + " ");
                                        break;
                                    case MRV:
                                        // mxrotv
                                        String mxrotv = GraphicDataUtil
                                                .setupConverter(
                                                        currFeature,
                                                        DMDAttributeIDs.MAX_ROTATIONAL_VEL
                                                                .toString(),
                                                        1, true);
                                            rval.append(mxrotv + " ");
                                        break;
                                    case HMRV:
                                        // htmxrv
                                        String htmxrv = GraphicDataUtil
                                                .setupConverter(
                                                        currFeature,
                                                        DMDAttributeIDs.HEIGHT_MAX_ROTATIONAL_VEL
                                                                .toString(),
                                                        1, true);
                                        rval.append(htmxrv + " ");
                                        break;
                                    case LL_CONV:
                                        // Low-Level Convergence
                                        String llcon = currFeature
                                                .getValue(DMDAttributeIDs._0_2KM_ARL_CONVERGENCE
                                                        .toString())
                                                + currFeature
                                                        .getUnits(DMDAttributeIDs._0_2KM_ARL_CONVERGENCE
                                                                .toString());
                                        if (!llcon.isEmpty()) {
                                            rval.append(llcon + "-llconv ");
                                        }
                                        break;

                                    case TVS:
                                        // Low-Level Convergence
                                        String tvs = currFeature
                                                .getValue(DMDAttributeIDs.ASSOCIATED_TVS
                                                        .toString())
                                                + currFeature
                                                        .getUnits(DMDAttributeIDs.ASSOCIATED_TVS
                                                                .toString());
                                        if (!tvs.isEmpty()) {
                                            rval.append(tvs + "-tvs ");
                                        }
                                        break;
                                    default:
                                        break;
                                    }
                                }
                            }
                        }
                    }
                    break;
                }
            }
        }

        return rval.toString();
    }

    private String formatDecimal(int places) {
        if (places == 0) {
            return "#0";
        }
        StringBuffer decimalFormatter = new StringBuffer();
        decimalFormatter.append("#0.0");
        for (int i = 1; i < places; i++) {
            decimalFormatter.append("0");
        }
        return decimalFormatter.toString();
    }

    private static enum DmdStyleAttribute {
        ICAO, MNEMONIC, RADAR_NAME, MESO_ID, LOCATION, BASE, DEPTH, RANK, MSI, RV, G2G, MRV, HMRV, LL_CONV, TVS;
    }

    private class DMDAttributeOrder implements Comparator<DmdAttribute> {

        @Override
        public int compare(DmdAttribute a1, DmdAttribute a2) {
            if (a1.getOrder() < a2.getOrder()) {
                return -1;
            } else if (a1.getOrder() > a2.getOrder()) {
                return 1;
            }
            return 0;
        }
    }
}
