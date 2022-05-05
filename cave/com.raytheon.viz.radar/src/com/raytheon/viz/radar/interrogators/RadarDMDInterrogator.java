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
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import javax.measure.UnitConverter;
import javax.xml.bind.JAXB;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Envelope;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.DMDPacket.DMDAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.AreaComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;
import com.raytheon.viz.radar.util.DmdAttribute;
import com.raytheon.viz.radar.util.DmdModifier;
import com.raytheon.viz.radar.util.GraphicDataUtil;

import si.uom.NonSI;
import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.quantity.Quantities;
import tec.uom.se.unit.MetricPrefix;

/**
 * Interrogator class for Radar DMD sampling.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 04, 2010            mnash       Initial creation
 * Mar 05, 2013 15313      kshresth    Added sampling for DMD
 * Sep 13, 2016 3239       nabowle     Use the Interrogatable API.
 *
 * </pre>
 *
 * @author mnash
 */

public class RadarDMDInterrogator extends RadarGraphicInterrogator
        implements IRadarInterrogator {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarDMDInterrogator.class);

    public static final InterrogationKey<String> TVS_KEY = new InterrogationKey<>();

    public static final InterrogationKey<String> MESO_ID_KEY = new InterrogationKey<>();

    public static final InterrogationKey<String> LL_CONV_KEY = new InterrogationKey<>();

    public static final InterrogationKey<String> HT_MAX_ROT_VEL_KEY = new InterrogationKey<>();

    public static final InterrogationKey<String> MAX_ROT_VEL_KEY = new InterrogationKey<>();

    public static final InterrogationKey<String> BASE_GTG_VEL_DIFF_KEY = new InterrogationKey<>();

    public static final InterrogationKey<String> BASE_ROT_VEL_KEY = new InterrogationKey<>();

    public static final InterrogationKey<String> MSI_KEY = new InterrogationKey<>();

    public static final InterrogationKey<String> RANK_KEY = new InterrogationKey<>();

    public static final InterrogationKey<String> DEPTH_KEY = new InterrogationKey<>();

    public static final InterrogationKey<String> BASE_HEIGHT_KEY = new InterrogationKey<>();

    private InterrogateMap _dataMap = new InterrogateMap();

    private DmdModifier modifier;

    public RadarDMDInterrogator() {
        super();
        initializeStylePreferences();
    }

    @Override
    public InterrogateMap sample(RadarRecord radarRecord, Coordinate latLon,
            ColorMapParameters params, Set<InterrogationKey<?>> keys) {
        sample(radarRecord, latLon, keys, _dataMap);
        return _dataMap;
    }

    @Override
    public int addParameters(RadarRecord radarRecord, Coordinate latLon,
            InterrogateMap dataMap, Set<InterrogationKey<?>> keys) {
        String stringValue = getDataValues(radarRecord, latLon, dataMap, keys);
        if (keys.contains(RadarDefaultInterrogator.VALUE_STRING)) {
            dataMap.put(RadarDefaultInterrogator.VALUE_STRING, stringValue);
        }
        return 0;
    }

    /**
     * Initializes the DMD sampling style preferences. DmdModifier are created
     * from the xml via JaxB.
     */
    private void initializeStylePreferences() {

        PathManager pm = (PathManager) PathManagerFactory.getPathManager();

        ILocalizationFile file = pm.getStaticLocalizationFile(
                "styleRules" + File.separator + "dmdModifier.xml");

        if (file.exists()) {
            try (InputStream is = file.openInputStream()) {
                this.modifier = JAXB.unmarshal(is, DmdModifier.class);
            } catch (IOException | LocalizationException e) {
                statusHandler.error("Unable to load style preferences.", e);
            }
        }

    }

    private String getDataValues(RadarRecord radarRecord, Coordinate latLon,
            InterrogateMap dataMap, Set<InterrogationKey<?>> keys) {
        StringBuilder rval = new StringBuilder();

        // Remove the attributes with an order of -1 and reorder.
        List<DmdAttribute> attributesCopy = new ArrayList<>();
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
                                    switch (attributeValue) {
                                    case RADAR_NAME:
                                        rval.append(radarRecord.getIcao())
                                                .append(" ");
                                        break;
                                    case MESO_ID:
                                        // Feature ID
                                        String mid = currFeature.getValue(
                                                DMDAttributeIDs.MESO_ID
                                                        .toString());
                                        if (!mid.isEmpty()) {
                                            rval.append(mid).append(" ");
                                            addValueToMap(dataMap, keys,
                                                    MESO_ID_KEY, mid);
                                        }
                                        break;
                                    case LOCATION:
                                        // Range @ Azimuth
                                        double range = Double.parseDouble(
                                                currFeature.getValue(
                                                        DMDAttributeIDs.BASE_RANGE
                                                                .toString()));
                                        UnitConverter converter = MetricPrefix.KILO(SI.METRE)
                                                .getConverterTo(
                                                        USCustomary.NAUTICAL_MILE);
                                        int rangeNm = (int) Math.round(
                                                converter.convert(range));
                                        String rangeUnits = "nm";
                                        String azimuth = currFeature.getValue(
                                                DMDAttributeIDs.BASE_AZIMUTH
                                                        .toString());
                                        // Get rid of decimal point
                                        azimuth = azimuth.contains(".")
                                                ? azimuth.split("\\.")[0]
                                                : azimuth;
                                        rval.append(String.format(
                                                "%7.7s@%-3.3s",
                                                String.format("%d%s", rangeNm,
                                                        rangeUnits),
                                                azimuth)).append(" ");
                                        addValueToMap(dataMap, keys,
                                                IRadarInterrogator.RANGE,
                                                Quantities.getQuantity(rangeNm,
                                                        USCustomary.NAUTICAL_MILE));
                                        addValueToMap(dataMap, keys,
                                                IRadarInterrogator.AZIMUTH,
                                                Quantities.getQuantity(
                                                        Integer.parseInt(
                                                                azimuth),
                                                        NonSI.DEGREE_ANGLE));
                                        break;
                                    case BASE:
                                        // Base
                                        String baseOnLowestElev = currFeature
                                                .getValue(
                                                        DMDAttributeIDs.BASE_ON_LOWEST_ELEV
                                                                .toString());

                                        String baseHeight = "";
                                        if (baseOnLowestElev
                                                .equalsIgnoreCase("Y")) {
                                            baseHeight += "<";
                                        }

                                        baseHeight += GraphicDataUtil
                                                .setupConverter(currFeature,
                                                        DMDAttributeIDs.BASE_HEIGHT
                                                                .toString(),
                                                        1, true);
                                        rval.append(baseHeight).append(" ");
                                        addValueToMap(dataMap, keys,
                                                BASE_HEIGHT_KEY, baseHeight);
                                        break;
                                    case DEPTH:
                                        // Depth
                                        String depth = GraphicDataUtil
                                                .setupConverter(currFeature,
                                                        DMDAttributeIDs.DEPTH
                                                                .toString(),
                                                        1, true);
                                        rval.append(depth).append(" ");
                                        addValueToMap(dataMap, keys, DEPTH_KEY,
                                                depth);
                                        break;
                                    case RANK:
                                        // Rank
                                        String rank = currFeature.getValue(
                                                DMDAttributeIDs.STRENGTH_RANK
                                                        .toString());
                                        rval.append("r").append(rank)
                                                .append(" ");

                                        addValueToMap(dataMap, keys, RANK_KEY,
                                                rank);
                                        break;
                                    case MSI:
                                        // MSI
                                        String msi = GraphicDataUtil
                                                .setupConverter(currFeature,
                                                        DMDAttributeIDs.MSI
                                                                .toString(),
                                                        1, true);
                                        rval.append(msi).append(" ");
                                        addValueToMap(dataMap, keys, MSI_KEY,
                                                msi);
                                        break;
                                    case RV:
                                        // llrotv
                                        String llrotv = GraphicDataUtil
                                                .setupConverter(currFeature,
                                                        DMDAttributeIDs.BASE_ROTATIONAL_VEL
                                                                .toString(),
                                                        1, true);
                                        rval.append(llrotv).append(" ");

                                        addValueToMap(dataMap, keys,
                                                BASE_ROT_VEL_KEY, llrotv);
                                        break;
                                    case G2G:
                                        // llg2g
                                        String llg2g = GraphicDataUtil
                                                .setupConverter(currFeature,
                                                        DMDAttributeIDs.BASE_GTG_VEL_DIFF
                                                                .toString(),
                                                        1, true);
                                        rval.append(llg2g).append(" ");
                                        addValueToMap(dataMap, keys,
                                                BASE_GTG_VEL_DIFF_KEY, llg2g);
                                        break;
                                    case MRV:
                                        // mxrotv
                                        String mxrotv = GraphicDataUtil
                                                .setupConverter(currFeature,
                                                        DMDAttributeIDs.MAX_ROTATIONAL_VEL
                                                                .toString(),
                                                        1, true);
                                        rval.append(mxrotv).append(" ");
                                        addValueToMap(dataMap, keys,
                                                MAX_ROT_VEL_KEY, mxrotv);
                                        break;
                                    case HMRV:
                                        // htmxrv
                                        String htmxrv = GraphicDataUtil
                                                .setupConverter(currFeature,
                                                        DMDAttributeIDs.HEIGHT_MAX_ROTATIONAL_VEL
                                                                .toString(),
                                                        1, true);
                                        rval.append(htmxrv).append(" ");
                                        addValueToMap(dataMap, keys,
                                                HT_MAX_ROT_VEL_KEY, htmxrv);
                                        break;
                                    case LL_CONV:
                                        // Low-Level Convergence
                                        String llcon = currFeature.getValue(
                                                DMDAttributeIDs._0_2KM_ARL_CONVERGENCE
                                                        .toString())
                                                + currFeature.getUnits(
                                                        DMDAttributeIDs._0_2KM_ARL_CONVERGENCE
                                                                .toString());
                                        if (!llcon.isEmpty()) {
                                            rval.append(llcon)
                                                    .append("-llconv ");
                                            addValueToMap(dataMap, keys,
                                                    LL_CONV_KEY, llcon);
                                        }
                                        break;

                                    case TVS:
                                        // Low-Level Convergence
                                        String tvs = currFeature.getValue(
                                                DMDAttributeIDs.ASSOCIATED_TVS
                                                        .toString())
                                                + currFeature.getUnits(
                                                        DMDAttributeIDs.ASSOCIATED_TVS
                                                                .toString());
                                        if (!tvs.isEmpty()) {
                                            rval.append(tvs).append("-tvs ");
                                            addValueToMap(dataMap, keys,
                                                    TVS_KEY, tvs);
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

    @Override
    public Set<InterrogationKey<?>> getInterrogationKeys() {
        Set<InterrogationKey<?>> keys = super.getInterrogationKeys();
        keys.add(BASE_GTG_VEL_DIFF_KEY);
        keys.add(BASE_HEIGHT_KEY);
        keys.add(BASE_ROT_VEL_KEY);
        keys.add(DEPTH_KEY);
        keys.add(HT_MAX_ROT_VEL_KEY);
        keys.add(LL_CONV_KEY);
        keys.add(MAX_ROT_VEL_KEY);
        keys.add(MESO_ID_KEY);
        keys.add(MSI_KEY);
        keys.add(RANK_KEY);
        keys.add(TVS_KEY);
        return keys;
    }

    @Override
    public Set<InterrogationKey<?>> getValueStringKeys() {
        Set<InterrogationKey<?>> keys = super.getValueStringKeys();
        keys.add(IRadarInterrogator.ICAO);
        keys.add(IRadarInterrogator.RANGE);
        keys.add(IRadarInterrogator.AZIMUTH);
        keys.add(BASE_GTG_VEL_DIFF_KEY);
        keys.add(BASE_HEIGHT_KEY);
        keys.add(BASE_ROT_VEL_KEY);
        keys.add(DEPTH_KEY);
        keys.add(HT_MAX_ROT_VEL_KEY);
        keys.add(LL_CONV_KEY);
        keys.add(MAX_ROT_VEL_KEY);
        keys.add(MESO_ID_KEY);
        keys.add(MSI_KEY);
        keys.add(RANK_KEY);
        keys.add(TVS_KEY);
        return keys;
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
