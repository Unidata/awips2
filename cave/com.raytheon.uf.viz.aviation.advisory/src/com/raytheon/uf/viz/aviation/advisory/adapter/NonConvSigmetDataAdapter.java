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
package com.raytheon.uf.viz.aviation.advisory.adapter;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Set;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.aviation.advisory.AdvisoryRecord;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import org.locationtech.jts.geom.Coordinate;

import gov.noaa.nws.ncep.common.dataplugin.nonconvsigmet.NonConvSigmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.nonconvsigmet.NonConvSigmetRecord;

/**
 *
 * A class for converting NonConvSigmet into OutlineRecords.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 2, 2009            bsteffen     Initial creation
 * Sep 2, 2020  80424     pbutler      Fixed display label on polygon.
 * Oct 11,2021  80424     smanoj       Additional check for missing hazard types
 *                                     (e.g, ICING) to plot those SIGMETs as
 *                                     polygons in D2D.
 * Dec 7, 2021  99346     thuggins     Adding hazard types for Widespread
 *                                     Dust/Sand storms
 *
 * </pre>
 *
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class NonConvSigmetDataAdapter extends AbstractAdvisoryDataAdapter {

    private static final transient IUFStatusHandler logger = UFStatus
            .getHandler(NonConvSigmetDataAdapter.class);

    private static final String INSPECT_FORMAT = "Valid UNTIL %02d%02d%02d\n%s";

    private static final float LINE_WIDTH = 1.5f;

    private static final LineStyle LINE_STYLE = LineStyle.SOLID;

    private static final String ICING_HAZARD_TYPE = "ICING";

    private static final String WIDESPREAD_DUSTSTORM_HAZARD_TYPE = "WDSPR DS";

    private static final String WIDESPREAD_SANDSTORM_HAZARD_TYPE = "WDSPR SS";

    private static final String TURBULENCE_HAZARD_TYPE = "TURBULENCE";

    private static final String VOLCANIC_ASH_CLOUD_HAZARD_TYPE = "VOLCANIC ASH CLOUD";

    private static final String TROPICAL_CYCLONE_HAZARD_TYPE = "TROPICAL CYCLONE";

    public static final String[] DUSTSTORM_TYPES = new String[] { "WDSPR DS",
            "DS", "DU" };

    public static final String[] SANDSTORM_TYPES = new String[] { "WDSPR SS" };

    public static final String[] ICING_TYPES = new String[] { "ICE", "ICGIC",
            "ICGICIP", "ICGIP", "ICING", "RIME", "MXD", "CLR", "MXD/CLR",
            "RIME/MXD" };

    public static final String[] TURBULENCE_TYPES = new String[] { "SEV TURB",
            "TURB", "TURBULENCE" };

    public static final String[] VOLCANIC_ASH_TYPES = new String[] { "VA",
            "VOLCANIC ASH", "VOLCANIC ASH CLOUD", "VA ERUPTION", "VA ERUPT" };

    public static final String[] TROPICAL_CYCLONE_TYPES = new String[] { "TC",
            "TROPICAL CYCLONE" };

    @XmlAttribute
    private String hazardType;

    @Override
    public Collection<AdvisoryRecord> convertRecords(
            Collection<PluginDataObject> records) {
        Collection<AdvisoryRecord> result = new ArrayList<>();
        for (PluginDataObject record : records) {
            result.addAll(convertRecord(record));
        }
        return result;
    }

    @Override
    public Collection<AdvisoryRecord> convertRecord(PluginDataObject record) {
        Collection<AdvisoryRecord> result = new ArrayList<>();
        if (record instanceof NonConvSigmetRecord) {
            NonConvSigmetRecord sigmetRecord = (NonConvSigmetRecord) record;
            String sigmetHazardType = sigmetRecord.getHazardType();

            // If hazard type is not a match;
            // Additional check to find the matching hazard type
            if (!hazardType.equals(sigmetHazardType)) {
                switch (hazardType) {
                case WIDESPREAD_DUSTSTORM_HAZARD_TYPE:
                    boolean isDust = false;
                    for (String dustType : DUSTSTORM_TYPES) {
                        if (sigmetHazardType.equalsIgnoreCase(dustType)) {
                            isDust = true;
                        }
                    }
                    if (!isDust) {
                        return result;
                    }
                    break;
                case WIDESPREAD_SANDSTORM_HAZARD_TYPE:
                    boolean isSand = false;
                    for (String sandType : SANDSTORM_TYPES) {
                        if (sigmetHazardType.equalsIgnoreCase(sandType)) {
                            isSand = true;
                        }
                    }
                    if (!isSand) {
                        return result;
                    }
                    break;
                case ICING_HAZARD_TYPE:
                    boolean isIce = false;
                    for (String iceType : ICING_TYPES) {
                        if (sigmetHazardType.equalsIgnoreCase(iceType)) {
                            isIce = true;
                        }
                    }
                    if (!isIce) {
                        return result;
                    }
                    break;
                case VOLCANIC_ASH_CLOUD_HAZARD_TYPE:
                    boolean isVA = false;
                    for (String vaType : VOLCANIC_ASH_TYPES) {
                        if (sigmetHazardType.equalsIgnoreCase(vaType)) {
                            isVA = true;
                        }
                    }
                    if (!isVA) {
                        return result;
                    }
                    break;
                case TROPICAL_CYCLONE_HAZARD_TYPE:
                    boolean isTC = false;
                    for (String tcType : TROPICAL_CYCLONE_TYPES) {
                        if (sigmetHazardType.equalsIgnoreCase(tcType)) {
                            isTC = true;
                        }
                    }
                    if (!isTC) {
                        return result;
                    }
                    break;
                case TURBULENCE_HAZARD_TYPE:
                    boolean isTurb = false;
                    for (String turbType : TURBULENCE_TYPES) {
                        if (sigmetHazardType.equalsIgnoreCase(turbType)) {
                            isTurb = true;
                        }
                    }
                    if (!isTurb) {
                        return result;
                    }
                    break;
                default:
                    logger.info("Unexpected hazardType: " + hazardType);
                    return result;
                }
            }

            Set<NonConvSigmetLocation> locations = sigmetRecord
                    .getNonConvSigmetLocation();
            if (locations == null || locations.size() == 0) {
                return result;
            }
            Coordinate[] coords = new Coordinate[locations.size()];
            for (NonConvSigmetLocation loc : locations) {
                coords[loc.getIndex() - 1] = new Coordinate(loc.getLongitude(),
                        loc.getLatitude());
            }
            String region = sigmetRecord.getForecastRegion();
            if (region == null) {
                region = "";
            } else if (region.length() >= 3) {
                region = region.substring(0, 3);
            }
            String id = sigmetRecord.getSigmetId();
            if (id == null) {
                id = "";
            } else if (id.length() > 3) {
                id = id.substring(id.length() - 1);
            }
            Calendar endTime = sigmetRecord.getEndTime();
            int day = 0;
            int hour = 0;
            int min = 0;
            if (endTime != null) {
                day = endTime.get(Calendar.DAY_OF_MONTH);
                hour = endTime.get(Calendar.HOUR_OF_DAY);
                min = endTime.get(Calendar.MINUTE);
            }

            // - create series label
            String bull = sigmetRecord.getBullMessage();
            String label = "";

            label = createLabel(sigmetRecord.getSigmetId());

            String inpsectString = String.format(INSPECT_FORMAT, day, hour, min,
                    bull);
            AdvisoryRecord aRecord = new AdvisoryRecord(coords, label,
                    inpsectString);
            result.add(aRecord);
        }
        return result;
    }

    public String createLabel(String sigmetid) {
        String buildLabel = "";
        String reportType = "SIGMET";

        buildLabel = reportType + " " + sigmetid;

        return buildLabel;

    }

    public void setHazardType(String hazardType) {
        this.hazardType = hazardType;
    }

    public String getHazardType() {
        return hazardType;
    }

    @Override
    public float getLineWidth() {
        return LINE_WIDTH;
    }

    @Override
    public LineStyle getLineStyle() {
        return LINE_STYLE;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((hazardType == null) ? 0 : hazardType.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        NonConvSigmetDataAdapter other = (NonConvSigmetDataAdapter) obj;
        if (hazardType == null) {
            if (other.hazardType != null) {
                return false;
            }
        } else if (!hazardType.equals(other.hazardType)) {
            return false;
        }
        return true;
    }

}
