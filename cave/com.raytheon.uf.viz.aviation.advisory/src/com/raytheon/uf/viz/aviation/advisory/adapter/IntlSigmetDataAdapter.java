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

import gov.noaa.nws.ncep.common.dataplugin.intlsigmet.IntlSigmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.intlsigmet.IntlSigmetRecord;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Set;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.aviation.advisory.AdvisoryRecord;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import org.locationtech.jts.geom.Coordinate;

/**
 * 
 * A class for converting IntlSigmet into AdvisoryRecords.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 2, 2009            bsteffen     Initial creation
 * Apr 7, 2018  20475     pwang        Plot International SIGMET on D2D
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class IntlSigmetDataAdapter extends AbstractAdvisoryDataAdapter {

    private static final String HAZARD_TYPE_ANY = "ANY";

    private static final String HAZARD_TYPE_TS = "THUNDERSTORMS";

    private static final float LINE_WIDTH = 1.5f;

    private static final LineStyle LINE_STYLE = LineStyle.SOLID;

    @XmlAttribute
    private String hazardType;

    private SimpleDateFormat sdf;

    /**
     * Constructor
     */
    public IntlSigmetDataAdapter() {
        // display same datetime format as NCEP ISIG
        sdf = new SimpleDateFormat("dd/HHmm");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    @Override
    public Collection<AdvisoryRecord> convertRecords(
            Collection<PluginDataObject> records) {
        Collection<AdvisoryRecord> result = new ArrayList<AdvisoryRecord>();
        for (PluginDataObject record : records) {
            result.addAll(convertRecord(record));
        }
        return result;
    }

    @Override
    public Collection<AdvisoryRecord> convertRecord(PluginDataObject record) {
        Collection<AdvisoryRecord> result = new ArrayList<>();
        if (record instanceof IntlSigmetRecord) {
            IntlSigmetRecord sigmetRecord = (IntlSigmetRecord) record;

            /*
             * When: 1) select "ANY": convert all available record 2) select
             * "THUNDERSTORMS": convert any thunderstorm records 3) select other
             * hazard type in the menu: only convert the record with selected
             * hazard type
             */
            if ((!hazardType.equals(HAZARD_TYPE_ANY))
                    && (!this.isThunderstormType(sigmetRecord.getHazardType()))
                    && (!hazardType.equals(sigmetRecord.getHazardType()))) {
                return result;
            }

            Set<IntlSigmetLocation> locations = sigmetRecord
                    .getIntlSigmetLocation();
            if (locations == null || locations.size() <= 0) {
                return result;
            }
            Coordinate[] coords = new Coordinate[locations.size()];
            for (IntlSigmetLocation loc : locations) {
                coords[loc.getIndex() - 1] = new Coordinate(loc.getLongitude(),
                        loc.getLatitude());
            }

            // Build label similar as NCEP/MISC/ISIG
            StringBuilder sb = new StringBuilder();
            sb.append(sigmetRecord.getMessageID()).append(" ");
            sb.append(sigmetRecord.getSequenceNumber());
            if ((hazardType.equals(HAZARD_TYPE_ANY))
                    || (hazardType.equals(HAZARD_TYPE_TS))) {
                sb.append(" ").append(sigmetRecord.getHazardType());
            }
            sb.append("\n");
            sb.append(sdf.format(sigmetRecord.getStartTime().getTime()))
                    .append("-");
            sb.append(sdf.format(sigmetRecord.getEndTime().getTime()));

            String label = sb.toString();

            if (sigmetRecord.getDistance() != -9999 && coords.length <= 2) {
                if (coords.length == 1) {
                    AdvisoryRecord aRecord = new AdvisoryRecord(coords[0],
                            sigmetRecord.getDistance(), label,
                            sigmetRecord.getBullMessage());
                    result.add(aRecord);
                } else {
                    AdvisoryRecord aRecord = new AdvisoryRecord(coords,
                            sigmetRecord.getDistance(), label,
                            sigmetRecord.getBullMessage());
                    result.add(aRecord);
                }
            } else {
                if (coords.length > 0
                        && !coords[0].equals(coords[coords.length - 1])) {
                    coords = Arrays.copyOf(coords, coords.length + 1);
                    coords[coords.length - 1] = coords[0];
                }

                AdvisoryRecord aRecord = new AdvisoryRecord(coords, label,
                        sigmetRecord.getBullMessage());
                result.add(aRecord);
            }
        }
        return result;
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
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        IntlSigmetDataAdapter other = (IntlSigmetDataAdapter) obj;
        if (hazardType == null) {
            if (other.hazardType != null)
                return false;
        } else if (!hazardType.equals(other.hazardType))
            return false;
        return true;
    }

    /**
     * @param dataHazardType
     * @return
     */
    private boolean isThunderstormType(String dataHazardType) {
        /*
         * 16 THUNDERSTORM Types:
         * 
         * "EMBEDED THUNDERSTORMS HAIL", "SQUALL EMBEDED THUNDERSTORMS",
         * "ISOLATED EMBEDED THUNDERSTORMS", "ISOLATED THUNDERSTORMS",
         * "EMBEDED THUNDERSTORMS", "FREQUENT THUNDERSTORMS",
         * "SQUALL THUNDERSTORMS", "THUNDERSTORMS HAIL",
         * "OBSCURE THUNDERSTORMS", "FREQUENT THUNDERSTORMS HAIL",
         * "OCCASIONAL THUNDERSTORMS", "EMBEDED THUNDERSTORMS HAIL",
         * "THUNDERSTORMS HAIL", "HAIL", "THUNDERSTORMS CUMULONIMBUS",
         * "THUNDERSTORMS CUMULONIMBUS"
         * 
         */

        if (!hazardType.equals(HAZARD_TYPE_TS)) {
            // Not "THUNDERSTORMS" hazardType is selected
            return false;
        } else if (dataHazardType.contains(HAZARD_TYPE_TS)) {
            // 15 of 16 Thunderstorms type name contains the keyword
            // "THUNDERSTORMS"
            return true;
        } else if (dataHazardType.equals("HAIL")) {
            // Only of thunderstorm type named 'HAIL"
            return true;
        } else {
            return false;
        }
    }

}
