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

import gov.noaa.nws.ncep.common.dataplugin.airmet.AirmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.airmet.AirmetRecord;
import gov.noaa.nws.ncep.common.dataplugin.airmet.AirmetReport;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.aviation.advisory.AdvisoryRecord;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * A class for converting AirmetRecords into AdvisoryRecords.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 2, 2009            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AirmetDataAdapter extends AbstractAdvisoryDataAdapter {

    private static final String LABEL_FORMAT = "%d%s";

    private static final String INSPECT_FORMAT = "Valid UNTIL %02d%02d%02d\n%s";

    private static final String REPORT_INDICATOR = "AIRMET";

    private static final String SEGMENT_SEPERATOR = "\n.  \n";

    private static final float LINE_WIDTH = 1.5f;

    private static final LineStyle LINE_STYLE = LineStyle.SOLID;

    @XmlAttribute
    private String hazardType;

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
        Collection<AdvisoryRecord> result = new ArrayList<AdvisoryRecord>();
        if (record instanceof AirmetRecord) {
            AirmetRecord airmetRecord = (AirmetRecord) record;
            if (airmetRecord.getAirmetReport() != null) {
                for (AirmetReport report : airmetRecord.getAirmetReport()) {
                    if (isValidReport(report)) {
                        AdvisoryRecord oRecord = convertReport(airmetRecord,
                                report);
                        if (oRecord != null) {
                            result.add(oRecord);
                        }
                    }
                }
            }
        }
        return result;
    }

    public AdvisoryRecord convertReport(AirmetRecord parent, AirmetReport report) {
        Set<AirmetLocation> locations = report.getAirmetLocation();
        if (locations == null || locations.size() == 0) {
            return null;
        }
        Coordinate[] coords = new Coordinate[locations.size()];
        for (AirmetLocation loc : locations) {
            coords[loc.getIndex() - 1] = new Coordinate(loc.getLongitude(),
                    loc.getLatitude());
        }
        int updateNumber = parent.getUpdateNumber();
        String sequenceId = report.getSequenceID();
        if (sequenceId == null) {
            sequenceId = "";
        } else if (sequenceId.length() >= 3) {
            sequenceId = sequenceId.substring(0, 3);
        }
        Calendar endTime = report.getEndTime();
        int day = 0;
        int hour = 0;
        int min = 0;
        if (endTime != null) {
            day = endTime.get(Calendar.DAY_OF_MONTH);
            hour = endTime.get(Calendar.HOUR_OF_DAY);
            min = endTime.get(Calendar.MINUTE);
        }
        String segment = report.getSegment();
        if (segment != null) {
            // This should remove FRZLVL... as well as the extra bonus '.'
            segment = segment.split(SEGMENT_SEPERATOR)[0];
        } else {
            segment = "";
        }
        String label = String.format(LABEL_FORMAT, updateNumber, sequenceId);
        String inspectString = String.format(INSPECT_FORMAT, day, hour, min,
                segment);
        AdvisoryRecord aRecord = new AdvisoryRecord(coords, label,
                inspectString);
        return aRecord;
    }

    /**
     * Check if a report needs to be converted by this adapter, reportIndicator
     * must be AIRMET and hazardTypes must match.
     * 
     * @param report
     *            the report to check
     * @return true if this report should be converted
     */
    private boolean isValidReport(AirmetReport report) {
        return hazardType.equals(report.getHazardType())
                && REPORT_INDICATOR.equals(report.getReportIndicator());
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
        int result = 1;
        result = prime * result
                + ((hazardType == null) ? 0 : hazardType.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        AirmetDataAdapter other = (AirmetDataAdapter) obj;
        if (hazardType == null) {
            if (other.hazardType != null)
                return false;
        } else if (!hazardType.equals(other.hazardType))
            return false;
        return true;
    }

}
