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
 * 
 *  * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 15, 2019  7596     lsingh    Updated units framework to JSR-363.
 * 
 * </pre>
 **/
package com.raytheon.uf.viz.aviation.advisory.adapter;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Set;

import javax.measure.UnitConverter;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;

import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.aviation.advisory.AdvisoryRecord;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;

import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetRecord;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetSection;
import si.uom.SI;
import systems.uom.common.USCustomary;

import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetRecord;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetSection;

/**
 *
 * Aviation data adapter for Conv Sigmet for Now cast and Forecast. This class
 * adds wx statement text to sigmet polygons in d2d.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * ????    2001  ????     ????      Initial creation
 * Jul 24, 2020  79536    pbutler   D2D and wx statements to hover over Sigmet polygons.
 * Dec 14  2020  82254    srussell  Updated convertIsol()
 * Feb 07, 2022  97246    tjensen   Add error handling for convertLine()
 *
 * </pre>
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ConvSigmetCastDataAdapter extends AbstractAdvisoryDataAdapter {

    private static String STR_AREA = "AREA";

    private static String STR_LINE = "LINE";

    private static String STR_ISOL = "ISOL";

    private static final float LINE_WIDTH = 1.5f;

    private static final LineStyle NOW_LINE_STYLE = LineStyle.SOLID;

    private static final LineStyle FORECAST_LINE_STYLE = LineStyle.DASHED_LARGE;

    protected static final UnitConverter KNTS_TO_MPS = USCustomary.KNOT
            .getConverterTo(SI.METRE_PER_SECOND);

    private static final String FORMAT = "Valid UNTIL %02d%02d%02d\n%s";

    private static final String SEGMENT_SEPERATOR = "\n.  \n";

    @XmlAttribute
    private boolean forecast = false;

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
        if (record instanceof ConvSigmetRecord) {
            ConvSigmetRecord sigmetRecord = (ConvSigmetRecord) record;
            if (sigmetRecord.getConvSigmetSection() != null) {
                for (ConvSigmetSection section : sigmetRecord
                        .getConvSigmetSection()) {
                    // - create new rec ----
                    AdvisoryRecord newRec = null;
                    if (forecast && isSectionDirectionOrSpeedInvalid(section)) {
                        continue;
                    }

                    if (section.getClassType().equals(STR_AREA)) {
                        newRec = convertSection(section);
                    } else if (section.getClassType().equals(STR_ISOL)) {
                        newRec = convertIsol(section);
                    } else if (section.getClassType().equals(STR_LINE)) {
                        newRec = convertLine(section);
                    }

                    if (newRec != null) {
                        result.add(newRec);
                    }
                }
            }
        }
        return result;
    }

    private boolean isSectionDirectionOrSpeedInvalid(
            ConvSigmetSection section) {
        boolean rval = false;

        switch (section.getDirection()) {
        case -9999:
        case -9999998:
            rval = true;
            break;
        }

        switch (section.getSpeed()) {
        case -9999:
        case -9999998:
            rval = true;
            break;
        }

        return rval;
    }

    private AdvisoryRecord convertSection(ConvSigmetSection section) {
        Set<ConvSigmetLocation> locations = section.getConvSigmetLocation();
        if (locations == null || locations.isEmpty()) {
            return null;
        }
        Coordinate[] coords = new Coordinate[locations.size()];
        for (ConvSigmetLocation loc : locations) {

            coords[loc.getIndex() - 1] = new Coordinate(loc.getLongitude(),
                    loc.getLatitude());

        }
        String sequenceId = section.getSequenceID();
        if (sequenceId == null) {
            sequenceId = "";
        } else if (sequenceId.length() >= 3) {
            sequenceId = sequenceId.substring(0, 3);
        }

        String inspectString = formatWxStatmentText(section);

        String label = "";

        AdvisoryRecord aRecord = null;

        if (coords.length >= 4) {
            aRecord = new AdvisoryRecord(coords, label, inspectString);
        }

        return aRecord;
    }

    private AdvisoryRecord convertIsol(ConvSigmetSection section) {
        Set<ConvSigmetLocation> locations = section.getConvSigmetLocation();
        // Coordinate[] coords = new Coordinate[locations.size()];

        if (locations == null || locations.isEmpty()) {
            return null;
        }
        ConvSigmetLocation loc = locations.iterator().next();

        Coordinate center = new Coordinate(loc.getLongitude(),
                loc.getLatitude());

        String label = section.getSequenceID();
        if (forecast) {
            movePoint(center, section);
            label = "";
        }

        String inspectString = formatWxStatmentText(section);

        AdvisoryRecord aRecord = null;

        /*-
         * TODO
         * The commented out section below does what it says, BUT it also
         * prevents ISOL SEV TS from rendering by forcing a return of a null
         * AdvisoryRecord object
         * - steve.russell@noaa.gov
         ---------------------------------------------------------------------
        // - check for making sure we do not get wrapping lines at top of d2d
        // view.
        if (coords.length >= 4) {
            aRecord = new AdvisoryRecord(center, section.getDistance(), label,
                    inspectString);
        }
        -----------------------------------------------------------------------
        */

        if (center != null && section != null) {
            aRecord = new AdvisoryRecord(center, section.getDistance(), label,
                    inspectString);
        }

        return aRecord;
    }

    private AdvisoryRecord convertLine(ConvSigmetSection section) {
        Set<ConvSigmetLocation> locations = section.getConvSigmetLocation();
        /*
         * If we somehow have a line with no points, return null. Calling method
         * convertRecord() has handling for a null return
         */
        if (locations.isEmpty()) {
            return null;
        }
        Coordinate[] coords = new Coordinate[locations.size()];

        for (ConvSigmetLocation loc : locations) {
            coords[loc.getIndex() - 1] = new Coordinate(loc.getLongitude(),
                    loc.getLatitude());
        }
        String label = section.getSequenceID();
        if (forecast) {
            movePoints(coords, section);
            label = "";
        }

        String inspectString = formatWxStatmentText(section);

        return new AdvisoryRecord(coords, section.getDistance(), label,
                inspectString);

    }

    private String formatWxStatmentText(ConvSigmetSection section) {
        Calendar endTime = section.getEndTime();
        int day = 0;
        int hour = 0;
        int min = 0;
        if (endTime != null) {
            day = endTime.get(Calendar.DAY_OF_MONTH);
            hour = endTime.get(Calendar.HOUR_OF_DAY);
            min = endTime.get(Calendar.MINUTE);
        }
        String segment = section.getSegment();
        if (segment != null) {
            segment = segment.split(SEGMENT_SEPERATOR)[0];
        } else {
            segment = "";
        }

        String inspectString = String.format(FORMAT, day, hour, min, segment);

        return inspectString;
    }

    /**
     * Move an array of Coordinates by the direction and speed specified in a
     * ConvSigmetSection
     *
     * @param coords
     *            the coordinates to move in Lat, Lon
     * @param section
     *            The section to use for the direction and speed
     * @return the original Coordinate array with all x and y shifted correctly
     */
    private Coordinate[] movePoints(Coordinate[] coords,
            ConvSigmetSection section) {
        for (Coordinate coord : coords) {
            movePoint(coord, section);
        }
        return coords;
    }

    /**
     * Move a coord by the direction and speed specified in a ConvSigmetSection
     *
     * @param coord
     *            the coordinate to move in Lat, Lon
     * @param section
     *            The section to use for the direction and speed
     * @return the original coord with x and y shifted correctly
     */
    private Coordinate movePoint(Coordinate coord, ConvSigmetSection section) {
        double speedInMps = KNTS_TO_MPS.convert(section.getSpeed());
        double distance = speedInMps * 3600;
        Coordinate newCoord = AdvisoryRecord.getPointOnCircle(coord, distance,
                section.getDirection() + 180);
        coord.x = newCoord.x;
        coord.y = newCoord.y;
        return coord;
    }

    @Override
    public float getLineWidth() {
        return LINE_WIDTH;
    }

    @Override
    public LineStyle getLineStyle() {
        if (forecast) {
            return FORECAST_LINE_STYLE;
        } else {
            return NOW_LINE_STYLE;
        }
    }

    public void setForecast(boolean forecast) {
        this.forecast = forecast;
    }

    public boolean isForecast() {
        return forecast;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + (forecast ? 1231 : 1237);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        ConvSigmetCastDataAdapter other = (ConvSigmetCastDataAdapter) obj;
        if (forecast != other.forecast) {
            return false;
        }
        return true;
    }

}
