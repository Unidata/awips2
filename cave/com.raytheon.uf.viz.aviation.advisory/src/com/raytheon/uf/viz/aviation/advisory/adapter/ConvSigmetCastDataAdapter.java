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

import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetRecord;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetSection;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.aviation.advisory.AdvisoryRecord;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.vividsolutions.jts.geom.Coordinate;

@XmlAccessorType(XmlAccessType.NONE)
public class ConvSigmetCastDataAdapter extends AbstractAdvisoryDataAdapter {

    private static String STR_AREA = "AREA";

    private static String STR_LINE = "LINE";

    private static String STR_ISOL = "ISOL";

    private static final float LINE_WIDTH = 1.5f;

    private static final LineStyle NOW_LINE_STYLE = LineStyle.SOLID;

    private static final LineStyle FORECAST_LINE_STYLE = LineStyle.DASHED_LARGE;

    protected static final UnitConverter KNTS_TO_MPS = NonSI.KNOT
            .getConverterTo(SI.METERS_PER_SECOND);

    @XmlAttribute
    private boolean forecast = false;

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
        if (record instanceof ConvSigmetRecord) {
            ConvSigmetRecord sigmetRecord = (ConvSigmetRecord) record;
            if (sigmetRecord.getConvSigmetSection() != null) {
                for (ConvSigmetSection section : sigmetRecord
                        .getConvSigmetSection()) {
                    AdvisoryRecord newRec = null;
                    if (forecast && isSectionDirectionOrSpeedInvalid(section)) {
                        continue;
                    }
                    if (section.getClassType().equals(STR_AREA)) {
                        newRec = convertArea(section);
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

    private boolean isSectionDirectionOrSpeedInvalid(ConvSigmetSection section) {
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

    private AdvisoryRecord convertLine(ConvSigmetSection section) {
        Set<ConvSigmetLocation> locations = section.getConvSigmetLocation();
        if (locations == null) {
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
        return new AdvisoryRecord(coords, section.getDistance(), label, "");
    }

    private AdvisoryRecord convertIsol(ConvSigmetSection section) {
        Set<ConvSigmetLocation> locations = section.getConvSigmetLocation();
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
        return new AdvisoryRecord(center, section.getDistance(), label, "");
    }

    /**
     * convert an Area sigmet section
     * 
     * @param section
     *            the section convert
     */
    private AdvisoryRecord convertArea(ConvSigmetSection section) {
        Set<ConvSigmetLocation> locations = section.getConvSigmetLocation();
        if (locations == null) {
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

        return new AdvisoryRecord(coords, label, "");

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
        for (int i = 0; i < coords.length; i++) {
            movePoint(coords[i], section);
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
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ConvSigmetCastDataAdapter other = (ConvSigmetCastDataAdapter) obj;
        if (forecast != other.forecast)
            return false;
        return true;
    }

}
