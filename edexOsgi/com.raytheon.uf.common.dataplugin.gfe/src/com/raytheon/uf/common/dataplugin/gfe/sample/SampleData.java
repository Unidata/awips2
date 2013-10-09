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
package com.raytheon.uf.common.dataplugin.gfe.sample;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.serialization.adapters.CoordAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPoint;

/**
 * Contains a set of Sample Data (a sample set).
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 14, 2008	879			rbell	Initial creation
 * 
 * </pre>
 * 
 * @author rbell
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SampleData implements Cloneable {

    private static final String LEGACY_FILE_HEADER_LINE_PATTERN = "\\d+";

    private static final String LEGACY_FILE_COORDINATE_LINE_PATTERN = "-?\\d+(?:\\.\\d+)?\\s-?\\d+(?:\\.\\d+)?";

    private static final String LINE_STRING_PATTERN = "LINESTRING \\((-?\\d+(?:\\.\\d+)? -?\\d+(?:\\.\\d+)?.*)\\)";

    private static final String LINE_STRING_COORDINATE_PATTERN = "\\s*(-?\\d+(?:\\.\\d+)?)\\s+(-?\\d+(?:\\.\\d+)?)\\s*";

    private static final SingleTypeJAXBManager<SampleData> jaxb = SingleTypeJAXBManager
            .createWithoutException(SampleData.class);

    private SampleId sampleId;

    @XmlJavaTypeAdapter(value = CoordAdapter.class)
    @DynamicSerializeElement
    private List<Coordinate> points;

    /**
     * Returns the JAXBManager that handles SampleData
     * 
     * @return
     */
    public static SingleTypeJAXBManager<SampleData> getJAXBManager() {
        return jaxb;
    }

    /**
     * Default constructor
     */
    public SampleData() {
        this.sampleId = null;
        this.points = new ArrayList<Coordinate>();
    }

    /**
     * @param anId
     * @param somePoints
     */
    public SampleData(final SampleId anId, final List<Coordinate> somePoints) {
        this.sampleId = anId;
        this.points = new ArrayList<Coordinate>(somePoints);
    }

    /**
     * Copy constructor
     * 
     * @param rhs
     */
    public SampleData(SampleData rhs) {
        this.sampleId = rhs.sampleId;
        this.points = new ArrayList<Coordinate>();
        for (Coordinate thisCoord : rhs.points) {
            this.points.add((Coordinate) thisCoord.clone());
        }
    }

    public static SampleData createSampleDataFromLineString(String aLineString) {
        SampleData rVal = new SampleData();
        rVal.points = SampleData
                .getCoordinatesFromLineStringContents(aLineString);

        return rVal;
    }

    public static SampleData createSampleDataFromLegacyFileContents(
            String aLegacyFileContentsString) {
        SampleData rVal = new SampleData();
        rVal.points = SampleData
                .getCoordinatesFromLegacyFileContents(aLegacyFileContentsString);

        return rVal;
    }

    public static SampleData createSampleDataFromLineString(String aLineString,
            SampleId anId) {
        SampleData rVal = new SampleData();
        rVal.sampleId = anId;
        rVal.points = SampleData
                .getCoordinatesFromLineStringContents(aLineString);

        return rVal;
    }

    public static SampleData createSampleDataFromLegacyFileContents(
            String aLegacyFileContentsString, SampleId anId) {
        SampleData rVal = new SampleData();
        rVal.sampleId = anId;
        rVal.points = SampleData
                .getCoordinatesFromLegacyFileContents(aLegacyFileContentsString);

        return rVal;
    }

    private static ArrayList<Coordinate> getCoordinatesFromLegacyFileContents(
            String aLegacyFileContentsString) {
        ArrayList<Coordinate> rVal = new ArrayList<Coordinate>();

        Pattern pattern;
        Matcher matcher;

        String peices[] = aLegacyFileContentsString.split("\\n");
        if (peices.length == 0) {
            throw new IllegalArgumentException(
                    "The supplied contents are empty");
        }

        String line = peices[0];
        pattern = Pattern.compile(LEGACY_FILE_HEADER_LINE_PATTERN);
        matcher = pattern.matcher(line);
        if (!matcher.matches()) {
            throw new IllegalArgumentException(
                    "The first line is not a positive integer: " + line);
        }

        int numberOfCoordinates = Integer.parseInt(line);
        if (numberOfCoordinates != peices.length - 1) {
            throw new IllegalArgumentException(
                    "Contents contain number of coordinate lines not equal amount specified: "
                            + numberOfCoordinates);
        }

        pattern = Pattern.compile(LEGACY_FILE_COORDINATE_LINE_PATTERN);
        for (int i = 1; i <= numberOfCoordinates; i++) {
            line = peices[i];
            matcher = pattern.matcher(line);
            if (!matcher.matches()) {
                throw new IllegalArgumentException(
                        "Contents contain line that is not a coordinate: "
                                + line);
            }

            String coordinatePeices[] = line.split(" ");
            double thisLat = Double.parseDouble(coordinatePeices[0]);
            double thisLon = Double.parseDouble(coordinatePeices[1]);
            rVal.add(new Coordinate(thisLat, thisLon));
        }

        return rVal;
    }

    private static List<Coordinate> getCoordinatesFromLineStringContents(
            String aFileContentsString) {
        Pattern pattern;
        Matcher matcher;

        pattern = Pattern.compile(LINE_STRING_PATTERN);
        matcher = pattern.matcher(aFileContentsString);
        if (!matcher.matches()) {
            throw new IllegalArgumentException(
                    "The file contents are not valid");
        }

        ArrayList<Coordinate> rVal = new ArrayList<Coordinate>();
        String[] coords = matcher.group(1).split(",");
        pattern = Pattern.compile(LINE_STRING_COORDINATE_PATTERN);
        for (String thisCoord : coords) {
            matcher = pattern.matcher(thisCoord);
            if (!matcher.matches()) {
                throw new IllegalArgumentException("Invalid coordinate: "
                        + thisCoord);
            }
            double thisLat = Double.parseDouble(matcher.group(1));
            double thisLon = Double.parseDouble(matcher.group(2));
            rVal.add(new Coordinate(thisLat, thisLon));
        }

        return rVal;
    }

    /**
     * @return the sampleId
     */
    public SampleId getSampleId() {
        return this.sampleId;
    }

    /**
     * @return the points
     */
    public List<Coordinate> getPoints() {
        return this.points;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "Id: " + this.sampleId + " Points: " + this.points;
    }

    /**
     * Get the geometry
     * 
     * @return the geometry
     */
    public MultiPoint getGeometry() {
        GeometryFactory geometryFactory = new GeometryFactory();
        MultiPoint geom = geometryFactory.createMultiPoint(this.points
                .toArray(new Coordinate[this.points.size()]));
        return geom;
    }

    /**
     * Set the geometry.
     * 
     * @param g
     *            the geometry to set
     */
    public void setGeometry(MultiPoint g) {
        if (!(g instanceof MultiPoint)) {
            throw new IllegalArgumentException("MultiPoint expected, got: " + g);
        }

        MultiPoint mp = g;
        Coordinate[] c = mp.getCoordinates();
        this.points.clear();
        this.points.addAll(Arrays.asList(c));
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public SampleData clone() {
        return new SampleData(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((this.sampleId == null) ? 0 : this.sampleId.hashCode());
        result = prime * result
                + ((this.points == null) ? 0 : this.points.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
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
        final SampleData other = (SampleData) obj;
        if (this.sampleId == null) {
            if (other.sampleId != null) {
                return false;
            }
        } else if (!this.sampleId.equals(other.sampleId)) {
            return false;
        }
        if (this.points == null) {
            if (other.points != null) {
                return false;
            }
        } else if (!this.points.equals(other.points)) {
            return false;
        }
        return true;
    }

    /**
     * @param sampleId
     *            the sampleId to set
     */
    public void setSampleId(SampleId sampleId) {
        this.sampleId = sampleId;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setPoints(List<Coordinate> points) {
        this.points = points;
    }

}
