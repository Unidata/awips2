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
package com.raytheon.viz.awipstools.common;

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.adapters.CoordAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Data representing a storm track that can be saved to XML
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 12, 2009            bwoodle     Initial creation
 * Jun 03, 2014 3191       njensen      Added xml annotations
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class StormTrackData {

    @XmlJavaTypeAdapter(DateAdapter.class)
    @XmlElement(name = "stormDate")
    private Date date;

    @XmlElement(name = "stormAngle")
    private double motionDirection;

    @XmlElement(name = "stormSpeed")
    private double motionSpeed;

    @XmlJavaTypeAdapter(CoordAdapter.class)
    @XmlElement(name = "stormCoordinates")
    private Coordinate[] coordinates;

    public StormTrackData() {
        date = null;
        motionDirection = Double.NaN;
        motionSpeed = Double.NaN;
        coordinates = null;
    }

    public StormTrackData(StormTrackData data) {
        date = data.date;
        motionSpeed = data.motionSpeed;
        motionDirection = data.motionDirection;
        if (data.coordinates != null) {
            coordinates = new Coordinate[data.coordinates.length];
            for (int i = 0; i < data.coordinates.length; ++i) {
                coordinates[i] = new Coordinate(data.coordinates[i]);
            }
        }
    }

    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    /**
     * Angle in 0-360 format
     * 
     * @return
     */
    public double getMotionDirection() {
        return motionDirection;
    }

    /**
     * Angle in 0-360 format
     * 
     * @param motionDirection
     */
    public void setMotionDirection(double motionDirection) {
        this.motionDirection = motionDirection;
    }

    /**
     * Storm speed in knots
     * 
     * @return
     */
    public double getMotionSpeed() {
        return motionSpeed;
    }

    /**
     * Storm speed in knots
     * 
     * @param motionSpeed
     */
    public void setMotionSpeed(double motionSpeed) {
        this.motionSpeed = motionSpeed;
    }

    public Coordinate[] getCoordinates() {
        return coordinates;
    }

    public void setCoordinates(Coordinate[] coordinates) {
        this.coordinates = coordinates;
    }

    public boolean isValid() {
        return date != null && !Double.isNaN(motionDirection)
                && !Double.isNaN(motionSpeed);
    }

    private static class DateAdapter extends XmlAdapter<Long, Date> {

        @Override
        public Date unmarshal(Long v) throws Exception {
            return new Date(v);
        }

        @Override
        public Long marshal(Date v) throws Exception {
            return v.getTime();
        }

    }

}
