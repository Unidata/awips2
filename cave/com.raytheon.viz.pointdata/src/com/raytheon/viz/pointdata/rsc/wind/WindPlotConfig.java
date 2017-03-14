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
package com.raytheon.viz.pointdata.rsc.wind;

import java.util.HashSet;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

/**
 * 
 * JAXB compatible representation of all the configuration options needed to
 * request data and sampling information for wind vectors.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Nov 13, 2015  4903     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class WindPlotConfig {

    @XmlElement
    private WindPlotParameter longitude = new WindPlotParameter("longitude");

    @XmlElement
    private WindPlotParameter latitude = new WindPlotParameter("latitude");

    @XmlElement
    private WindPlotParameter speed = new WindPlotParameter("windSpd", "kn");

    @XmlElement
    private WindPlotParameter direction = new WindPlotParameter("windDir");

    @XmlElement
    private SampleFormat sample;

    public WindPlotParameter getLongitude() {
        return longitude;
    }

    public void setLongitude(WindPlotParameter longitude) {
        this.longitude = longitude;
    }

    public WindPlotParameter getLatitude() {
        return latitude;
    }

    public void setLatitude(WindPlotParameter latitude) {
        this.latitude = latitude;
    }

    public WindPlotParameter getSpeed() {
        return speed;
    }

    public void setSpeed(WindPlotParameter speed) {
        this.speed = speed;
    }

    public WindPlotParameter getDirection() {
        return direction;
    }

    public void setDirection(WindPlotParameter direction) {
        this.direction = direction;
    }

    public SampleFormat getSample() {
        return sample;
    }

    public void setSample(SampleFormat sample) {
        this.sample = sample;
    }

    public Set<String> getUniqueParameters() {
        Set<String> result = new HashSet<>(5);
        result.add(longitude.getParameter());
        result.add(latitude.getParameter());
        result.add(speed.getParameter());
        result.add(direction.getParameter());
        if (sample != null) {
            result.addAll(sample.getUniqueParameters());
        }
        return result;
    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static class SampleFormat {

        @XmlAttribute
        private String text;

        @XmlElement(name = "field")
        private WindPlotParameter[] fields;

        public String getText() {
            return text;
        }

        public void setText(String text) {
            this.text = text;
        }

        public WindPlotParameter[] getFields() {
            return fields;
        }

        public void setFields(WindPlotParameter[] fields) {
            this.fields = fields;
        }

        public Set<String> getUniqueParameters() {
            Set<String> result = new HashSet<>();
            for (WindPlotParameter p : fields) {
                result.add(p.getParameter());
            }
            return result;
        }

    }
}
