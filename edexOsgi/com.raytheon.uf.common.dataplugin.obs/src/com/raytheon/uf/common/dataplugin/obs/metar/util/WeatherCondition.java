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

package com.raytheon.uf.common.dataplugin.obs.metar.util;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Class representing a weather condition item contained in a metar message
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 14, 2007  139      Phillippe   Initial creation    
 * Dec 03, 2013  2537     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class WeatherCondition implements Serializable {

    private static final long serialVersionUID = 1L;

    /** Regular expression for the current weather phenomena */
    public static final Pattern WEATHER_EXP = Pattern
            .compile(" (\\+|\\-|VC)?(MI|PR|BC|DR|BL|SH|TS|FZ)?((DZ|RA|SN|SG|IC|PL|GR|GS|UP)*)(BR|FG|FU|VA|DU|SA|HZ|PY)?(PO|SQ|FC|SS|DS)?\\b");
    // Group indices within the WEATHER_EXP pattern.
    private static final int INT_PROX_GRP = 1;
    private static final int DESCR_GRP = 2;
    private static final int PRECIP_GRP = 3;
    private static final int OBSURE_GRP = 5;
    private static final int OTHER_GRP = 6;


    private static final String EMPTY = "";
    
    /** The primary key for the database table * */
    @Id
    @GeneratedValue
    private Integer key;

    /**
     * The sequence identifier tracks the position of the element
     * within the present weather group(s). 
     */
    @XmlElement
    @DynamicSerializeElement
    @Column
    private Integer sequenceId = 0;
    
    /** The intensity proximity notation * */
    @XmlElement
    @DynamicSerializeElement
    @Column
    private String intensityProximity = EMPTY;

    /** The descriptor notation * */
    @XmlElement
    @DynamicSerializeElement
    @Column
    private String descriptor = EMPTY;

    /** The precipitation notation * */
    @XmlElement
    @DynamicSerializeElement
    @Column
    private String precipitation = EMPTY;

    /** The obscuration notation * */
    @XmlElement
    @DynamicSerializeElement
    @Column
    private String obscuration = EMPTY;

    /** The other notation * */
    @XmlElement
    @DynamicSerializeElement
    @Column
    private String other = EMPTY;

    /** The METAR this weather condition belongs to */
    @ManyToOne
    @JoinColumn(name="parentMetar", nullable=false)
    private MetarRecord parentMetar;

    /**
     * No-Arg Constructor.
     */
    public WeatherCondition() {
    }

    /**
     * Constructor.
     * 
     * @param intensityProximity
     * @param descriptor
     * @param precipitation
     * @param obscuration
     * @param other
     */
    public WeatherCondition(String intensityProximity, String descriptor,
            String precipitation, String obscuration, String other) {

        setIntensityProximity(intensityProximity);
        setDescriptor(descriptor);
        setPrecipitation(precipitation);
        setObscuration(obscuration);
        setOther(other);
    }

    /**
     * Constructor.
     * 
     * @param intensityProximity
     * @param descriptor
     * @param precipitation
     * @param obscuration
     * @param other
     */
    public WeatherCondition(Integer seq, String intensityProximity, String descriptor,
            String precipitation, String obscuration, String other) {
        this(intensityProximity,descriptor,precipitation,obscuration,other);
        setSequenceId(seq);
    }
    
    /**
     * @return the sequenceId
     */
    public Integer getSequenceId() {
        return sequenceId;
    }

    /**
     * @param sequenceId the sequenceId to set
     */
    public void setSequenceId(Integer sequenceId) {
        this.sequenceId = sequenceId;
    }

    /**
     * @return the descriptor
     */
    public String getDescriptor() {
        return descriptor;
    }

    /**
     * @param descriptor
     *            the descriptor to set
     */
    public void setDescriptor(String descriptor) {
        if (descriptor != null) {
            this.descriptor = descriptor;
        }
    }

    /**
     * @return the intensityProximity
     */
    public String getIntensityProximity() {
        return intensityProximity;
    }

    /**
     * @param intensityProximity
     *            the intensityProximity to set
     */
    public void setIntensityProximity(String intensityProximity) {
        if (intensityProximity != null) {
            this.intensityProximity = intensityProximity;
        }
    }

    /**
     * @return the key
     */
    public Integer getKey() {
        return key;
    }

    /**
     * Private since only Hibernate should be setting this generated key.
     * 
     * @param key
     *            the key to set
     */
    @SuppressWarnings("unused")
    private void setKey(Integer key) {
        this.key = key;
    }

    /**
     * @return the obscuration
     */
    public String getObscuration() {
        return obscuration;
    }

    /**
     * @param obscuration
     *            the obscuration to set
     */
    public void setObscuration(String obscuration) {
        if (obscuration != null) {
            this.obscuration = obscuration;
        }
    }

    /**
     * @return the other
     */
    public String getOther() {
        return other;
    }

    /**
     * @param other
     *            the other to set
     */
    public void setOther(String other) {
        if (other != null) {
            this.other = other;
        }
    }

    /**
     * @return the precipitation
     */
    public String getPrecipitation() {
        return precipitation;
    }

    /**
     * @param precipitation
     *            the precipitation to set
     */
    public void setPrecipitation(String precipitation) {
        if (precipitation != null) {
            this.precipitation = precipitation;
        }
    }

    public MetarRecord getParentMetar() {
        return parentMetar;
    }

    public void setParentMetar(MetarRecord parentMetar) {
        this.parentMetar = parentMetar;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Int:");
        sb.append(intensityProximity);
        sb.append(":Desc:");
        sb.append(descriptor);
        sb.append(":Precip:");
        sb.append(precipitation);
        sb.append(":Obscuration:");
        sb.append(obscuration);
        sb.append(":other:");
        sb.append(other);
        return sb.toString();
    }

    /**
     * 
     * @return
     */
    public String toCanonicalForm() {
        StringBuilder sb = new StringBuilder();
        if((intensityProximity != null)&&(!"".equals(intensityProximity))) {
            sb.append(intensityProximity);
        }
        if((descriptor != null)&&(!"".equals(descriptor))) {
            sb.append(descriptor);
        }
        if((precipitation != null)&&(!"".equals(precipitation))) {
            sb.append(precipitation);
        }
        if((obscuration != null)&&(!"".equals(obscuration))) {
            sb.append(obscuration);
        }
        if((other != null)&&(!"".equals(other))) {
            sb.append(other);
        }
        return sb.toString();
    }
    
    @Override
    public int hashCode() {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result
                + ((descriptor == null) ? 0 : descriptor.hashCode());
        result = PRIME
                * result
                + ((intensityProximity == null) ? 0 : intensityProximity
                        .hashCode());
        result = PRIME * result + ((key == null) ? 0 : key.hashCode());
        result = PRIME * result
                + ((obscuration == null) ? 0 : obscuration.hashCode());
        result = PRIME * result + ((other == null) ? 0 : other.hashCode());
        result = PRIME * result
                + ((precipitation == null) ? 0 : precipitation.hashCode());
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
        final WeatherCondition other = (WeatherCondition) obj;
        if (descriptor == null) {
            if (other.descriptor != null)
                return false;
        } else if (!descriptor.equals(other.descriptor))
            return false;
        if (intensityProximity == null) {
            if (other.intensityProximity != null)
                return false;
        } else if (!intensityProximity.equals(other.intensityProximity))
            return false;
        if (key == null) {
            if (other.key != null)
                return false;
        } else if (!key.equals(other.key))
            return false;
        if (obscuration == null) {
            if (other.obscuration != null)
                return false;
        } else if (!obscuration.equals(other.obscuration))
            return false;
        if (this.other == null) {
            if (other.other != null)
                return false;
        } else if (!this.other.equals(other.other))
            return false;
        if (precipitation == null) {
            if (other.precipitation != null)
                return false;
        } else if (!precipitation.equals(other.precipitation))
            return false;
        return true;
    }

    /**
     * 
     * @param weather
     * @return
     */
    public static final List<WeatherCondition> parseWeather(String weather) {
        List<WeatherCondition> wxList = new ArrayList<WeatherCondition>();
        
        Matcher matcher = WEATHER_EXP.matcher(" " + (weather.trim()));
        while (matcher.find()) {
            if (matcher.groupCount() >= OTHER_GRP) {
                WeatherCondition cond = new WeatherCondition(
                        matcher.group(INT_PROX_GRP), matcher
                                .group(DESCR_GRP), matcher.group(PRECIP_GRP),
                        matcher.group(OBSURE_GRP), matcher
                                .group(OTHER_GRP));
                wxList.add(cond);
            }
        }
        return wxList;
    }
    
    /**
     * 
     * @param weather
     * @return
     */
    public static final WeatherCondition [] splitWeather(WeatherCondition weather) {
        WeatherCondition [] wcs = null;
        if(weather != null) {
            String precip = weather.getPrecipitation();
            // Empty precip sub-group?
            if(!"".equals(precip)) {
                // Are there at least 2 precip types in the sub-group?
                int groups = precip.length() / 2;
                if(groups >= 2) {
                    wcs = new WeatherCondition[groups];
                    weather.precipitation = precip.substring(0,2);
                    wcs[0] = weather;
                    int sequence = weather.getSequenceId() + 1;
                    for(int i = 1;i < groups;i++) {
                        WeatherCondition wx = new WeatherCondition();
                        wx.setSequenceId(sequence++);
                        wx.setIntensityProximity(weather.getIntensityProximity());
                        String desc = weather.getDescriptor();
                        if("TS".endsWith(desc)) {
                        } else if("SH".endsWith(desc)) {
                        } else {
                            desc = "";
                        }
                        wx.setDescriptor(desc);
                        wx.setPrecipitation(precip.substring(i*2, i*2 + 2));
                        wcs[i] = wx;
                    }
                } else {
                    wcs = new WeatherCondition[] { weather };
                }
            }
        }
        return (wcs == null) ? new WeatherCondition [0] : wcs;
    }

    /**
     * 
     * @return
     */
    public static String toCanonicalForm(List<WeatherCondition> groups) {
        StringBuilder sb = new StringBuilder();
        if((groups != null)&&(groups.size() > 0)) {
            sb.append(groups.get(0).toCanonicalForm());
            for(int i = 1;i < groups.size();i++) {
                sb.append(" ");
                sb.append(groups.get(i).toCanonicalForm());
            }
        }
        return sb.toString();
    }
    
    /**
     * 
     * @param args
     */
    public static final void main(String [] args) {
        
        String data = "+TSRA BR";
        
        List<WeatherCondition> wc = parseWeather(data);
        
        for(WeatherCondition w : wc) {
            System.out.println(w);
            System.out.println(w.toCanonicalForm());
        }
        System.out.println((data.equals(toCanonicalForm(wc))) ? "Passed" : "Failed");
    }
}
