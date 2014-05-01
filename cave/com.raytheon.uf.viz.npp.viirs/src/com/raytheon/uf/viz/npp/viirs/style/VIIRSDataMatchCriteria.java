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
package com.raytheon.uf.viz.npp.viirs.style;

import java.util.Arrays;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.style.MatchCriteria;
import com.raytheon.uf.common.style.StyleException;

/**
 * Style match criteria for VIIRS data, order of importance is parameter,
 * wavelength, channelType, region
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 7, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "viirsDataMatches")
public class VIIRSDataMatchCriteria extends MatchCriteria {

    @XmlElement
    private String parameter;

    @XmlElement
    private Double[] wavelength;

    @XmlElement
    private String channelType;

    @XmlElement
    private String region;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.style.MatchCriteria#matches(com.raytheon
     * .uf.common.style.MatchCriteria)
     */
    @Override
    public int matches(MatchCriteria aCriteria) throws StyleException {
        int rval = -1;
        if (aCriteria instanceof VIIRSDataRecordCriteria) {
            return VIIRSDataRecordCriteria.matches(
                    (VIIRSDataRecordCriteria) aCriteria, this);
        } else if (aCriteria instanceof VIIRSDataMatchCriteria) {
            rval = 0;
            VIIRSDataMatchCriteria criteria = (VIIRSDataMatchCriteria) aCriteria;
            if (region != null && region.equals(criteria.region)) {
                rval |= (1 << 0);
            }
            if (channelType != null && channelType.equals(criteria.channelType)) {
                rval |= (1 << 1);
            }
            if (wavelength != null && criteria.wavelength != null
                    && Arrays.equals(wavelength, criteria.wavelength)) {
                rval |= (1 << 2);
            }
            if (parameter != null && parameter.equals(criteria.parameter)) {
                rval |= (1 << 3);
            }
        }
        return rval;
    }

    /**
     * @return the parameter
     */
    public String getParameter() {
        return parameter;
    }

    /**
     * @param parameter
     *            the parameter to set
     */
    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

    /**
     * @return the wavelength
     */
    public Double[] getWavelength() {
        return wavelength;
    }

    /**
     * @param wavelength
     *            the wavelength to set
     */
    public void setWavelength(Double[] wavelength) {
        this.wavelength = wavelength;
    }

    /**
     * @return the channelType
     */
    public String getChannelType() {
        return channelType;
    }

    /**
     * @param channelType
     *            the channelType to set
     */
    public void setChannelType(String channelType) {
        this.channelType = channelType;
    }

    /**
     * @return the region
     */
    public String getRegion() {
        return region;
    }

    /**
     * @param region
     *            the region to set
     */
    public void setRegion(String region) {
        this.region = region;
    }

}
