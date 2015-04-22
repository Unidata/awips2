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
package gov.noaa.nws.ncep.viz.rsc.solarimage.display;

import gov.noaa.nws.ncep.viz.rsc.solarimage.rsc.SolarImageResourceData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.style.MatchCriteria;
import com.raytheon.uf.common.style.StyleException;

//import com.raytheon.uf.common.*;

/**
 * Match criteria in styleRule
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer             Description
 * ------------ ---------- -----------          --------------------------
 * 01/22/2014   958        qzhou                Initial creation
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "solarImageMatches")
public class SolarImageMatchCriteria extends MatchCriteria {

    @XmlElement
    private String instrument;

    @XmlElement
    private String wavelength;

    @XmlElement
    private String intTime;

    @XmlElement
    private String satellite;

    public static SolarImageMatchCriteria constructFromResourceData(
            SolarImageResourceData rscdata) {

        SolarImageMatchCriteria criteria = new SolarImageMatchCriteria();

        if (!rscdata.getInstrument().isEmpty()) {
            criteria.setInstrument(rscdata.getInstrument());
        }

        if (!rscdata.getWavelength().isEmpty()) {
            criteria.setWavelength(rscdata.getWavelength());
        }

        if (!rscdata.getIntTime().isEmpty()) {
            criteria.setIntTime(rscdata.getIntTime());
        }

        return criteria;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.style.MatchCriteria#matches(com.raytheon .uf.
     * viz.core.style.MatchCriteria)
     */
    @Override
    public int matches(MatchCriteria aCriteria) throws StyleException {
        int rval = -1;
        if (aCriteria instanceof SolarImageMatchCriteria) {
            rval = 0;

            SolarImageMatchCriteria criteria = (SolarImageMatchCriteria) aCriteria;

            if (instrument != null
                    && (instrument.contains(criteria.instrument) || (criteria.instrument != null && criteria.instrument
                            .contains(instrument)))) {
                rval++;
            }

            if (wavelength != null
                    && wavelength.equalsIgnoreCase(criteria.wavelength)) {
                rval++;
            }

            if (intTime != null && intTime.equalsIgnoreCase(criteria.intTime)) {
                rval++;
            }

            /*
             * if (detector != null &&
             * detector.equalsIgnoreCase(criteria.detector)) { rval++; }
             */

            if (rval == getTotalNotNullValues()) {
                rval = 1;
            } else {
                rval = 0;
            }
        }
        return rval;
    }

    public int getTotalNotNullValues() throws StyleException {

        int tval = 0;
        if (instrument != null)
            tval++;
        if (wavelength != null)
            tval++;
        if (intTime != null)
            tval++;

        return tval;
    }

    /**
     * @return the wavelength
     */
    public String getWavelength() {
        return wavelength;
    }

    /**
     * @param wavelength
     *            the wavelength to set
     */
    public void setWavelength(String wavelength) {
        this.wavelength = wavelength;
    }

    /**
     * @return the intTime
     */
    public String getIntTime() {
        return intTime;
    }

    /**
     * @param intTime
     *            the intTime to set
     */
    public void setIntTime(String intTime) {
        this.intTime = intTime;
    }

    public String getInstrument() {
        return instrument;
    }

    public void setInstrument(String instrument) {
        this.instrument = instrument;
    }

    public String getSatellite() {
        return satellite;
    }

    public void setSatellite(String satellite) {
        this.satellite = satellite;
    }
}
