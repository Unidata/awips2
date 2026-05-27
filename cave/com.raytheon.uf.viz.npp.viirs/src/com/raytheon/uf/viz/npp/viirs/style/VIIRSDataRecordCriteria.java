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

import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.style.MatchCriteria;
import com.raytheon.uf.common.style.StyleException;

/**
 * Match criteria for a populated {@link VIIRSDataRecord} object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2012            mschenke    Initial creation
 * Nov 22, 2013 2361       njensen     Added no-arg constructor
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSDataRecordCriteria extends MatchCriteria {

    private String parameter;

    private Double wavelength;

    private String channelType;

    private String region;

    /**
     * Constructor that exists to keep the StyleManager's JAXBManager happy.
     * JAXB will throw an error when introspecting this class if there's not a
     * no-arg constructor.
     */
    protected VIIRSDataRecordCriteria() {

    }

    public VIIRSDataRecordCriteria(VIIRSDataRecord record) {
        this.parameter = record.getParameter();
        this.wavelength = record.getWavelength();
        this.channelType = record.getChannelType();
        this.region = record.getRegion();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.style.MatchCriteria#matches(com.raytheon.uf.
     * common.style.MatchCriteria)
     */
    @Override
    public int matches(MatchCriteria aCriteria) throws StyleException {
        int rval = -1;
        if (aCriteria instanceof VIIRSDataMatchCriteria) {
            return matches(this, (VIIRSDataMatchCriteria) aCriteria);
        } else if (aCriteria instanceof VIIRSDataRecordCriteria) {
            rval = 0;
            VIIRSDataRecordCriteria criteria = (VIIRSDataRecordCriteria) aCriteria;
            if (region != null && region.equals(criteria.region)) {
                rval |= (1 << 0);
            }
            if (channelType != null && channelType.equals(criteria.channelType)) {
                rval |= (1 << 1);
            }
            if (wavelength != null && wavelength.equals(criteria.wavelength)) {
                rval |= (1 << 2);
            }
            if (parameter != null && parameter.equals(criteria.parameter)) {
                rval |= (1 << 3);
            }
        }
        return rval;
    }

    /**
     * Matches a data record criteria to a viirs style rule criteria. Weight
     * from least to most goes: region, channelType, wavelength, parameter
     * 
     * @param recordCriteria
     * @param matchCriteria
     * @return
     */
    public static int matches(VIIRSDataRecordCriteria recordCriteria,
            VIIRSDataMatchCriteria matchCriteria) {
        int rval = 0;
        // Check region
        if (matchCriteria.getRegion() != null
                && matchCriteria.getRegion().equals(recordCriteria.region)) {
            rval |= (1 << 0);
        } else if (matchCriteria.getRegion() != null) {
            --rval;
        }
        // Check channelType
        if (matchCriteria.getChannelType() != null
                && matchCriteria.getChannelType().equals(
                        recordCriteria.channelType)) {
            rval |= (1 << 1);
        } else if (matchCriteria.getChannelType() != null) {
            --rval;
        }
        // Check wavelength
        if (matchCriteria.getWavelength() != null) {
            boolean matches = false;
            for (Double wavelength : matchCriteria.getWavelength()) {
                if (wavelength.equals(recordCriteria.wavelength)) {
                    matches = true;
                    break;
                }
            }
            if (matches) {
                rval |= (1 << 2);
            } else {
                --rval;
            }
        }
        // Check parameter
        if (matchCriteria.getParameter() != null
                && matchCriteria.getParameter()
                        .equals(recordCriteria.parameter)) {
            rval |= (1 << 3);
        } else if (matchCriteria.getParameter() != null) {
            --rval;
        }
        return rval;
    }
}
