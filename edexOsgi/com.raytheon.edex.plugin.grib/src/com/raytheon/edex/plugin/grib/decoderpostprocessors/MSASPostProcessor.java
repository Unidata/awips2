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
package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.parameter.Parameter;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class MSASPostProcessor implements IDecoderPostProcessor {

    private static final String TSLSA = "TSLSA";

    private static final String SFC = "SFC";

    private static final String PMSL = "PMSL";

    private static final String MSLP_ABBR = "MSLP";

    private static final String MSLP_DESC = "NWS mean sea level pressure";

    private static final String PT3_ABBR = "PT3";

    private static final String ALTI_ABBR = "Alti";

    private static final String ALTI_DESC = "Analysis of altimeter setting";

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.grib.decoderpostprocessors.IDecoderPostProcessor
     * #process(com.raytheon.uf.common.dataplugin.grib.GridRecord)
     */
    @Override
    public GridRecord[] process(GridRecord record) throws GribException {
        boolean modelInfoModified = false;
        String currentAbbr = record.getParameter().getAbbreviation();
        String levelName = record.getLevel().getMasterLevel().getName();

        // Reassign PMSL at the SFC to Altimeter
        if (currentAbbr.equals(PMSL) && levelName.equals(SFC)) {
            Parameter param = new Parameter(ALTI_ABBR, ALTI_DESC, record
                    .getParameter().getUnit());
            record.setParameter(param);
            modelInfoModified = true;
        }

        else if (currentAbbr.equals(PMSL)) {
            Parameter param = new Parameter(MSLP_ABBR, MSLP_DESC, record
                    .getParameter().getUnit());
            record.setParameter(param);
            modelInfoModified = true;
        }

        // Reassigns 3-hr pressure tendency to abbreviation used by MSAS
        else if (currentAbbr.equals(TSLSA)) {
            Parameter param = new Parameter(PT3_ABBR, record.getParameter()
                    .getName(), SI.MILLI(NonSI.BAR).times(100));
            record.setParameter(param);
            modelInfoModified = true;
        }

        if (modelInfoModified) {
            record.getInfo().setId(null);
            record.setDataURI(null);
            try {
                record.constructDataURI();
            } catch (Exception e) {
                throw new GribException(
                        "Error creating new dataURI for MSAS data!", e);
            }
        }

        record.setOverwriteAllowed(true);
        return new GridRecord[] { record };
    }
}
