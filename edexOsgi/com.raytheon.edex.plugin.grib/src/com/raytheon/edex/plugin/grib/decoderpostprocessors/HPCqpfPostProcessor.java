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

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;

/**
 * Grib decoder post processor implementation for transforming HPCqpf data.
 * <p>
 * This post processor currently looks for precipitation probability grids and
 * multiplies the data values by 100 to get an actual percentage (values 0-100)
 * instead of a decimal value (0-1)
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 6/11/2015    4542       bphillip    Initial creation
 * Oct 07, 2015 3756       nabowle     Extends DecoderPostProcessor.
 *
 * @author bphillip
 * @version 1.0
 */
public class HPCqpfPostProcessor extends DecoderPostProcessor {

    /** Parameter abbreviation patter for perecent probability grids */
    private static final String PCT_PROB_PATTERN = "ProbTP\\dp\\d{2}in\\d{1,2}hr";

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {
        String parameterAbbreviation = record.getParameter().getAbbreviation();
        // Check to see if this is a grid we are looking for
        if (parameterAbbreviation.matches(PCT_PROB_PATTERN)) {
            // If so, multiply the data by 100 to obtain a percent
            float[] data = getMessageData(record);
            for (int i = 0; i < data.length; i++) {
                if (data[i] >= 0.0 && data[i] <= 1.0) {
                    data[i] *= 100;
                }
            }
            record.setMessageData(data);
        }
        return new GridRecord[] { record };
    }
}
