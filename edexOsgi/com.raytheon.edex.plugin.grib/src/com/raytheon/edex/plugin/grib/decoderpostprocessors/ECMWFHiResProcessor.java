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
 * Grib post processor implementation to scale TP-ECMWF data to the proper unit.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 30, 2010  5875     bphillip    Initial Creation
 * Oct 15, 2013  2473     bsteffen    Removed unused method argument.
 * Oct 07, 2015  3756     nabowle     Changed to only do data scaling. Extends
 *                                    DecoderPostProcessor.
 *
 *
 * </pre>
 *
 * @author bphillip
 * @version 1
 */
public class ECMWFHiResProcessor extends DecoderPostProcessor {

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {
        // Post process the data if this is a Total Precipitation grid
        if (record.getParameter().getAbbreviation().equals("TP-ECMWF")) {
            float[] data = getMessageData(record);
            for (int i = 0; i < data.length; i++) {
                data[i] = data[i] * 1000;
            }
        }
        return new GridRecord[] { record };
    }
}
