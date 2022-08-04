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
 * Grib post processor implementation to eliminate 2-3hr duration grids from the
 * RUC130 model
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 9/9/10       #4419        bphillip    Initial Creation
 * Oct 07, 2015  3756        nabowle     Extends DecoderPostProcessor.
 *
 * </pre>
 *
 * @author bphillip
 * @version 1
 */
public class RUC130GribPostProcessor extends DecoderPostProcessor {

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {

        // Toss out all 2-3hr duration grids
        if (record.getParameter().getAbbreviation().endsWith("hr")
                && !record.getParameter().getAbbreviation().endsWith("1hr")) {
            return new GridRecord[] {};
        }
        return new GridRecord[] { record };
    }
}
