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
 * Grib post processor implementation to eliminate invalid 7HR and 8HR forecast
 * grids from the RUC236 model
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer          Description
 * ------------ ----------  -----------       --------------------------
 * 1/24/2012    DR 14263    M. Porricelli     Initial Creation
 * 
 * </pre>
 * 
 * @author porricel
 * @version 1
 */
public class RUC236GribPostProcessor implements IDecoderPostProcessor {

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {

        // Toss out all 7HR and 8HR forecast grids
        if (record.getDataTime().getFcstTime() == 25200
                || record.getDataTime().getFcstTime() == 28800) {

            return new GridRecord[] {};
        }
        return new GridRecord[] { record };
    }
}
