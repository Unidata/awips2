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

import java.util.Calendar;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;

/**
 * Grib post processor implementation to decode monthly CPC outlook grids
 * 
 * CPC long-range outlook grids (forecast projections of 30 days, 90-450 days at
 * 30-day intervals) do not contain the forecast periods within the data in the
 * usual way, and this must be calculated from the time information. The reftime
 * of each grid in the same run/delivery (which occurs monthly) contains a
 * subsequent hour (00:00:00 through 13:00:00). These hours are used to
 * determine the forecast period. E.g. for a reftime of 2008-10-15 01:00:00 the
 * '01' tells us that this grid is for the second forecast increment. Since
 * there is no 60-day forecast, an hour of '00' represents the 30-day outlook
 * and then there is a shift so that an hour of '01' represents the 90-day
 * outlook, hence the two calculations below, taken from the AWIPS I code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/09/11     4243        porricel    Initial Creation
 * 
 * </pre>
 * 
 * @author
 * @version
 */
public class CPCoutlookGribPostProcessor implements IDecoderPostProcessor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CPCoutlookGribPostProcessor.class);

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {

        Calendar refTime = record.getDataTime().getRefTimeAsCalendar();

        int hour = refTime.get(Calendar.HOUR_OF_DAY);
        int time1, time2;
        if (hour == 0) {
            time1 = 0;
            time2 = (hour + 1) * 30 * 86400;
        } else {
            time1 = 0;
            time2 = (hour + 2) * 30 * 86400;
        }
        int fcstTime = time2 - time1;
        refTime.set(Calendar.HOUR_OF_DAY, 0);

        DataTime newDataTime = new DataTime(refTime, fcstTime);
        record.setDataTime(newDataTime);
        record.setDataURI(null);

        try {
            record.setPluginName(GridConstants.GRID);
            record.constructDataURI();
        } catch (PluginException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error constructing dataURI!", e);
        }
        return new GridRecord[] { record };
    }
}
