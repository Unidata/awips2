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
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.exception.GribException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Grib post processor implementation for TPCWind grids. This class modifies the
 * probablity grids to contain the correct time ranges.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/13/11      #6644        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class TPCWindPostProcessor implements IDecoderPostProcessor {

    @Override
    public GribRecord[] process(GribRecord record) throws GribException {

        if (record.getModelInfo().getParameterAbbreviation().startsWith("Prob")) {
            DataTime oldDataTime = record.getDataTime();
            Calendar newStartTime = (Calendar) Calendar.getInstance(
                    TimeZone.getTimeZone("GMT")).clone();
            newStartTime.setTime(oldDataTime.getRefTime());
            newStartTime.add(Calendar.SECOND, oldDataTime.getFcstTime());
            TimeRange newTimeRange = new TimeRange(newStartTime.getTime(),
                    oldDataTime.getValidPeriod().getEnd());

            Calendar newRefTime = (Calendar) Calendar.getInstance(
                    TimeZone.getTimeZone("GMT")).clone();
            newRefTime.setTime(oldDataTime.getRefTime());

            DataTime newDataTime = new DataTime(newRefTime,
                    oldDataTime.getFcstTime(), newTimeRange);

            record.setDataTime(newDataTime);
            record.setDataURI(null);
            try {
                record.constructDataURI();
            } catch (Exception e) {
                throw new GribException(
                        "Error creating new dataURI for RTMA data!", e);
            }
        }
        return new GribRecord[] { record };
    }

}
