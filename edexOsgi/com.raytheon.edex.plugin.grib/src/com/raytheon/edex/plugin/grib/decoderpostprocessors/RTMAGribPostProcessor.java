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
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Grib post processor implementation correct the total cloud cover parameter in
 * the RTMA grib model
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 01, 2011  6320     bphillip    Initial Creation
 * Oct 15, 2013  2473     bsteffen    Remove deprecated method calls.
 * Oct 07, 2015  3756     nabowle     Extends DecoderPostProcessor.
 *
 *
 * </pre>
 *
 * @author bphillip
 * @version 1
 */
public class RTMAGribPostProcessor extends DecoderPostProcessor {

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {

        Calendar time = record.getDataTime().getRefTimeAsCalendar();
        if (record.getParameter().getAbbreviation().equals("TCC")
                && time.get(Calendar.MINUTE) > 0) {

            time.set(Calendar.MINUTE, 0);
            DataTime newDataTime = record.getDataTime().clone();
            newDataTime.setRefTime(time.getTime());
            newDataTime.setValidPeriod(new TimeRange(time, time));
            record.setDataTime(newDataTime);
            record.setDataURI(null);
        }
        return new GridRecord[] { record };
    }
}
