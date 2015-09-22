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
 * The ARIPostProcessor is a grib post processor implementation to update
 * parameter and time definitions that come for the Archival Recurrence Interval
 * FFG grids.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 21, 2015  4756     dhladky     Initial Creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 * */

public class ARIPostProcessor implements IDecoderPostProcessor {
    
    /** name of extraAttribute variable from GribDecoder.py */
    private static final String FORECAST_INTERVAL = "forecastInterval";
    
    /** name of extraAttribute variable from GribDecoder.py */
    private static final String FORECAST_INTERVAL_UNIT = "forecastIntervalUnit";
    
    /** 30 minute record identifier **/
    private static final String REFZC = "REFZC";
    
    /** 1 hr record identifier **/
    private static final String REFZI = "REFZI";
    
    /** Year **/
    private static final String YR = "YR";
    
    /** ARI (Archival Recurrence Interval **/
    private static final String ARI = "ARI";
    
    /** 30 min comparison time **/
    private static final String MIN30 = "30";
    
    /** 1 hour comparison time **/
    private static final String HOUR1 = "1";
    
    /** ARI is precip in mm **/
    private static final String UNIT = "mm";
          
    @Override
    public GridRecord[] process(GridRecord record) throws GribException {

        // With ARI we assume the FORECAST_INTERVAL is in years.
        if (record.getExtraAttribute(FORECAST_INTERVAL) != null
                && record.getExtraAttribute(FORECAST_INTERVAL_UNIT) != null) {
            // just to be certain about year encoding
            int forecast_unit = (Integer) record
                    .getExtraAttribute(FORECAST_INTERVAL_UNIT);
            if (forecast_unit == 4) {
                int forecastYear = (Integer) record
                        .getExtraAttribute(FORECAST_INTERVAL);
                record = setParameterIdentifier(record, forecastYear);
                // Alter the forecast time in the record to 0 so pypies can
                // handle
                // it.
                record.getDataTime().setFcstTime(0);
            } else {
                throw new IllegalArgumentException(
                        "Forecast_Interval_Unit encoded for years should == 4, value: "
                                + forecast_unit);
            }

        } else {
            throw new IllegalArgumentException(
                    "No Forecast_Interval available for this grid: "
                            + record.getDataURI());
        }

        return new GridRecord[] { record };
    }

    /**
     * Create a new parameter abbreviation based on the forecast year and
     * interval period.
     * 
     * @param record
     * @param forecastYear
     * @return
     */
    private GridRecord setParameterIdentifier(GridRecord record,
            int forecastYear) {

        String paramAbbrev = record.getInfo().getParameter().getAbbreviation();
        StringBuilder sb = new StringBuilder(64);
        StringBuilder sb2 = new StringBuilder(256);
        sb.append(ARI);
        sb.append(Integer.toString(forecastYear));
        sb2.append(Integer.toString(forecastYear));
        sb2.append(" ");
        sb.append(YR);
        sb2.append(YR);
        sb2.append(" ");
        if (REFZC.equals(paramAbbrev)) {
            sb.append(MIN30);
            sb2.append("30 minute precip");
        } else if (REFZI.equals(paramAbbrev)) {
            sb.append(HOUR1);
            sb2.append("1 hour precip");
        } else {
            throw new IllegalArgumentException("ARI paramName " + paramAbbrev
                    + " is not a valid param for ARI postprocessing");
        }
       
        // Give it a new name/description/unit
        record.getParameter().setAbbreviation(sb.toString());
        record.getParameter().setName(sb2.toString());
        record.getParameter().setUnitString(UNIT);
        
        return record;
    }

}
