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

package com.raytheon.uf.common.dataplugin.grid;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;

/**
 * Path provider for storing gridded data to HDF5
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/24/09       1994        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class GridPathProvider extends DefaultPathProvider {

    private static final ThreadLocal<DecimalFormat> forecastHourFormat = new ThreadLocal<DecimalFormat>() {

        @Override
        protected DecimalFormat initialValue() {
            return new DecimalFormat("000");
        }

    };

    private static final int SECONDS_PER_HOUR = 3600;

    public static final String FORECAST_HR_TOKEN = "-FH-";

    private static GridPathProvider instance = new GridPathProvider();

    public static final List<String> STATIC_PARAMETERS;

    static {
        STATIC_PARAMETERS = new ArrayList<String>();
        STATIC_PARAMETERS.add("staticTopo");
        STATIC_PARAMETERS.add("staticXspacing");
        STATIC_PARAMETERS.add("staticYspacing");
        STATIC_PARAMETERS.add("staticCoriolis");
        STATIC_PARAMETERS.add("staticSpacing");
    }

    public static GridPathProvider getInstance() {
        return instance;
    }

    protected GridPathProvider() {

    }

    @Override
    public String getHDFFileName(String pluginName, IPersistable persistable) {

        if (persistable == null) {
            throw new IllegalArgumentException(
                    "Expected argument persistable is null");
        }

        if (!(persistable instanceof GridRecord)) {
            throw new IllegalArgumentException(
                    "Argument persistable is of wrong type. Expected "
                            + GridRecord.class + " but got "
                            + persistable.getClass());
        } else if (pluginName == null) {
            throw new IllegalArgumentException(
                    "Expected argument pluginName not set on object "
                            + persistable.toString());
        }

        GridRecord pdo = (GridRecord) persistable;
        StringBuffer sb = new StringBuffer(64);
        sb.append(pdo.getDatasetId());
        Date refTime = pdo.getDataTime().getRefTime();
        String refTimeString = null;
        refTimeString = fileNameFormat.get().format(refTime);
        sb.append(refTimeString);
        sb.append(FORECAST_HR_TOKEN);
        if (STATIC_PARAMETERS.contains(pdo.getParameter().getAbbreviation())) {
            sb.append("000");
        } else {
            long number = pdo.getDataTime().getFcstTime() / SECONDS_PER_HOUR;
            String numberString = forecastHourFormat.get().format(number);
            sb.append(numberString);
        }
        sb.append(".h5");

        return sb.toString();
    }

    public String formatTime(Date date) {
        String retVal = null;
        retVal = fileNameFormat.get().format(date);
        return retVal;
    }
}
