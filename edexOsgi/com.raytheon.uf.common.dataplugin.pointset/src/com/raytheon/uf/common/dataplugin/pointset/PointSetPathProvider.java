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

package com.raytheon.uf.common.dataplugin.pointset;

import java.text.DecimalFormat;
import java.util.Date;

import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Path provider for storing pointset data to HDF5
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 31, 2016 5584        nabowle     Initial Creation
 *
 * </pre>
 *
 * @author nabowle
 */
public class PointSetPathProvider extends DefaultPathProvider {

    private static final ThreadLocal<DecimalFormat> forecastHourFormat = new ThreadLocal<DecimalFormat>() {
        @Override
        protected DecimalFormat initialValue() {
            return new DecimalFormat("000");
        }
    };

    public static final String FORECAST_HR_TOKEN = "-FH-";

    private static PointSetPathProvider instance = new PointSetPathProvider();

    public static PointSetPathProvider getInstance() {
        return instance;
    }

    protected PointSetPathProvider() {
        super();
    }

    @Override
    public String getHDFFileName(String pluginName, IPersistable persistable) {

        if (persistable == null) {
            throw new IllegalArgumentException(
                    "Expected argument persistable is null");
        }

        if (!(persistable instanceof PointSetRecord)) {
            throw new IllegalArgumentException(
                    "Argument persistable is of wrong type. Expected "
                            + PointSetRecord.class + " but got "
                            + persistable.getClass());
        } else if (pluginName == null) {
            throw new IllegalArgumentException(
                    "Expected argument pluginName not set on object "
                            + persistable.toString());
        }

        PointSetRecord pdo = (PointSetRecord) persistable;
        StringBuilder sb = new StringBuilder(64);
        sb.append(pdo.getDatasetId());
        Date refTime = pdo.getDataTime().getRefTime();
        String refTimeString = null;
        refTimeString = fileNameFormat.get().format(refTime);
        sb.append(refTimeString);
        if (pdo.getDataTime().getUtilityFlags().contains(FLAG.FCST_USED)) {
            sb.append(FORECAST_HR_TOKEN);
            long number = pdo.getDataTime().getFcstTime()
                    / TimeUtil.SECONDS_PER_HOUR;
            String numberString = forecastHourFormat.get().format(number);
            sb.append(numberString);
        }
        sb.append(".h5");

        return sb.toString();
    }
}
