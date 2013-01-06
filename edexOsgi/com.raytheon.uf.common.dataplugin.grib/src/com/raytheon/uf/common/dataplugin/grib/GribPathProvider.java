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

package com.raytheon.uf.common.dataplugin.grib;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;

/**
 * Deprecated, use grid
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
@Deprecated
public class GribPathProvider extends DefaultPathProvider {

    private static final DecimalFormat forecastHourFormat = new DecimalFormat(
            "000");

    private static final int SECONDS_PER_HOUR = 3600;

    public static final String FORECAST_HR_TOKEN = "-FH-";

    private static GribPathProvider instance = new GribPathProvider();

    public static final List<String> STATIC_PARAMETERS;

    static {
        STATIC_PARAMETERS = new ArrayList<String>();
        STATIC_PARAMETERS.add("staticTopo");
        STATIC_PARAMETERS.add("staticXspacing");
        STATIC_PARAMETERS.add("staticYspacing");
        STATIC_PARAMETERS.add("staticCoriolis");
        STATIC_PARAMETERS.add("staticSpacing");
    }

    public static GribPathProvider getInstance() {
        return instance;
    }

    protected GribPathProvider() {

    }

    @Override
    public String getHDFFileName(String pluginName, IPersistable persistable) {

        if (persistable == null) {
            throw new IllegalArgumentException(
                    "Expected argument persistable is null");
        }

        if (!(persistable instanceof GribRecord)) {
            throw new IllegalArgumentException(
                    "Argument persistable is of wrong type. Expected "
                            + GribRecord.class + " but got "
                            + persistable.getClass());
        } else if (pluginName == null) {
            throw new IllegalArgumentException(
                    "Expected argument pluginName not set on object "
                            + persistable.toString());
        }

        GribRecord pdo = (GribRecord) persistable;
        StringBuffer sb = new StringBuffer(64);
        sb.append(pdo.getModelInfo().getModelName());
        Date refTime = pdo.getDataTime().getRefTime();
        sb.append(fileNameFormat.get().format(refTime));
        sb.append(FORECAST_HR_TOKEN);
        if (STATIC_PARAMETERS.contains(pdo.getModelInfo()
                .getParameterAbbreviation())) {
            sb.append("000");
        } else {
            long number = pdo.getDataTime().getFcstTime() / SECONDS_PER_HOUR;
            String numberString = null;
            synchronized (forecastHourFormat) {
                numberString = forecastHourFormat.format(number);
            }
            sb.append(numberString);
        }
        sb.append(".h5");

        return sb.toString();
    }

    public String formatTime(Date date) {
        return fileNameFormat.get().format(date);
    }

    @Override
    public String getHDFPath(String pluginName, IPersistable persistable) {
        return super.getHDFPath(GridConstants.GRID, persistable);
    }

    @Override
    public List<String> getKeyNames(String pluginName) {
        List<String> keys = super.getKeyNames(GridConstants.GRID);
        List<String> newKeys = new ArrayList<String>(keys.size());
        for (String key : keys) {
            if (key.equals(GridConstants.DATASET_ID)) {
                newKeys.add("modelInfo.modelName");
            } else if (key.equals(GridConstants.MASTER_LEVEL_NAME)) {
                newKeys.add("modelInfo.level.masterLevel.name");
            } else if (key.equals(GridConstants.PARAMETER_ABBREVIATION)) {
                newKeys.add("modelInfo.parameterAbbreviation");
            } else if (key.equals(GridConstants.LEVEL_ONE)) {
                newKeys.add("modelInfo.level.levelonevalue");
            } else if (key.equals(GridConstants.LEVEL_TWO)) {
                newKeys.add("modelInfo.level.leveltwovalue");
            } else {
                newKeys.add(key);
            }
        }
        return newKeys;
    }

    public String getGroup(GribRecord record) {
        StringBuilder datauri = new StringBuilder("/grid/");
        datauri.append(record.getDataTime().toString().replace(" ", "_"));
        datauri.append("/");
        datauri.append(record.getModelInfo().getModelName());
        // secondaryid
        datauri.append("/");
        if (record.getGridVersion() != 0) {
            datauri.append("Version");
            datauri.append(record.getGridVersion());
        } else {
            datauri.append("null");
        }
        datauri.append("/");
        if (record.getModelInfo().getPerturbationNumber() != null) {
            switch (record.getModelInfo().getPerturbationNumber()) {
            case 1:
                datauri.append("ctl1");
                break;
            case 2:
                datauri.append("ctl2");
                break;
            case 3:
                datauri.append("n1");
                break;
            case 4:
                datauri.append("p1");
                break;
            case 5:
                datauri.append("n2");
                break;
            case 6:
                datauri.append("p2");
                break;
            case 7:
                datauri.append("n3");
                break;
            case 8:
                datauri.append("p3");
                break;
            case 9:
                datauri.append("n4");
                break;
            case 10:
                datauri.append("p4");
                break;
            case 11:
                datauri.append("n5");
                break;
            case 12:
                datauri.append("p5");
                break;
            default:
                datauri.append("null");
            }
        } else {
            datauri.append("null");
        }
        datauri.append("/");
        datauri.append(record.getModelInfo().getLocation().getId());
        datauri.append("/");
        datauri.append(record.getModelInfo().getParameterAbbreviation());
        datauri.append("/");
        datauri.append(record.getModelInfo().getLevelName());
        datauri.append("/");
        datauri.append(record.getModelInfo().getLevelOneValue());
        datauri.append("/");
        datauri.append(record.getModelInfo().getLevelTwoValue());
        return datauri.toString();
    }
}
