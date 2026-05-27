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
package com.raytheon.uf.viz.pdc;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.pdc.PDCConstants;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;

/**
 * Class to handle the setting of the preset string for PDC.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2008            mpduff      Initial creation
 * Sep 21, 2018 7379       mduff       Moved for PDC Refactor.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PointDataControlPresets {
    /**
     * Maximum length of the preset string
     */
    private static final int DOUBLE_REMK_LEN = 512;

    private static final String[] PC_OPTION_ARRAY = new String[] { "at", "ct",
            "da", "de", "dh", "ds", "dt", "eo", "ev", "fl", "fo", "ic", "id",
            "nm", "ns", "pe", "pp", "pr", "ps", "qm", "rs", "sb", "se", "sf",
            "sm", "sp", "sr", "st", "sv", "sz", "ti", "tm", "tp", "ts", "vl",
            "vo", "vt", "vv", "wf", "xs" };

    /**
     * Build the preset String from options.
     * 
     * @return The preset String
     */
    public static String buildPresetStringFromOptions() {
        StringBuilder buffer = new StringBuilder();
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        pcOptions.setHsaList(pdcManager.getServiceBackupInfo());

        for (int i = 0; i < PC_OPTION_ARRAY.length; i++) {
            switch (i) {
            case PDCConstants.INSTANTANEOUS_PRECIP_ACCUM_TIME:
                if (pcOptions
                        .getInstPrecipAccumTimeSelection() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i]).append("=");
                    buffer.append(pcOptions.getInstPrecipAccumTimeSelection())
                            .append(";");
                }
                break;

            case PDCConstants.SELECTED_TYPE_SOURCES:
                if (pcOptions.getTypeSourceChosenCount() > 0) {
                    /* Update the selected datasource information. */
                    buffer.append(PC_OPTION_ARRAY[i] + "=");
                    for (int j = 0; j < pcOptions
                            .getTypeSourceChosenCount(); j++) {
                        if (j > 0) {
                            buffer.append(",");
                        }

                        buffer.append(
                                pcOptions.getTypeSourceChosenList().get(j));
                    }

                    buffer.append(";");
                } else {
                    continue;
                }
                break;

            case PDCConstants.DATA_TYPE:
                if (pcOptions.getElementType() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getElementType() + ";");
                }
                break;

            case PDCConstants.DERIVE_STAGE_FLOW:
                if (pcOptions
                        .getDeriveStageFlow() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getDeriveStageFlow() + ";");
                }
                break;

            case PDCConstants.DUR_HOURS:
                if (pcOptions.getDurHours() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getDurHours() + ";");
                }
                break;

            case PDCConstants.FILTER_BY_DATA_SOURCE:
                if (pcOptions
                        .getFilterByDataSource() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getFilterByDataSource() + ";");
                }
                break;

            case PDCConstants.DATETIME:
                /*
                 * The processing of the Point Data Preset date time was
                 * modified to use a relative day and an absolute time. Get the
                 * current time in seconds.
                 */

                Calendar now = Calendar
                        .getInstance(TimeZone.getTimeZone("GMT"));

                /* Set the current hour, minute and second to zero. */
                now.set(Calendar.HOUR, 0);
                now.set(Calendar.HOUR_OF_DAY, 0);
                now.set(Calendar.MINUTE, 0);
                now.set(Calendar.SECOND, 0);
                Date nowDate = now.getTime();

                /*
                 * Recreate the current time with the modified hour, minute and
                 * second values.
                 */
                Calendar currentDayMidnight = Calendar
                        .getInstance(TimeZone.getTimeZone("GMT"));
                currentDayMidnight.set(Calendar.HOUR, 0);
                currentDayMidnight.set(Calendar.MINUTE, 0);
                currentDayMidnight.set(Calendar.SECOND, 0);
                currentDayMidnight.set(Calendar.HOUR_OF_DAY, 0);

                /* Retrieve the data time. */
                Calendar dataTime = Calendar
                        .getInstance(TimeZone.getTimeZone("GMT"));
                dataTime.setTime(pcOptions.getValidTime());

                /* Store the hour and minute of the data time. */
                int dataHours = dataTime.get(Calendar.HOUR_OF_DAY);
                int dataMinutes = dataTime.get(Calendar.MINUTE);

                /* Set the data hour, minute and second to zero. */
                dataTime.set(Calendar.HOUR, 0);
                dataTime.set(Calendar.MINUTE, 0);
                dataTime.set(Calendar.SECOND, 0);

                /*
                 * Subtract the modified data time from the modified current
                 * time to get the relative number of days.
                 */
                long relativeDaysInSeconds = (dataTime.getTimeInMillis()
                        - currentDayMidnight.getTimeInMillis()) / 1000;
                int relativeDays = (int) relativeDaysInSeconds
                        / HydroConstants.SECONDS_PER_DAY;

                boolean useSpecialDateTimeString = false;

                // if in Time STEP MODE and
                // the hour the user is trying to save as a preset is the
                // current top of hour, then don't save any time at all
                if (pcOptions.getQueryMode() == 1) {
                    long topOfHour = (now.getTimeInMillis()
                            / HydroConstants.MILLIS_PER_HOUR);
                    topOfHour *= HydroConstants.MILLIS_PER_HOUR;

                    if (pcOptions.getValidTime().getTime() == nowDate
                            .getTime()) {
                        useSpecialDateTimeString = true;
                    }
                }

                if (useSpecialDateTimeString) {
                    // This means the latest time
                    buffer.append(PC_OPTION_ARRAY[i] + "=;");
                } else {
                    // Use normal data time string
                    buffer.append(PC_OPTION_ARRAY[i] + "=" + relativeDays + ","
                            + dataHours + ":" + dataMinutes + ";");
                }
                break;
            case PDCConstants.ELEV_FILTER_OPERATION:
                if (pcOptions
                        .getElevFilterOperation() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getElevFilterOperation() + ";");
                }
                break;

            case PDCConstants.ELEV_FILTER_VALUE:
                if (pcOptions
                        .getElevFilterValue() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getElevFilterValue() + ";");
                }
                break;

            case PDCConstants.FLOOD_LEVEL:
                if (pcOptions.getFloodLevel() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getFloodLevel() + ";");
                }
                break;

            case PDCConstants.FCST_ONLY:
                if (pcOptions.getFcstptsOnly() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getFcstptsOnly() + ";");
                }
                break;

            case PDCConstants.SHOW_ICON:
                if (pcOptions.getIcon() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "=" + pcOptions.getIcon()
                            + ";");
                }
                break;

            case PDCConstants.SHOW_ID:
                if (pcOptions.getId() != PDCConstants.MISSING_VALUE) {
                    buffer.append(
                            PC_OPTION_ARRAY[i] + "=" + pcOptions.getId() + ";");
                }
                break;

            case PDCConstants.SHOW_NAME:
                if (pcOptions.getName() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "=" + pcOptions.getName()
                            + ";");
                }
                break;

            case PDCConstants.NUM_SOURCES:
                if (pcOptions
                        .getDataSourceChosenCount() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getDataSourceChosenCount() + ";");
                }
                break;

            case PDCConstants.SELECTED_ADHOC_PE:
                if (pcOptions.getSelectedAdHocElementString() != null) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getSelectedAdHocElementString() + ";");
                }
                break;

            case PDCConstants.PC_AND_PP:
                if (pcOptions.getPcAndpp() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getPcAndpp() + ";");
                }
                break;

            case PDCConstants.PRIMARY:
                if (pcOptions.getPrimary() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getPrimary() + ";");
                }
                break;

            case PDCConstants.PROCESS_SELECTED:
                if (pcOptions
                        .getProcessSelected() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getProcessSelected() + ";");
                }
                break;

            case PDCConstants.SELECTED_QUERY_MODE:
                buffer.append(PC_OPTION_ARRAY[i] + "="
                        + pcOptions.getQueryMode() + ";");
                break;

            case PDCConstants.SHOW_RIVER_STATUS:
                if (pcOptions.getRiverStatus() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getRiverStatus() + ";");
                }
                break;

            case PDCConstants.STAGE_BASIS:
                if (pcOptions.getStageBasis() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getStageBasis() + ";");
                }
                break;

            case PDCConstants.SHOW_ELEVATION:
                if (pcOptions.getElevation() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getElevation() + ";");
                }
                break;

            case PDCConstants.STREAM_STATION_FILTER:
                if (pcOptions
                        .getRiverStationFilter() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getRiverStationFilter() + ";");
                }
                break;

            case PDCConstants.SUPPRESS_MISSING:
                if (pcOptions
                        .getSupressMissing() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getSupressMissing() + ";");
                }
                break;

            case PDCConstants.SHOW_PARAM_CODE:
                if (pcOptions.getParamCode() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getParamCode() + ";");
                }
                break;

            case PDCConstants.SOURCE_LIST:
                if (pcOptions.getDataSourceChosenCount() > 0) {
                    /* Update the selected datasource information. */
                    buffer.append(PC_OPTION_ARRAY[i] + "=");
                    for (int j = 0; j < pcOptions
                            .getDataSourceChosenCount(); j++) {
                        if (pcOptions.getDataSourcesChosen()[j] != null) {
                            if (j > 0) {
                                buffer.append(",");
                            }

                            buffer.append(pcOptions.getDataSourcesChosen()[j]);
                        }
                    }
                    buffer.append(";");
                } else {
                    continue;
                }
                break;

            case PDCConstants.SELECTED_TIME_STEP_ELEMENT:
                if (pcOptions
                        .getTsDataElement() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getTsDataElement() + ";");
                }
                break;

            case PDCConstants.SERVICE_BACKUP:
                if (pcOptions.getFilterByHSA() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getFilterByHSA() + ";");
                }
                break;

            case PDCConstants.SHOW_TIME:
                if (pcOptions.getTime() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "=" + pcOptions.getTime()
                            + ";");
                }
                break;

            case PDCConstants.TIME_MODE:
                if (pcOptions.getTimeMode() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getTimeMode() + ";");
                }
                break;

            case PDCConstants.TIME_STEP_PRECIP_PE_FILTER:
                if (pcOptions
                        .getPrecipPeFilter() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getPrecipPeFilter() + ";");
                }
                break;

            case PDCConstants.FILTER_BY_TYPE_SOURCE:
                if (pcOptions
                        .getFilterByTypeSource() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getFilterByTypeSource() + ";");
                }
                break;

            case PDCConstants.SHOW_VALUE:
                if (pcOptions.getValue() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getValue() + ";");
                }
                break;

            case PDCConstants.VALUE_FILTER_OPERATION:
                if (pcOptions
                        .getValueFilterOperation() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getValueFilterOperation() + ";");
                }
                break;

            case PDCConstants.VALUE_TYPE:
                if (pcOptions.getValueType() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getValueType() + ";");
                }
                break;

            case PDCConstants.VALUE_FILTER_VALUE:
                if (pcOptions
                        .getValueFilterValue() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getValueFilterValue() + ";");
                }
                break;

            case PDCConstants.WFO_LIST:
                if (pcOptions.getHsaList().size() > 0) {
                    buffer.append(PC_OPTION_ARRAY[i] + "=");

                    for (int j = 0; j < pcOptions.getHsaList().size(); j++) {
                        if (j > 0) {
                            buffer.append(",");
                        }
                        buffer.append(pcOptions.getHsaList().get(j));
                    }
                    buffer.append(";");
                } else {
                    continue;
                }
                break;

            case PDCConstants.PE_SELECTION_INDEX:
                if (pcOptions.getPeSelection() != PDCConstants.MISSING_VALUE) {
                    buffer.append(PC_OPTION_ARRAY[i] + "="
                            + pcOptions.getPeSelection());
                }
                break;

            default:
                break;
            }
        }

        if (buffer.length() > DOUBLE_REMK_LEN) {
            // TODO - Log this error
            // fprintf ( stderr ,
            // "\nIn routine 'build_pointdata_preset_string':\n"
            // "The preset group is too long. It will be %d\n"
            // "characters long after the addition of '%s',\n"
            // "but is only allowed to be %d characters long.\n"
            // "The contents of the group before it exceeded\n"
            // "the limit was '%s'.\n" , preset_string_length ,
            // option_values_string , DOUBLE_REMK_LEN ,
            // preset_string ) ;
            return null;
        }

        // TODO - Log this message
        // printf("%s final preset_string = %s\n", header, preset_string);
        return buffer.toString();
    }

    public static PDCOptionData getPcOptions(Map<String, String[]> presetData) {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        pcOptions.reset();
        String[] valueArr = null;

        for (int i = 0; i < PointDataControlPresets.PC_OPTION_ARRAY.length; i++) {
            String key = PointDataControlPresets.PC_OPTION_ARRAY[i];
            if (presetData.get(key) != null) {
                valueArr = presetData.get(key);
                if (valueArr.length > 0) {
                    switch (i) {
                    case 0: // InstantaneousPrecipAccumTime,
                        int value = Integer.parseInt(valueArr[0]);
                        pcOptions.setInstPrecipAccumTimeSelection(value);
                        break;

                    case 1: // SelectedTypeSources
                        List<String> tsList = new ArrayList<>();

                        for (String s : valueArr) {
                            tsList.add(s);
                        }
                        pcOptions.setTypeSourceChosenList(tsList);
                        pcOptions.setTypeSourceChosenCount(tsList.size());
                        break;

                    case 2: // DataType
                        pcOptions.setElementType(Integer.parseInt(valueArr[0]));
                        break;

                    case 3: // DeriveStageFlow
                        pcOptions.setDeriveStageFlow(
                                Integer.parseInt(valueArr[0]));
                        break;

                    case 4: // DurHours
                        pcOptions.setDurHours(Integer.parseInt(valueArr[0]));
                        break;

                    case 5: // FilterByDataSource
                        pcOptions.setFilterByDataSource(
                                Integer.parseInt(valueArr[0]));
                        break;

                    case 6: // DateTime
                        if (valueArr.length == 2) {
                            int numberOfDays = Integer.parseInt(valueArr[0]);
                            if (valueArr[0].length() > 0) {
                                /*
                                 * Set the hours and minutes to those retrieved
                                 * from the presets group. Hours and minutes are
                                 * absolute while the day is relative.
                                 */
                                String[] timeArr = valueArr[1].split(":");
                                Calendar now = Calendar.getInstance(
                                        TimeZone.getTimeZone("GMT"));

                                now.add(Calendar.DAY_OF_MONTH, numberOfDays);
                                int hours = Integer.parseInt(timeArr[0]);
                                now.set(Calendar.HOUR_OF_DAY, hours);

                                int minutes;
                                if (timeArr.length > 1) {
                                    minutes = Integer.parseInt(timeArr[1]);
                                } else {
                                    minutes = 0;
                                }

                                now.set(Calendar.MINUTE, minutes);

                                pcOptions.setValidTime(now.getTime());
                            }
                        }
                        break;

                    case 7: // ElevFilterOperation
                        if (valueArr.length > 1) {
                            pcOptions.setElevFilterOperation(
                                    Integer.parseInt(valueArr[0]));

                            /* if the value is not a valid number (0-7) */
                            if ((pcOptions.getElevFilterOperation() < 0)
                                    || (pcOptions
                                            .getElevFilterOperation() >= 7)) {
                                pcOptions.setElevFilterOperation(0);
                            }
                        } else {
                            pcOptions.setElevFilterOperation(0);
                        }
                        break;

                    case 8: // ElevFilterValue
                        pcOptions.setElevFilterValue(
                                Double.parseDouble(valueArr[0]));
                        break;

                    case 9: // FloodLevel
                        pcOptions.setFloodLevel(Integer.parseInt(valueArr[0]));
                        break;

                    case 10: // FcstOnly
                        pcOptions.setFcstptsOnly(Integer.parseInt(valueArr[0]));
                        break;

                    case 11: // ShowIcon
                        pcOptions.setIcon(Integer.parseInt(valueArr[0]));
                        break;

                    case 12: // ShowId
                        pcOptions.setId(Integer.parseInt(valueArr[0]));
                        break;

                    case 13: // ShowName
                        pcOptions.setName(Integer.parseInt(valueArr[0]));
                        break;

                    case 14: // NumSources
                        pcOptions.setDataSourceChosenCount(
                                Integer.parseInt(valueArr[0]));
                        break;

                    case 15: // SelectedAdHocPE
                        String[] parts = valueArr[0].split(" ", 2);
                        pcOptions.setSelectedAdHocElementString(parts[0]);
                        break;

                    case 16: // PCandPP
                        pcOptions.setPcAndpp(Integer.parseInt(valueArr[0]));
                        break;

                    case 17: // Primary
                        pcOptions.setPrimary(Integer.parseInt(valueArr[0]));
                        break;

                    case 18: // ProcessSelected
                        pcOptions.setProcessSelected(
                                Integer.parseInt(valueArr[0]));
                        break;

                    case 19: // SelectedQueryMode
                        pcOptions.setQueryMode(Integer.parseInt(valueArr[0]));

                        /* 0 is Ad_hoc mode, 1 is time-step mode */
                        if (pcOptions.getQueryMode() != 1) {
                            pcOptions.setQueryMode(0);
                        }
                        break;

                    case 20: // ShowRiverStatus
                        pcOptions.setRiverStatus(Integer.parseInt(valueArr[0]));
                        break;

                    case 21: // StageBasis
                        pcOptions.setStageBasis(Integer.parseInt(valueArr[0]));
                        break;

                    case 22: // ShowElevation
                        pcOptions.setElevation(Integer.parseInt(valueArr[0]));
                        break;

                    case 23: // StreamStationFilter
                        pcOptions.setRiverStationFilter(
                                Integer.parseInt(valueArr[0]));
                        break;

                    case 24: // SuppressMissing
                        pcOptions.setSupressMissing(
                                Integer.parseInt(valueArr[0]));
                        break;

                    case 25: // ShowParamCode
                        pcOptions.setParamCode(Integer.parseInt(valueArr[0]));
                        break;

                    case 26: // SourceList
                        List<String> sourceList = new ArrayList<>();

                        for (String element : valueArr) {
                            sourceList.add(element);
                        }

                        pcOptions.setDataSourcesChosen(sourceList
                                .toArray(new String[sourceList.size()]));
                        pcOptions.setDataSourceChosenCount(sourceList.size());
                        break;

                    case 27: // SelectedTimeStepElement
                        pcOptions.setTsDataElement(
                                Integer.parseInt(valueArr[0]));
                        break;

                    case 28: // ServiceBackup
                        pcOptions.setFilterByHSA(Integer.parseInt(valueArr[0]));
                        break;

                    case 29: // SupressZero
                        int suppressZero = Integer.parseInt(valueArr[0]);

                        /*
                         * This suppressZero options has been replaced by value
                         * filtering. This case is for backward compatibility
                         * with older presets saved in the database. Sets the
                         * Filter to 'Value Not ='
                         */
                        if (suppressZero == 1) {
                            pcOptions.setValueFilterOperation(2);
                            pcOptions.setValueFilterValue(0.0);
                        }
                        break;

                    case 30: // ShowTime
                        pcOptions.setTime(Integer.parseInt(valueArr[0]));
                        break;

                    case 31: // TimeMode
                        pcOptions.setTimeMode(Integer.parseInt(valueArr[0]));
                        break;

                    case 32: // TimeStepPrecipPEFilter
                        pcOptions.setPrecipPeFilter(
                                Integer.parseInt(valueArr[0]));
                        break;

                    case 33: // FilterByTypeSource
                        pcOptions.setFilterByTypeSource(
                                Integer.parseInt(valueArr[0]));
                        break;

                    case 34: // ShowValue
                        pcOptions.setValue(Integer.parseInt(valueArr[0]));
                        break;

                    case 35: // ValueFilterOperation
                        if (valueArr[0].length() > 0) {
                            pcOptions.setValueFilterOperation(
                                    Integer.parseInt(valueArr[0]));

                            /* if the value is not a valid number (0-7) */
                            if ((pcOptions.getValueFilterOperation() < 0)
                                    || (pcOptions
                                            .getValueFilterOperation() >= 7)) {
                                pcOptions.setValueFilterOperation(0);
                            }
                        } else {
                            pcOptions.setValueFilterOperation(0);
                        }
                        break;

                    case 36: // ValueType
                        pcOptions.setValueType(Integer.parseInt(valueArr[0]));
                        break;

                    case 37: // ValueFilterValue
                        pcOptions.setValueFilterValue(
                                Double.parseDouble(valueArr[0]));
                        break;

                    case 38: // WFOlist
                        List<String> hsaList = new ArrayList<>();
                        if (valueArr.length > 0) {
                            for (String element : valueArr) {
                                hsaList.add(element);
                            }
                            pcOptions.setHsaList(hsaList);
                        } else {
                            hsaList.add(PDCConstants.ALL_AREAS);
                            pcOptions.setHsaList(hsaList);
                        }
                        break;

                    case PDCConstants.PE_SELECTION_INDEX:
                        pcOptions.setPeSelection(Integer.parseInt(valueArr[0]));

                    }
                }
            }
        }

        return pcOptions;
    }
}
