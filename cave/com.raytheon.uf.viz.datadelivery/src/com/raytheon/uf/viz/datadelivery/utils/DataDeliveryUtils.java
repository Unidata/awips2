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
package com.raytheon.uf.viz.datadelivery.utils;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.auth.resp.SuccessfulExecution;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest.RequestType;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedTime;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryConstants;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.site.SiteData;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionManagerRowData;
import com.raytheon.uf.viz.datadelivery.subscription.approve.SubscriptionApprovalRowData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Data Delivery UI Utilities
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 3, 2012             mpduff       Initial creation
 * Jun 07, 2012   687      lvenable     Refactor to consolidate code.
 * Jun 12, 2012   702      jpiatt       Added group name & code clean up.
 * Jul 25, 2012   955      djohnson     Use List instead of ArrayList, thread-safe access to DecimalFormat.
 * Aug 29, 2012   223      mpduff       Add cycles to the subscription details.
 * Oct 31, 2012  1278      mpduff       Moved spatial methods to SpatialUtils.
 * Nov 20, 2012 1286       djohnson     Add showYesNoMessage.
 * Dec 20, 2012 1413       bgonzale     Added PendingSubColumnNames.valueOfColumnName(String).
 * Jan 10, 2013 1420       mdpuff       Added getMaxLatency().
 * Jan 14, 2013 1286       djohnson     Fix IndexOutOfBounds exception from getMaxLatency.
 * Jan 22, 2013 1519       djohnson     Correct getMaxLatency() calculations.
 * Jan 30, 2013 1543       djohnson     Use List instead of ArrayList.
 * Apr 08, 2013 1826       djohnson     Add getDisplayData() method to subscription columns.
 * Apr 10, 2013 1891       djohnson     Add getDisplayData() method to pending subscription columns.
 * May 15, 2013 1040       mpduff       Using Set for office Ids.
 * May 20, 2013 2000       djohnson     Add message to inform the user changes were applied.
 * Jun 04, 2013  223       mpduff       Add point data stuff.
 * Jun 11, 2013 2064       mpduff       Don't output Parameter header if none exist.
 * Jun 12, 2013 2064       mpduff       Use SizeUtil to format data size output.
 * Jul 26, 2031 2232       mpduff       Removed sendAuthorizationRequest method.
 * Aug 30, 2013 2288       bgonzale     Added latency to details display.
 * Sep 30, 2013 1797       dhladky      Time GriddedTime separation
 * Oct 11, 2013 2386       mpduff       Refactor DD Front end.
 * Nov 07, 2013 2291       skorolev     Added showText() method for messages with many lines.
 * Feb 11, 2014 2771       bgonzale     Added Data Delivery ID, getter, and retrieval method.
 * Apr 2,  2014 2974       dhladky      DD ID added to list for dropdowns in DD.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DataDeliveryUtils {

    /**
     * Default latency applied to hourly datasets.
     */
    public static final int GRIDDED_HOURLY_DATASET_LATENCY_IN_MINUTES = 40;

    /**
     * Default latency applied non-hourly datasets.
     */
    public static final int GRIDDED_NON_HOURLY_DATASET_LATENCY_IN_MINUTES = 75;

    /**
     * Required default latency value for point data.
     */
    public static final int POINT_DATASET_DEFAULT_LATENCY_IN_MINUTES = 15;

    private static final int UNSET = -1;

    /** Decimal format */
    private final static ThreadLocal<DecimalFormat> format = new ThreadLocal<DecimalFormat>() {

        @Override
        protected DecimalFormat initialValue() {
            DecimalFormat dTemp = new DecimalFormat("###.##");
            return dTemp;
        }
    };

    public static final String UNABLE_TO_RETRIEVE_PENDING_SUBSCRIPTIONS = "Unable to retrieve pending subscriptions!";

    private static final String dataDeliveryId = retrieveDataDeliveryId();

    /**
     * TABLE_TYPE enumeration.
     */
    public static enum TABLE_TYPE {
        /** Notification Table */
        NOTIFICATION,
        /** Subscription Table */
        SUBSCRIPTION,
        /** Browser Table */
        BROWSER,
        /** Pending Table */
        PENDING_SUBSCRIPTION;
    }

    /** Enumeration to use for notification table columns */
    public static enum NotifColumnNames {
        /** Column Time */
        TIME("Time", null),
        /** Column Priority */
        PRIORITY("Priority", null),
        /** Column Category */
        CATEGORY("Category", null),
        /** Column User */
        USER("User", "User performing the action"),
        /** Column Message */
        MESSAGE("Message", "Action taken");

        /** Column name */
        private final String columnName;

        /** Tool tip text */
        private String toolTip;

        private NotifColumnNames(String columnName, String toolTip) {
            this.columnName = columnName;
        }

        /**
         * Get column name.
         * 
         * @return Column Name
         */
        public String getColumnName() {
            return columnName;
        }

        /**
         * Get the tool tip
         * 
         * @return The tool tip.
         */
        public String getToolTip() {
            return toolTip;
        }

        @Override
        public String toString() {
            return columnName;
        }
    }

    /** Enumeration to use for subscription table columns */
    public static enum SubColumnNames {
        /** Column Name */
        NAME("Name", null) {
            @Override
            public String getDisplayData(SubscriptionManagerRowData rd) {
                return rd.getName();
            }
        },
        /** Column Owner */
        OWNER("Owner", null) {
            @Override
            public String getDisplayData(SubscriptionManagerRowData rd) {
                return rd.getOwner();
            }
        },
        /** Column Status */
        STATUS("Status", null) {
            @Override
            public String getDisplayData(SubscriptionManagerRowData rd) {
                return rd.getStatus();
            }
        },
        /** Column Priority */
        PRIORITY("Priority", null) {
            @Override
            public String getDisplayData(SubscriptionManagerRowData rd) {
                return String.valueOf(rd.getPriority());
            }
        },
        /** Column Description */
        DESCRIPTION("Description", null) {
            @Override
            public String getDisplayData(SubscriptionManagerRowData rd) {
                return rd.getDescription();
            }
        },
        /** Column Subscription Start */
        SUBSCRIPTION_START("Subscription Start", "Date subscription will begin") {
            @Override
            public String getDisplayData(SubscriptionManagerRowData rd) {
                Date date = rd.getSubscriptionStart();
                if (date != null) {
                    return formatMMddyyyyHH(date);
                }
                return null;
            }
        },
        /** Column Subscription Expiration */
        SUBSCRIPTION_EXPIRATION("Subscription Expiration",
                "Date subscription will expire") {
            @Override
            public String getDisplayData(SubscriptionManagerRowData rd) {
                Date date = rd.getSubscriptionEnd();
                if (date == null) {
                    return "No Expiration";
                } else {
                    return formatMMddyyyyHH(date);
                }
            }
        },
        /** Column Active Period Start */
        ACTIVE_START("Active Period Start", null) {
            @Override
            public String getDisplayData(SubscriptionManagerRowData rd) {
                Date date = rd.getActiveStart();
                if (date != null) {
                    return formatMMddHH(date);
                }
                return null;
            }
        },
        /** Column Active Period Start */
        ACTIVE_END("Active Period End", null) {
            @Override
            public String getDisplayData(SubscriptionManagerRowData rd) {
                Date date = rd.getActiveEnd();
                if (date != null) {
                    return formatMMddHH(date);
                }
                return null;
            }
        },
        /** Column Office Id */
        OFFICE_ID("Office ID", null) {
            @Override
            public String getDisplayData(SubscriptionManagerRowData rd) {
                return rd.getOfficeIdsDisplayList();
            }
        },
        /** Column Full Dataset */
        FULL_DATA_SET("Full Dataset", null) {
            @Override
            public String getDisplayData(SubscriptionManagerRowData rd) {
                return rd.getFullDataSet().toString();
            }
        },
        /** Column Data Size */
        DATA_SIZE("Data Size", null) {
            @Override
            public String getDisplayData(SubscriptionManagerRowData rd) {
                return SizeUtil.prettyKiloByteSize(rd.getDataSetSize());
            }
        },
        /** Column Group Name */
        GROUP_NAME("Group Name", null) {
            @Override
            public String getDisplayData(SubscriptionManagerRowData rd) {
                return rd.getGroupName();
            }
        };

        /** Column name */
        private final String columnName;

        /** Tool tip text */
        private final String toolTip;

        private SubColumnNames(String columnName, String toolTip) {
            this.columnName = columnName;
            this.toolTip = toolTip;
        }

        /**
         * Get column name.
         * 
         * @return Column Name
         */
        public String getColumnName() {
            return columnName;
        }

        /**
         * Get the tool tip
         * 
         * @return The tool tip.
         */
        public String getToolTip() {
            return toolTip;
        }

        @Override
        public String toString() {
            return columnName;
        }

        public abstract String getDisplayData(SubscriptionManagerRowData rd);

        /**
         * @param name
         * @return
         */
        public static SubColumnNames fromDisplayString(String name) {
            for (SubColumnNames subColumnName : values()) {
                if (subColumnName.toString().equals(name)) {
                    return subColumnName;
                }
            }
            throw new IllegalArgumentException(
                    "Unable to find enumeration with display string [" + name
                            + "]");
        }

        private static String formatMMddyyyyHH(Date date) {
            return formatDate(date, "MM/dd/yyyy HH");
        }

        private static String formatMMddHH(Date date) {
            return formatDate(date, "MM/dd HH");
        }

        private static String formatDate(Date date, String format) {
            SimpleDateFormat sdf2 = new SimpleDateFormat(format);
            sdf2.setTimeZone(TimeZone.getTimeZone("GMT"));
            return sdf2.format(date) + "Z";
        }
    }

    /**
     * BrowserColumnNames enumeration.
     */
    public static enum BrowserColumnNames {
        /** Dataset name */
        NAME("Name", "Owner of the subscription"),
        /** Subscription name */
        SUBSCRIPTION("Subscription", "Name of the subscription"),
        /** Provider */
        PROVIDER("Provider", "Provider of the subscription data");

        /** Column name */
        private final String columnName;

        /** Tool tip text */
        private final String toolTip;

        private BrowserColumnNames(String columnName, String toolTip) {
            this.columnName = columnName;
            this.toolTip = toolTip;
        }

        /**
         * Get column name.
         * 
         * @return Column Name
         */
        public String getColumnName() {
            return columnName;
        }

        /**
         * Get the tool tip
         * 
         * @return The tool tip.
         */
        public String getToolTip() {
            return toolTip;
        }
    }

    /**
     * Pending Subscription column names enumeration
     */
    public static enum PendingSubColumnNames {
        /** Subscription name */
        NAME("Subscription Name", null) {
            @Override
            public String getDisplayData(SubscriptionApprovalRowData rd) {
                return rd.getSubName();
            }
        },
        /** Requested Action */
        ACTION("Action", null) {
            @Override
            public String getDisplayData(SubscriptionApprovalRowData rd) {
                return rd.getAction();
            }
        },
        /** Subscription owner */
        OWNER("Owner", null) {
            @Override
            public String getDisplayData(SubscriptionApprovalRowData rd) {
                return rd.getOwner();
            }
        },
        /** Change ID */
        CHANGE_ID("Requested Change", null) {
            @Override
            public String getDisplayData(SubscriptionApprovalRowData rd) {
                return rd.getChangeOwner();
            }
        },
        /** Office ID */
        OFFICE("Office Id", null) {
            @Override
            public String getDisplayData(SubscriptionApprovalRowData rd) {
                return rd.getOfficeIdsAsList();
            }
        },
        /** Description */
        DESCRIPTION("Description", null) {
            @Override
            public String getDisplayData(SubscriptionApprovalRowData rd) {
                return rd.getDescription();
            }
        };

        private final String columnName;

        private String toolTip;

        private PendingSubColumnNames(String columnName, String toolTip) {
            this.columnName = columnName;
        }

        /**
         * Get column name.
         * 
         * @return Column Name
         */
        public String getColumnName() {
            return columnName;
        }

        /**
         * Get the tool tip
         * 
         * @return The tool tip.
         */
        public String getToolTip() {
            return toolTip;
        }

        /**
         * Find the PendingSubColumnNames value representing the given
         * columnName.
         * 
         * @param columnName
         * @return the corresponding PendingSubColumnNames enum value.
         */
        public static PendingSubColumnNames valueOfColumnName(String columnName) {
            for (PendingSubColumnNames val : PendingSubColumnNames.values()) {
                if (val.columnName.equals(columnName)) {
                    return val;
                }
            }
            // default to NAME.
            return NAME;
        }

        /**
         * Get the data this column displays for the row.
         * 
         * @param rd
         *            the row data
         * @return the display value
         */
        public abstract String getDisplayData(SubscriptionApprovalRowData rd);
    }

    /**
     * Get the column titles (column names).
     * 
     * @param tableType
     *            Table type.
     * @return String array of column titles.
     */
    public static String[] getColumnTitles(TABLE_TYPE tableType) {
        String[] colTitles = null;

        if (tableType == TABLE_TYPE.SUBSCRIPTION) {
            colTitles = new String[SubColumnNames.values().length];
            for (int i = 0; i < SubColumnNames.values().length; i++) {
                colTitles[i] = SubColumnNames.values()[i].getColumnName();
            }
        } else if (tableType == TABLE_TYPE.NOTIFICATION) {
            colTitles = new String[NotifColumnNames.values().length];
            for (int i = 0; i < NotifColumnNames.values().length; i++) {
                colTitles[i] = NotifColumnNames.values()[i].getColumnName();
            }
        } else if (tableType == TABLE_TYPE.BROWSER) {
            colTitles = new String[BrowserColumnNames.values().length];
            for (int i = 0; i < BrowserColumnNames.values().length; i++) {
                colTitles[i] = BrowserColumnNames.values()[i].getColumnName();
            }
        } else if (tableType == TABLE_TYPE.PENDING_SUBSCRIPTION) {
            colTitles = new String[PendingSubColumnNames.values().length];
            for (int i = 0; i < PendingSubColumnNames.values().length; i++) {
                colTitles[i] = PendingSubColumnNames.values()[i]
                        .getColumnName();
            }
        }

        return colTitles;
    }

    /**
     * Get the column tool tips.
     * 
     * @param tableType
     *            Table type.
     * @return String array of tool tips.
     */
    public static Map<String, String> getColumnToolTipsMap(TABLE_TYPE tableType) {
        HashMap<String, String> toolTipMap = new HashMap<String, String>();

        if (tableType == TABLE_TYPE.SUBSCRIPTION) {
            for (SubColumnNames scn : SubColumnNames.values()) {
                toolTipMap.put(scn.getColumnName(), scn.getToolTip());
            }
        } else if (tableType == TABLE_TYPE.NOTIFICATION) {
            for (NotifColumnNames ncn : NotifColumnNames.values()) {
                toolTipMap.put(ncn.getColumnName(), ncn.getToolTip());
            }
        } else if (tableType == TABLE_TYPE.BROWSER) {
            for (BrowserColumnNames bcn : BrowserColumnNames.values()) {
                toolTipMap.put(bcn.getColumnName(), bcn.getToolTip());
            }
        } else if (tableType == TABLE_TYPE.PENDING_SUBSCRIPTION) {

            for (PendingSubColumnNames pcn : PendingSubColumnNames.values()) {
                toolTipMap.put(pcn.getColumnName(), pcn.getToolTip());
            }
        }

        return toolTipMap;
    }

    /**
     * Show a MessageBox.
     * 
     * @param shell
     *            The parent shell
     * @param style
     *            The message box style bits
     * @param messageTitle
     *            The message box title
     * @param messageText
     *            The message box message
     * @return The selected return value
     */
    public static int showMessage(Shell shell, int style, String messageTitle,
            String messageText) {
        MessageBox messageDialog = new MessageBox(shell, style);
        messageDialog.setText(messageTitle);
        messageDialog.setMessage(messageText);
        return messageDialog.open();
    }

    /**
     * Show a Yes/No MessageBox.
     * 
     * @param shell
     *            the shell reference
     * @param title
     *            the title
     * @param message
     *            the message
     */
    public static int showYesNoMessage(Shell shell, String title, String message) {
        return showMessage(shell, SWT.YES | SWT.NO, title, message);
    }

    /**
     * Inform the user their changes were applied.
     * 
     * @param shell
     *            the shell reference
     */
    public static void showChangesWereAppliedMessage(Shell shell) {
        showMessage(shell, SWT.OK, "Changes Applied",
                "The changes were successfully applied.");
    }

    /**
     * Show message with long list of lines.
     * 
     * @param shell
     * @param messageTitle
     * @param messageText
     */
    public static void showText(Shell shell, String messageTitle,
            String messageText) {
        TextMessageDlg textMsgDlgdlg = new TextMessageDlg(shell, messageTitle,
                messageText);
        textMsgDlgdlg.open();
    }

    /**
     * Provides the text for the subscription details dialog
     * 
     * @param sub
     *            The subscription object
     * 
     * @return The formated details string
     */
    public static String formatDetails(Subscription<Time, Coverage> sub) {
        final String newline = StringUtil.NEWLINE;
        final String space = " ";
        final String comma = ", ";

        StringBuilder fmtStr = new StringBuilder();

        fmtStr.append("Subscription Name: ").append(sub.getName())
                .append(newline);
        fmtStr.append("Group Name: ").append(sub.getGroupName())
                .append(newline);
        fmtStr.append("Dataset Name: ").append(sub.getDataSetName())
                .append(newline);
        fmtStr.append("Dataset Size: ")
                .append(SizeUtil.prettyKiloByteSize(sub.getDataSetSize()))
                .append(newline);
        fmtStr.append("Provider: ").append(sub.getProvider()).append(newline);
        fmtStr.append("Office IDs: ")
                .append(getFormatedList(sub.getOfficeIDs())).append(newline);
        fmtStr.append("Priority: ")
                .append(sub.getPriority().getPriorityValue()).append(newline);
        fmtStr.append("Network: ").append(sub.getRoute()).append(newline);
        fmtStr.append("Latency Minutes: ").append(sub.getLatencyInMinutes())
                .append(newline);

        fmtStr.append("Coverage: ").append(newline);
        final Coverage coverage = sub.getCoverage();
        if (coverage.getProjection() != null) {
            fmtStr.append("------ Projection: ")
                    .append(coverage.getProjection()).append(newline);
        } else {
            fmtStr.append("------ Projection: ").append(newline);
        }
        final DecimalFormat decimalFormat = format.get();
        final Coordinate requestLowerRight = coverage.getRequestLowerRight();
        final Coordinate requestUpperLeft = coverage.getRequestUpperLeft();
        if (requestLowerRight == null || requestUpperLeft == null) {
            fmtStr.append("------ Upper Left:  ")
                    .append(decimalFormat.format(coverage.getUpperLeft().x))
                    .append(comma)
                    .append((decimalFormat.format(coverage.getUpperLeft().y)))
                    .append(newline);
            fmtStr.append("------ Lower Right: ")
                    .append(decimalFormat.format(coverage.getLowerRight().x))
                    .append(comma)
                    .append(decimalFormat.format(coverage.getLowerRight().y))
                    .append(newline);
        } else {
            fmtStr.append("------ Upper Left:  ")
                    .append(decimalFormat.format(requestUpperLeft.x))
                    .append(comma)
                    .append(decimalFormat.format(requestUpperLeft.y))
                    .append(newline);
            fmtStr.append("------ Lower Right: ")
                    .append(decimalFormat.format(requestLowerRight.x))
                    .append(comma)
                    .append(decimalFormat.format(requestLowerRight.y))
                    .append(newline);
        }

        final Time subTime = sub.getTime();

        if (subTime instanceof GriddedTime) {

            GriddedTime gtime = (GriddedTime) subTime;

            final List<String> fcstHours = gtime.getFcstHours();
            if (!CollectionUtil.isNullOrEmpty(fcstHours)) {
                fmtStr.append("Forecast Hours: ").append(newline);
                fmtStr.append("------ ");
                for (int idx : gtime.getSelectedTimeIndices()) {
                    fmtStr.append(fcstHours.get(idx)).append(space);
                }

                fmtStr.append(newline);
            }
            final List<Integer> cycles = gtime.getCycleTimes();
            if (cycles != null && !cycles.isEmpty()) {
                fmtStr.append("Cycles: ").append(newline);
                fmtStr.append("------ ");
                for (int cycle : cycles) {
                    fmtStr.append(cycle).append(space);
                }

                fmtStr.append(newline);
            }
        } else if (subTime instanceof PointTime) {
            // Nothing done for Point at this time
        }

        List<Parameter> parmArray = sub.getParameter();
        if (!CollectionUtil.isNullOrEmpty(parmArray)) {
            fmtStr.append("Parameters:").append(newline);
            for (Parameter p : parmArray) {
                fmtStr.append("------ Name: ").append(p.getName())
                        .append(newline);
                fmtStr.append("------ Provider Name: ")
                        .append(p.getProviderName()).append(newline);
                fmtStr.append("------ Definition: ").append(p.getDefinition())
                        .append(newline);
                fmtStr.append("------ Data Type: ").append(p.getDataType())
                        .append(newline);

                fmtStr.append("------ Level Type: ").append(newline);
                for (DataLevelType dlt : p.getLevelType()) {
                    fmtStr.append("------------ Type: ").append(dlt.getType())
                            .append(newline);
                    fmtStr.append("------------ ID: ").append(dlt.getId())
                            .append(newline);
                    if (dlt.getUnit() != null) {
                        fmtStr.append("------------ Unit: ")
                                .append(dlt.getUnit()).append(newline);
                    } else {
                        fmtStr.append("------------ Unit: ").append(newline);
                    }
                }
            }
        }

        return fmtStr.toString();
    }

    /**
     * Get a formatted list.
     * 
     * @param list
     *            List of items
     * @return a formatted list as a String
     */
    public static String getFormatedList(Set<String> list) {
        return StringUtil.getIndentedList(list, "            ");
    }

    /**
     * Get the maximum latency for the provided subscription. Calculated as the
     * maximum cyclic difference.
     * 
     * @param subscription
     *            The subscription
     * @return the maximum latency in minutes
     */
    public static int getMaxLatency(Subscription<Time, Coverage> subscription) {
        if (subscription.getDataSetType() == DataType.POINT) {
            return subscription.getLatencyInMinutes();
        } else if (subscription.getDataSetType() == DataType.GRID) {
            return getMaxLatency(((GriddedTime) subscription.getTime())
                    .getCycleTimes());
        } else {
            throw new IllegalArgumentException("Invalid Data Type: "
                    + subscription.getDataSetType().name());
        }
    }

    /**
     * Get the maximum latency for the provided cycles. Calculated as the
     * maximum cyclic difference.
     * 
     * @param cycles
     *            The list of cycles
     * @return the maximum latency in minutes
     */
    public static int getMaxLatency(List<Integer> cycles) {
        Collections.sort(cycles);
        int maximumTimeBetweenCycles = UNSET;

        final int size = cycles.size();
        for (int i = 0; i < size; i++) {
            final int nextIndex = i + 1;
            if (nextIndex < size) {
                int tempMax = cycles.get(nextIndex) - cycles.get(i);
                maximumTimeBetweenCycles = Math.max(maximumTimeBetweenCycles,
                        tempMax);
            }
        }

        // If there was only one cycle, then default to the number of minutes in
        // the day
        if (maximumTimeBetweenCycles == UNSET) {
            maximumTimeBetweenCycles = TimeUtil.HOURS_PER_DAY;
        }

        return maximumTimeBetweenCycles * TimeUtil.MINUTES_PER_HOUR;
    }

    /**
     * Get the maximum latency for the provided dataSet. Calculated as the
     * maximum cyclic difference.
     * 
     * @param dataSet
     *            the dataset
     * @return the maximum latency in minutes
     */
    public static int getMaxLatency(GriddedDataSet dataSet) {
        return getMaxLatency(new ArrayList<Integer>(dataSet.getCycles()));
    }

    public static String getDataDeliveryId() {
        return dataDeliveryId;
    }

    private static String retrieveDataDeliveryId() {
        IBandwidthRequest request = new IBandwidthRequest();
        request.setRequestType(RequestType.GET_DATADELIVERY_ID);
        try {
            SuccessfulExecution response = (SuccessfulExecution) RequestRouter
                    .route(request, DataDeliveryConstants.DATA_DELIVERY_SERVER);
            return (String) response.getResponse();
        } catch (Exception e) {
            throw new RuntimeException(
                    "Unable to retrieve Data Delivery ID from EDEX.", e);
        }
    }
    
    /**
     * Gets the DD id containing site List.
     * @return
     */
    public static List<String> getDataDeliverySiteList() {
        
        Map<String, SiteData> siteData = SiteMap.getInstance().getSiteData();
        Set<String> sites = siteData.keySet();
        List<String> siteList = new ArrayList<String>(sites);
        String DDid = DataDeliveryUtils.getDataDeliveryId();
        if (!siteList.contains(DDid)) {
            siteList.add(DDid);
            Collections.sort(siteList);
        }
        
        return siteList;
    }
}
