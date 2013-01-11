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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.util.CollectionUtil;
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
 * Nov 20, 2012 1286       djohnson    Add showYesNoMessage.
 * Dec 20, 2012 1413       bgonzale    Added PendingSubColumnNames.valueOfColumnName(String).
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DataDeliveryUtils {

    /** Decimal format */
    private final static ThreadLocal<DecimalFormat> format = new ThreadLocal<DecimalFormat>() {

        @Override
        protected DecimalFormat initialValue() {
            DecimalFormat dTemp = new DecimalFormat("###.##");
            return dTemp;
        }
    };

    public static final String UNABLE_TO_RETRIEVE_PENDING_SUBSCRIPTIONS = "Unable to retrieve pending subscriptions!";

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
        NAME("Name", null),
        /** Column Owner */
        OWNER("Owner", null),
        /** Column Status */
        STATUS("Status", null),
        /** Column Priority */
        PRIORITY("Priority", null),
        /** Column Description */
        DESCRIPTION("Description", null),
        /** Column Subscription Start */
        SUBSCRIPTION_START("Subscription Start", "Date subscription will begin"),
        /** Column Subscription Expiration */
        SUBSCRIPTION_EXPIRATION("Subscription Expiration",
                "Date subscription will expire"),
        /** Column Active Period Start */
        ACTIVE_START("Active Period Start", null),
        /** Column Active Period Start */
        ACTIVE_END("Active Period End", null),
        /** Column Delivery */
        DELIVERY("Delivery/Notify", "Delivery or Notify indicator"),
        /** Column Office Id */
        OFFICE_ID("Office ID", null),
        /** Column Full Dataset */
        FULL_DATA_SET("Full Dataset", null),
        /** Column Data Size */
        DATA_SIZE("Data Size", null),
        /** Column Group Name */
        GROUP_NAME("Group Name", null);

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
        NAME("Subscription Name", null),
        /** Requested Action */
        ACTION("Action", null),
        /** Subscription owner */
        OWNER("Owner", null),
        /** Change ID */
        CHANGE_ID("Requested Change", null),
        /** Office ID */
        OFFICE("Office Id", null),
        /** Description */
        DESCRIPTION("Description", null);

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
    public static HashMap<String, String> getColumnToolTipsMap(
            TABLE_TYPE tableType) {
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
     * Provides the text for the subscription details dialog
     *
     * @param sub
     *            The subscription object
     *
     * @return The formated details string
     */
    public static String formatDetails(Subscription sub) {
        final String newline = System.getProperty("line.separator");
        final String space = " ";
        final String comma = ", ";

        StringBuilder fmtStr = new StringBuilder();

        fmtStr.append("Subscription Name: ").append(sub.getName())
                .append(newline);
        fmtStr.append("Group Name: ").append(sub.getGroupName())
                .append(newline);
        fmtStr.append("Dataset Name: ").append(sub.getDataSetName())
                .append(newline);
        fmtStr.append("Dataset Size: ").append(sub.getDataSetSize())
                .append(newline);
        fmtStr.append("Provider : ").append(sub.getProvider()).append(newline);
        fmtStr.append("Office ID: ").append(sub.getOfficeID()).append(newline);
        fmtStr.append("Priority : ").append(sub.getPriority()).append(newline);

        fmtStr.append("Coverage: ").append(newline);
        final Coverage coverage = sub.getCoverage();
        if (coverage.getProjection() != null) {
            fmtStr.append("------ Projection : ")
                    .append(coverage.getProjection()).append(newline);
        } else {
            fmtStr.append("------ Projection : ").append(newline);
        }
        final DecimalFormat decimalFormat = format.get();
        final Coordinate requestLowerRight = coverage.getRequestLowerRight();
        final Coordinate requestUpperLeft = coverage.getRequestUpperLeft();
        if (requestLowerRight == null || requestUpperLeft == null) {
            fmtStr.append("------ Upper Left : ")
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
            fmtStr.append("------ Upper Left : ")
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

        // Get forecast hours
        final Time subTime = sub.getTime();
        final List<String> fcstHours = subTime.getFcstHours();
        if (!CollectionUtil.isNullOrEmpty(fcstHours)) {
            fmtStr.append("Forecast Hours: ").append(newline);
            fmtStr.append("------ ");
            for (int idx : subTime.getSelectedTimeIndices()) {
                fmtStr.append(fcstHours.get(idx)).append(space);
            }

            fmtStr.append(newline);
        }
        final List<Integer> cycles = subTime.getCycleTimes();
        if (cycles != null) {
            fmtStr.append("Cycles: ").append(newline);
            fmtStr.append("------ ");
            for (int cycle : cycles) {
                fmtStr.append(cycle).append(space);
            }

            fmtStr.append(newline);
        }

        fmtStr.append("Parameters:").append(newline);
        ArrayList<Parameter> parmArray = sub.getParameter();
        for (Parameter p : parmArray) {
            fmtStr.append("------ Name: ").append(p.getName()).append(newline);
            fmtStr.append("------ Provider Name: ").append(p.getProviderName())
                    .append(newline);
            fmtStr.append("------ Definition: ").append(p.getDefinition())
                    .append(newline);
            fmtStr.append("------ Data Type : ").append(p.getDataType())
                    .append(newline);

            fmtStr.append("------ Level Type: ").append(newline);
            for (DataLevelType dlt : p.getLevelType()) {
                fmtStr.append("------------ Type: ").append(dlt.getType())
                        .append(newline);
                fmtStr.append("------------ ID  : ").append(dlt.getId())
                        .append(newline);
                if (dlt.getUnit() != null) {
                    fmtStr.append("------------ Unit: ").append(dlt.getUnit())
                            .append(newline);
                } else {
                    fmtStr.append("------------ Unit: ").append(newline);
                }
            }
        }

        return fmtStr.toString();
    }

}
