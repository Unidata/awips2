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
package com.raytheon.uf.edex.ohd.reportalarm;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.EnumSet;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.StringUtil;

/**
 * Place holder for user defined report options
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 15, 2011  9377      jnjanga      Initial creation
 * Jul 12, 2013  15711     wkwock       Fix verbose, observe mode, etc
 * Feb 12, 2014  #2783     dgilling     Major refactor, cleanup.
 * 
 * </pre>
 * 
 * @author jnjanga
 * @version 1.0
 */

class ReportOptions {

    private static final String DB_NAME = "ihfs";

    private static final String PRODUCT_ID_KEY = "alarm_product_id";

    private static final String REPORT_MODE_KEY = "alarm_report_mode";

    private static final String INCLUDE_FLAGS_KEY = "alarm_filter";

    private static final String PHYSICAL_ELEMENT_KEY = "alarm_pe_filter";

    private static final String FILE_SUFFIX_KEY = "alarm_file_suffix";

    private static final String MINUTES_KEY = "alarm_minutes";

    private static final String VERBOSE_MODE_KEY = "alarm_verbose";

    private static final EnumSet<ReportMode> IGNORE_MINS = EnumSet.of(
            ReportMode.ALL, ReportMode.UNREPORTED, ReportMode.NEAREST,
            ReportMode.LATEST_MAXFCST);

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReportOptions.class);

    private String productId;

    private ReportMode mode = null;

    private EnumSet<FilterOption> filter = EnumSet.noneOf(FilterOption.class);

    private String PEfilter = null;

    private DateFormat fileSuffix = null;

    private Integer minutes = null;

    private boolean verboseFlag;

    /**
     * @param appsDefaults
     * @throws IllegalArgumentException
     */
    public ReportOptions(final AppsDefaults appsDefaults)
            throws IllegalArgumentException {
        setProductId(appsDefaults.getToken(PRODUCT_ID_KEY));

        if (appsDefaults.getTokens().contains(REPORT_MODE_KEY)) {
            setMode(appsDefaults.getToken(REPORT_MODE_KEY));
        }

        if (appsDefaults.getTokens().contains(INCLUDE_FLAGS_KEY)) {
            setFilter(appsDefaults.getToken(INCLUDE_FLAGS_KEY));
        }

        if (appsDefaults.getTokens().contains(PHYSICAL_ELEMENT_KEY)) {
            setPEfilter(appsDefaults.getToken(PHYSICAL_ELEMENT_KEY));
        }

        if (appsDefaults.getTokens().contains(FILE_SUFFIX_KEY)) {
            setFileSuffix(appsDefaults.getToken(FILE_SUFFIX_KEY));
        }

        if (appsDefaults.getTokens().contains(MINUTES_KEY)) {
            setMinutes(appsDefaults.getInt(MINUTES_KEY, -1));
        }

        setVerbose(appsDefaults.getBoolean(VERBOSE_MODE_KEY, false));

        if (isMinutesGiven() && IGNORE_MINS.contains(this.mode)) {
            statusHandler.warn("Minutes value ignored for this report mode.");
        }
    }

    public String getProductId() {
        return productId;
    }

    private void setProductId(String productId) throws IllegalArgumentException {
        int pidlength = (productId != null) ? productId.length() : 0;
        if (pidlength != 9 && pidlength != 10) {
            throw new IllegalArgumentException(
                    "Invalid length for Product_Id : " + pidlength);
        }
        this.productId = productId;
    }

    public int getMinutes() {
        return (minutes != null) ? minutes : 0;
    }

    private void setMinutes(Integer minutes) throws IllegalArgumentException {
        if (minutes != null && !isWithinWindow(minutes)) {
            throw new IllegalArgumentException(getWindowOptionUsage(minutes));
        }

        this.minutes = minutes;
    }

    public boolean isMinutesGiven() {
        return (minutes != null);
    }

    public ReportMode getMode() {
        return mode;
    }

    private void setMode(String mode) throws IllegalArgumentException {
        try {
            this.mode = ReportMode.valueOf(mode);
        } catch (Exception e) {
            final String msg = "Invalid report mode : " + mode + Constants.EOL
                    + "Report mode must be either : "
                    + printValid(ReportMode.values());
            throw new IllegalArgumentException(msg);
        }
    }

    public EnumSet<FilterOption> getFilter() {
        return filter;
    }

    private void setFilter(String filter) throws IllegalArgumentException {
        int fltrlen = filter.length();
        if (fltrlen == 0 || fltrlen > 8) {
            throw new IllegalArgumentException(
                    "Invalid number of filter options specified. Filter string must be between 1 and 8 characters long.");
        }

        this.filter = EnumSet.noneOf(FilterOption.class);
        for (char flag : filter.toCharArray()) {
            try {
                this.filter.add(FilterOption.fromArg(flag));
            } catch (IllegalArgumentException e) {
                throw new IllegalArgumentException(printFilterOptionUsage(flag));
            }
        }
    }

    public String getPEfilter() {
        return PEfilter;
    }

    private void setPEfilter(String pEfilter) throws IllegalArgumentException {
        if (pEfilter.length() != 2) {
            throw new IllegalArgumentException(
                    "PE filter option must be two characters");
        }
        this.PEfilter = pEfilter;
    }

    public String getFileSuffix() {
        String retVal = (fileSuffix != null) ? fileSuffix.format(TimeUtil
                .newDate()) : "";
        return retVal;
    }

    private void setFileSuffix(String fileSuffix) {
        if (!StringUtil.isEmptyString(fileSuffix)) {
            this.fileSuffix = new SimpleDateFormat(fileSuffix);
        }
    }

    public boolean getVerbose() {
        return verboseFlag;
    }

    private void setVerbose(boolean verboseFlg) {
        this.verboseFlag = verboseFlg;
    }

    public String getDbname() {
        return DB_NAME;
    }

    @Override
    public String toString() {
        StringBuilder str = new StringBuilder();
        str.append("Product_Id  = ").append(productId).append(Constants.EOL);
        str.append("Report_Mode = ").append(mode.toString())
                .append(Constants.EOL);
        str.append("Filter      = ").append(filter).append(Constants.EOL);
        str.append("PE Filter   = ").append(PEfilter).append(Constants.EOL);
        str.append("File suffix = ").append(fileSuffix).append(Constants.EOL);
        str.append("minutes     = ").append(minutes).append(Constants.EOL);
        return str.toString();
    }

    private static <E> String printValid(E[] array) {
        StringBuilder str = new StringBuilder();
        for (E element : array) {
            str.append(element.toString() + ", ");
        }
        str.delete(str.length() - 2, str.length());
        return str.toString();
    }

    private boolean isWithinWindow(int min) {
        return (min >= 0 && min <= 999999);
    }

    private String getWindowOptionUsage(int min) {
        return "Invalid number of minutes : " + min
                + " .  Must be between 1 - 999999 ";
    }

    private String printFilterOptionUsage(char flag) {
        return "Invalid token for filter option : " + flag + Constants.EOL
                + "Filter option must be either : "
                + printValid(FilterOption.values());
    }

    public String getFileName() {
        String retVal = productId;
        if (fileSuffix != null) {
            retVal = retVal + '.' + getFileSuffix();
        }
        return retVal;
    }
}