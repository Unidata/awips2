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
package com.raytheon.edex.plugin.shef.alarms;

import java.util.ArrayList;

import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;

/**
 * Place holder for user defined report options
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 15, 2011    9377     jnjanga     Initial creation
 * 
 * </pre>
 * 
 * @author jnjanga
 * @version 1.0
 */

class ReportOptions {

    private String dbname=null;

    private String productId=null;

    private ReportMode mode=null;

    private String filter=null;

    private String PEfilter=null;

    private String fileSuffix=null;

    private float min_val_diff=0;

    private int minutes=0;

    private boolean minGiven=false;

    ReportOptions() {
    }

    public String getDbname() {
        return dbname;
    }
    
    public String getProductId() {
        return productId;
    }

    public void setProductId(String productId) {
        int pidlength = productId.length();
        if (pidlength != 9 && pidlength != 10) {
            final String msg = "Invalid length for Product_Id : " + pidlength;
            throw new IllegalArgumentException(msg);
        }
        this.productId = productId;
    }

    
    public void setDbname(String dbname) {
        if(!dbname.equals( System.getProperty("ih.db.name"))) {
            final String msg = "Invalid database name : " + dbname;
            throw new IllegalArgumentException(msg);
        }       
        this.dbname = ShefConstants.IHFS;
    }
    
    public int getMinutes() {
        return minutes;
    }

    public void setMinutes(String minutes) throws IllegalArgumentException {
        boolean invalid = false;
        int argl = minutes.length();
        int tmp = Integer.valueOf(minutes);
        if (argl == 0 || argl > 6) {
            invalid = true;
        } else {
            invalid = isWithinWindow(tmp);
        }

        if (invalid) {
            final String msg = getWindowOptionUsage(tmp);
            throw new IllegalArgumentException(msg);
        }

        this.minutes = tmp;
        minGiven = true;
    }

    public boolean isMinutesGiven() {
        return minGiven;
    }

    public ReportMode getMode() {
        return mode;
    }

    public void setMode(String mode) throws IllegalArgumentException {
        try {
            this.mode = ReportMode.valueOf(mode);
        } catch (Exception e) {
            final String msg = "Invalid report mode : " + mode + Constants.EOL
                    + "Report mode must be either : "
                    + printValid(ReportMode.values());
            throw new IllegalArgumentException(msg);
        }
    }

    public String getFilter() {
        return filter;
    }

    public void setFilter(String filter) throws IllegalArgumentException {
        boolean invalid = false;
        int fltrlen = filter.length();
        if (fltrlen == 0 || fltrlen > 8)
            invalid = true;
        else
            invalid = !tokensValid(filter);

        if (invalid) {
            final String msg = printFilterOptionUsage(filter);
            throw new IllegalArgumentException(msg);
        }

        this.filter = filter;
    }

    public String getPEfilter() {
        return PEfilter;
    }

    public void setPEfilter(String pEfilter) throws IllegalArgumentException {
        if (pEfilter.length() != 2) {
            final String msg = "PE filter option must be two characters";
            throw new IllegalArgumentException(msg);
        }
        PEfilter = pEfilter;
    }

    public String getFileSuffix() {
        return fileSuffix;
    }

    public void setFileSuffix(String fileSuffix) {
        this.fileSuffix = fileSuffix;
    }

    public float getMin_val_diff() {
        return min_val_diff;
    }

    public void setMin_val_diff(float min_val_diff) {
        this.min_val_diff = min_val_diff;
    }

    public void addOption(CmdlineOption option) throws IllegalArgumentException {
        String arg = (String) option.getArg();
        switch (option.getId()) {
        case DB_NAME:
            setDbname(arg);
            break;
        case PRODUCT_ID:
            setProductId(arg);
            break;
        case REPORT_MODE:
            setMode(arg);
            break;
        case FLAGS:
            setFilter(arg);
            break;
        case PE:
            setPEfilter(arg);
            break;
        case MINUTES:
            setMinutes(arg);
            break;
        case FILE_SUFFIX:
            setFileSuffix(arg);
            break;
        default:
            break;
        }
    }

    public String toString() {
        StringBuilder str = new StringBuilder();

        str.append("Product_Id  = " + productId + Constants.EOL);
        str.append("Report_Mode = " + mode.toString() + Constants.EOL);
        str.append("Filter      = " + filter + Constants.EOL);
        str.append("PE Filter   = " + PEfilter + Constants.EOL);
        str.append("File suffix = " + fileSuffix + Constants.EOL);
        str.append("minutes     = " + minutes + Constants.EOL);
        return str.toString();
    }

    private static <E> String printValid(E[] array) {
        StringBuilder str = new StringBuilder();
        for (E element : array)
            str.append(element.toString() + ", ");
        str.delete(str.length() - 2, str.length());
        return str.toString();
    }

    private boolean isWithinWindow(int min) {
        return (min <= 0 || min > 999999);
    }

    private String getWindowOptionUsage(int min) {
        return "Invalid number of minutes : " + min
                + " .  Must be between 1 - 999999 ";
    }

    private String printFilterOptionUsage(String filter) {
        return "Invalid length or token for filter option : " + filter + Constants.EOL
                + "Filter option must be either : "
                + printValid(FilterOption.values());
    }

    private boolean tokensValid(String filter) {
        ArrayList<Character> validArgs = FilterOption.asList();
        for (char c : filter.toCharArray())
            if (!validArgs.contains(c))
                return false;
        return true;
    }

}