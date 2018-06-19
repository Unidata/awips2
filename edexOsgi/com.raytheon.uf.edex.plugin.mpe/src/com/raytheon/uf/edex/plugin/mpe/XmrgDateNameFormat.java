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
package com.raytheon.uf.edex.plugin.mpe;

import java.text.SimpleDateFormat;
import java.util.Map;
import java.util.HashMap;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Enumeration representative of the different ways the date/time can be encoded
 * within the name of a XMRG file. This enum has been created to take the place
 * of String constants.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 23, 2016 5631       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public enum XmrgDateNameFormat {
    MONTH_FIRST("mdY", "MMddyyyyHHmm"), YEAR_FIRST("Ymd", "yyyyMMddHHmm");

    private static Map<String, XmrgDateNameFormat> dateFormatTextLookupMap;

    public static final String VALUE_MDY = "mdY";

    public static final String VALUE_YMD = "Ymd";

    private final String text;

    private final String format;

    private final ThreadLocal<SimpleDateFormat> sdf;

    private XmrgDateNameFormat(final String text, final String format) {
        this.text = text;
        this.format = format;
        this.sdf = TimeUtil.buildThreadLocalSimpleDateFormat(format,
                TimeUtil.GMT_TIME_ZONE);
    }

    public static synchronized XmrgDateNameFormat lookupDateFormatByText(
            final String lookupText) {
        if (lookupText == null) {
            return null;
        }
        if (dateFormatTextLookupMap == null) {
            dateFormatTextLookupMap = new HashMap<>(
                    XmrgDateNameFormat.values().length, 1.0f);
            for (XmrgDateNameFormat dateFormat : XmrgDateNameFormat.values()) {
                dateFormatTextLookupMap.put(dateFormat.getText(), dateFormat);
            }
        }

        return dateFormatTextLookupMap.get(lookupText);
    }

    public String getFormat() {
        return format;
    }

    public ThreadLocal<SimpleDateFormat> getSdf() {
        return sdf;
    }

    public String getText() {
        return text;
    }
}