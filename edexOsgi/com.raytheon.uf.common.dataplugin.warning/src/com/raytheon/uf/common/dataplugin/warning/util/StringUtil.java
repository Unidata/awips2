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
package com.raytheon.uf.common.dataplugin.warning.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2011            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class StringUtil {

    private static final Pattern cwaBackUp = Pattern
            .compile("([A-Z]{3})/([A-Z\\s/-]{1,})");

    public static String[] parseBackupCWAs(String cwa) {
        cwa = cwa.trim();
        String[] parts = cwa.split("/");
        String site = parts[0];
        String office = parts[1];
        Matcher m = cwaBackUp.matcher(cwa);
        if (m.find()) {
            site = m.group(1);
            office = m.group(2);
        }
        return new String[] { site, office };
    }
}
