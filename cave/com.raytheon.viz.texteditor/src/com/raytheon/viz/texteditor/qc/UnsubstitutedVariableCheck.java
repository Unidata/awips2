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
package com.raytheon.viz.texteditor.qc;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Check for unsubstituted Velocity variables
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2016  5411      randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class UnsubstitutedVariableCheck implements IQCCheck {

    private static final Pattern UNSUB_VAR_PATTERN = Pattern
            .compile("(\\$\\{[a-z][a-z0-9-_]*\\}|\\$[a-z][a-z0-9-_]*(?=\\W))");

    @Override
    public String runQC(String header, String body, String nnn) {
        String errorMsg = "";

        Matcher matcher = UNSUB_VAR_PATTERN.matcher(body);
        if (matcher.find()) {
            errorMsg = "An unsubstituted variable reference was found: "
                    + matcher.group() + "\nThis is likely a template issue.\n";
        }

        return errorMsg;
    }

}
