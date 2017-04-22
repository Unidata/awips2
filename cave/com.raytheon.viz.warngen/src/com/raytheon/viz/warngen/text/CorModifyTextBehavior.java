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
package com.raytheon.viz.warngen.text;

/**
 * Modifies the warning text by adding 'CORRECTED' in the MND header.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2012    15322   jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class CorModifyTextBehavior implements IModifyTextBehavior {

    private static final String[] types = new String[] { "WARNING", "WATCH",
            "STATEMENT", "ADVISORY" };

    private static final String correctedText = "...CORRECTED";

    @Override
    public String modify(String text) {
        int index = text.indexOf("NATIONAL WEATHER SERVICE");

        int typeIdx = -1, i = 0;
        if (index > 0) {
            for (i = 0; i < types.length; i++) {
                if (text.lastIndexOf(types[i], index) != -1) {
                    typeIdx = text.lastIndexOf(types[i], index);
                    break;
                }
            }
        }

        if (index > 0 && typeIdx > 0 && !text.contains(correctedText)) {
            text = text.substring(0, typeIdx + types[i].length())
                    + correctedText
                    + text.substring(typeIdx + types[i].length());
        }

        return text;
    }

}
