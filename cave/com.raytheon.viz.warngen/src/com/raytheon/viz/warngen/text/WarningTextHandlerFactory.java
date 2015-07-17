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

import java.util.regex.Matcher;

import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;

/**
 * Returns a WarningTextHandler with the appropriate behaviors based on the type
 * of warning is created.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2012    15322   jsanchez    Initial creation
 * Jun 02, 2015     4441   randerso    Made first bullet regex case insensitive
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class WarningTextHandlerFactory {

    public static WarningTextHandler getHandler(WarningAction action,
            String text, boolean autoLock) {
        WarningTextHandler handler = new WarningTextHandler();

        if (action == WarningAction.NEW) {
            handler.setModifyTextBehavior(new NewModifyTextBehavior());
        } else if (action == WarningAction.COR) {
            handler.setModifyTextBehavior(new CorModifyTextBehavior());
        } else {
            System.out.println("The text will not be updated based on action.");
            handler.setModifyTextBehavior(null);
        }

        if (!autoLock) {
            System.out.println("Config has autoLock set to 'false'");
            handler.setLockingBehavior(null);
        } else if (isInitialWarning(action, text)) {
            handler.setLockingBehavior(new InitialLockingBehavior());
        } else {
            handler.setLockingBehavior(new FollowUpLockingBehavior());
        }

        return handler;
    }

    private static boolean isInitialWarning(WarningAction action, String text) {
        if ((action == WarningAction.NEW) || (action == WarningAction.EXT)) {
            return true;
        } else if (action == WarningAction.COR) {
            // TODO Need a better solution not to include the text in the
            // factory.
            Matcher m = WarnGenPatterns.firstBulletPtrn.matcher(text);
            return m.find();
        }

        return false;
    }
}
