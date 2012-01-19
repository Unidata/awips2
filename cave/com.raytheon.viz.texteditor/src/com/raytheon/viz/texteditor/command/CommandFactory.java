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
package com.raytheon.viz.texteditor.command;

import com.raytheon.edex.textdb.dbapi.impl.AFOSParser;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.texteditor.util.TextEditorUtil;

/**
 * Command factory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2009 2191       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class CommandFactory {
    private static String localSiteId = LocalizationManager.getInstance()
            .getCurrentSite();

    public static ICommand getAfosCommand(String afosCommand) {
        return new AFOSCommand(afosCommand, localSiteId);
    }

    public static ICommand getPreviousForAfosCommand(ICommand cmd) {
        String text = TextEditorUtil.getCommandText(cmd);
        AFOSParser parser = new AFOSParser(text, localSiteId);
        int version = parser.getPastVersNumber();
        if (version > 0) {
            version++;
        } else {
            version = 1;
        }

        String afosCommand = "-" + version + ":" + parser.getCcc()
                + parser.getNnn() + parser.getXxx();
        return new AFOSCommand(afosCommand, localSiteId);
    }

    public static ICommand getNextForAfosCommand(ICommand cmd) {
        String text = TextEditorUtil.getCommandText(cmd);
        AFOSParser parser = new AFOSParser(text, localSiteId);
        int version = parser.getPastVersNumber();
        String afosCommand = null;

        if (version > 1) {
            version--;
            afosCommand = "-" + version + ":" + parser.getCcc()
                    + parser.getNnn() + parser.getXxx();
        } else {
            afosCommand = parser.getCcc() + parser.getNnn() + parser.getXxx();
        }

        return new AFOSCommand(afosCommand, localSiteId);
    }

    public static ICommand getLatestForAfosCommand(ICommand cmd) {
        String text = TextEditorUtil.getCommandText(cmd);
        AFOSParser parser = new AFOSParser(text, localSiteId);
        String afosCommand = parser.getCcc() + parser.getNnn()
                + parser.getXxx();
        return new AFOSCommand(afosCommand, localSiteId);
    }

    public static ICommand getAllForAfosCommand(ICommand cmd) {
        String text = TextEditorUtil.getCommandText(cmd);
        AFOSParser parser = new AFOSParser(text, localSiteId);
        String afosCommand = "ALL:" + parser.getCcc() + parser.getNnn()
                + parser.getXxx();
        return new AFOSCommand(afosCommand, localSiteId);
    }

    public static ICommand getWmoCommand(String wmoId, String cccc) {
        return getWmoCommand(wmoId, cccc, null, null, null);
    }

    public static ICommand getWmoCommand(String wmoId, String cccc,
            String awipsid, String hdrtime, String bbbid) {
        return new WMOCommand(wmoId, cccc, awipsid, hdrtime, bbbid);
    }

    public static ICommand getAwipsCommand(String awipsId) {
        return getAwipsCommand(awipsId, null, null, null, null);
    }

    public static ICommand getAwipsCommand(String awipsId, String wmoId,
            String cccc, String hdrtime, String bbbid) {
        return new AWIPSCommand(awipsId, wmoId, cccc, hdrtime, bbbid);
    }

    public static ICommand getCommand(String wmoId, String cccc,
            String awipsid, String hdrtime, String bbbid, String lasthours,
            String fullRead) {
        return new GeneralCommand(wmoId, cccc, awipsid, hdrtime, bbbid,
                lasthours, fullRead);
    }
}
