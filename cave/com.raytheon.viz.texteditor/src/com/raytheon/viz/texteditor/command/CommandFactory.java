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

import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * Command factory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2009 2191       rjpeter     Initial creation
 * May 23, 2010 14952      rferrel     Allow using refTime in AFOS commands.
 * Feb 12, 2016 4716       rferrel     {@link #getAwipsCommand(String, String)} changed to
 *                                     take site argument.
 * Aug 28, 2016 5839       rferrel     Fix for version number queries.
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
        return new AFOSCommand(afosCommand, localSiteId, null);
    }

    public static ICommand getAfosCommand(String afosCommand, Long refTime) {
        return new AFOSCommand(afosCommand, localSiteId, refTime);
    }

    public static ICommand getPreviousForCommand(ICommand cmd) {
        return cmd.getPrevious();
    }

    public static ICommand getNextForCommand(ICommand cmd) {
        return cmd.getNext();
    }

    public static ICommand getLatestForCommand(ICommand cmd) {
        return cmd.getLatest();
    }

    public static ICommand getAllForCommand(ICommand cmd) {
        return cmd.getAll();
    }

    public static ICommand getWmoCommand(String wmoId, String cccc) {
        return getWmoCommand(wmoId, cccc, null, null, null);
    }

    public static ICommand getWmoCommand(String wmoId, String cccc,
            String awipsid, String hdrtime, String bbbid) {
        return new WMOCommand(wmoId, cccc, awipsid, hdrtime, bbbid);
    }

    public static ICommand getAwipsCommand(String awipsId, String site) {
        return getAwipsCommand(awipsId, null, site, null, null);
    }

    public static ICommand getAwipsCommand(String awipsId, String wmoId,
            String cccc, String hdrtime, String bbbid) {
        return getAwipsCommand(awipsId, wmoId, cccc, hdrtime, null, bbbid,
                false);
    }

    public static ICommand getAwipsCommand(String awipsId, String wmoId,
            String cccc, String hdrtime, String pastVersion, String bbbid,
            boolean singleProduct) {
        return new AWIPSCommand(awipsId, wmoId, cccc, hdrtime, pastVersion,
                bbbid, singleProduct);
    }

    public static ICommand getCommand(String wmoId, String cccc, String awipsid,
            String hdrtime, String bbbid, String lasthours, String fullRead) {
        return new GeneralCommand(wmoId, cccc, awipsid, hdrtime, bbbid,
                lasthours, fullRead);
    }
}
