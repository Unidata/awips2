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
package com.raytheon.viz.gfe.dialogs.sbu;

import java.util.Set;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.dataplugin.gfe.request.CheckPermissionsRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.CheckServiceBackupPrimarySiteRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetServiceBackupPrimarySiteRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * A utility class for the Service Backup GUI to do permissions checks.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2011            bphillip     Initial creation
 * Nov 14, 2012 		   jdynina		Added check for national center
 * May 02, 2013  #1762     dgilling     Replace national center check with
 *                                      a svcbu PRIMARY_SITES check.
 * Jul 22, 2013  #1762     dgilling     Ensure all fields of
 *                                      CheckServiceBackupPrimarySiteRequest are
 *                                      filled.
 * Jun 10, 2013 DR-17401	lshi	    Added getPrimarySites()
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class CheckPermissions {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ServiceBackupDlg.class);

    /**
     * A private constructor so that Java does not attempt to create one for us.
     * As this class should not be instantiated, do not attempt to ever call
     * this constructor; it will simply throw an AssertionError.
     * 
     */
    private CheckPermissions() {
        throw new AssertionError();
    }

    @SuppressWarnings("unchecked")
    public static boolean getAuthorization() {
        boolean authorized = false;
        IUser user = UserController.getUserObject();
        CheckPermissionsRequest request = new CheckPermissionsRequest();
        request.setUser(user);
        try {
            ServerResponse<String> obj = (ServerResponse<String>) ThriftClient
                    .sendRequest(request);
            if (obj.isOkay()) {
                authorized = true;
            } else {
                authorized = false;
            }
        } catch (VizException e) {
            statusHandler.error("Error checking permissions for: " + user, e);
            authorized = false;
        }
        return authorized;
    }

    public static boolean runningAsPrimary(String siteId) {
        CheckServiceBackupPrimarySiteRequest request = new CheckServiceBackupPrimarySiteRequest();
        request.setSiteID(siteId);
        request.setWorkstationID(VizApp.getWsId());
        try {
            @SuppressWarnings("unchecked")
            ServerResponse<Boolean> sr = (ServerResponse<Boolean>) ThriftClient
                    .sendRequest(request);
            return sr.getPayload();
        } catch (VizException e) {
            statusHandler
                    .error("Error checking if running as primary site!", e);
        }
        return false;
    }
    
    public static Set<String> getPrimarySites() {
        Set <String> primary = null;
        
        GetServiceBackupPrimarySiteRequest request = new GetServiceBackupPrimarySiteRequest();
        try {
            @SuppressWarnings("unchecked")
            ServerResponse<Set<String>> sr = (ServerResponse<Set<String>>) ThriftClient
                    .sendRequest(request);
            primary = sr.getPayload();
            return primary;
        } catch (VizException e) {
            statusHandler
            .error("Error getting primary site(s)!", e);
        }
        return primary;
    }
}
