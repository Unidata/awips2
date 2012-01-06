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
package com.raytheon.viz.texteditor;

import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.Enumeration;

import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.preference.PreferenceStore;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.core.mode.CAVEMode;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 11, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TextWorkstationConstants {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextWorkstationConstants.class);

    private static IPersistentPreferenceStore store = null;

    private static final String TEXTWORKSTATION_QUEUE = "textWorkstation";

    public static final String P_TEXTWORKSTATION_ID = "workstationId";

    private static String host = null;

    private static String TEXTWS = System.getenv("TEXTWS");

    public static String getId() {
        IPersistentPreferenceStore store = getPreferenceStore();
        String id = store.getString(P_TEXTWORKSTATION_ID);
        if (id == null || id.trim().equals("")) {
            id = TEXTWS;
            if (id != null && !id.trim().equals("")) {
                store.putValue(P_TEXTWORKSTATION_ID, id);
                try {
                    store.save();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
        return id;
    }

    /**
     * Gets the ip address of the host machine calling the function
     * 
     * @return
     */
    public static String getHostName() {
        if (host == null) {
            InetAddress addrToUse = null;
            boolean found = false;
            try {
                Enumeration<NetworkInterface> nis = NetworkInterface
                        .getNetworkInterfaces();
                while (nis.hasMoreElements() && !found) {
                    NetworkInterface ni = nis.nextElement();
                    ni.isVirtual();
                    ni.isUp();
                    Enumeration<InetAddress> addrs = ni.getInetAddresses();
                    while (addrs.hasMoreElements() && !found) {
                        InetAddress addr = addrs.nextElement();
                        if (addr.isLinkLocalAddress() == false
                                && addr.isSiteLocalAddress() == false
                                && addr.isLoopbackAddress() == false) {
                            addrToUse = addr;
                            found = true;
                        }
                    }
                }
            } catch (SocketException e) {
                e.printStackTrace();
            }

            if (addrToUse == null) {
                try {
                    // Grab whatever is in the preference for cave
                    host = InetAddress.getByName(getId()).getHostName();
                } catch (UnknownHostException e) {
                    e.printStackTrace();
                }
            } else {
                host = addrToUse.getHostName();
            }
        }
        return host;
    }

    public static IPersistentPreferenceStore getPreferenceStore() {
        if (store == null) {
            IPathManager pm = PathManagerFactory.getPathManager();
            final LocalizationFile file = pm.getLocalizationFile(pm
                    .getContext(LocalizationType.CAVE_STATIC,
                            LocalizationLevel.WORKSTATION), "textWs"
                    + File.separator + "textws.prefs");
            File f = file.getFile();
            if (f.exists() == false) {
                try {
                    f.getParentFile().mkdirs();
                    f.createNewFile();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error getting text workstation preferences", e);
                }
            }
            store = new PreferenceStore(file.getFile().getAbsolutePath()) {

                @Override
                public void save() throws IOException {
                    super.save();
                    try {
                        file.save();
                    } catch (LocalizationOpFailedException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error saving text workstation preferences", e);
                    }
                }

            };
            try {
                ((PreferenceStore) store).load();
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error loading text workstation preferences", e);
            }
        }
        return store;
    }

    public static String getDestinationTextWorkstationQueueName()
            throws UnknownHostException {
        StringBuilder queueName = getTextWorkstationQueueNameBuilder();
        queueName.append("_");
        queueName.append(getHostNameStr());
        return queueName.toString();
    }

    public static String getTextWorkstationQueueName() {
        StringBuilder queueName = getTextWorkstationQueueNameBuilder();
        queueName.append("_");
        queueName.append(TextWorkstationConstants.getHostName());
        return queueName.toString();
    }

    private static StringBuilder getTextWorkstationQueueNameBuilder() {
        StringBuilder queueName = new StringBuilder(
                TextWorkstationConstants.TEXTWORKSTATION_QUEUE);
        if (CAVEMode.getMode().equals(CAVEMode.PRACTICE)) {
            queueName.append("_");
            queueName.append(CAVEMode.PRACTICE.name());
        }
        return queueName;
    }

    private static String getHostNameStr() throws UnknownHostException {
        String host = TextWorkstationConstants.getId();
        if (host == null || "".equals(host)) {
            throw new UnknownHostException("Host: " + host + " is not valid");
        }
        return host;
    }

}
