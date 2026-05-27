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

import java.io.IOException;
import java.io.InputStream;
import java.net.UnknownHostException;

import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.preference.PreferenceStore;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.SystemUtil;
import com.raytheon.viz.core.mode.CAVEMode;

/**
 * Constants for the text workstation
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 11, 2009           mschenke  Initial creation
 * Nov 12, 2015  4834     njensen   Changed LocalizationOpFailedException to
 *                                  LocalizationException
 * Nov 03, 2017  5896     randerso  Allow multiple saves of text workstation
 *                                  preferences
 * Apr 29, 2021  8137     randerso  Force use of short hostname for text
 *                                  workstation queue. Get default from
 *                                  SystemUtil.getHostName() instead of TEXT WS
 *                                  variable.
 *
 * </pre>
 *
 * @author mschenke
 */

public class TextWorkstationConstants {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextWorkstationConstants.class);

    private static IPersistentPreferenceStore store = null;

    private static final String TEXTWORKSTATION_QUEUE = "textWorkstation";

    /**
     * Text Workstation ID preference name
     */
    public static final String P_TEXTWORKSTATION_ID = "workstationId";

    /**
     * @return text workstation id preference value
     */
    public static String getId() {
        return shorten(getPreferenceStore().getString(P_TEXTWORKSTATION_ID));
    }

    private static String shorten(String hostName) {
        String shortHostName = hostName;
        int pos = shortHostName.indexOf('.');
        if (pos > 0) {
            shortHostName = shortHostName.substring(0, pos);
        }
        return shortHostName;
    }

    /**
     * @return the short host name of the local machine
     */
    public static synchronized String getShortHostName() {
        return shorten(SystemUtil.getHostName());
    }

    private static ILocalizationFile getLocalizationFile() {
        IPathManager pm = PathManagerFactory.getPathManager();
        ILocalizationFile lf = pm.getLocalizationFile(
                pm.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.WORKSTATION),
                LocalizationUtil.join("textWs", "textws.prefs"));

        return lf;
    }

    /**
     * @return the text workstation preference store
     */
    public static synchronized IPersistentPreferenceStore getPreferenceStore() {
        if (store == null) {
            store = new PreferenceStore() {
                @Override
                public void save() throws IOException {
                    ILocalizationFile lf = getLocalizationFile();

                    try (SaveableOutputStream out = lf.openOutputStream()) {
                        super.save(out, null);
                        out.save();
                    } catch (LocalizationException e) {
                        statusHandler.error(
                                "Error saving text workstation preferences", e);
                    }
                }

                @Override
                public void load() throws IOException {
                    ILocalizationFile lf = getLocalizationFile();

                    if (lf.exists()) {
                        try (InputStream in = lf.openInputStream()) {
                            super.load(in);
                        } catch (LocalizationException e) {
                            statusHandler.error(
                                    "Error loading text workstation preferences",
                                    e);
                        }
                    }
                }
            };
            store.setDefault(P_TEXTWORKSTATION_ID, getShortHostName());

            try {
                ((PreferenceStore) store).load();
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error loading text workstation preferences", e);
            }
        }
        return store;
    }

    /**
     * @return the destination text workstation queue name
     *
     * @throws UnknownHostException
     */
    public static String getDestinationTextWorkstationQueueName()
            throws UnknownHostException {
        StringBuilder queueName = getTextWorkstationQueueNameBuilder();
        queueName.append("_");
        queueName.append(getHostNameStr());
        return queueName.toString();
    }

    /**
     * @return the local text workstation queue name
     */
    public static String getLocalTextWorkstationQueueName() {
        StringBuilder queueName = getTextWorkstationQueueNameBuilder();
        queueName.append("_");
        queueName.append(getShortHostName());
        return queueName.toString();
    }

    private static StringBuilder getTextWorkstationQueueNameBuilder() {
        StringBuilder queueName = new StringBuilder(
                TextWorkstationConstants.TEXTWORKSTATION_QUEUE);
        if (CAVEMode.PRACTICE.equals(CAVEMode.getMode())) {
            queueName.append("_");
            queueName.append(CAVEMode.PRACTICE.name());
        }
        return queueName;
    }

    private static String getHostNameStr() throws UnknownHostException {
        String host = TextWorkstationConstants.getId();
        if (host == null || host.isEmpty()) {
            throw new UnknownHostException("Host: " + host + " is not valid");
        }
        return host;
    }

}
