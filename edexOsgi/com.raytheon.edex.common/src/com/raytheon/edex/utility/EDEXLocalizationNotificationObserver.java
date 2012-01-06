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
package com.raytheon.edex.utility;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationNotificationObserver;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * Localization notification observer for edex
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class EDEXLocalizationNotificationObserver {

    private static EDEXLocalizationNotificationObserver instance = null;

    private LocalizationNotificationObserver observer;

    public static synchronized EDEXLocalizationNotificationObserver getInstance() {
        if (instance == null) {
            instance = new EDEXLocalizationNotificationObserver();
        }
        return instance;
    }

    private EDEXLocalizationNotificationObserver() {
        observer = LocalizationNotificationObserver.getInstance();
        observer.addGlobalFileChangeObserver(new ILocalizationFileObserver() {

            @Override
            public void fileUpdated(FileUpdatedMessage message) {
                System.out.println("Recieved " + message.getChangeType()
                        + " message for " + message.getContext() + "/"
                        + message.getFileName());
            }
        });
    }

    public void fileUpdated(byte[] bytes) throws LocalizationException {
        try {
            Object obj = SerializationUtil.transformFromThrift(bytes);
            observer.fileUpdateMessageRecieved((FileUpdatedMessage) obj);
        } catch (SerializationException e) {
            throw new LocalizationException(
                    "Error processing file update message: "
                            + e.getLocalizedMessage(), e);
        }
    }

}
