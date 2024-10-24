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
package com.raytheon.viz.aviation.monitor;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.sounds.SoundUtil;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;

/**
 * TODO Add Description
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#   Engineer    Description
 * ------------ --------- ----------- --------------------------
 * Dec 2, 2009            avarani     Initial creation
 * Oct 20,2015  17445     yteng       Set alert interval
 * Apr 29, 2019 7591      tgurney     Replace sun.audio with
 *                                    javax.sound.sampled
 *
 * </pre>
 *
 * @author avarani
 */

public class NotifyAudioManager {

    private static long lastAlertTime = 0;

    private static NotifyAudioManager instance;

    private NotifyAudioManager() {
    }

    public static synchronized NotifyAudioManager getInstance() {
        if (instance == null) {
            instance = new NotifyAudioManager();
        }
        return instance;
    }

    public void playFile(String filename) throws Exception {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        int alertIntervalMinutes = configMgr
                .getResourceAsInt(ResourceTag.AlertIntervalMinutes);

        long currentTime = System.currentTimeMillis();
        if (currentTime >= lastAlertTime
                + alertIntervalMinutes * TimeUtil.MILLIS_PER_MINUTE) {
            lastAlertTime = currentTime;
            SoundUtil.playSound(filename);
        }
    }

    public static void resetAlertTime() {
        lastAlertTime = 0;
    }
}
