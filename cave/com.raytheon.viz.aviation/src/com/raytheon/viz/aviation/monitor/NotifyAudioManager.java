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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;

import sun.audio.AudioData;
import sun.audio.AudioDataStream;
import sun.audio.AudioPlayer;
import sun.audio.AudioStream;

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
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */

public class NotifyAudioManager {

    private static long lastAlertTime = 0;

    private static NotifyAudioManager nam;

    private String filename;

    private AudioDataStream ads;

    private NotifyAudioManager() {
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        throw new CloneNotSupportedException();
    }

    public static synchronized NotifyAudioManager getInstance() {
        if (nam == null) {
            nam = new NotifyAudioManager();
        }

        return nam;
    }

    public void playFile(String filename) throws IOException {

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        int alertIntervalMinutes = configMgr.getResourceAsInt(ResourceTag.AlertIntervalMinutes);

        long currentTime = System.currentTimeMillis();
        if (currentTime >= (lastAlertTime + alertIntervalMinutes*60*1000)) {
            lastAlertTime = currentTime; 

            if (!filename.equals(this.filename)) {
                File soundFile = new File(filename);
                InputStream in = new FileInputStream(soundFile);
                AudioStream as = new AudioStream(in);
                AudioData data = as.getData();
                ads = new AudioDataStream(data);
            }
            AudioPlayer.player.stop(ads);
            AudioPlayer.player.start(ads);
        }
    }

    public static void resetAlertTime() {
        lastAlertTime = 0;
    }
}
