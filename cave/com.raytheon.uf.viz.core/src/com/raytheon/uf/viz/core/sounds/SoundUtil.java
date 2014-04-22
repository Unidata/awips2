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
package com.raytheon.uf.viz.core.sounds;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import sun.audio.AudioData;
import sun.audio.AudioDataStream;
import sun.audio.AudioPlayer;
import sun.audio.AudioStream;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Utility class for playing sounds.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2014   2636     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SoundUtil {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SoundUtil.class);

    /** AudioDataStream object */
    private static AudioDataStream ads = null;

    /**
     * Play a sound from the file at the provided path.
     * 
     * @param filename
     *            The filename path
     */
    public static void playSound(String filename) {
        if (filename == null || filename.isEmpty()) {
            return;
        }
        File soundFile = new File(filename);
        InputStream in = null;
        AudioStream as = null;
        AudioData data = null;
        try {
            if (ads != null) {
                AudioPlayer.player.stop(ads);
                ads.close();
                ads = null;
            }
            in = new FileInputStream(soundFile);
            as = new AudioStream(in);
            data = as.getData();
            ads = new AudioDataStream(data);
            AudioPlayer.player.start(ads);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to read sound file",
                    e);
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException e) {
                // Ignore
            }
            try {
                if (as != null) {
                    as.close();
                }
            } catch (IOException e) {
                // Ignore
            }
        }
    }
}
