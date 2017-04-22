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
package com.raytheon.uf.viz.alertviz.ui.audio;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.swt.widgets.Display;

import sun.audio.AudioData;
import sun.audio.AudioDataStream;
import sun.audio.AudioPlayer;
import sun.audio.AudioStream;
import sun.audio.ContinuousAudioDataStream;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * This class manages the audio alerts.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Oct 2008             lvenable    Initial creation.
 * 02 Apr 2009             lvenable    TTR fixes.
 * 22 Nov 2010  2235       cjeanbap    Added hasAudioFile().
 * 21 Jan 2011  1978       cjeanbap    Removed method, doesFileExist()
 * 03 Mar 2011  8059       rferrel     Use AlarmBeepJob to play System beep.
 * 31 May 2011  8058       cjeanbap    Kill sound based on TextMsgBox id.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class AlertAudioMgr {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlertAudioMgr.class, "GDN_ADMIN", "GDN_ADMIN");

    /**
     * Parent display.
     */
    private Display parentDisplay;

    /**
     * Audio timer.
     */
    private Timer[] audioTimer;

    /**
     * Audio duration in seconds.
     */
    private int durationInSeconds;

    /**
     * Task executed when the timer fires.
     */
    private TimerTask[] timerTask;

    /**
     * Input stream.
     */
    private InputStream[] in = null;

    /**
     * Audio data.
     */
    private AudioData[] data = null;

    /**
     * A continuous audio data stream.
     */
    private ContinuousAudioDataStream[] cas;

    /**
     * Audio data stream.
     */
    private AudioDataStream[] ads;

    private boolean[] hasSoundFile;

    private boolean[] playingAudio;

    private AlarmBeepJob[] alarmBeepJob;
    /**
     * Constructor.
     */
    public AlertAudioMgr(Display parentDisplay, int numTextMsgBoxComp) {
        this.parentDisplay = parentDisplay;
        audioTimer = new Timer[numTextMsgBoxComp];
        timerTask = new TimerTask[numTextMsgBoxComp];
        in = new InputStream[numTextMsgBoxComp];
        data = new AudioData[numTextMsgBoxComp];
        cas = new ContinuousAudioDataStream[numTextMsgBoxComp];
        ads = new AudioDataStream[numTextMsgBoxComp];
        hasSoundFile = new boolean[numTextMsgBoxComp];
        playingAudio = new boolean[numTextMsgBoxComp];

        for (int i = 0; i < numTextMsgBoxComp; i++) {
            audioTimer[i] = null;
            timerTask[i] = null;
            in[i] = null;
            data[i] = null;
            cas[i] = null;
            ads[i] = null;
            
            hasSoundFile[i] = false;
            playingAudio[i] = false;
        }
        alarmBeepJob = new AlarmBeepJob[numTextMsgBoxComp];
    }

    /**
     * Set the audio file to play and the duration in seconds to play the file.
     * Call doesFileExist(soundFile) to validate the file exist before
     * attempting to play the sound.
     * 
     * @param soundFile
     *            Audio file.
     * @param durationInSeconds
     *            Duration in seconds.
     */
    public void setAudioFile(String soundFile, int durationInSeconds, int textMsgBoxId) {
        /**
         * If the duration in seconds is set to 0 then set the duration to 1
         * hour. The current system has it as a "run forever" but 1 hour should
         * be long enough.
         */
        if (durationInSeconds == 0) {
            this.durationInSeconds = 3600;
        } else {
            this.durationInSeconds = durationInSeconds;
        }

        if (soundFile == null) {
            hasSoundFile[textMsgBoxId] = false;
            return;
        }

        hasSoundFile[textMsgBoxId] = true;

        try {
            in[textMsgBoxId] = new FileInputStream(soundFile);
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.INFO, e.getLocalizedMessage(), e);
            hasSoundFile[textMsgBoxId] = false;
            return;
        }

        AudioStream as = null;

        try {
            as = new AudioStream(in[textMsgBoxId]);
            data[textMsgBoxId] = as.getData();
        } catch (IOException e) {
            // Unable to get audio data set up for system beep.
            statusHandler.handle(Priority.INFO, e.getLocalizedMessage(), e);
            hasSoundFile[textMsgBoxId] = false;
            return;
        }

        // Create ContinuousAudioDataStream
        cas[textMsgBoxId] = new ContinuousAudioDataStream(data[textMsgBoxId]);

        // Create AudioDataStream
        ads[textMsgBoxId] = new AudioDataStream(data[textMsgBoxId]);
    }

    /**
     * Play the sound file one time.
     */
    public void playSound(int textMsgBoxId) {
        stopSound(textMsgBoxId);
        if (hasSoundFile[textMsgBoxId] == true) {
            AudioPlayer.player.start(ads[textMsgBoxId]);
            playingAudio[textMsgBoxId] = true;
        } else {
            // No sound file defined so play the system beep
            parentDisplay.beep();
        }
    }

    /**
     * Play the sound file in a continuous loop.
     */
    public void playLoopSound(int textMsgBoxId) {
        stopTimer(textMsgBoxId);
        if (hasSoundFile[textMsgBoxId] == true) {
            AudioPlayer.player.start(cas[textMsgBoxId]);
            playingAudio[textMsgBoxId] = true;
        } else {
            // No sound file defined so play the system beep
            alarmBeepJob[textMsgBoxId] = new AlarmBeepJob(durationInSeconds);
            alarmBeepJob[textMsgBoxId].schedule();
        }
        startTimer(textMsgBoxId);
    }

    /**
     * Stop any sound files that are playing.
     */
    public void stopSound(int textMsgBoxId) {
        if (hasSoundFile[textMsgBoxId] == true) {
            AudioPlayer.player.stop(ads[textMsgBoxId]);
            AudioPlayer.player.stop(cas[textMsgBoxId]);
            playingAudio[textMsgBoxId] = false;
        } else if (alarmBeepJob[textMsgBoxId] != null) {
            alarmBeepJob[textMsgBoxId].dispose();
            alarmBeepJob[textMsgBoxId] = null;
        }
    }

    /**
     * Start the timer so the sound file will stop after a certain duration.
     */
    private void startTimer(final int textMsgBoxId) {
        if (audioTimer[textMsgBoxId] != null) {
            audioTimer[textMsgBoxId].cancel();
            audioTimer[textMsgBoxId] = null;
        }

        audioTimer[textMsgBoxId] = new Timer();

        timerTask[textMsgBoxId] = new TimerTask() {
            public void run() {
                stopTimer(textMsgBoxId);
            }
        };

        (audioTimer[textMsgBoxId]).schedule(timerTask[textMsgBoxId], durationInSeconds * 1000);
    }

    /**
     * Stop the duration timer.
     */
    public void stopTimer(int textMsgBoxId) {
        if (audioTimer != null && audioTimer[textMsgBoxId] != null) {
            audioTimer[textMsgBoxId].cancel();
            audioTimer[textMsgBoxId] = null;
        }
        stopSound(textMsgBoxId);
        playingAudio[textMsgBoxId] = false;
    }

    /**
     * Determine if Audio Manager has an audio file to play.
     */
    public boolean hasAudioFile(int textMsgBoxId) {
        return hasSoundFile[textMsgBoxId];
    }

    /**
     * Determine if Audio Manager is currently playing an Audio file.
     * 
     * @return boolean true if Audio file is currently playing.
     */
    public boolean isPlayingAudio(int textMsgBoxId) {
        return this.playingAudio[textMsgBoxId];
    }
}
