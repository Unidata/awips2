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

import java.io.File;
import java.time.Duration;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.CopyOnWriteArrayList;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.AlertvizException;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.ui.audio.IAlertSoundJob.IJobFinshedListener;

/**
 * This class manages the audio alerts.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 05, 2008           lvenable  Initial creation.
 * Apr 02, 2009           lvenable  TTR fixes.
 * Nov 22, 2010  2235     cjeanbap  Added hasAudioFile().
 * Jan 21, 2011  1978     cjeanbap  Removed method, doesFileExist()
 * Mar 03, 2011  8059     rferrel   Use AlarmBeepJob to play System beep.
 * May 31, 2011  8058     cjeanbap  Kill sound based on TextMsgBox id.
 * Sep 20, 2018  7457     randerso  Major refactor to simplify interaction with
 *                                  AlertViz GUIs. Removed dependence on
 *                                  sun.audio which will be removed in Java 9.
 * Oct 04, 2018  7484     randerso  Changed to use AV_ADMIN for internal errors
 * Oct 08, 2018  7515     randerso  Adjusted priorities of AlertViz internal
 *                                  errors.
 * Oct 09, 2018  7457     randerso  Fix audio playback when text is not enabled.
 * Oct 15, 2018  7515     randerso  Added file name to error message.
 * Nov 13, 2018  7512     randerso  Moved AlertViz audio files
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class AlertAudioMgr {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlertAudioMgr.class, "AV_ADMIN", "AV_ADMIN");

    private static class AudioJobKiller extends TimerTask {

        private IAlertSoundJob job;

        public AudioJobKiller(IAlertSoundJob job) {
            super();
            this.job = job;
        }

        @Override
        public void run() {
            job.kill();
        }

    }

    /** List of running jobs */
    private static List<IAlertSoundJob> jobList;

    /** Timer for killing looping jobs */
    private static Timer timer;

    private static boolean initialized = false;

    /** Listener to remove finished jobs from jobList */
    private static IJobFinshedListener closeListener;

    /**
     * Private constructor for static class
     */
    private AlertAudioMgr() {
    }

    /**
     * Initialize the AlertAudioMgr
     */
    public static synchronized void initialize() {
        if (!initialized) {
            jobList = new CopyOnWriteArrayList<>();
            timer = new Timer();
            closeListener = new IJobFinshedListener() {
                @Override
                public void audioJobFinished(IAlertSoundJob job) {
                    jobList.remove(job);
                }
            };
            initialized = true;
        }
    }

    /**
     * Shut down the AlertAudioMgr
     *
     * Stops all playing audio and frees all resources
     */
    public static synchronized void shutDown() {
        if (initialized) {
            stopAllSound();
            timer.cancel();

            jobList = null;
            timer = null;
            closeListener = null;

            initialized = false;
        }
    }

    private static IAlertSoundJob createJob(File file) {
        IAlertSoundJob job;
        if (file != null) {
            try {
                job = new AlertAudioJob(file);
            } catch (AlertvizException e) {
                statusHandler.warn(String.format(
                        "Error creating AlertAudioJob for file: %s, using system beep instead",
                        file), e);
                job = new AlertBeepJob();
            }
        } else {
            job = new AlertBeepJob();
        }
        jobList.add(job);
        job.addFinishedListener(closeListener);
        return job;
    }

    /**
     * Play a sound once
     *
     * @param file
     *            file containing sound to be played
     * @return the audio job playing the sound
     */
    public static IAlertSoundJob playSound(File file) {
        IAlertSoundJob job = createJob(file);
        job.play();

        return job;
    }

    /**
     * Play the sound file in a continuous loop.
     *
     * @param file
     *            file containing sound to be played
     * @param duration
     *            how long to play sound in milliseconds
     * @return the audio job playing the sound
     */
    public static IAlertSoundJob loopSound(File file, Duration duration) {
        IAlertSoundJob job = createJob(file);
        job.loop();
        timer.schedule(new AudioJobKiller(job), duration.toMillis());

        return job;
    }

    /**
     * Stop a sound that is playing.
     *
     * @param job
     *            the audioJob to be stopped
     */
    public static void stopSound(IAlertSoundJob job) {
        job.kill();
    }

    /**
     * Stop all sounds that are playing
     */
    public static void stopAllSound() {
        for (IAlertSoundJob job : jobList) {
            job.kill();
        }
    }

    /**
     * Get the audio file defined by either the StatusMessage or the
     * AlertMetaData
     *
     * @param amd
     *            the AlertMetaData
     * @param statMsg
     *            the StatusMessage
     * @return the audioFile to be played or null if none
     */
    public static File getAudioFile(AlertMetadata amd, StatusMessage statMsg) {
        // first check statMsg audio file
        String fileName = StringUtils.trimToNull(statMsg.getAudioFile());
        if ("NONE".equals(fileName)) {
            // statMsg has no audio
            fileName = null;
        }

        // if statMsg has no audio, try amd
        if (fileName == null && amd.isAudioEnabled()) {
            fileName = StringUtils.trimToNull(amd.getAudioFile());
        }

        if (fileName == null) {
            return null;
        }

        File file = new File(fileName);
        if (!file.exists()) {
            file = PathManagerFactory.getPathManager().getStaticFile(
                    LocalizationUtil.join("alertViz", "audio", file.getName()));
            if (file != null && !file.exists()) {
                file = null;
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to locate audio file: " + fileName);
            }
        }
        return file;
    }

}
