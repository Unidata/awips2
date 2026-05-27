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
import java.io.IOException;

import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.LineEvent;
import javax.sound.sampled.LineEvent.Type;
import javax.sound.sampled.LineListener;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.UnsupportedAudioFileException;

import com.raytheon.uf.viz.alertviz.AlertvizException;

/**
 * Job to play audio for alerts
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2018  7457     randerso     Initial creation
 *
 * </pre>
 *
 * @author randerso
 */
public class AlertAudioJob implements IAlertSoundJob {
    private File file;

    private Clip clip;

    /**
     * Constructor
     *
     * @param file
     *            the audio file to be played
     * @throws AlertvizException
     *             if an audio clip cannot be created
     */
    public AlertAudioJob(File file) throws AlertvizException {
        this.file = file;
        try {
            this.clip = AudioSystem.getClip();
            clip.open(AudioSystem.getAudioInputStream(file));
        } catch (LineUnavailableException e) {
            throw new AlertvizException("No audio resource available", e);
        } catch (IOException e) {
            throw new AlertvizException("Error reading audio file: " + file, e);
        } catch (UnsupportedAudioFileException e) {
            throw new AlertvizException(
                    "Unsupported audio format for file: " + file, e);
        }
    }

    @Override
    public void kill() {
        if (clip.isRunning()) {
            clip.stop();
        }
        if (clip.isOpen()) {
            clip.close();
        }
    }

    @Override
    public void play() {
        clip.addLineListener(new LineListener() {

            @Override
            public void update(LineEvent event) {
                if (event.getType().equals(Type.STOP)) {
                    clip.close();
                }
            }
        });
        clip.start();
    }

    @Override
    public void loop() {
        clip.loop(Clip.LOOP_CONTINUOUSLY);
    }

    @Override
    public void addFinishedListener(final IJobFinshedListener listener) {
        clip.addLineListener(new LineListener() {

            @Override
            public void update(LineEvent event) {
                if (event.getType().equals(Type.CLOSE)) {
                    listener.audioJobFinished(AlertAudioJob.this);
                }
            }
        });
    }

    @Override
    public String toString() {
        return "AudioJob(" + file + ")";
    }
}
