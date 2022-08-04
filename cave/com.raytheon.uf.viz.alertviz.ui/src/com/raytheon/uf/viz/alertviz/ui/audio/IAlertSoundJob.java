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

/**
 * Interface for alert sound jobs
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Sep 20, 2018  7457     randerso  Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public interface IAlertSoundJob {
    /**
     * Interface for being notified when an audio job finishes
     */
    public interface IJobFinshedListener {
        /**
         * Called when an audio job finishes
         *
         * @param job
         *            the job that just finished
         */
        public void audioJobFinished(IAlertSoundJob job);
    }

    /**
     * Stop the audio and kill the job
     */
    void kill();

    /**
     * Play the associated audio clip once
     */
    void play();

    /**
     * Play the associated audio clip in a continuous loop
     */
    void loop();

    /**
     * Add a listener to be notified when the job finishes
     *
     * @param listener
     */
    void addFinishedListener(IJobFinshedListener listener);

}