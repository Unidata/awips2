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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Button;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;

/**
 * SiteBlinker (Site Blinker Alert) class. Use to blink site buttons during an
 * alert.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2010            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class SiteBlinker {

    /**
     * The singleton instance of this class.
     */
    static private SiteBlinker instance = null;

    /**
     * The key is the site button to blink and the object is how many more
     * seconds the button should blink.
     */
    private Map<Button, AtomicInteger> siteMap = new HashMap<Button, AtomicInteger>();

    /**
     * Toggle to control background color.
     */
    private int blinkState = 0;

    /**
     * When not null the active timer that handles the blinking.
     */
    private Timer timer = null;

    /**
     * Get the single instance of the site blinker.
     * 
     * @return siteBlinker
     */
    public static SiteBlinker getInstance() {
        if (instance == null) {
            instance = new SiteBlinker();
        }
        return instance;
    }

    /**
     * There can be only one so make constructor private.
     */
    private SiteBlinker() {
        super();
    }

    /**
     * Start a timer with a task to handle blinking.
     */
    private void startTimer() {
        timer = new Timer();

        TimerTask timerTask = new TimerTask() {
            public void run() {

                if (noSites()) {
                    return;
                }

                VizApp.runAsync(new Runnable() {
                    public void run() {
                        blinkState = (blinkState + 1) % 2;
                        ResourceConfigMgr configMgr = ResourceConfigMgr
                                .getInstance();

                        // These can change so must get them every time.
                        Color defBackground = configMgr
                                .getDefaultBackgroundColor();

                        Color blinkBackground = defBackground;
                        if (blinkState == 1) {
                            blinkBackground = configMgr
                                    .getDefaultBackgroundOffsetColor();
                        }

                        toggleBlink(defBackground, blinkBackground);
                    }
                });
            }
        };
        timer.schedule(timerTask, 0L, 1000L);
    }

    // These synchronized methods keeps the list of Buttons and the Timer
    // working properly.

    /**
     * Adds the site button to the list to be blinked; when already on the list
     * resets number of seconds the button should blink. When needed starts the
     * timer.
     * 
     * @param siteIdBtn
     *            site button to blink
     * @param seconds
     *            Number of seconds to blink the button
     */
    public synchronized void add(Button siteIdBtn, int seconds) {
        if (siteIdBtn == null) {
            return;
        }

        siteMap.put(siteIdBtn, new AtomicInteger(seconds));

        if (timer == null) {
            startTimer();
        }
    }

    /**
     * Removes a site button from the blink list and restores its normal
     * background.
     * 
     * @param siteIdBtn
     *            site button to remove
     */
    public synchronized void remove(Button siteIdBtn) {
        if (siteIdBtn != null && siteMap.containsKey(siteIdBtn)) {
            // This allows proper setting of stieIdBtn's background and timer
            // clean up.
            add(siteIdBtn, 0);
        }
    }

    /**
     * Check for blinking sites, and when needed stops the timer.
     * 
     * @return true when no sites remain to blink otherwise false
     */
    private synchronized boolean noSites() {
        boolean state = siteMap.isEmpty();
        if (state && (timer != null)) {
            timer.cancel();
            timer = null;
        }
        return state;
    }

    /**
     * Changes site buttons background color. This should only be called from
     * the UI thread.
     * 
     * @param defBackground
     *            The color to restore buttons to when done blinking
     * @param blinkBackground
     *            The new blink color
     */
    private synchronized void toggleBlink(Color defBackground,
            Color blinkBackground) {
        Iterator<Button> iter = siteMap.keySet().iterator();
        while (iter.hasNext()) {
            Button siteIdBtn = iter.next();
            if (siteIdBtn.isDisposed()) {
                iter.remove();
            } else if (siteMap.get(siteIdBtn).getAndDecrement() <= 0) {
                iter.remove();
                siteIdBtn.setBackground(defBackground);
            } else {
                siteIdBtn.setBackground(blinkBackground);
            }
        }
        noSites();
    }
}