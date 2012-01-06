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
package com.raytheon.viz.ui.statusline;

import java.util.Comparator;
import java.util.concurrent.ConcurrentSkipListSet;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * Objects of this class are widgets that resemble LEDs that can be off, on, or
 * blinking.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 11, 2008		#1223	randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class NewMessageIndicator {
    public static RGB DEFAULT_COLOR = new RGB(0, 255, 0);

    /**
     * Job to blink the indicator until stopTime is reached.
     */
    private static class BlinkJob extends Job {

        /**
         * @param name
         */
        public BlinkJob() {
            super("Blink");
            this.setSystem(true);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            // toggle the blink state
            blinkState = 1 - blinkState;

            // update the state of all active indicators
            Display.getDefault().syncExec(new Runnable() {

                @Override
                public void run() {
                    long now = System.currentTimeMillis();
                    for (NewMessageIndicator nmi : activeList) {
                        if (!nmi.indicator.isDisposed()) {
                            if (nmi.offTime <= 0 || nmi.offTime > now) {
                                if (nmi.blinking) {
                                    nmi.indicator
                                            .setBackground(nmi.color[blinkState]);
                                } else {
                                    nmi.indicator.setBackground(nmi.color[1]);
                                }
                            } else {
                                nmi.indicator.setBackground(nmi.color[0]);
                                activeList.remove(nmi);
                            }
                        }
                    }
                }

            });

            // if any indicators are still active reschedule
            if (activeList.size() > 0) {
                this.schedule(BLINK_PERIOD);
            }

            return Status.OK_STATUS;
        }
    }

    /** Relative intensity of off color to on color */
    private static final float OFF_PERCENT = 0.75f;

    /** half period of blink frequency */
    private static final long BLINK_PERIOD = 200;

    /** current blink sate, 0 = off, 1 = on */
    private static int blinkState = 0;

    private static ConcurrentSkipListSet<NewMessageIndicator> activeList = new ConcurrentSkipListSet<NewMessageIndicator>(
            new Comparator<NewMessageIndicator>() {

                @Override
                public int compare(NewMessageIndicator o1,
                        NewMessageIndicator o2) {
                    return o1.hashCode() - o2.hashCode();
                }

            });

    /** Job to blink the indicator */
    private static BlinkJob blinkJob = new BlinkJob();

    /** the indicator widget */
    private Label indicator;

    /** indicator colors, 0 = off, 1 = on */
    private Color color[];

    /** time to stop blinking, <=0 = never */
    private long offTime;

    /** true if indicator should blink when on */
    private boolean blinking;

    /**
     * Constructor using DEFAULT_COLOR
     * 
     * @param parent
     *            parent composite
     */
    public NewMessageIndicator(Composite parent) {
        this(parent, DEFAULT_COLOR);
    }

    /**
     * Constructor
     * 
     * @param parent
     *            parent composite
     * @param rgb
     *            indicator "On" color
     */
    public NewMessageIndicator(Composite parent, RGB rgb) {
        indicator = new Label(parent, SWT.BORDER);
        color = new Color[2];

        blinking = false;
        setColor(rgb);
        off();
    }

    /**
     * @see org.eclipse.swt.widgets.Label#setLayoutData(Object)
     */
    public void setLayoutData(Object layoutData) {
        indicator.setLayoutData(layoutData);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#finalize()
     */
    @Override
    protected void finalize() throws Throwable {
        dispose();
        super.finalize();
    }

    /**
     * dispose of all resources
     */
    public void dispose() {
        activeList.remove(this);

        indicator.dispose();

        if (color[1] != null) {
            color[1].dispose();
        }

        if (color[0] != null) {
            color[0].dispose();
        }
    }

    /**
     * set the indicator color
     * 
     * @param rgb
     */
    public void setColor(final RGB rgb) {
        if (color[1] != null) {
            color[1].dispose();
        }
        color[1] = new Color(indicator.getParent().getDisplay(), rgb);

        if (color[0] != null) {
            color[0].dispose();
        }
        color[0] = new Color(indicator.getParent().getDisplay(),
                (int) (rgb.red * OFF_PERCENT), (int) (rgb.green * OFF_PERCENT),
                (int) (rgb.blue * OFF_PERCENT));

        indicator.setBackground(color[0]);
    }

    /**
     * Turn the indicator on
     * 
     * @param timeOut
     * @param rgb
     */
    public void on(long timeOut, RGB rgb) {
        activate(timeOut, rgb, false);
    }

    /**
     * Blink the indicator
     * 
     * @param timeOut
     * @param rgb
     */
    public void blink(long timeOut, RGB rgb) {
        activate(timeOut, rgb, true);
    }

    /**
     * Activate the indicator
     * 
     * @param timeOut
     *            time in milliseconds indicator should remain on, 0 = forever
     * @param rgb
     *            indicator color, null = current;
     * @param blinking
     *            true if indicator should blink while on
     */
    public void activate(long timeOut, RGB rgb, boolean blinking) {
        if (rgb != null) {
            setColor(rgb);
        }
        this.blinking = blinking;

        // set the indicator to the appropriate color
        indicator.setBackground(color[blinking ? blinkState : 1]);

        // set the offTime, 0 means never
        offTime = timeOut > 0 ? System.currentTimeMillis() + timeOut : 0;

        // if it's blinking or needs to be timed out put it in the activeList
        // and start the job if necessary
        if (timeOut > 0 || blinking) {
            activeList.add(this);
            if (blinkJob.getState() == Job.NONE) {
                blinkJob.schedule();
            }
        }
    }

    /**
     * Turn the indicator off
     */
    public void off() {
        offTime = System.currentTimeMillis();
        activeList.remove(this);
        indicator.setBackground(color[0]);
    }

    /**
     * Test program
     * 
     * @param args
     */
    public static void main(String[] args) {
        Window window = new Window((Shell) null) {

            @Override
            protected Control createContents(Composite parent) {
                Composite comp = (Composite) super.createContents(parent);
                comp.setLayout(new GridLayout(4, true));

                NewMessageIndicator nmi1 = new NewMessageIndicator(comp,
                        new RGB(0, 255, 0));
                nmi1.setLayoutData(new GridData(20, 12));
                nmi1.activate(10000, new RGB(255, 0, 0), true);

                NewMessageIndicator nmi2 = new NewMessageIndicator(comp,
                        new RGB(0, 255, 0));
                nmi2.setLayoutData(new GridData(20, 12));
                nmi2.activate(0, null, true);

                NewMessageIndicator nmi3 = new NewMessageIndicator(comp,
                        new RGB(127, 127, 255));
                nmi3.setLayoutData(new GridData(20, 12));
                nmi3.activate(10000, null, false);

                NewMessageIndicator nmi4 = new NewMessageIndicator(comp,
                        new RGB(255, 255, 0));
                nmi4.setLayoutData(new GridData(20, 12));
                return comp;
            }

        };
        window.setBlockOnOpen(true);
        window.open();

    }
}
