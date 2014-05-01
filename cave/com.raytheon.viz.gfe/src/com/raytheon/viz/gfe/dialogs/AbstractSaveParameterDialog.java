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
package com.raytheon.viz.gfe.dialogs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.GfeClientConfig;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.ui.HazardUIUtils;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Abstract base class for save parameter dialogs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2011            randerso    Initial creation
 * Oct 30, 2012 1298       rferrel     Code clean for non-blocking dialog.
 * Feb 13, 2013 1597       randerso    Made number of concurrent save threads a configurable value.
 *                                     Added logging to support GFE Performance metrics
 * Nov 20, 2013 2488       randerso    Changed to use DejaVu font
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public abstract class AbstractSaveParameterDialog extends CaveJFACEDialog
        implements DisposeListener {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractSaveParameterDialog.class);

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("GFE:");

    protected DataManager dataManager;

    protected List<Parm> modparms;

    protected boolean tempHazGrids;

    protected boolean hazardsModified;

    protected Parm hazParm;

    protected Composite master;

    protected Font font;

    private ITimer saveTimer;

    protected AbstractSaveParameterDialog(Shell parentShell,
            DataManager dataManager) {
        super(parentShell);
        this.dataManager = dataManager;
        this.modparms = new ArrayList<Parm>(Arrays.asList(dataManager
                .getParmManager().getModifiedParms()));

        // see if we have a temporary haz parm displayed
        this.tempHazGrids = HazardUIUtils.tempHazardsExist(dataManager);

        // see if Hazards WE is modified and if so save it
        this.hazParm = HazardUIUtils.hazardsWEModified(this.dataManager);
        this.hazardsModified = this.hazParm != null;
        this.saveTimer = TimeUtil.getTimer();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        master = (Composite) super.createDialogArea(parent);
        master.addDisposeListener(this);

        FontData fd = master.getDisplay().getSystemFont().getFontData()[0];
        // TODO not have hard coded font name
        fd.setName("DejaVu Sans Mono");
        font = new Font(master.getDisplay(), fd);

        initializeComponents();

        return master;
    }

    protected abstract void initializeComponents();

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.DisposeListener#widgetDisposed(org.eclipse.swt
     * .events.DisposeEvent)
     */
    @Override
    public void widgetDisposed(DisposeEvent e) {
        if (this.font != null) {
            this.font.dispose();
        }
    }

    /**
     * Callback for saving the parameters
     */
    protected void saveParms(List<Parm> parmsToSave) {
        saveTimer.reset();
        saveTimer.start();
        this.getShell().setEnabled(false);
        final Cursor origCursor = this.getShell().getCursor();
        this.getShell().setCursor(
                this.getShell().getDisplay().getSystemCursor(SWT.CURSOR_WAIT));

        final ConcurrentLinkedQueue<Parm> parms = new ConcurrentLinkedQueue<Parm>(
                parmsToSave);

        // spawn a top level job
        Job saveJob = new Job("SaveParameter") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                final AtomicBoolean allSuccessful = new AtomicBoolean(true);
                try {
                    GfeClientConfig cfg = GfeClientConfig.getInstance();
                    int saveThreads = cfg.getMaxSaveThreads();
                    final CountDownLatch latch = new CountDownLatch(saveThreads);

                    // spawn separate jobs top save parms
                    for (int i = 0; i < saveThreads; i++) {
                        new Job("Saving Parms") {
                            @Override
                            protected IStatus run(IProgressMonitor monitor) {
                                try {
                                    Parm parm = null;
                                    while ((parm = parms.poll()) != null) {
                                        String parmString = parm.getParmID()
                                                .toString();
                                        try {
                                            // save data
                                            if (!parm.saveParameter(true)) {
                                                allSuccessful.set(false);
                                            }
                                        } catch (Throwable e) {
                                            allSuccessful.set(false);
                                            statusHandler.handle(
                                                    Priority.ERROR,
                                                    "Error occurred saving parm "
                                                            + parmString, e);
                                        }
                                    }
                                } catch (Throwable e) {
                                    allSuccessful.set(false);
                                    statusHandler.handle(Priority.ERROR,
                                            e.getLocalizedMessage(), e);
                                } finally {
                                    latch.countDown();
                                }

                                return Status.OK_STATUS;
                            }
                        }.schedule();
                    }

                    latch.await();
                } catch (Throwable e) {
                    allSuccessful.set(false);
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                } finally {
                    if (!allSuccessful.get()) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Some grids were not saved. See log for details.");
                    }

                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            AbstractSaveParameterDialog.this.getShell()
                                    .setCursor(origCursor);
                            AbstractSaveParameterDialog.this
                                    .saveFinished(allSuccessful.get());
                        }
                    });
                }
                return Status.OK_STATUS;
            }
        };

        saveJob.setSystem(true);
        saveJob.schedule();
    }

    private void saveFinished(boolean success) {
        if (success) {
            super.okPressed();
        } else {
            super.cancelPressed();
        }

        saveTimer.stop();
        perfLog.logDuration("Save Forecast", saveTimer.getElapsedTime());
    }

}
