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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.progress.UIJob;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.config.ConfigCatalog;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Dialog to select the desired GFE config file
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket  Engineer  Description
 * ------------- ------- --------- ---------------------------------------------
 * Apr 30, 2009          randerso  Initial creation
 * Oct 30, 2012  1298    rferrel   Code cleanup for non-blocking dialog.
 * Oct 28, 2015  5054    randerso  Place GfeConfigDialog on current monitor if
 *                                 parent shell is not visible.
 * Nov 12, 2015  4834    njensen   Changed LocalizationOpFailedException to
 *                                 LocalizationException
 * Feb 05, 2016  5242    dgilling  Remove calls to deprecated Localization APIs.
 * Apr 21, 2017  6239    randerso  Prevent UI deadlock if an async task calls
 *                                 getPreferenceStore() while the dialog is
 *                                 open.
 * Jan 25, 2018  7153    randerso  Changes to allow new GFE config file to be
 *                                 selected when perspective is re-opened.
 * Apr 09, 2018  7260    randerso  Removed close button from title bar
 * Dec 09, 2019  7989    dgilling  Support changes to ConfigCatalog.
 * Jul 02, 2020  7597    randerso  Fix GUI issues exposed by Eclipse 4.16
 *                                 upgrade
 *
 * </pre>
 *
 * @author randerso
 */

public class GFEConfigDialog extends CaveJFACEDialog {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEConfigDialog.class);

    private static final String DEFAULT_CONFIG = "gfeConfig";

    private static final String LAST_CONFIG = LocalizationUtil.join("gfe",
            "lastConfig.txt");

    // Number of votes for Extra Photos
    private static final int[][] splashVotes = new int[][] { { 14, 10 },
            { 15, 8 }, { 16, 7 }, { 17, 5 }, { 18, 5 }, { 19, 8 }, { 20, 5 },
            { 21, 7 }, { 22, 7 }, { 23, 5 }, { 24, 6 }, { 25, 9 }, { 26, 6 },
            { 27, 9 }, { 28, 8 } };

    private static final int[][] splashDist = new int[splashVotes.length][2];

    // Percentage of time to show a winner
    private static final double winnerThreshold = .50;

    // Percentage of time to show an extra photo
    private static final double extrasThreshold = .25;

    // The remainder will be developer's choice photos
    private static final int totalPics = 45;

    private static final int totalVotes;

    static {
        int val = 0;
        for (int i = 0; i < splashVotes.length; i++) {
            val += splashVotes[i][1];
            splashDist[i][0] = splashVotes[i][0];
            splashDist[i][1] = val;
        }
        totalVotes = val;
    }

    private Set<Integer> usedImages = new HashSet<>(totalPics);

    private Image image;

    private Canvas imgCanvas;

    private Text configId;

    private String config;

    private java.util.List<String> availableConfigs;

    private Set<String> hiddenConfigs;

    private Button okButton;

    private List configList;

    private UpdateJob updateJob;

    private class UpdateJob extends UIJob {
        private static final long UPDATE_PERIOD = 5
                * TimeUtil.MILLIS_PER_SECOND;

        public UpdateJob() {
            super("GFEConfigUpdate");
            this.setSystem(true);
        }

        @Override
        public IStatus runInUIThread(IProgressMonitor monitor) {
            if ((imgCanvas != null) && !imgCanvas.isDisposed()) {
                Image oldImage = image;
                image = nextImage();
                imgCanvas.redraw();
                oldImage.dispose();

                this.schedule(UPDATE_PERIOD);
            }
            return Status.OK_STATUS;
        }
    }

    /**
     * @param parentShell
     */
    public GFEConfigDialog(Shell parentShell) {
        super(parentShell);
        super.setShellStyle(SWT.TITLE | SWT.BORDER);
        setReturnCode(CANCEL);

        ConfigCatalog configCatalog = new ConfigCatalog();
        Map<String, Boolean> inventory = configCatalog.getNames();
        this.availableConfigs = new ArrayList<>(inventory.keySet());
        Collections.sort(this.availableConfigs);
        this.hiddenConfigs = inventory.entrySet().stream()
                .filter(e -> e.getValue().booleanValue()).map(e -> e.getKey())
                .collect(Collectors.toSet());

        loadLastConfig();
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("GFE Startup");
    }

    @Override
    protected Point getInitialLocation(Point initialSize) {
        Point loc = super.getInitialLocation(initialSize);

        if (!getParentShell().isVisible()) {
            // center on current monitor
            Display display = getShell().getDisplay();
            Monitor[] monitors = display.getMonitors();

            Point cursor = display.getCursorLocation();
            for (Monitor m : monitors) {
                Rectangle b = m.getBounds();
                if (b.contains(cursor)) {
                    loc.x = b.x + ((b.width - initialSize.x) / 2);
                    loc.y = b.y + ((b.height - initialSize.y) / 2);
                    break;
                }
            }
        }

        return loc;
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);
        GridLayout layout = (GridLayout) comp.getLayout();

        image = nextImage();

        imgCanvas = new Canvas(comp, SWT.NONE);
        Rectangle rect = image.getBounds();
        GridData gridData = new GridData(rect.width, rect.height);
        imgCanvas.setLayoutData(gridData);

        imgCanvas.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                updateJob.cancel();
                image.dispose();
            }

        });

        imgCanvas.addPaintListener(new PaintListener() {

            @Override
            public void paintControl(PaintEvent e) {
                e.gc.drawImage(image, 0, 0);
            }

        });

        Composite configComp = new Composite(comp, SWT.NONE);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        configComp.setLayoutData(layoutData);
        layout = new GridLayout(1, true);
        configComp.setLayout(layout);

        Label configLabel = new Label(configComp, SWT.NONE);
        configLabel.setText("Config");

        configList = new List(configComp,
                SWT.SINGLE | SWT.V_SCROLL | SWT.BORDER);
        Set<String> configSet = new HashSet<>(availableConfigs);
        configSet.removeAll(hiddenConfigs);
        String[] configs = configSet.toArray(new String[0]);
        Arrays.sort(configs, String.CASE_INSENSITIVE_ORDER);

        for (String config : configs) {
            configList.add(config);
        }
        configList.setSelection(0);
        /*
         * Call to getItemHeight must be after populating list to work around
         * https://bugs.eclipse.org/bugs/show_bug.cgi?id=563189
         */
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.heightHint = configList.getItemHeight() * 10;
        configList.setLayoutData(layoutData);

        Label identifier = new Label(configComp, SWT.NONE);
        identifier.setText("Identifier");

        configId = new Text(configComp, SWT.SINGLE | SWT.BORDER);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        configId.setLayoutData(layoutData);

        int index = configList.indexOf(config);
        if (index < 0) {
            configList.deselectAll();
        } else {
            configList.setSelection(index);
        }
        configId.setText(config);

        configList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                int index = configList.getSelectionIndex();
                if (index >= 0) {
                    configId.setText(configList.getItem(index));
                }
                okPressed();
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = configList.getSelectionIndex();
                if (index >= 0) {
                    configId.setText(configList.getItem(index));
                }
            }
        });

        configId.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                validateConfigIdText();
            }
        });

        updateJob = new UpdateJob();
        updateJob.schedule(UpdateJob.UPDATE_PERIOD);

        return comp;
    }

    /**
     * Validate the file name in configId against the available configuration
     * files. The user can type a hidden config file's name into the control, so
     * validation has to be done against catalog.getFiles() rather than
     * catalog.getNames().
     */
    protected void validateConfigIdText() {
        String name = configId.getText();

        okButton.setEnabled(this.availableConfigs.contains(name));

        // keep configList in synch
        int index = configList.indexOf(name);
        if (index < 0) {
            configList.deselectAll();
        } else {
            configList.setSelection(index);
        }
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        okButton = createButton(parent, IDialogConstants.OK_ID,
                IDialogConstants.OK_LABEL, true);
        validateConfigIdText();
    }

    /**
     *
     */
    private Image nextImage() {
        int imageNumber = randomSplash();
        String imageName = "SSa" + imageNumber + ".gif";
        ImageDescriptor id = AbstractUIPlugin.imageDescriptorFromPlugin(
                Activator.PLUGIN_ID, FileUtil.join("images", imageName));
        return id.createImage();
    }

    private int randomSplash() {

        Random random = new Random();

        int pic = 0;
        while (true) {
            double seed = random.nextDouble();
            if (seed <= winnerThreshold) {
                pic = random.nextInt(13) + 1;
            } else if (seed <= (winnerThreshold + extrasThreshold)) {
                // Extras
                int newSeed = random.nextInt(totalVotes);
                for (int[] element : splashDist) {
                    pic = element[0];
                    if (newSeed <= element[1]) {
                        break;
                    }
                }
            } else {
                // Developer's choice
                pic = random.nextInt((45 - 29) + 1) + 29;
            }
            if (!usedImages.contains(pic)) {
                break;
            }
        }

        // Special cases
        Calendar today = Calendar.getInstance();
        Date now = SimulatedTime.getSystemTime().getTime();
        today.setTime(now);
        int month = today.get(Calendar.MONTH);
        int date = today.get(Calendar.DAY_OF_MONTH);
        if ((month == Calendar.OCTOBER) && (date == 31)) {
            // Halloween
            pic = 50;
        } else if ((month == Calendar.JULY) && (date == 4)) {
            // Independence Day
            pic = 49;
        }

        usedImages.add(pic);
        if (usedImages.size() >= totalPics) {
            usedImages.clear();
        }

        return pic;
    }

    @Override
    protected void okPressed() {
        Display display = this.getShell().getDisplay();

        config = configId.getText();
        super.okPressed();

        /* allow repaint so dialog doesn't appear to hang */
        display.update();

        /* load the selected python preferences */
        Activator.getDefault().loadConfiguration(config);
        statusHandler.info("GFE started with configuration: " + config);

        /*
         * save the last selected config to be used as the default next time GFE
         * is started
         */
        saveLastConfig();
    }

    private void saveLastConfig() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        ILocalizationFile lf = pathMgr.getLocalizationFile(context,
                LAST_CONFIG);

        try (SaveableOutputStream outStream = lf.openOutputStream();
                Writer out = new BufferedWriter(
                        new OutputStreamWriter(outStream))) {
            out.write(config);
            out.close();
            outStream.save();
        } catch (IOException e) {
            statusHandler.error("Error writing config file selection", e);
        } catch (LocalizationException e) {
            statusHandler.error("Error saving config file selection", e);
        }
    }

    private void loadLastConfig() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        ILocalizationFile lf = pathMgr.getLocalizationFile(context,
                LAST_CONFIG);

        config = null;
        if (lf.exists()) {
            try (InputStream inStream = lf.openInputStream();
                    BufferedReader in = new BufferedReader(
                            new InputStreamReader(inStream))) {
                config = in.readLine();
            } catch (IOException | LocalizationException e) {
                statusHandler.error("Error loading config file selection", e);
                config = null;
            }
        }

        if (config == null) {
            config = DEFAULT_CONFIG;
        } else if (hiddenConfigs.contains(config)) {
            config = "";
        }
        return;
    }

    @Override
    public boolean close() {
        if (getReturnCode() == OK) {
            return super.close();
        }
        return false;
    }
}
