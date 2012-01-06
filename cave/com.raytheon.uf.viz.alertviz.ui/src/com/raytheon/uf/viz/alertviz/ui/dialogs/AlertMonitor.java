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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.config.Configuration;
import com.raytheon.uf.viz.alertviz.config.ConfigurationItem;
import com.raytheon.uf.viz.alertviz.config.ConfigurationMonitor;
import com.raytheon.uf.viz.alertviz.config.MonitorMetadata;
import com.raytheon.uf.viz.alertviz.config.Source;
import com.raytheon.uf.viz.alertviz.ui.Activator;

/**
 * Manage monitor icon and label in Alert viz toolbar.
 */

public class AlertMonitor {

    public static final RGB WHITE = new RGB(255, 255, 255);

    public static final RGB BLACK = new RGB(0, 0, 0);

    private Label label;

    private MonitorToolTip toolTip;

    private String monitorMessage;

    private final String name;

    private String imageName;

    private Image image;

    private ImageData imageData;

    private boolean isOmitted;

    private Priority priority;

    private RGB background;

    private RGB foreground;

    public AlertMonitor(String name, String imageName) {
        this.name = name;
        this.imageName = imageName;
        this.isOmitted = false;
        this.monitorMessage = defaultMessage();
        this.priority = null;
    }

    private String defaultMessage() {
        return "Nothing yet for " + getName();
    }

    public boolean isOmitted() {
        return isOmitted;
    }

    public void setOmit(boolean omitMonitor) {
        this.isOmitted = omitMonitor;
        if (omitMonitor) {
            this.dispose();
        }
    }

    public void setImageName(String imageName) {
        this.imageName = imageName;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    public void dispose() {
        if (this.image != null) {
            this.image.dispose();
        }
    }

    public void updateImage(StatusMessage statusMessage, Configuration config,
            Display display) {
        if (!this.isOmitted) {
            setPriority(statusMessage.getPriority(), config, display);
            this.setMonitorMessage(statusMessage.getMessage(), config, display);
        }
    }

    private void setPriority(Priority priority, Configuration config,
            Display display) {
        this.priority = priority;
        loadConfig(config, display);
    }

    private void updateImageData(Display display) {
        try {
            if (this.image != null) {
                this.image.dispose();
            }
            this.imageData = new ImageData(loadMonitorImage(this.imageName));

            PaletteData pd = this.imageData.palette;

            if (pd != null) {
                if (pd.colors == null) {
                    pd.colors = new RGB[2];
                }
                pd.colors[0] = this.foreground;
                pd.colors[1] = this.background;
            }
            this.image = new Image(display, this.imageData);
            if (this.label != null && !this.label.isDisposed()) {
                this.label.setImage(this.image);
            }
        } catch (SWTException e) {
            StringBuilder sb = new StringBuilder(
                    "Failed to load monitor image: ");
            sb.append(this.imageName);
            Activator.handler.handle(Priority.SIGNIFICANT, sb.toString(), e);
            this.image = null;
        }
    }

    private void loadConfig(Configuration config, Display display) {
        Source source = config.lookupSource(this.name);
        ConfigurationItem configItem = source.getConfigurationItem();
        AlertMetadata alertMeta = configItem.lookup(this.priority);

        if (alertMeta == null) {
            this.background = WHITE;
            this.foreground = BLACK;
        } else {
            this.background = alertMeta.getBackground();
            this.foreground = alertMeta.getForeground();
        }

        if (this.imageName == null) {
            ConfigurationMonitor configMonitor = source
                    .getConfigurationMonitor();
            MonitorMetadata monMeta = configMonitor.getMonitorMetadata();
            if (monMeta != null && monMeta.getImageFile() != null) {
                this.imageName = monMeta.getImageFile();
            }
        }
        updateImageData(display);
    }

    private void setMonitorMessage(String message, Configuration config,
            Display display) {
        this.monitorMessage = message;
        updateLabelData(config, display);
    }

    private void updateLabelData(Configuration config, Display display) {
        this.label.setData(MonitorToolTip.tooltipTextKey, this.monitorMessage);

        this.loadConfig(config, display);
        this.label.setBackground(new Color(display, background));
        this.label.setForeground(new Color(display, foreground));
    }

    public void init(Composite comp, final Configuration config,
            final Display display) {
        if (!this.isOmitted) {
            this.label = new Label(comp, SWT.NONE);
            updateLabelData(config, display);
            this.toolTip = new MonitorToolTip(this.label, false);
            this.label.addMouseTrackListener(new MouseTrackAdapter() {
                @Override
                public void mouseHover(MouseEvent e) {
                    toolTip.open();
                }
            });
            this.label.addMouseListener(new MouseListener() {
                @Override
                public void mouseUp(MouseEvent e) {
                    if (e.button == 2) {
                        // middle click, clear monitor event
                        reset(config, display);
                    }
                }

                @Override
                public void mouseDown(MouseEvent e) {
                }

                @Override
                public void mouseDoubleClick(MouseEvent e) {
                }
            });
        }
    }

    protected void reset(Configuration config, Display display) {
        this.priority = null;
        this.setMonitorMessage(defaultMessage(), config, display);
        loadConfig(config, display);
    }

    /**
     * Load the image (information) for moving the dialog.
     * 
     * @return The "Info" image.
     */
    private String loadMonitorImage(String imageFile) {
        IPathManager pm = PathManagerFactory.getPathManager();
        String subDir = "monitorIcons" + File.separatorChar + imageFile;
        File imgFile = pm.getStaticFile(subDir);
        return imgFile == null ? null : imgFile.getAbsolutePath();
    }
}
