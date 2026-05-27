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

import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.viz.alertviz.INeedsSaveListener;
import com.raytheon.uf.viz.alertviz.config.MonitorMetadata;
import com.raytheon.uf.viz.alertviz.ui.dialogs.AlertVizTips.TIP;

/**
 * This class the controls for a specific priority.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Jan 19, 2011           cjeanbap   Initial creation.
 * Feb 07, 2013  15292    Xiaochuan  Add modifyImageFile method,
 *                                   addModifyListener() on imageText to
 *                                   response user deletion the name of image
 *                                   file.
 * Sep 24, 2018  7481     randerso   Code and GUI cleanup.
 * Nov 13, 2018  7512     randerso   Moved monitor icon files
 * Mar 13, 2019  7763     randerso   Moved tool tip text to a separate container
 *                                   class. Added additional tool tips that were
 *                                   missing.
 *
 * </pre>
 *
 * @author cjeanbap
 *
 */
public class MonitorControls {

    private Text imageText;

    private boolean omitMonitor;

    private Composite parentComp;

    private MonitorMetadata monitorMetadata;

    private boolean imageChanged;

    private FileSelectDlg imageDlg;

    private INeedsSaveListener needsSaveListener;

    /**
     * Constructor
     *
     * @param parentComp
     * @param needsSaveListener
     */
    public MonitorControls(Composite parentComp,
            INeedsSaveListener needsSaveListener) {
        this.parentComp = parentComp;
        this.needsSaveListener = needsSaveListener;
        this.imageChanged = false;
    }

    /**
     * create the monitor image controls
     */
    public void createImageControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Composite monitorComp = new Composite(this.parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.marginHeight = 0;
        monitorComp.setLayout(gl);
        monitorComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
        Label monitorImageLabel = new Label(monitorComp, SWT.LEFT);
        monitorImageLabel.setText("Monitor Image: ");
        monitorImageLabel.setLayoutData(gd);
        monitorImageLabel.setToolTipText(AlertVizTips.getTip(TIP.IMAGE));

        gd = new GridData(150, SWT.DEFAULT);
        imageText = new Text(monitorComp,
                SWT.BORDER | SWT.SINGLE | SWT.READ_ONLY);
        imageText.setBackground(monitorComp.getBackground());

        imageText.setLayoutData(gd);
        imageText.setText("");
        imageText.setEnabled(true);
        imageText.setToolTipText("Image file not available...");

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        Button changeImageBtn = new Button(monitorComp, SWT.PUSH);
        changeImageBtn.setText("...");
        changeImageBtn.setLayoutData(gd);
        changeImageBtn.setEnabled(true);
        changeImageBtn.setToolTipText("Change image file");
        changeImageBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectImageFile();
            }
        });

        omitMonitor = false;
    }

    /**
     * Select the image file.
     */
    private void selectImageFile() {
        String imageFile = imageText.getText();

        imageDlg = new FileSelectDlg(parentComp.getShell(), "Image",
                LocalizationUtil.join("alertViz", "monitorIcons"),
                new String[] { ".png" }, null);
        imageDlg.setSelectedFile(imageFile);
        boolean retVal = imageDlg.open();

        if (retVal) {
            String selectedFile = imageDlg.getSelectedFile();
            imageChanged = !StringUtils.equals(selectedFile, imageFile);

            if (selectedFile != null) {
                imageText.setText(selectedFile);
                if (monitorMetadata == null) {
                    monitorMetadata = new MonitorMetadata(selectedFile, false);
                } else {
                    monitorMetadata.setImageFile(selectedFile);
                }
            } else {
                imageText.setText("");
                if (monitorMetadata != null) {
                    monitorMetadata.setImageFile(null);
                }
            }
            if (imageChanged) {
                needsSaveListener.saveNeeded(true);
            }
        }
    }

    /**
     * @return true if image is changed
     */
    public boolean isImageChanged() {
        return this.imageChanged;
    }

    /**
     * Set the monitor data
     *
     * @param monitorMetadata
     * @param imageFile
     * @param omitMonitor
     */
    public void setMonitorData(MonitorMetadata monitorMetadata,
            String imageFile, boolean omitMonitor) {
        this.monitorMetadata = monitorMetadata;
        this.omitMonitor = omitMonitor;
        imageText.setText((imageFile == null ? new String("") : imageFile));

        if (imageDlg != null && !imageDlg.isDisposed()) {
            imageDlg.setSelectedFile(imageFile);
        }
    }

    /**
     * @return the omitMonitor
     */
    public boolean isOmitMonitor() {
        return omitMonitor;
    }
}
