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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.alertviz.INeedsSaveListener;
import com.raytheon.uf.viz.alertviz.config.MonitorMetadata;

/**
 * This class the controls for a specific priority.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 19 Jan 2011             cjeanbap    Initial creation.
 * 07 Feb 2013	15292	   Xiaochuan   Add modifyImageFile method, addModifyListener() 
 * 									   on imageText to response user deletion the name 
 * 									   of image file.
 * 									   	
 * </pre>
 * 
 * @author cjeanbap
 * @version 1.0
 * 
 */
public class MonitorControls {

    private Text imageText;

    private boolean omitMonitor;

    private Button changeImageBtn;

    private Composite parentComp;

    private MonitorMetadata monitorMetadata;

    private boolean imageChanged;

    private FileSelectDlg imageDlg;

    private INeedsSaveListener needsSaveListener;

    public MonitorControls(Composite parentComp,
            INeedsSaveListener needsSaveListener) {
        this.parentComp = parentComp;
        this.needsSaveListener = needsSaveListener;
        this.imageChanged = false;
    }

    public void createImageControls() {
        GridData gd = new GridData(SWT.FILL, SWT.TOP, true, true);
        // gd.horizontalSpan = 2;
        Composite monitorComp = new Composite(this.parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 2;
        monitorComp.setLayout(gl);
        monitorComp.setLayoutData(gd);

        // Filler
        new Label(monitorComp, SWT.NONE);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label monitorImageLabel = new Label(monitorComp, SWT.LEFT);
        monitorImageLabel.setText("Monitor Image: ");
        monitorImageLabel.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        imageText = new Text(monitorComp, SWT.BORDER | SWT.SINGLE);
                
        imageText.setLayoutData(gd);
        imageText.setText("");
        imageText.setEnabled(true);
        imageText.setToolTipText("Image file not available...");
        imageText.addModifyListener(new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		modifyImageFile();
			}
        });

        gd = new GridData(28, SWT.DEFAULT);
        changeImageBtn = new Button(monitorComp, SWT.PUSH);
        changeImageBtn.setText("...");
        changeImageBtn.setLayoutData(gd);
        changeImageBtn.setEnabled(true);
        changeImageBtn.setToolTipText("Change image file");
        changeImageBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                selectImageFile();
            }
        });

        // Filler
        new Label(monitorComp, SWT.NONE);

        gd = new GridData(200, SWT.DEFAULT);
        omitMonitor = false;
    }

    /**
     * Delete image file name from Text field.
     */
    private void modifyImageFile()
    {
    	String newFile = imageText.getText();

		if( newFile.equals(" ") || newFile.isEmpty()) {

			if (monitorMetadata != null) {
				String oldValue = monitorMetadata.getImageFile();
                imageChanged = (oldValue != null);
                monitorMetadata.setImageFile(null);
				needsSaveListener.saveNeeded(true);
			}
		}
    }
    
    /**
     * Select the image file.
     */
    private void selectImageFile() {
        String imageFile = imageText.getText();

        imageDlg = new FileSelectDlg(parentComp.getShell(),
                SWT.APPLICATION_MODAL, imageText, "monitorIcons",
                new String[] { ".png" });
        imageDlg.setSelectedFile(imageFile);
        Boolean retVal = (Boolean) imageDlg.open("Image Selection File",
                imageFile);

        if (retVal != null && retVal == true) {
            File selectedFile = imageDlg.getSelectedFile();
            if (selectedFile != null) {
                String fileName = selectedFile.getName();
                imageText.setText(fileName);
                imageChanged = false;
                if (monitorMetadata == null) {
                    monitorMetadata = new MonitorMetadata(fileName, false);
                    imageChanged = true;
                } else {
                    String oldValue = monitorMetadata.getImageFile();
                    imageChanged = !fileName.equals(oldValue);
                    monitorMetadata.setImageFile(fileName);
                }
            } else {
                if (monitorMetadata != null) {
                    String oldValue = monitorMetadata.getImageFile();
                    imageChanged = (oldValue != null);
                    monitorMetadata.setImageFile(null);
                }
            }
            if (imageChanged) {
                needsSaveListener.saveNeeded(true);
            }
        }
    }

    public boolean isImageChanged() {
        return this.imageChanged;
    }

    public void setMonitor(MonitorMetadata monitorMetadata, String imageFile,
            boolean omitMonitor) {
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
