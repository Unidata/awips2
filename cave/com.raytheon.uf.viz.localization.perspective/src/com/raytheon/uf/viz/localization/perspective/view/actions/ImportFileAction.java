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
package com.raytheon.uf.viz.localization.perspective.view.actions;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Import a file from the file system into localization
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 1, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ImportFileAction extends Action {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ImportFileAction.class);

    private static final String FORMAT_STRING = "The file '%s' already exists at the %s level and "
            + "will be deleted. Proceed?";

    private static final String ASTERISK = "*";

    private String directoryPath;

    private LocalizationType contextType;

	private String[] fileExtensionFilterArr;

    public ImportFileAction(LocalizationType contextType, String directoryPath) {
        super("Import File...");
        this.contextType = contextType;
        this.directoryPath = directoryPath;
    }

    public ImportFileAction(LocalizationType contextType, String directoryPath, String[] filter) {
    	this(contextType, directoryPath);
		if (filter != null) {
			this.fileExtensionFilterArr = new String[filter.length];
			for (int i = 0; i < filter.length; ++i) {
				if (filter[i] != null && filter[i].startsWith(".")) {
					// prepend an asterisk as required by FileDialog.
					this.fileExtensionFilterArr[i] = ASTERISK + filter[i];
				} else {
					this.fileExtensionFilterArr[i] = filter[i];
				}
			}
		}
	}

	/*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        Shell parent = VizWorkbenchManager.getInstance().getCurrentWindow()
                .getShell();
        FileDialog dialog = new FileDialog(parent);
        if (fileExtensionFilterArr != null) {
        	dialog.setFilterExtensions(fileExtensionFilterArr);
        }
        String fileToImport = dialog.open();
        if (fileToImport != null) {
            File importFile = new File(fileToImport);
            importFile(contextType, directoryPath, new File[] { importFile });
        }
    }

    public static boolean importFile(LocalizationType contextType,
            String directoryPath, File[] importFiles) {
        List<File> applicable = new ArrayList<File>(importFiles.length);
        for (File importFile : importFiles) {
            if (importFile.exists() && importFile.isDirectory() == false
                    && importFile.isHidden() == false) {
                applicable.add(importFile);
            }
        }
        List<LocalizationFile> localizationFiles = new ArrayList<LocalizationFile>(
                applicable.size());
        List<LocalizationFile> existing = new ArrayList<LocalizationFile>(
                applicable.size());

        for (File importFile : applicable) {
            String name = importFile.getName();
            String newFilePath = directoryPath + IPathManager.SEPARATOR + name;
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationFile lf = pm.getLocalizationFile(
                    pm.getContext(contextType, LocalizationLevel.USER),
                    newFilePath);
            localizationFiles.add(lf);
            if (lf.exists()) {
                existing.add(lf);
            }
        }

        List<LocalizationFile> skip = new ArrayList<LocalizationFile>();
        if (existing.size() > 0) {
            if (existing.size() > 1) {
                MultiConfirmDialog dialog = new MultiConfirmDialog(existing);
                dialog.open();
                existing.removeAll(dialog.getConfirmedFiles());
            } else {
                LocalizationFile file = existing.get(0);
                if (MessageDialog.openConfirm(VizWorkbenchManager.getInstance()
                        .getCurrentWindow().getShell(), "Confirm Overwrite",
                        String.format(FORMAT_STRING, file.getName(), file
                                .getContext().getLocalizationLevel()))) {
                    existing.clear();
                }
            }
            skip = existing;
        }

        int addCount = 0;
        for (int i = 0; i < applicable.size(); ++i) {
            File importFile = applicable.get(i);
            LocalizationFile lf = localizationFiles.get(i);
            if (skip.contains(lf) == false) {
                try {
                    lf.write(FileUtil.file2bytes(importFile));
                    ++addCount;
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error importing file into localization", e);
                }
            }
        }
        return addCount > 0;
    }

    private static class MultiConfirmDialog extends MessageDialog {

        private List<LocalizationFile> confirmedFiles;

        private List<LocalizationFile> existingFiles;

        private int curIdx = 0;

        /**
         * @param parentShell
         * @param dialogTitle
         * @param dialogTitleImage
         * @param dialogMessage
         * @param dialogImageType
         * @param dialogButtonLabels
         * @param defaultIndex
         */
        public MultiConfirmDialog(List<LocalizationFile> existingFiles) {
            super(VizWorkbenchManager.getInstance().getCurrentWindow()
                    .getShell(), "Confirm Overwrite", null, "",
                    MessageDialog.CONFIRM, new String[] { "Yes To All", "No",
                            "Cancel", "Yes" }, 0);
            this.existingFiles = existingFiles;
            this.confirmedFiles = new ArrayList<LocalizationFile>(
                    existingFiles.size());
            setShellStyle(getShellStyle() | SWT.SHEET);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.dialogs.MessageDialog#buttonPressed(int)
         */
        @Override
        protected void buttonPressed(int buttonId) {
            switch (buttonId) {
            case 0:
                // Yes To All
                confirmedFiles.clear();
                confirmedFiles.addAll(existingFiles);
                close();
                break;
            case 1:
                // No
                curIdx++;
                if (curIdx == existingFiles.size()) {
                    close();
                } else {
                    updateText();
                }
                break;
            case 2:
                // Cancel
                confirmedFiles.clear();
                close();
                break;
            case 3:
                // Yes
                confirmedFiles.add(existingFiles.get(curIdx));
                curIdx++;
                if (curIdx == existingFiles.size()) {
                    close();
                } else {
                    updateText();
                }
                break;
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.jface.dialogs.IconAndMessageDialog#createMessageArea(
         * org.eclipse.swt.widgets.Composite)
         */
        @Override
        protected Control createMessageArea(Composite composite) {
            Control ctrl = super.createMessageArea(composite);
            updateText();
            return ctrl;
        }

        private void updateText() {
            LocalizationFile file = existingFiles.get(curIdx);
            messageLabel.setText(String.format(FORMAT_STRING, file.getName(),
                    file.getContext().getLocalizationLevel()));
        }

        public List<LocalizationFile> getConfirmedFiles() {
            return confirmedFiles;
        }
    }
}
