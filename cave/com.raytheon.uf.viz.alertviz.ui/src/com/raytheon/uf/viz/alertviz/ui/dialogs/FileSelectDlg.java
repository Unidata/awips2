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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.ui.dialogs.DialogUtil;

/**
 * This class displays the file selection dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 05, 2008           lvenable  Initial creation.
 * Dec 01, 2010  5133     cjeanbap  Add functionality for restoring/displaying
 *                                  the selected sound file.
 * Dec 07, 2010  6531     cjeanbap  Refactored class to be an Abstract class.
 * Mar 02, 2011  5632     cjeanbap  Update Listbox title text
 * May 06, 2011  9101     cjeanbap  Changed Constructor method signature.
 * Nov 12, 2015  4834     njensen   Changed LocalizationOpFailedException to
 *                                  LocalizationException
 * Jan 29, 2016  5289     tgurney   Add missing close button in trim
 * Sep 21, 2016  5901     randerso  Fix dialog centering issue introduced in
 *                                  Eclipse 4
 * Sep 24, 2018  7481     randerso  Code cleanup.
 * Oct 04, 2018  7484     randerso  Changed to use AV_ADMIN for internal errors
 * Oct 08, 2018  7515     randerso  Adjusted priorities of AlertViz internal
 *                                  errors.
 * Oct 10, 2018  7511     randerso  Select and close when double clicked.
 * Nov 02, 2018  7600     randerso  Changes to support standard script files for
 *                                  AlertViz actions.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class FileSelectDlg extends Dialog {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FileSelectDlg.class, "AV_ADMIN", "AV_ADMIN");

    private static final String SELECT_ONE = "Select One";

    private static final String TITLE_FMT = "%s File Selection";

    private static final String LABEL_FMT = "Available %s Files:";

    /**
     * Dialog shell.
     */
    private Shell shell;

    /**
     * File selection list control.
     */
    private List fileList;

    /**
     * Return object when the shell is disposed.
     */
    private boolean returnObj = false;

    /**
     * Array of localization files.
     */
    private java.util.List<LocalizationFile> locFiles;

    private String[] fileExtensions;

    private String[] locFileExtensions;

    private String path;

    private int selectedIndex;

    private String[] filteredExtensions;

    private String fileType;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent shell.
     *
     * @param fileType
     *            Type of file (e.g. Audio, Image, Action)
     * @param path
     *            path
     * @param extensions
     *            array of extension to retrieve, null for any
     * @param filteredExtensions
     *            extensions to be hidden, null for none
     */
    public FileSelectDlg(Shell parent, String fileType, String path,
            String[] extensions, String[] filteredExtensions) {
        super(parent, SWT.APPLICATION_MODAL);
        this.fileType = fileType;
        this.selectedIndex = 0;
        this.path = path;
        if (extensions != null) {
            setFileExtensions(extensions);
        }

        this.filteredExtensions = filteredExtensions;

        try {
            getAvailableFiles();
        } catch (LocalizationException e) {
            statusHandler.warn(e.getLocalizedMessage(), e);
        }
    }

    /**
     * @return true if dialog is disposed
     */
    public boolean isDisposed() {
        return shell == null || shell.isDisposed();
    }

    /**
     * Open method used to display the dialog.
     *
     * @return true if OK, pressed, false if Cancel pressed
     */
    public boolean open() {
        Shell parent = getParent();
        Display display = parent.getDisplay();
        shell = new Shell(parent, getStyle() | SWT.DIALOG_TRIM);
        shell.setText(String.format(TITLE_FMT, fileType));

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        try {
            initializeComponents(fileType);
        } catch (LocalizationException e) {
            statusHandler.warn(e.getLocalizedMessage(), e);
        }

        shell.pack();
        DialogUtil.centerOnParentShell(parent, shell);
        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return returnObj;
    }

    /**
     * Initialize the controls on the display.
     *
     * @throws LocalizationException
     */
    private void initializeComponents(String fileType)
            throws LocalizationException {

        createListControl(fileType);
        // Create the buttons at the bottom of the display.
        createBottomButtons();
    }

    /**
     * Create the list control.
     *
     * @throws LocalizationException
     */
    private void createListControl(String fileType)
            throws LocalizationException {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));
        listComp.setLayoutData(gd);

        Label listLbl = new Label(listComp, SWT.NONE);
        listLbl.setText(String.format(LABEL_FMT, fileType));

        fileList = new List(listComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL | SWT.H_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = fileList.getItemHeight() * 10;
        fileList.setLayoutData(gd);
        fileList.add(SELECT_ONE);
        for (LocalizationFile locFile : locFiles) {
            fileList.add(locFile.getFile(false).getName());
        }
        fileList.select(getIndex());
        fileList.showSelection();
        fileList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                selectedIndex = fileList.getSelectionIndex();
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                selectedIndex = fileList.getSelectionIndex();
                returnObj = true;
                shell.close();
            }
        });
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, true));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                returnObj = true;
                shell.close();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                returnObj = false;
                shell.close();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button importNewBtn1 = new Button(buttonComp, SWT.PUSH);
        importNewBtn1.setText("Import...");
        importNewBtn1.setLayoutData(gd);
        importNewBtn1.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                FileDialog newFileDlg = new FileDialog(shell,
                        SWT.OPEN | SWT.SINGLE);
                newFileDlg.setFilterExtensions(fileExtensions);
                String newFileName = newFileDlg.open();
                if (newFileName != null) {
                    File newFile = new File(newFileName);

                    if (newFile.exists() && newFile.isFile()) {
                        IPathManager pm = PathManagerFactory.getPathManager();
                        LocalizationContext ctx = pm.getContext(
                                LocalizationType.CAVE_STATIC,
                                LocalizationLevel.SITE);
                        LocalizationFile locFile = pm.getLocalizationFile(ctx,
                                path + File.separator + newFile.getName());
                        try {
                            saveToLocalizationFile(newFile, locFile);

                            int index = Math.abs(
                                    Collections.binarySearch(locFiles, locFile))
                                    - 1;
                            if (index > locFiles.size()) {
                                locFiles.add(locFile);
                                fileList.add(newFile.getName());
                            } else {
                                locFiles.add(Math.abs(index), locFile);
                                fileList.add(newFile.getName(),
                                        Math.abs(index) + 1);
                            }
                            setSelectedFile(newFile.getName());
                        } catch (LocalizationException | IOException e) {
                            statusHandler.warn(e.getLocalizedMessage(), e);
                        }
                    }
                }
            }
        });
    }

    private void saveToLocalizationFile(File file, LocalizationFile locFile)
            throws IOException, LocalizationException {

        try (InputStream in = new FileInputStream(file);
                SaveableOutputStream out = locFile.openOutputStream()) {
            FileUtil.copy(in, out);
            out.save();
        }
    }

    /**
     * Get the list of available files.
     *
     * @throws LocalizationException
     */
    private void getAvailableFiles() throws LocalizationException {
        LocalizationFile[] files = PathManagerFactory.getPathManager()
                .listStaticFiles(this.path, locFileExtensions, false, true);

        locFiles = Arrays.stream(files).filter(
                s -> !StringUtils.endsWithAny(s.getPath(), filteredExtensions))
                .sorted().collect(Collectors.toCollection(ArrayList::new));
    }

    /**
     * Get the selected file name.
     *
     * @return The file name or null if none selected.
     */
    public String getSelectedFile() {
        return isSelected() ? locFiles.get(getLocIndex()).getFile().getName()
                : null;
    }

    private boolean isSelected() {
        return selectedIndex > 0;
    }

    private int getLocIndex() {
        return selectedIndex - 1;
    }

    private int getIndex() {
        return selectedIndex;
    }

    /**
     * Set the selected file list object based upon the file name.
     *
     * @param fileName
     *            The name of the file.
     */
    public void setSelectedFile(String fileName) {
        int index = 0;

        try {
            for (LocalizationFile locFile : locFiles) {
                if (locFile.getFile(false).getName().equals(fileName)) {
                    selectedIndex = index + 1;
                    if (fileList != null) {
                        fileList.select(selectedIndex);
                        fileList.showSelection();
                    }
                    break;
                }
                ++index;
            }
        } catch (LocalizationException e) {
            statusHandler.warn(e.getLocalizedMessage(), e);
        }
    }

    private void setFileExtensions(String[] extensions) {
        this.locFileExtensions = extensions.clone();
        this.fileExtensions = extensions.clone();
        for (int i = 0; i < this.fileExtensions.length; ++i) {
            if (!this.fileExtensions[i].startsWith("*")) {
                this.fileExtensions[i] = "*" + this.fileExtensions[i];
            }
        }
    }
}
