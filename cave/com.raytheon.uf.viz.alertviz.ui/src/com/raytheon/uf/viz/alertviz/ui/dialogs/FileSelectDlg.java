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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.Vector;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
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
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * This class displays the file selection dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Oct 2008             lvenable    Initial creation.
 * 01 Dec 2010  5133       cjeanbap    Add functionality for restoring/displaying the
 *                                     selected sound file.
 * 07 Dec 2010  6531       cjeanbap    Refactored class to be an Abstract class.
 * 02 Mar 2011  5632       cjeanbap    Update Listbox title text
 * 06 May 2011  9101       cjeanbap    Changed Constructor method signature.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class FileSelectDlg extends Dialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FileSelectDlg.class, "GDN_ADMIN", "GDN_ADMIN");

    private static final String SELECT_ONE = "Select One";

    /**
     * Dialog shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    private Display display;

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * File selection list control.
     */
    private List fileList;

    /**
     * Return object when the shell is disposed.
     */
    private Boolean returnObj = null;

    /**
     * Array of localization files.
     */
    private java.util.List<LocalizationFile> locFiles;

    private String[] fileExtensions;

    private String[] locFileExtensions;

    private String path;

    private int selectedIndex;

    private Text associatedTextBox;

    @SuppressWarnings("unused")
    private boolean useExtension;

    private Map<String, String> filteredExtensions;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param style
     *            SWT Style.
     * @param path
     *            path
     * @param extensions
     *            array of extension to retrieve
     * @param useExtension
     *            load all files from the path
     */
    public FileSelectDlg(Shell parent, int style, String path,
            String[] extensions, boolean useExtension,
            Map<String, String> filteredExtentions) {
        super(parent, style);
        this.selectedIndex = 0;
        this.path = path;
        this.useExtension = useExtension;
        if (useExtension) {
            setFileExtensions(extensions);
        }

        this.filteredExtensions = filteredExtentions;
        try {
            getAvailableFiles();
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    public FileSelectDlg(Shell parent, int style, String path,
            String[] extensions, String selectedFile) {
        this(parent, style, path, extensions, true, null);
    }

    public FileSelectDlg(Shell parent, int style, Text text, String path,
            String[] extensions) {
        this(parent, style, path, extensions, true, null);
        this.associatedTextBox = text;
    }

    public FileSelectDlg(Shell parent, int style, Text text, String path,
            String[] extensions, String selectedFile) {
        this(parent, style, text, path, extensions);
    }

    public boolean isDisposed() {
        return !(fileList != null && !fileList.isDisposed());
    }

    /**
     * Open method used to display the dialog.
     * 
     * @param file
     *            The name of the file, if null default to index 0.
     * @return True/False.
     */
    public Object open(String title, String file) {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.TITLE);
        shell.setText(title);

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        try {
            initializeComponents();
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        setSelectedFile(file);

        shell.pack();
        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        controlFont.dispose();

        return returnObj;
    }

    /**
     * Initialize the controls on the display.
     * 
     * @throws LocalizationException
     */
    private void initializeComponents() throws LocalizationException {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createListControl();
        // Create the buttons at the bottom of the display.
        createBottomButtons();
    }

    /**
     * Create the list control.
     * 
     * @throws LocalizationException
     */
    private void createListControl() throws LocalizationException {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));
        listComp.setLayoutData(gd);

        Label listLbl = new Label(listComp, SWT.NONE);
        int pos = shell.getText().indexOf(" ");
        listLbl.setText("Available " + shell.getText().substring(0, pos)
                + " Files:");

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 400;
        gd.heightHint = 400;
        gd.horizontalSpan = 2;
        fileList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL
                | SWT.H_SCROLL);
        fileList.setLayoutData(gd);
        fileList.setFont(controlFont);
        fileList.add(SELECT_ONE);
        for (LocalizationFile locFile : locFiles) {
            fileList.add(locFile.getFile(false).getName());
        }
        fileList.select(getIndex());
        fileList.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                selectedIndex = fileList.getSelectionIndex();
                if (associatedTextBox != null) {
                    LocalizationFile selected = null;

                    if (isSelected()) {
                        selected = locFiles.get(getLocIndex());
                    }
                    String name = null;
                    if (selected == null) {
                        name = "";
                    } else {
                        name = selected.getFile().getName();
                    }
                    associatedTextBox.setText(name);
                }
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                returnObj = true;
                shell.dispose();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (isSelected()) {
                    returnObj = false;
                } else {
                    returnObj = null;
                }

                shell.dispose();
            }
        });
        gd = new GridData(100, SWT.DEFAULT);
        Button importNewBtn1 = new Button(buttonComp, SWT.PUSH);
        importNewBtn1.setText("Import");
        importNewBtn1.setLayoutData(gd);
        importNewBtn1.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                FileDialog newFileDlg = new FileDialog(shell, SWT.OPEN
                        | SWT.SINGLE);
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
                            if (saveToLocalizationFile(newFile, locFile)) {
                                int index = Math.abs(Collections.binarySearch(
                                        locFiles, locFile)) - 1;
                                if (index > locFiles.size()) {
                                    locFiles.add(locFile);
                                    fileList.add(newFile.getName());
                                } else {
                                    locFiles.add(Math.abs(index), locFile);
                                    fileList.add(newFile.getName(),
                                            Math.abs(index) + 1);
                                }
                                setSelectedFile(newFile.getName());
                            }
                        } catch (LocalizationOpFailedException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        } catch (IOException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        }
                    }
                }
            }
        });
    }

    private boolean saveToLocalizationFile(File file, LocalizationFile locFile)
            throws IOException, LocalizationOpFailedException {
        File newFile = locFile.getFile();
        InputStream in = new FileInputStream(file);
        OutputStream out = new FileOutputStream(newFile);
        byte[] buff = new byte[1024];
        int len;

        while ((len = in.read(buff)) > 0) {
            out.write(buff, 0, len);
        }
        in.close();
        out.close();
        return locFile.save();
    }

    /**
     * Get the list of available files.
     * 
     * @throws LocalizationException
     */
    private void getAvailableFiles() throws LocalizationException {
        locFiles = new ArrayList<LocalizationFile>();
        LocalizationFile[] files = null;

        files = PathManagerFactory.getPathManager().listStaticFiles(this.path,
                locFileExtensions, false, true);

        if (filteredExtensions != null) {
            Vector<Object> v = new Vector<Object>();
            for (LocalizationFile lf : files) {
                String filename = lf.getFile().getAbsoluteFile()
                        .getAbsolutePath();
                int index = filename.indexOf(".");
                if (index > 0) {
                    String fileExtension = filename.substring(index);
                    if (!this.filteredExtensions.containsKey(fileExtension)) {
                        v.add(lf);
                    }
                }
            }

            LocalizationFile[] tFiles = new LocalizationFile[v.size()];
            for (int i = 0; i < v.size(); i++) {
                tFiles[i] = (LocalizationFile) v.get(i);
            }

            files = tFiles;
        }

        locFiles.addAll(Arrays.asList(files));
        Collections.sort(locFiles);
    }

    /**
     * Get the selected file.
     * 
     * @return The file.
     */
    public File getSelectedFile() {
        return isSelected() ? locFiles.get(getLocIndex()).getFile() : null;
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
                    }
                    break;
                }
                ++index;
            }
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    public void setFileExtensions(String[] extensions) {
        this.locFileExtensions = extensions.clone();
        this.fileExtensions = extensions.clone();
        for (int i = 0; i < this.fileExtensions.length; ++i) {
            if (!this.fileExtensions[i].startsWith("*")) {
                this.fileExtensions[i] = "*" + this.fileExtensions[i];
            }
        }
    }

    public void setFilesLocationPath(String path) {
        this.path = path;
    }
}
