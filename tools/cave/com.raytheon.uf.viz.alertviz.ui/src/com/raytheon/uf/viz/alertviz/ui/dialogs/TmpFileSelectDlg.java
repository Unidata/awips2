package com.raytheon.uf.viz.alertviz.ui.dialogs;

import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;

public abstract class TmpFileSelectDlg extends Dialog {

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
     * Sound file list control.
     */
    private List soundFileList;
    
    /**
     * Return object when the shell is disposed.
     */
    private Boolean returnObj = null;
    
    /**
     * Array of localization files.
     */
    private LocalizationFile[] locFiles;
    
    /**
     * Current sound file.
     */
    public File currentFile;
    
    /**
     * New sound file.
     */
    private File newFile;
    
    private String path;
    
    private String fileExtension;
    
    public TmpFileSelectDlg(Shell parent, int style) {
        super(parent, style);
    }
    
    /**
     * Open method used to display the dialog.
     * 
     * @param audioFile 
     *              The name of the audio file, if null default to index 0. 
     * @return True/False.
     */
    public Object open(String title, String actionFile) {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.TITLE);
        //shell.setText("Action File Selection");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();
        
        if (actionFile != null) {
            setSelectedAudioFile(actionFile);
        }

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
     */
    private void initializeComponents() {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createListControl();
        // Create the buttons at the bottom of the display.
        createBottomButtons();

        getAvailableFiles();
    }
    
    /**
     * Set the selected index of file list object based upon the 
     * audio file. 
     * 
     * @param audioFile The name of the audio file.
     */
    private void setSelectedAudioFile(String audioFile) {
        String[] audioFiles = soundFileList.getItems();
        int index = 0;
        for (int i = 0; i < audioFiles.length; i++) {
            if (audioFile.endsWith(audioFiles[i])) {
                index = i;
                break;
            }
        }
        soundFileList.select(index);
    }
    
    /**
     * Create the list control.
     */
    private void createListControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));
        listComp.setLayoutData(gd);

        Label listLbl = new Label(listComp, SWT.NONE);
        listLbl.setText("Available Sound Files:");

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 400;
        gd.heightHint = 400;
        gd.horizontalSpan = 2;
        soundFileList = new List(listComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL | SWT.H_SCROLL);
        soundFileList.setLayoutData(gd);
        soundFileList.setFont(controlFont);
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
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (soundFileList.getSelectionIndex() != 0) {
                    newFile = locFiles[soundFileList.getSelectionIndex()].getFile();
                } else {
                    newFile = locFiles[0].getFile();
                }
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
                returnObj = false;
                shell.dispose();
            }
        });
    }
    
    /**
     * Get the list of available sound files.
     */
    private void getAvailableFiles() {
        String[] extensions = new String[] { fileExtension };
        locFiles = PathManagerFactory.getPathManager().listStaticFiles(
                path, extensions, true, true);
        
        for (int i = 0; i < locFiles.length; i++) {
            soundFileList.add(locFiles[i].getName());
        }

        if (currentFile != null) {
            soundFileList.select(soundFileList.indexOf(currentFile
                    .getName()));
        }
    }
    
    /**
     * Get the selected sound file.
     * 
     * @return The sound file.
     */
    public File getSelectedFile() {
        return newFile;
    }
    
    public void setFileExtension(String fileExtension) {
        this.fileExtension = fileExtension;
    }
    
    public void setFile(String file) {
        if (file != null) {
            this.currentFile = new File(file);
        }
    }
    
    public void setFilesLocationPath(String path) {
        this.path = path;
    }
}