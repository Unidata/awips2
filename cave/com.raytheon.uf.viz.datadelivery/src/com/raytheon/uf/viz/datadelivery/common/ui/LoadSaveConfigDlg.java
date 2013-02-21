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
package com.raytheon.uf.viz.datadelivery.common.ui;

import java.io.File;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.TreeMap;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.datadelivery.common.xml.IDisplayXml;
import com.raytheon.uf.viz.datadelivery.filter.config.xml.FilterSettingsXML;
import com.raytheon.uf.viz.datadelivery.filter.config.xml.FilterTypeXML;
import com.raytheon.uf.viz.datadelivery.notification.xml.NotificationConfigXML;
import com.raytheon.uf.viz.datadelivery.notification.xml.NotificationFilterXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.DateRangeTimeXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.SpecificDateTimeXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.SubsetXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.TimeXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.VerticalXML;
import com.raytheon.uf.viz.datadelivery.subscription.xml.SubscriptionManagerConfigXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Load Save Configuration Dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 23, 2012            jpiatt     Added file save action.
 * Jun  1, 2012    645     jpiatt     Added tooltips.
 * Jun 19, 2012    717     jpiatt     Save action update.
 * Aug 22, 2012   0743     djohnson   Add new TimeXML sub-classes.
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */

public class LoadSaveConfigDlg extends CaveSWTDialog {

    /**
     * Dialog Type Enumeration. Types include OPEN,
     * SAVE_AS, DELETE, SELECT_DEFAULT.
     *
     */
    public static enum DialogType {
        /**
         * Open dialog.
         */
        OPEN, 
        /**
         * Save as dialog.
         */
        SAVE_AS, 
        /**
         * Delete dialog.
         */
        DELETE, 
        /**
         * Default dialog.
         */
        SELECT_DEFAULT
        
    };

    /** DialogType object */
    private final DialogType dialogType;

    /** Control font object */
    private Font controlFont;

    /** Configuration file list */
    private List cfgFileList;

    /** Selected file */
    private LocalizationFile selectedFile = null;

    /** Localization file */
    private LocalizationFile[] locFiles;

    /** Localization file tree map */
    private TreeMap<String, LocalizationFile> locFileMap;

    /** New entered file name */
    private Text newFileNameTF;

    /** Action button */
    private Button actionBtn;

    /** Site button */
    private Button siteBtn;

    /** User button */
    private Button userBtn;

    /** Name to exclude */
    private String excludedNameForSaving = "";

    /** Path of the file */
    private final String fileNamePath;

    /** Composite list of preview files */
    private Composite listPreviewComp;

    /** Preview composite */
    private Composite previewComp;

    /** Preview file text */
    private StyledText previewTxt;

    /** Preview flag */
    private boolean previewOn = false;

    /** Preview button */
    private Button previewBtn;

    /** Configuration file */
    private final NotificationConfigXML xml = null;

    /** JAXB context */
    private JAXBContext jax;

    /** Marshaller object */
    private Marshaller marshaller;

    /** Unmarshaller object */
    private Unmarshaller unmarshaller;

    /** Show preview flag */
    private boolean showPreview = false;

    /**
     * Constructor with a flag determining if the preview pane should be shown.
     * 
     * @param parent
     *            Parent shell.
     * @param type
     *            Dialog type.
     * @param fileNamePath
     *            File name path.
     * @param excludedNameForSaving
     *            Exclude name for saving flag.
     * @param showPreview
     */
    public LoadSaveConfigDlg(Shell parent, DialogType type, String fileNamePath, String excludedNameForSaving,
            boolean showPreview) {
        super(parent, SWT.TITLE);

        if (type == DialogType.OPEN) {
            setText("Load Configuration");
        }
        else if (type == DialogType.SAVE_AS) {
            setText("Save Configuration");
        }
        else if (type == DialogType.SELECT_DEFAULT) {
            setText("Select Default");
        } else if (type == DialogType.DELETE) {
            setText("Delete Configuration");
        }

        this.showPreview = showPreview;

        dialogType = type;
        this.fileNamePath = fileNamePath;
        if (excludedNameForSaving != null) {
            this.excludedNameForSaving = excludedNameForSaving;
        }

        createContext();
    }

    /**
     * Constructor that does not show the preview pane.
     * 
     * @param parent
     *           parent shell
     * @param type
     *           dialog type
     * @param fileNamePath
     *           path of the file
     * @param excludedNameForSaving
     *            exclude name
     */
    public LoadSaveConfigDlg(Shell parent, DialogType type, String fileNamePath, String excludedNameForSaving) {
        this(parent, type, fileNamePath, excludedNameForSaving, false);
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
        // setReturnValue(selectedFile);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        locFileMap = new TreeMap<String, LocalizationFile>();
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        // Create the List and preview controls.
        createListPreviewComp();

        // Create the buttons at the bottom of the display.
        createBottomButtons();

        getAvailableConfigFiles();
    }

    /**
     * Create the preview composite.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createListPreviewComp() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        listPreviewComp = new Composite(shell, SWT.NONE);
        listPreviewComp.setLayout(new GridLayout(2, false));
        listPreviewComp.setLayoutData(gd);

        createListControl(listPreviewComp);
        if (showPreview) {
            createPreviewControl(listPreviewComp);
        }
    }

    /**
     * Create the list control.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createListControl(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite controlComp = new Composite(parentComp, SWT.NONE);
        controlComp.setLayout(new GridLayout(1, false));
        controlComp.setLayoutData(gd);

        Label listLbl = new Label(controlComp, SWT.NONE);
        listLbl.setText("Available Configuration Files:");

        gd = new GridData();
        gd.widthHint = 350;
        gd.heightHint = 350;
        cfgFileList = new List(controlComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        cfgFileList.setLayoutData(gd);
        cfgFileList.setFont(controlFont);
        cfgFileList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (dialogType == DialogType.SAVE_AS) {
                    String selItem = cfgFileList.getItem(cfgFileList.getSelectionIndex());

                    int idx = selItem.lastIndexOf(":");
                    String newStr = selItem.substring(idx + 1);

                    newFileNameTF.setText(newStr);
                    if (selItem.startsWith(LocalizationContext.LocalizationLevel.SITE.name())
                            || selItem.startsWith(LocalizationContext.LocalizationLevel.BASE.name())) {
                        siteBtn.setSelection(true);
                        userBtn.setSelection(false);
                    }
                    else {
                        userBtn.setSelection(true);
                        siteBtn.setSelection(false);
                    }
                }
                else if (dialogType == DialogType.OPEN) {
                    if (showPreview) {
                        updatePreview();
                    }
                }
            }
        });

        cfgFileList.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDoubleClick(MouseEvent e) {
                if ((e.button == 1) && (dialogType == DialogType.OPEN)) {
                    if (cfgFileList.getSelectionIndex() >= 0) {
                        loadAction();
                    }
                }
            }
        });

        if (dialogType == DialogType.SAVE_AS) {
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            gd.horizontalSpan = ((GridLayout)controlComp.getLayout()).numColumns;
            Label sepLbl = new Label(controlComp, SWT.SEPARATOR | SWT.HORIZONTAL);
            sepLbl.setLayoutData(gd);

            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            userBtn = new Button(controlComp, SWT.RADIO);
            userBtn.setText("User");
            userBtn.setLayoutData(gd);
            userBtn.setSelection(true);
            userBtn.setToolTipText("Save file under User");

            siteBtn = new Button(controlComp, SWT.RADIO);
            siteBtn.setText("Site");
            siteBtn.setLayoutData(gd);
            siteBtn.setToolTipText("Save file under Site");

            Label newFileLbl = new Label(controlComp, SWT.NONE);
            newFileLbl.setText("Enter file name:");

            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            newFileNameTF = new Text(controlComp, SWT.BORDER);
            newFileNameTF.setLayoutData(gd);
            newFileNameTF.setToolTipText("Enter a file name for the xml file");
        }

        if (dialogType == DialogType.OPEN && showPreview) {
            createPreviewButton(controlComp);
        }
    }

    /**
     * Create the preview control.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createPreviewControl(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        previewComp = new Composite(parentComp, SWT.NONE);
        previewComp.setLayout(new GridLayout(1, false));
        previewComp.setLayoutData(gd);

        Label previewLbl = new Label(previewComp, SWT.NONE);
        previewLbl.setText("Preview:");

        gd = new GridData();
        gd.widthHint = 350;
        gd.heightHint = 350;
        previewTxt = new StyledText(previewComp, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        previewTxt.setLayoutData(gd);

        previewComp.setVisible(false);
        ((GridData)previewComp.getLayoutData()).exclude = true;

        parentComp.layout();
    }

    /**
     * Create the preview button.
     * 
     * @param controlComp
     *            Parent composite.
     */
    private void createPreviewButton(Composite controlComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite previewBtnComp = new Composite(controlComp, SWT.NONE);
        previewBtnComp.setLayout(new GridLayout(1, false));
        previewBtnComp.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 120;
        previewBtn = new Button(previewBtnComp, SWT.PUSH);
        previewBtn.setText("Preview >>");
        previewBtn.setLayoutData(gd);
        previewBtn.setToolTipText("Preview XML file");
        previewBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handlePreviewAction();
            }
        });
    }

    private void createBottomButtons() {
        addSeparator(shell);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        actionBtn = new Button(buttonComp, SWT.PUSH);
        actionBtn.setLayoutData(gd);

        //Open Dialog
        if (dialogType == DialogType.OPEN) {
            actionBtn.setText("Load");
            actionBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    loadAction();
                }
            });
        }
        //Delete Dialog
        else if (dialogType == DialogType.DELETE) {
            actionBtn.setText("Delete");
            actionBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    openDeleteSelectAction();
                }
            });
        }
        //Save As Dialog
        else if (dialogType == DialogType.SAVE_AS) {
            actionBtn.setText("Save");
            actionBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    if (validateFileName() == true) {
                        saveAction();
                        close();
                    }
                }
            });
        }
        else if (dialogType == DialogType.SELECT_DEFAULT) {
            actionBtn.setText("Select");
            actionBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    openDeleteSelectAction();
                }
            });
        }

        gd = new GridData(100, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }
    
    /**
     * Perform a configuration delete action.
     */
    private void openDeleteSelectAction() {
        int selectedIndex = cfgFileList.getSelectionIndex();
        String str = cfgFileList.getItem(selectedIndex);

        if (dialogType == DialogType.DELETE) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            mb.setText("Delete");
            mb.setMessage("You are about to delete the file:\n\n" + str + "\n\nDo you wish to continue?");
            int result = mb.open();

            if (result == SWT.NO) {
                return;
            }
        }

        selectedFile = locFileMap.get(str);
        setReturnValue(selectedFile);
        close();
    }

    /**
     * Perform a configuration Load action.
     */
    private void loadAction() {
        if (cfgFileList.getSelectionCount() > 0) {
            int selectedIndex = cfgFileList.getSelectionIndex();
            String str = cfgFileList.getItem(selectedIndex);
            selectedFile = locFileMap.get(str);

            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext context = pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.USER);

            if (str.startsWith("SITE")) {
                context = pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
            }

            LocalizationFile locFile = pm.getLocalizationFile(context, selectedFile.getName());

            setReturnValue(locFile);

            close();
        }
    }

    /**
     * Perform a configuration Save action.
     */
    private void saveAction() {

        String fileName = fileNamePath + File.separator + newFileNameTF.getText();

        IPathManager pm = PathManagerFactory.getPathManager();

        // check if USER or SITE radio button
        LocalizationContext context = pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        if (siteBtn.getSelection() == true) {
            context = pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        }

        LocalizationFile locFile = pm.getLocalizationFile(context, fileName);

        if (locFile != null) {
            for (LocalizationFile lf: locFiles) {
                if (locFile.equals(lf)) {
                    String msg = locFile.getFile().getName() + " already exists.\n\nAre you sure you want to overwrite the file?";
                
                    int answer = DataDeliveryUtils.showMessage(getShell(), SWT.YES | SWT.NO, "Overwrite?",
                    msg);
                    
                    if (answer == SWT.YES) {
                        setReturnValue(locFile);
                    }
                } 
            }
            
            //set return value if null
            if (getReturnValue() == null) {
            	setReturnValue(locFile);
            }
        }
    }

    /**
     * Create the JAXB context
     */
    @SuppressWarnings("rawtypes")
    private void createContext() {
        Class[] classes = new Class[] { NotificationConfigXML.class, NotificationFilterXML.class,
                FilterSettingsXML.class, FilterTypeXML.class, SubscriptionManagerConfigXML.class,
                SubsetXML.class, TimeXML.class, SpecificDateTimeXML.class,
                DateRangeTimeXML.class, VerticalXML.class };

        try {
            jax = JAXBContext.newInstance(classes);
            this.unmarshaller = jax.createUnmarshaller();
            this.marshaller = jax.createMarshaller();
        } catch (JAXBException e) {
            // statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
            // e);
            e.printStackTrace();
        }
    }

    
    private boolean validateFileName() {
        StringBuffer strBuf = new StringBuffer(newFileNameTF.getText().trim());

        if (strBuf.length() == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Warning");
            mb.setMessage("No file name has been entered.");
            mb.open();
            return false;
        }

        if (strBuf.toString().matches("[A-Za-z0-9._-]+") == false) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Warning");
            mb.setMessage("File name contains invalid charaters.  The file name can only\n"
                    + "contain A-Z, a-z, 0-9, or periods, underscores, or dashes.");
            mb.open();
            return false;
        }

        if (strBuf.toString().compareTo(excludedNameForSaving) == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Warning");
            mb.setMessage("Cannot save to the base default file name.\n" + "Please choose anther file name.");
            mb.open();
            return false;
        }

        String[] listItems = cfgFileList.getItems();

        for (String listItem : listItems) {
            int idx = listItem.lastIndexOf("/");
            String fn = listItem.substring(idx + 1);

            if (fn.compareTo(strBuf.toString()) == 0) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.YES | SWT.NO);
                mb.setText("Warning");
                mb.setMessage("File name already exists.  Do you wish to overwrite\n" + "the existing file?.");
                int result = mb.open();

                if (result == SWT.NO) {
                    return false;
                }
            }
        }

        if (strBuf.toString().endsWith(".xml") == false) {
            strBuf.append(".xml");
            newFileNameTF.setText(strBuf.toString().trim());
        }

        return true;
    }

    /**
     * Get the available configuration files to list.
     */
    private void getAvailableConfigFiles() {
        String[] extensions = new String[] { ".xml" };
        IPathManager pm = PathManagerFactory.getPathManager();

        ArrayList<LocalizationContext> contextList = new ArrayList<LocalizationContext>();
        contextList.add(pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.SITE));
        contextList.add(pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.USER));
        locFiles = pm.listFiles(contextList.toArray(new LocalizationContext[contextList.size()]), fileNamePath,
                extensions, false, true);

        if (locFiles == null) {
            return;
        }

        for (int i = 0; i < locFiles.length; i++) {

            // only display the context plus file name
            String locFile = locFiles[i].getName();
            int idx = locFile.lastIndexOf("/");
            String newStr = locFile.substring(idx + 1);

            locFileMap.put(locFiles[i].getContext().getLocalizationLevel() + ":" + newStr, locFiles[i]);
        }

        for (String str : locFileMap.keySet()) {
            cfgFileList.add(str);
        }

        if (cfgFileList.getSelectionCount() > 0) {
            cfgFileList.setSelection(0);
        }
    }

    /**
     * Get the selected localization file.
     * 
     * @return Localization file
     */
    public LocalizationFile getSelectedFile() {
        return selectedFile;
    }

    /**
     * Adds a separator to an composite.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout)parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Action performed when clicking the preview button. This will change the
     * preview button text and show/hide the preview code.
     */
    private void handlePreviewAction() {
        previewOn = !previewOn;

        if (previewOn) {
            previewBtn.setText("<< Preview");
            previewComp.setVisible(true);
            ((GridData)previewComp.getLayoutData()).exclude = false;

            updatePreview();
        }
        else {
            previewBtn.setText("Preview >>");
            previewComp.setVisible(false);
            ((GridData)previewComp.getLayoutData()).exclude = true;
        }

        this.listPreviewComp.layout();
        shell.pack();
    }

    /**
     * Update the previewed file.
     */
    private void updatePreview() {
        int selectedIndex = cfgFileList.getSelectionIndex();

        if (selectedIndex < 0) {
            this.listPreviewComp.layout();
            shell.pack();
            return;
        }

        String str = cfgFileList.getItem(selectedIndex);
        selectedFile = locFileMap.get(str);

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.USER);

        if (str.startsWith("SITE")) {
            context = pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        }

        LocalizationFile locFile = pm.getLocalizationFile(context, selectedFile.getName());

        try {

            // TODO : Check for instance of IDisplayXml

            Object obj = unmarshaller.unmarshal(locFile.getFile());

            if (obj instanceof IDisplayXml) {
                IDisplayXml dispXml = (IDisplayXml)obj;
                previewTxt.setText(dispXml.getDisplayXmlString());
            }
            else {
                this.marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
                StringWriter sw = new StringWriter();
                this.marshaller.marshal(obj, sw);
                previewTxt.setText(sw.toString());
            }

        } catch (JAXBException e) {
            // statusHandler.handle(Priority.PROBLEM,
            // e.getLocalizedMessage(), e);
            e.printStackTrace();
        }
    }
}
