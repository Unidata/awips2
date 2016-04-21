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
package com.raytheon.viz.avnconfig;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;

import org.apache.commons.configuration.ConfigurationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.text.AfosWmoIdDataContainer;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.dataplugin.text.request.GetPartialAfosIdRequest;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TAF product configuration dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 MAY 2008  1119       lvenable    Initial creation
 *  9 Jul 2010  5078       rferrel     Added catches for File Not Found.
 *  1 Oct 2010  4345       rferrel     Cleanup to work like AWIPS I.
 * 12 Oct 2012  1229       rferrel     Convert to CaveSWTDialog subclass
 *                                      and make non-blocking.
 * 15 OCT 2012  1229       rferrel     Changes for non-blocking HelpUsageDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TafProductConfigDlg extends CaveSWTDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TafProductConfigDlg.class);

    /**
     * Composite containing message status controls.
     */
    private MessageStatusComp msgStatusComp;

    /**
     * Products text control.
     */
    private Text productsTF;

    /**
     * Products list control.
     */
    private List productsList;

    /**
     * Idents text control.
     */
    private Text identsTF;

    /**
     * Idents list control.
     */
    private List identsList;

    /**
     * Worl PIL text control.
     */
    private Text workPilTF;

    /**
     * Collective PIL text control.
     */
    private Text collectivePilTF;

    /**
     * Font for the list controls.
     */
    private Font listFont;

    /**
     * List of TAF products
     */
    private Map<String, java.util.List<String>> productsMap;

    private HelpUsageDlg usageDlg;

    /**
     * Constructor.
     * 
     * @param parent
     */
    public TafProductConfigDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK);
        setText("AvnFPS TAF Product Configuration");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        mainLayout.verticalSpacing = 5;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    @Override
    protected void disposed() {
        listFont.dispose();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        Display display = getParent().getDisplay();
        listFont = new Font(display, "Monospace", 10, SWT.NORMAL);

        createBottomMessageControls();
        createMainLayout();
    }

    /**
     * Create the main layout of the display.
     */
    private void createMainLayout() {
        Composite mainControlComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainControlComp.setLayout(new GridLayout(3, false));
        mainControlComp.setLayoutData(gd);

        createProductControls(mainControlComp);

        createIdentControls(mainControlComp);

        createButtonControls(mainControlComp);
    }

    /**
     * Create the Product controls.
     * 
     * @param mainControlComp
     *            Main composite.
     */
    private void createProductControls(Composite mainControlComp) {
        // -----------------------------------------------
        // Create the Products group and controls
        // -----------------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group productsGroup = new Group(mainControlComp, SWT.NONE);
        productsGroup.setText(" Products ");
        GridLayout gl = new GridLayout(1, false);
        productsGroup.setLayout(gl);
        productsGroup.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label productsLbl = new Label(productsGroup, SWT.CENTER);
        productsLbl.setText("Products");
        productsLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        productsTF = new Text(productsGroup, SWT.BORDER);
        productsTF.setLayoutData(gd);
        productsTF.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                if (e.character == SWT.CR) {
                    String tafProduct = productsTF.getText().trim();
                    java.util.List<String> product = productsMap
                            .get(tafProduct);

                    if (product == null) {
                        msgStatusComp.setMessageText("Product " + tafProduct
                                + " does not exist, it will be created.",
                                new RGB(40, 240, 120));

                        productsList.add(tafProduct);
                        productsMap.put(tafProduct, new ArrayList<String>());
                        workPilTF.setText("XXXWRKTAF");
                        identsList.removeAll();
                    }
                    productsList.setSelection(productsList.indexOf(tafProduct));
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 300;
        gd.widthHint = 150;
        productsList = new List(productsGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL | SWT.H_SCROLL);
        productsList.setLayoutData(gd);
        productsList.setFont(listFont);

        try {
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            productsMap = config.getAllProducts();

            if (productsMap != null) {
                java.util.List<String> keySet = config.getProductList();

                for (String key : keySet) {
                    productsList.add(key);
                }
            }
        } catch (FileNotFoundException e) {
            msgStatusComp.setMessageText(e.getMessage(), new RGB(255, 0, 0));
        } catch (ConfigurationException e) {
            msgStatusComp.setMessageText(
                    "An error occured when loading the default product.",
                    new RGB(255, 0, 0));
        }

        productsList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (productsList.getItemCount() == 0) {
                } else if (productsList.getSelectionIndex() < 0) {
                } else {
                    String product = productsList.getItem(productsList
                            .getSelectionIndex());

                    java.util.List<String> idents = productsMap.get(product);
                    productsTF.setText(product);
                    identsList.removeAll();

                    if (idents != null) {
                        for (String id : idents) {
                            identsList.add(id);
                        }
                    }

                    ITafSiteConfig config;
                    try {
                        config = TafSiteConfigFactory.getInstance();
                        workPilTF.setText(config.getProductWorkPil(product));
                        collectivePilTF.setText(config
                                .getProductCollectivePil(product));
                    } catch (FileNotFoundException e) {
                        workPilTF.setText("XXWRKTAF");
                        collectivePilTF.setText(e.getMessage());
                    } catch (ConfigurationException e) {
                        workPilTF.setText("XXWRKTAF");
                        collectivePilTF.setText("");
                    }
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button productsDeleteBtn = new Button(productsGroup, SWT.PUSH);
        productsDeleteBtn.setText("Delete");
        productsDeleteBtn.setLayoutData(gd);
        productsDeleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int index = productsList.getSelectionIndex();
                String selection = productsList.getItem(index);
                MessageBox questionMB = new MessageBox(shell, SWT.ICON_WARNING
                        | SWT.OK | SWT.CANCEL);
                questionMB.setText("Warning!  About to Delete!");
                questionMB
                        .setMessage("Are you sure you want to delete TAF product "
                                + selection + "?");
                int result = questionMB.open();

                if (result == SWT.OK) {
                    productsList.remove(index);
                    productsMap.remove(selection);
                    identsList.removeAll();
                    try {
                        ITafSiteConfig config = TafSiteConfigFactory
                                .getInstance();
                        config.deleteProduct(selection);
                        msgStatusComp.setMessageText("Product " + selection
                                + " deleted.", new RGB(0, 255, 0));
                    } catch (FileNotFoundException e) {
                        msgStatusComp.setMessageText(e.getMessage(), new RGB(
                                255, 0, 0));
                    } catch (ConfigurationException e) {
                        msgStatusComp
                                .setMessageText(
                                        "An error occured while attempting to delete a product.",
                                        new RGB(255, 0, 0));
                    } catch (LocalizationOpFailedException e) {
                        msgStatusComp
                                .setMessageText(
                                        "An error occured while attempting to delete a product.",
                                        new RGB(255, 0, 0));
                    }
                }
            }
        });
    }

    /**
     * Create the Ident controls.
     * 
     * @param mainControlComp
     *            Main composite.
     */
    private void createIdentControls(Composite mainControlComp) {
        // -----------------------------------------------
        // Create the Idents group and controls
        // -----------------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group identsGroup = new Group(mainControlComp, SWT.NONE);
        identsGroup.setText(" Idents ");
        GridLayout gl = new GridLayout(1, false);
        identsGroup.setLayout(gl);
        identsGroup.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label identsLbl = new Label(identsGroup, SWT.CENTER);
        identsLbl.setText("Idents");
        identsLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        identsTF = new Text(identsGroup, SWT.BORDER);
        identsTF.setLayoutData(gd);
        identsTF.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                if (e.character == SWT.CR) {
                    int index = productsList.getSelectionIndex();
                    if (index >= 0) {
                        String product = productsList.getItem(index);
                        java.util.List<String> idents = productsMap
                                .remove(product);
                        String id = identsTF.getText().trim().toUpperCase();
                        identsTF.setText("");
                        identsList.add(id);
                        idents.add(id);
                        productsMap.put(product, idents);
                    }
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 300;
        gd.widthHint = 75;
        identsList = new List(identsGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL | SWT.H_SCROLL);
        identsList.setLayoutData(gd);
        identsList.setFont(listFont);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button identsDeleteBtn = new Button(identsGroup, SWT.PUSH);
        identsDeleteBtn.setText("Delete");
        identsDeleteBtn.setLayoutData(gd);
        identsDeleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int index = identsList.getSelectionIndex();
                if (index >= 0) {
                    String id = identsList.getItem(index);
                    int productIndex = productsList.getSelectionIndex();
                    String product = productsList.getItem(productIndex);
                    java.util.List<String> list = productsMap.remove(product);
                    identsList.remove(index);
                    list.remove(list.indexOf(id));
                    productsMap.put(product, list);
                }
            }
        });
    }

    /**
     * Create the buttons on the right side of the display.
     * 
     * @param mainControlComp
     *            Main composite.
     */
    private void createButtonControls(Composite mainControlComp) {
        Composite controlComp = new Composite(mainControlComp, SWT.NONE);
        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        controlComp.setLayout(new GridLayout(1, false));
        controlComp.setLayoutData(gd);

        // --------------------------------------
        // Create the control buttons
        // --------------------------------------
        Composite buttonComp = new Composite(controlComp, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 100;

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button saveBtn = new Button(buttonComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setToolTipText("Saves route info to a file");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int idx = productsList.getSelectionIndex();

                if (idx >= 0) {
                    String product = productsList.getItem(idx);

                    try {
                        ITafSiteConfig config = TafSiteConfigFactory
                                .getInstance();
                        config.saveProduct(product, productsMap.get(product),
                                workPilTF.getText(), collectivePilTF.getText());
                        msgStatusComp.setMessageText("Product " + product
                                + " saved.", new RGB(0, 255, 0));
                    } catch (FileNotFoundException e) {
                        msgStatusComp.setMessageText(e.getMessage(), new RGB(
                                255, 0, 0));
                    } catch (ConfigurationException e) {
                        msgStatusComp.setMessageText(
                                "An error occured while saving product "
                                        + product + ".", new RGB(255, 0, 0));
                    } catch (LocalizationOpFailedException e) {
                        msgStatusComp.setMessageText(
                                "An error occured while saving product "
                                        + product + ".", new RGB(255, 0, 0));
                    }
                }
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button verifyBtn = new Button(buttonComp, SWT.PUSH);
        verifyBtn.setText("Verify");
        verifyBtn.setToolTipText("Verifies existence of all relevant files");
        verifyBtn.setLayoutData(gd);
        verifyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                verify();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button defaultBtn = new Button(buttonComp, SWT.PUSH);
        defaultBtn.setText("Default");
        defaultBtn.setToolTipText("Makes selected product the default");
        defaultBtn.setLayoutData(gd);
        defaultBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                try {
                    int index = productsList.getSelectionIndex();

                    if (index > 0) {
                        String defaultProduct = productsList.getItem(index);
                        index = 0;
                        ITafSiteConfig config = TafSiteConfigFactory
                                .getInstance();
                        config.setDefault(defaultProduct);
                        update();
                        productsList.setSelection(0);
                    }

                    msgStatusComp.setMessageText(productsList.getItem(index)
                            + " set as default product.", new RGB(0, 255, 0));
                } catch (FileNotFoundException e) {
                    msgStatusComp.setMessageText(e.getMessage(), new RGB(255,
                            0, 0));
                } catch (ConfigurationException e) {
                    msgStatusComp
                            .setMessageText(
                                    "An error occured when attempting to set default product.",
                                    new RGB(255, 0, 0));
                } catch (LocalizationOpFailedException e) {
                    msgStatusComp
                            .setMessageText(
                                    "An error occured when attempting to set default product.",
                                    new RGB(255, 0, 0));
                }
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setToolTipText("Closes this dialog");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button helpBtn = new Button(buttonComp, SWT.PUSH);
        helpBtn.setText("Help");
        helpBtn.setToolTipText("Shows help");
        helpBtn.setLayoutData(gd);
        helpBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(usageDlg)) {
                    String description = "TAF Product Configuration Help";

                    String helpText = "This dialog is used to define TAF/TWEB product (list of\nforecasts).\n\nThee products should be defined AFTER relevant site/route\nconfiguration files have been created.\n\nTo add a new product, enter product label in the \"Products\"\nentry field and press <Enter>. Then enter all TAF ids or\nTWEB routes in the \"Idents\" entry field, press <Enter>\nafter typing one item. Press \"Save\" button to save\nconfiguration file.\n\nTo remove a product, press \"Delede\" below \"Products\" list.\n\nTo remove an ident from the product definition, use\n\"Delete\" button in the \"Idents\" column. You must then save\nthe product. This will NOT delete TAF/TWEB configuration\nfiles, this can only be done from the command line.\n\nThe \"Verify\" button can be used to check for existence and\nproper syntax of all relevant files.";
                    usageDlg = new HelpUsageDlg(shell, description,
                            helpText);
                    usageDlg.open();
                } else {
                    usageDlg.bringToTop();
                }
            }
        });

        // -------------------------------------------------
        // Add the 'Work PIL' label and text control
        // -------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label workPilLbl = new Label(controlComp, SWT.CENTER);
        workPilLbl.setText("Work PIL");
        workPilLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 120;
        workPilTF = new Text(controlComp, SWT.BORDER);
        workPilTF.setLayoutData(gd);

        // -------------------------------------------------
        // Add the 'Collective PIL' label and text control
        // -------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label collectivePilLbl = new Label(controlComp, SWT.CENTER);
        collectivePilLbl.setText("Collective PIL");
        collectivePilLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 120;
        collectivePilTF = new Text(controlComp, SWT.BORDER);
        collectivePilTF.setLayoutData(gd);
    }

    /**
     * Create the message status composite.
     */
    private void createBottomMessageControls() {
        msgStatusComp = new MessageStatusComp(shell,
                StatusMessageType.TafProdConfig, null, null);
    }

    /**
     * Get the product list from configuration and repopulate the product list.
     * This assumes getting the new list will places the default product at the
     * top of the list.
     */
    private void update() {
        try {
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            productsMap = config.getAllProducts();
            java.util.List<String> allProducts = config.getProductList();

            productsList.removeAll();
            for (String product : allProducts) {
                productsList.add(product);
            }

        } catch (IOException ex) {
            // TODO handle this
            ex.printStackTrace();
        } catch (Exception ex) {
            // TODO handle this
            ex.printStackTrace();
        }
    }

    /**
     * The verify action call back. Displays problems in the status message.
     */
    private void verify() {
        try {
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            int index = productsList.getSelectionIndex();
            if (index < 0) {
                throw new Exception("No product selected");
            }
            String product = productsList.getItem(index);
            String[] idents = identsList.getItems();

            if (idents.length == 0) {
                throw new Exception("No idents to verify.");
            }

            for (String id : idents) {
                String afos;
                String wmo;
                GetPartialAfosIdRequest request = new GetPartialAfosIdRequest();
                request.setCccc(null);

                if (!collectivePilTF.getText().equals("")) {
                    request.setNnn(collectivePilTF.getText().substring(3, 6));
                    request.setXxx(collectivePilTF.getText().substring(6));
                } else {
                    request.setNnn("TAF");
                    request.setXxx(id.substring(1));
                }

                Object response = ThriftClient.sendRequest(request);

                if (response == null) {
                    throw new Exception(
                            "ThriftClient.sendRequest(request) response is null.");
                }
                if (!(response instanceof AfosWmoIdDataContainer)) {
                    throw new Exception(
                            "ThriftClient.sendRequest(request) response is not an instance of AfosWmoIdDataContainer.");
                }

                AfosWmoIdDataContainer container = (AfosWmoIdDataContainer) response;
                if (container.getErrorMessage() != null) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error occurred looking up WMO IDs\nMessage from server["
                                    + container.getErrorMessage() + "]");
                }

                java.util.List<AfosToAwips> list = container.getIdList();

                if (list.size() < 1) {
                    throw new Exception(id
                            + " AFOS ID not found in afos_to_awips table.");
                }

                AfosToAwips a2a = list.get(0);
                afos = a2a.getAfosid();
                wmo = a2a.getWmottaaii() + " " + a2a.getWmocccc();

                TafSiteData site = config.getSite(id);

                if (site == null) {
                    throw new Exception("Cannot parse " + id + " info file.");
                } else {
                    if (!site.wmo.equals(wmo)) {
                        throw new Exception(
                                id
                                        + " WMO headers disagree between info.cfg and A2A entry");
                    }

                    if (!site.afos.equals(afos)) {
                        throw new Exception(
                                id
                                        + " AFOS PIL disagree between info.cfg and A2A entry");
                    }
                }
            }

            msgStatusComp.setMessageText("Verify succeeded for " + product
                    + ".", new RGB(0, 255, 0));
        } catch (ConfigurationException e) {
            msgStatusComp.setMessageText(
                    "An error occured durring verification.",
                    new RGB(255, 0, 0));
        } catch (IOException e) {
            msgStatusComp.setMessageText(e.getMessage(), new RGB(255, 0, 0));
        } catch (VizException e) {
            msgStatusComp.setMessageText(e.getMessage(), new RGB(255, 0, 0));
        } catch (Exception e) {
            msgStatusComp.setMessageText(e.getMessage(), new RGB(255, 0, 0));
        }
    }
}
