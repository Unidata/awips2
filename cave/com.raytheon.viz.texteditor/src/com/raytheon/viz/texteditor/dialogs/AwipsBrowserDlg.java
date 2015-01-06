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

package com.raytheon.viz.texteditor.dialogs;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.texteditor.command.CommandFactory;
import com.raytheon.viz.texteditor.command.CommandFailedException;
import com.raytheon.viz.texteditor.command.ICommand;
import com.raytheon.viz.texteditor.msgs.IWmoBrowserCallback;
import com.raytheon.viz.texteditor.util.ScrolledLists;
import com.raytheon.viz.texteditor.util.TextEditorUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The WMO browser dialog allows the user to look through and choose text
 * products by Wmo, Site, and AWIPS ID to be displayed in the text window.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/04/2009   2191        rjpeter     Initial implementation.
 * 04/14/2010   4734        mhuang      Corrected StdTextProduct import
 * 09/11/2014   3580        mapeters    Removed IQueryTransport usage (no longer exists).
 * 12/08/2014   1231        nabowle     Fix selecting an AWIPS ID.
 * </pre>
 * 
 * @author rjpeter
 */
public class AwipsBrowserDlg extends CaveJFACEDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(AwipsBrowserDlg.class);
    /**
     *
     */
    private Composite top;

    private Label ddhhmmLbl;

    /**
     * List control displaying awipsId.
     */
    private List awipsIdList;

    /**
     * List control displaying TTAAii CCCC.
     */
    private List ttaaiiCcccList;

    /**
     * List control displaying ddhhmm.
     */
    private List ddhhmmList;

    /**
     * List control displaying bbb.
     */
    private List bbbList;

    /**
     * Load and continue button.
     */
    private Button loadContinueBtn;

    /**
     * Load and close button.
     */
    private Button loadCloseBtn;

    /**
     * Interface variable for Wmo Browser call back.
     */
    private IWmoBrowserCallback callbackClient = null;

    /**
     * AWIPS Id, TTAAiiCCCC, DDHHMM, BBB
     */
    private TreeMap<String, TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>>> availableProducts;

    private ScrolledLists scrolledLists = null;

    private StdTextProduct currentProduct = null;

    /**
     *
     * @param parent
     * @param browserHdr
     * @param cbClient
     * @param products
     */
    public AwipsBrowserDlg(Shell parent, IWmoBrowserCallback cbClient,
            java.util.List<StdTextProduct> products) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS);
        this.callbackClient = cbClient;
        generateProductMaps(products);
    }

    private void generateProductMaps(java.util.List<StdTextProduct> products) {
        TreeMap<String, TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>>> awipsIdMap = new TreeMap<String, TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>>>();
        TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>> ttaaiiCcccMap = null;
        TreeMap<String, TreeMap<String, StdTextProduct>> ddhhmmMap = null;
        TreeMap<String, StdTextProduct> bbbMap = null;

        for (StdTextProduct prod : products) {
            String awipsId = prod.getNnnid() + prod.getXxxid();
            ttaaiiCcccMap = awipsIdMap.get(awipsId);

            if (ttaaiiCcccMap == null) {
                ttaaiiCcccMap = new TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>>();
                awipsIdMap.put(awipsId, ttaaiiCcccMap);
            }

            String ttaaiiCccc = prod.getWmoid() + "    " + prod.getSite();
            ddhhmmMap = ttaaiiCcccMap.get(ttaaiiCccc);

            if (ddhhmmMap == null) {
                ddhhmmMap = new TreeMap<String, TreeMap<String, StdTextProduct>>();
                ttaaiiCcccMap.put(ttaaiiCccc, ddhhmmMap);
            }

            String ddhhmm = prod.getHdrtime();
            bbbMap = ddhhmmMap.get(ddhhmm);

            if (bbbMap == null) {
                bbbMap = new TreeMap<String, StdTextProduct>();
                ddhhmmMap.put(ddhhmm, bbbMap);
            }

            String bbb = prod.getBbbid();
            if (bbb == null || bbb.trim().length() == 0) {
                bbb = "NOR";
            }

            bbbMap.put(bbb, prod);
        }

        availableProducts = awipsIdMap;
    }

    private void updateProductInventory(java.util.List<StdTextProduct> products) {

        if (products.size() > 0) {
            // all products must have the same awipsId, ttaaii, cccc
            StdTextProduct tmpProd = products.get(0);
            String awipsId = tmpProd.getNnnid() + tmpProd.getXxxid();
            String ttaaiiCccc = tmpProd.getWmoid() + "    " + tmpProd.getSite();

            TreeMap<String, TreeMap<String, StdTextProduct>> ddhhmmMap = availableProducts
                    .get(awipsId).get(ttaaiiCccc);
            TreeMap<String, StdTextProduct> bbbMap = null;

            // clear the current time list
            ddhhmmMap.clear();

            // load new times
            for (StdTextProduct prod : products) {
                String ddhhmm = prod.getHdrtime();
                bbbMap = ddhhmmMap.get(ddhhmm);

                if (bbbMap == null) {
                    bbbMap = new TreeMap<String, StdTextProduct>();
                    ddhhmmMap.put(ddhhmm, bbbMap);
                }

                String bbb = prod.getBbbid();
                if (bbb == null || bbb.trim().length() == 0) {
                    bbb = "NOR";
                }

                bbbMap.put(bbb, prod);
            }

        }
    }

    /**
     *
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout(1, false);
        top.setLayout(layout);

        initializeComponents();
        loadInitialLists();
        top.layout();

        return top;
    }

    /**
     * Initialize the components of the dialog.
     */
    private void initializeComponents() {
        createListControls();
    }

    /**
     *
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("AWIPS Browser");
    }

    /**
     *
     */
    @Override
    protected void buttonPressed(int buttonId) {
        super.buttonPressed(buttonId);

        if (buttonId == IDialogConstants.PROCEED_ID
                || buttonId == IDialogConstants.OK_ID) {
            if (currentProduct != null) {
                // need to load the product
                ICommand command = CommandFactory.getAwipsCommand(
                        currentProduct.getNnnid() + currentProduct.getXxxid(),
                        currentProduct.getWmoid(), currentProduct.getSite(),
                        currentProduct.getHdrtime(), currentProduct.getBbbid());

                try {
                    java.util.List<StdTextProduct> prodList = command
                            .executeCommand();

                    if (prodList != null && prodList.size() > 0) {
                        StdTextProduct prod = prodList.get(0);
                        callbackClient.setDisplayedProduct(prod);
                        callbackClient.setCommandText(TextEditorUtil
                                .getCommandText(command));
                    }
                } catch (CommandFailedException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving product", e);
                }
            }
        }
    }

    /**
     *
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        loadContinueBtn = createButton(parent, IDialogConstants.PROCEED_ID,
                "Load and Continue", false);
        loadCloseBtn = createButton(parent, IDialogConstants.OK_ID,
                "Load and Close", false);
        createButton(parent, IDialogConstants.CANCEL_ID, "Close", false);

        loadContinueBtn.setEnabled(false);
        loadCloseBtn.setEnabled(false);
    }

    /**
     * Create the List box controls.
     */
    private void createListControls() {
        // Create the composite that will hold the controls.
        Composite listComp = new Composite(top, SWT.NONE);
        GridLayout gridLayout = new GridLayout(4, false);
        listComp.setLayout(gridLayout);

        GridData gd = new GridData(SWT.LEFT, SWT.TOP, true, false);
        Label awipsIdLbl = new Label(listComp, SWT.NONE);
        awipsIdLbl.setText("AWIPS ID");
        awipsIdLbl.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.TOP, true, false);
        Label ttaaiiLbl = new Label(listComp, SWT.NONE);
        ttaaiiLbl.setText("TTAAii    CCCC");
        ttaaiiLbl.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.TOP, true, false);
        gd.widthHint = 130;
        ddhhmmLbl = new Label(listComp, SWT.NONE);
        ddhhmmLbl.setText("DDHHMM(Latest)");
        ddhhmmLbl.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.TOP, true, false);
        Label bbbLbl = new Label(listComp, SWT.NONE);
        bbbLbl.setText("BBB");
        bbbLbl.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.TOP, true, true);
        gd.widthHint = 60;
        gd.heightHint = 200;
        awipsIdList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        awipsIdList.setLayoutData(gd);
        awipsIdList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                loadSelectedAwipsId();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.TOP, true, true);
        gd.widthHint = 100;
        gd.heightHint = 200;
        ttaaiiCcccList = new List(listComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        ttaaiiCcccList.setLayoutData(gd);
        ttaaiiCcccList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                selectTTAAiiCccc(ttaaiiCcccList.getSelectionIndex());
            }
        });

        gd = new GridData(SWT.LEFT, SWT.TOP, true, true);
        gd.widthHint = 120;
        gd.heightHint = 200;
        ddhhmmList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        ddhhmmList.setLayoutData(gd);
        ddhhmmList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                selectDDHHMM(ddhhmmList.getSelectionIndex());
            }
        });

        gd = new GridData(SWT.LEFT, SWT.TOP, true, true);
        gd.widthHint = 60;
        gd.heightHint = 200;
        bbbList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        bbbList.setLayoutData(gd);
        bbbList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                selectBBB(bbbList.getSelectionIndex());
            }
        });

        scrolledLists = new ScrolledLists(ttaaiiCcccList, ddhhmmList);
    }

    private void setLoadBtnEnabled(boolean flag) {
        if (!flag) {
            currentProduct = null;
        }

        if (loadContinueBtn != null) {
            loadContinueBtn.setEnabled(flag);
        }

        if (loadCloseBtn != null) {
            loadCloseBtn.setEnabled(flag);
        }
    }

    private void loadInitialLists() {
        setLoadBtnEnabled(false);
        ddhhmmLbl.setText("DDHHMM(Latest)");
        awipsIdList.removeAll();
        ttaaiiCcccList.removeAll();
        ddhhmmList.removeAll();
        bbbList.removeAll();

        Set<String> awipsIds = availableProducts.keySet();

        for (String id : awipsIds) {
            awipsIdList.add(id);
        }

        awipsIdList.select(0);

        loadSelectedAwipsId();
    }

    private void loadSelectedAwipsId() {
        ttaaiiCcccList.removeAll();
        ddhhmmList.removeAll();
        bbbList.removeAll();
        setLoadBtnEnabled(false);

        Map<String, String> timesToLoad = new HashMap<String, String>();
        String awipsId = awipsIdList.getItem(awipsIdList.getSelectionIndex());
        Map<String, TreeMap<String, TreeMap<String, StdTextProduct>>> selectedMap = availableProducts
                .get(awipsId);

        for (Entry<String, TreeMap<String, TreeMap<String, StdTextProduct>>> ttaaiiCCCCEntry : selectedMap
                .entrySet()) {
            String ttaaiiCccc = ttaaiiCCCCEntry.getKey();
            ttaaiiCcccList.add(ttaaiiCccc);

            for (String ddhhmm : ttaaiiCCCCEntry.getValue().keySet()) {
                String currentDDHHMM = timesToLoad.get(ttaaiiCccc);

                if (currentDDHHMM == null
                        || ddhhmm.compareTo(currentDDHHMM) > 0) {
                    timesToLoad.put(ttaaiiCccc, ddhhmm);
                }
            }
        }

        for (String val : ttaaiiCcccList.getItems()) {
            String ddhhmm = timesToLoad.get(val);
            if (ddhhmm != null) {
                ddhhmmList.add(ddhhmm);
            } else {
                ddhhmmList.add("");
            }
        }

        scrolledLists.joinLists();
    }

    private void loadDDHHMMList(Set<String> set) {
        setLoadBtnEnabled(false);
        ddhhmmList.removeAll();
        bbbList.removeAll();

        for (String val : set) {
            ddhhmmList.add(val);
        }

        if (ddhhmmList.getItemCount() > 0) {
            selectDDHHMM(0);
        }
    }

    private void loadBBBList(Set<String> set) {
        setLoadBtnEnabled(false);
        bbbList.removeAll();

        for (String val : set) {
            bbbList.add(val);
        }

        if (bbbList.getItemCount() > 0) {
            selectBBB(0);
        }
    }

    private void selectTTAAiiCccc(int index) {
        boolean deselect = true;

        int awipsIdIndex = awipsIdList.getSelectionIndex();
        if (awipsIdIndex >= 0) {
            if (index >= 0 && index < ttaaiiCcccList.getItemCount()) {
                scrolledLists.breakLists();
                ttaaiiCcccList.setSelection(index);
                ttaaiiCcccList.showSelection();

                // run command to do an inventory request for the ddhhmm
                ddhhmmLbl.setText("DDHHMM(Inventory)");
                String awipsId = awipsIdList.getItem(awipsIdIndex);
                String ttaaiiCccc = ttaaiiCcccList.getItem(index);
                String[] fields = ttaaiiCccc.split(" ");
                try {
                    ICommand command = CommandFactory.getAwipsCommand(awipsId,
                            fields[0], fields[1], "000000", null);
                    java.util.List<StdTextProduct> prods = command
                            .executeCommand();

                    // Add the ddhhmm for the selected ttaaii cccc
                    updateProductInventory(prods);

                    // needs to be ordered by create time, not DDHHMM
                    Set<String> ddhhmmSet = availableProducts.get(awipsId).get(
                            ttaaiiCccc).descendingKeySet();
                    loadDDHHMMList(ddhhmmSet);
                    deselect = false;
                } catch (CommandFailedException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving metatdata", e);

                }
            }
        }

        if (deselect) {
            ttaaiiCcccList.deselectAll();
        }
    }

    private void selectDDHHMM(int index) {
        boolean deselect = true;

        int awipsIdIndex = awipsIdList.getSelectionIndex();
        if (awipsIdIndex >= 0) {
            int ttaaiiCcccIndex = ttaaiiCcccList.getSelectionIndex();
            if (ttaaiiCcccIndex >= 0) {
                if (index >= 0 && index < ddhhmmList.getItemCount()) {
                    deselect = false;
                    ddhhmmList.setSelection(index);
                    ddhhmmList.showSelection();

                    // add the bbb for the selected ddhhmm
                    String awipsId = awipsIdList.getItem(awipsIdIndex);
                    String ttaaiiCccc = ttaaiiCcccList.getItem(ttaaiiCcccIndex);
                    String ddhhmm = ddhhmmList.getItem(index);
                    Set<String> bbbSet = availableProducts.get(awipsId).get(
                            ttaaiiCccc).get(ddhhmm).keySet();
                    loadBBBList(bbbSet);
                }
            }
        }

        if (deselect) {
            ddhhmmList.deselectAll();
        }
    }

    private void selectBBB(int index) {
        boolean deselect = true;

        int awipsIdIndex = awipsIdList.getSelectionIndex();
        if (awipsIdIndex >= 0) {
            int ttaaiiIndex = ttaaiiCcccList.getSelectionIndex();
            if (ttaaiiIndex >= 0) {
                int ddhhmmIndex = ddhhmmList.getSelectionIndex();
                if (ddhhmmIndex >= 0) {
                    if (index >= 0 && index < bbbList.getItemCount()) {
                        deselect = false;
                        bbbList.setSelection(index);
                        bbbList.showSelection();
                        String awipsId = awipsIdList.getItem(awipsIdIndex);
                        String ttaaiiCccc = ttaaiiCcccList.getItem(ttaaiiIndex);
                        String ddhhmm = ddhhmmList.getItem(ddhhmmIndex);
                        String bbb = bbbList.getItem(index);
                        currentProduct = availableProducts.get(awipsId).get(
                                ttaaiiCccc).get(ddhhmm).get(bbb);
                        setLoadBtnEnabled(true);
                    }
                }
            }
        }

        if (deselect) {
            bbbList.deselectAll();
        }
    }
}
