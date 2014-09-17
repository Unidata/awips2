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
 *                                       dependency
 * 09/11/2014   3580        mapeters    Removed IQueryTransport usage (no longer exists).
 * </pre>
 * 
 * @author rjpeter
 */
public class WmoBrowserDlg extends CaveJFACEDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WmoBrowserDlg.class);

    /**
     * 
     */
    private Composite top;

    /**
     * List control displaying TTAAii.
     */
    private List ttaaiiList;

    /**
     * List control displaying cccc.
     */
    private List ccccList;

    /**
     * List control displaying awipsId.
     */
    private List awipsIdList;

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

    private TreeMap<String, TreeMap<String, TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>>>> availableProducts;

    private ScrolledLists scrolledLists = null;

    private StdTextProduct currentProduct = null;

    /**
     * 
     * @param parent
     * @param browserHdr
     * @param cbClient
     * @param products
     */
    public WmoBrowserDlg(Shell parent, IWmoBrowserCallback cbClient,
            java.util.List<StdTextProduct> products) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS);
        this.callbackClient = cbClient;
        generateProductMaps(products);
    }

    private void generateProductMaps(java.util.List<StdTextProduct> products) {
        TreeMap<String, TreeMap<String, TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>>>> wmoIdMap = new TreeMap<String, TreeMap<String, TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>>>>();
        TreeMap<String, TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>>> siteMap = null;
        TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>> awipsIdMap = null;
        TreeMap<String, TreeMap<String, StdTextProduct>> ddhhmmMap = null;
        TreeMap<String, StdTextProduct> bbbMap = null;

        for (StdTextProduct prod : products) {
            String wmoId = prod.getWmoid();
            siteMap = wmoIdMap.get(wmoId);

            if (siteMap == null) {
                siteMap = new TreeMap<String, TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>>>();
                wmoIdMap.put(wmoId, siteMap);
            }

            String site = prod.getSite();
            awipsIdMap = siteMap.get(site);

            if (awipsIdMap == null) {
                awipsIdMap = new TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>>();
                siteMap.put(site, awipsIdMap);
            }

            String awipsId = prod.getNnnid() + prod.getXxxid();
            ddhhmmMap = awipsIdMap.get(awipsId);

            if (ddhhmmMap == null) {
                ddhhmmMap = new TreeMap<String, TreeMap<String, StdTextProduct>>();
                awipsIdMap.put(awipsId, ddhhmmMap);
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

        availableProducts = wmoIdMap;
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
        // Create the composite that will hold the controls.
        Composite listComp = new Composite(top, SWT.NONE);
        GridLayout gridLayout = new GridLayout(5, false);
        listComp.setLayout(gridLayout);

        GridData gd = new GridData(SWT.LEFT, SWT.TOP, true, false);
        Label ttaaiiLbl = new Label(listComp, SWT.LEFT);
        ttaaiiLbl.setText("TTAAii");
        ttaaiiLbl.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.TOP, true, false);
        Label ccccLbl = new Label(listComp, SWT.LEFT);
        ccccLbl.setText("CCCC");
        ccccLbl.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.TOP, true, false);
        Label awipsIdLbl = new Label(listComp, SWT.LEFT);
        awipsIdLbl.setText("AWIPS ID");
        awipsIdLbl.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.TOP, true, false);
        Label ddhhmmLbl = new Label(listComp, SWT.LEFT);
        ddhhmmLbl.setText("DDHHMM(Latest)");
        ddhhmmLbl.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.TOP, true, false);
        Label bbbLbl = new Label(listComp, SWT.LEFT);
        bbbLbl.setText("BBB");
        bbbLbl.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.TOP, true, true);
        gd.widthHint = 60;
        gd.heightHint = 200;
        ttaaiiList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        ttaaiiList.setLayoutData(gd);
        ttaaiiList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                selectTTAAii(ttaaiiList.getSelectionIndex());
            }
        });

        gd = new GridData(SWT.LEFT, SWT.TOP, true, true);
        gd.widthHint = 60;
        gd.heightHint = 200;
        ccccList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        ccccList.setLayoutData(gd);
        ccccList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                selectCCCC(ccccList.getSelectionIndex());
            }
        });

        gd = new GridData(SWT.LEFT, SWT.TOP, true, true);
        gd.widthHint = 60;
        gd.heightHint = 200;
        awipsIdList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        awipsIdList.setLayoutData(gd);
        awipsIdList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                selectAwipsId(awipsIdList.getSelectionIndex());
            }
        });

        gd = new GridData(SWT.LEFT, SWT.TOP, true, true);
        gd.widthHint = 100;
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

    }

    /**
     * 
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("WMO Browser");
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
                ICommand command = CommandFactory.getWmoCommand(
                        currentProduct.getWmoid(), currentProduct.getSite(),
                        currentProduct.getNnnid() + currentProduct.getXxxid(),
                        currentProduct.getHdrtime(), currentProduct.getBbbid());

                try {
                    java.util.List<StdTextProduct> prodList = command
                            .executeCommand();

                    if (prodList != null && prodList.size() > 0) {
                        StdTextProduct prod = prodList.get(0);
                        callbackClient.setCommandText("WMO:" + prod.getWmoid()
                                + prod.getSite());
                        callbackClient.setDisplayedProduct(prod);
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

        Set<String> wmoIds = availableProducts.keySet();

        for (String wmoId : wmoIds) {
            ttaaiiList.add(wmoId);
        }

        Map<String, String> timesToLoad = new HashMap<String, String>();

        List listToLink = null;
        if (ttaaiiList.getItemCount() == 1) {
            ttaaiiList.select(0);
            ccccList.deselectAll();
            listToLink = ccccList;

            for (Entry<String, TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>>> siteEntry : availableProducts
                    .get(ttaaiiList.getItem(0)).entrySet()) {
                String site = siteEntry.getKey();
                ccccList.add(site);

                for (Entry<String, TreeMap<String, TreeMap<String, StdTextProduct>>> awipsIdEntry : siteEntry
                        .getValue().entrySet()) {
                    for (String ddhhmm : awipsIdEntry.getValue().keySet()) {
                        String currentDDHHMM = timesToLoad.get(site);

                        if (currentDDHHMM == null
                                || ddhhmm.compareTo(currentDDHHMM) > 0) {
                            timesToLoad.put(site, ddhhmm);
                        }
                    }
                }
            }
        } else {
            // was a cccc query, all cccc are the same grab one
            ttaaiiList.select(0);
            ttaaiiList.deselect(0);
            ccccList.add(availableProducts.get(ttaaiiList.getItem(0))
                    .firstKey());
            ccccList.select(0);
            listToLink = ttaaiiList;

            for (Entry<String, TreeMap<String, TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>>>> ttaaiiEntry : availableProducts
                    .entrySet()) {
                String ttaaii = ttaaiiEntry.getKey();
                for (Entry<String, TreeMap<String, TreeMap<String, TreeMap<String, StdTextProduct>>>> siteEntry : ttaaiiEntry
                        .getValue().entrySet()) {
                    for (Entry<String, TreeMap<String, TreeMap<String, StdTextProduct>>> awipsIdEntry : siteEntry
                            .getValue().entrySet()) {
                        for (String ddhhmm : awipsIdEntry.getValue().keySet()) {
                            String currentDDHHMM = timesToLoad.get(ttaaii);

                            if (currentDDHHMM == null
                                    || ddhhmm.compareTo(currentDDHHMM) > 0) {
                                timesToLoad.put(ttaaii, ddhhmm);
                            }
                        }
                    }
                }
            }

        }

        for (String site : listToLink.getItems()) {
            String ddhhmm = timesToLoad.get(site);
            if (ddhhmm != null) {
                ddhhmmList.add(ddhhmm);
            } else {
                ddhhmmList.add("");
            }
        }

        scrolledLists = new ScrolledLists(listToLink, ddhhmmList);
        scrolledLists.joinLists();
    }

    private void loadCCCCList(Set<String> set) {
        setLoadBtnEnabled(false);
        ccccList.removeAll();
        awipsIdList.removeAll();
        ddhhmmList.removeAll();
        bbbList.removeAll();

        for (String val : set) {
            ccccList.add(val);
        }

        if (ccccList.getItemCount() > 0) {
            selectCCCC(0);
        }
    }

    private void loadAwipsIdList(Set<String> set) {
        setLoadBtnEnabled(false);
        awipsIdList.removeAll();
        ddhhmmList.removeAll();
        bbbList.removeAll();

        for (String val : set) {
            awipsIdList.add(val);
        }

        if (awipsIdList.getItemCount() > 0) {
            selectAwipsId(0);
        }
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

    private void selectTTAAii(int index) {
        if (index >= 0 && index < ttaaiiList.getItemCount()) {
            scrolledLists.breakLists();
            ttaaiiList.setSelection(index);
            ttaaiiList.showSelection();
            String ttaaii = ttaaiiList.getItem(index);
            callbackClient.setTTAAiiField(ttaaii);

            // Add the cccc for the selected ttaaii
            Set<String> ccccSet = availableProducts.get(ttaaii).keySet();
            loadCCCCList(ccccSet);
        } else {
            ttaaiiList.deselectAll();
        }
    }

    private void selectCCCC(int index) {
        boolean deselect = true;

        int ttaaiiIndex = ttaaiiList.getSelectionIndex();
        if (ttaaiiIndex >= 0) {
            if (index >= 0 && index < ccccList.getItemCount()) {
                deselect = false;
                scrolledLists.breakLists();
                ccccList.setSelection(index);
                ccccList.showSelection();
                String ttaaii = ttaaiiList.getItem(ttaaiiIndex);
                String cccc = ccccList.getItem(index);
                callbackClient.setCCCCField(cccc);

                // add the awipsId for the selected cccc
                Set<String> awipsIdSet = availableProducts.get(ttaaii)
                        .get(cccc).keySet();
                loadAwipsIdList(awipsIdSet);
            }
        }

        if (deselect) {
            ccccList.deselectAll();
        }
    }

    private void selectAwipsId(int index) {
        boolean deselect = true;

        int ttaaiiIndex = ttaaiiList.getSelectionIndex();
        if (ttaaiiIndex >= 0) {
            int ccccIndex = ccccList.getSelectionIndex();
            if (ccccIndex >= 0) {
                if (index >= 0 && index < awipsIdList.getItemCount()) {
                    deselect = false;
                    awipsIdList.setSelection(index);
                    awipsIdList.showSelection();

                    // add the ddhhmm for the selected awipsId
                    String ttaaii = ttaaiiList.getItem(ttaaiiIndex);
                    String cccc = ccccList.getItem(ccccIndex);
                    String awipsId = awipsIdList.getItem(index);
                    Set<String> ddhhmmSet = availableProducts.get(ttaaii)
                            .get(cccc).get(awipsId).descendingKeySet();
                    loadDDHHMMList(ddhhmmSet);
                }
            }
        }

        if (deselect) {
            awipsIdList.deselectAll();
        }
    }

    private void selectDDHHMM(int index) {
        boolean deselect = true;

        int ttaaiiIndex = ttaaiiList.getSelectionIndex();
        if (ttaaiiIndex >= 0) {
            int ccccIndex = ccccList.getSelectionIndex();
            if (ccccIndex >= 0) {
                int awipsIdIndex = awipsIdList.getSelectionIndex();
                if (awipsIdIndex >= 0) {
                    if (index >= 0 && index < ddhhmmList.getItemCount()) {
                        deselect = false;
                        ddhhmmList.setSelection(index);
                        ddhhmmList.showSelection();

                        // add the bbb for the selected ddhhmm
                        String ttaaii = ttaaiiList.getItem(ttaaiiIndex);
                        String cccc = ccccList.getItem(ccccIndex);
                        String awipsId = awipsIdList.getItem(awipsIdIndex);
                        String ddhhmm = ddhhmmList.getItem(index);
                        Map<String, StdTextProduct> prodSet = availableProducts
                                .get(ttaaii).get(cccc).get(awipsId).get(ddhhmm);
                        if (prodSet != null) {
                            Set<String> bbbSet = prodSet.keySet();
                            loadBBBList(bbbSet);
                        }
                    }
                }
            }
        }

        if (deselect) {
            ddhhmmList.deselectAll();
        }
    }

    private void selectBBB(int index) {
        boolean deselect = true;

        int ttaaiiIndex = ttaaiiList.getSelectionIndex();
        if (ttaaiiIndex >= 0) {
            int ccccIndex = ccccList.getSelectionIndex();
            if (ccccIndex >= 0) {
                int awipsIdIndex = awipsIdList.getSelectionIndex();
                if (awipsIdIndex >= 0) {
                    int ddhhmmIndex = ddhhmmList.getSelectionIndex();
                    if (ddhhmmIndex >= 0) {
                        if (index >= 0 && index < bbbList.getItemCount()) {
                            deselect = false;
                            bbbList.setSelection(index);
                            bbbList.showSelection();
                            String ttaaii = ttaaiiList.getItem(ttaaiiIndex);
                            String cccc = ccccList.getItem(ccccIndex);
                            String awipsId = awipsIdList.getItem(awipsIdIndex);
                            String ddhhmm = ddhhmmList.getItem(ddhhmmIndex);
                            String bbb = bbbList.getItem(index);
                            currentProduct = availableProducts.get(ttaaii)
                                    .get(cccc).get(awipsId).get(ddhhmm)
                                    .get(bbb);
                            setLoadBtnEnabled(true);
                        }
                    }
                }
            }
        }

        if (deselect) {
            bbbList.deselectAll();
        }
    }
}
