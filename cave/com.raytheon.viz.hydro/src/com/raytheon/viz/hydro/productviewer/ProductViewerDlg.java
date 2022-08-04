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

package com.raytheon.viz.hydro.productviewer;

import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.hydro.productviewer.ProductViewerConstants.ProdListType;
import com.raytheon.viz.hydro.productviewer.ProductViewerConstants.SortType;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * This class displays the Product Viewer dialog for HydroView.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation.
 * 10/14/2008   1555       grichard    Support product viewer.
 * 12/11/2008   1782       grichard    New up locationPostTimeData.
 * 12/16/2008   1782       grichard    Refreshed Product Viewer.
 * 12/11/2009   2488       mpduff      Refactored dialog to work as 
 *                                     in AWIPS 1
 * 02/07/2013   1578       rferrel     Make dialog non-blocking.
 * 06/19/2013   2119       rferrel     Remove no longer needed shouldOpen.
 * 04/12/2016   5483       dgilling    Refactor based on CaveJFACEDialog, 
 *                                     cleanup hi-dpi issues.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class ProductViewerDlg extends CaveJFACEDialog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    /**
     * List displaying product information.
     */
    private Table prodInfoTable;

    /**
     * List combo box.
     */
    private Combo listCbo;

    /**
     * Selected location text control.
     */
    private Text selectedLocTF;

    /**
     * Product ID filter text control.
     */
    private Text prodIdFilterTF;

    /**
     * Sort by combo box.
     */
    private Combo sortByCbo;

    /**
     * Multi-line text control.
     */
    private StyledText textViewer;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public ProductViewerDlg(Shell parent) {
        super(parent);
        setShellStyle(SWT.DIALOG_TRIM);
        setBlockOnOpen(false);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Product Viewer");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);

        SashForm sashForm = new SashForm(composite, SWT.VERTICAL);
        sashForm.setLayout(new FillLayout());
        sashForm.SASH_WIDTH = 10;

        createProductInformationGroup(sashForm);

        createTextViewerControl(sashForm);

        sashForm.setWeights(new int[] { 1, 2 });

        loadProductList();

        return composite;
    }

    /**
     * Create the Product Information group container.
     * 
     * @param parent
     */
    private void createProductInformationGroup(Composite parent) {
        Group productInfoGroup = new Group(parent, SWT.NONE);
        productInfoGroup.setText("Product Information");
        productInfoGroup.setLayout(new GridLayout(2, false));
        productInfoGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true));

        // -----------------------------------------------
        // Create the LEFT side (TABLE)
        // -----------------------------------------------
        prodInfoTable = new Table(productInfoGroup, SWT.SINGLE
                | SWT.FULL_SELECTION | SWT.V_SCROLL);
        prodInfoTable.setLinesVisible(false);
        prodInfoTable.setHeaderVisible(true);
        String[] headerTitles = new String[] { "Product Id", "Product Time",
                "Posting Time" };
        for (String title : headerTitles) {
            TableColumn column = new TableColumn(prodInfoTable, SWT.LEFT);
            column.setText(title);
        }
        prodInfoTable.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                // Get the data from the list.
                ProductInfo prodInfo = (ProductInfo) e.widget.getData();
                if (prodInfo == null) {
                    prodInfo = (ProductInfo) prodInfoTable.getItem(
                            prodInfoTable.getSelectionIndex()).getData();
                }

                // Get the text product
                ProductViewerDataManager dataManager = ProductViewerDataManager
                        .getInstance();
                String product = dataManager.getTextProduct(prodInfo);

                textViewer.setText(product);
            }
        });
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = prodInfoTable.getHeaderHeight()
                + (prodInfoTable.getItemHeight() * 7);
        GC gc = new GC(prodInfoTable);
        gc.setFont(JFaceResources.getTextFont());
        gd.widthHint = gc.getFontMetrics().getAverageCharWidth() * 55;
        gc.dispose();
        prodInfoTable.setLayoutData(gd);

        // -----------------------------------------------
        // Create the RIGHT side (list box and labels)
        // -----------------------------------------------
        Composite rightComp = new Composite(productInfoGroup, SWT.NONE);
        rightComp.setLayout(new GridLayout(2, false));
        rightComp.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, true));

        Label listLbl = new Label(rightComp, SWT.NONE);
        listLbl.setText("List:");
        listLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        listCbo = new Combo(rightComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (ProdListType prodType : ProdListType.values()) {
            listCbo.add(prodType.getStringValue());
        }
        listCbo.select(0);
        listCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                loadProductList();
            }
        });

        Label selectedLocLbl = new Label(rightComp, SWT.NONE);
        selectedLocLbl.setText("Selected Location:");
        selectedLocLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                true));

        selectedLocTF = new Text(rightComp, SWT.BORDER);
        String lid = HydroDisplayManager.getInstance().getCurrentLid();
        if ((lid != null) && (!lid.isEmpty())) {
            selectedLocTF.setText(lid);
        }
        selectedLocTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
            }
        });
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gc = new GC(selectedLocTF);
        gd.widthHint = gc.getFontMetrics().getAverageCharWidth() * 8;
        gc.dispose();
        selectedLocTF.setLayoutData(gd);

        // Add a separator line
        Label sepLbl = new Label(rightComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gd.horizontalSpan = 2;
        sepLbl.setLayoutData(gd);

        Label prodIdFilterLbl = new Label(rightComp, SWT.NONE);
        prodIdFilterLbl.setText("Product Id Filter:");
        prodIdFilterLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER,
                false, true));

        prodIdFilterTF = new Text(rightComp, SWT.BORDER);
        prodIdFilterTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
            }
        });
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gc = new GC(prodIdFilterTF);
        gd.widthHint = gc.getFontMetrics().getAverageCharWidth() * 15;
        gc.dispose();
        prodIdFilterTF.setLayoutData(gd);

        Label sortByLbl = new Label(rightComp, SWT.NONE);
        sortByLbl.setText("Sort By:");
        sortByLbl
                .setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        sortByCbo = new Combo(rightComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (SortType sortType : SortType.values()) {
            sortByCbo.add(sortType.getStringValue());
        }
        sortByCbo.select(0);
        sortByCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                loadProductList();
            }
        });
    }

    /**
     * Create the text viewer control.
     * 
     * @param parent
     */
    private void createTextViewerControl(Composite parent) {
        Composite textViewerComp = new Composite(parent, SWT.NONE);
        textViewerComp.setLayout(new GridLayout(1, false));

        textViewer = new StyledText(textViewerComp, SWT.BORDER | SWT.MULTI
                | SWT.READ_ONLY | SWT.V_SCROLL | SWT.H_SCROLL);
        textViewer.setFont(JFaceResources.getTextFont());
        textViewer.setWordWrap(true);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GC gc = new GC(textViewer);
        gd.heightHint = gc.getFontMetrics().getHeight() * 25;
        gc.dispose();
        textViewer.setLayoutData(gd);
    }

    /**
     * Set the location value and if needed clear the prod ID filter.
     * 
     * @param lid
     */
    public void setLid(String lid) {
        if (!selectedLocTF.getText().equals(lid)) {
            selectedLocTF.setText(lid);
            prodIdFilterTF.setText("");
        }
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.CLOSE_ID,
                IDialogConstants.CLOSE_LABEL, false);
    }

    @Override
    protected void buttonPressed(int buttonId) {
        switch (buttonId) {
        case IDialogConstants.CLOSE_ID:
            close();
            break;
        default:
            statusHandler.warn(String.format(
                    "Unrecognized button ID [%d] pressed.", buttonId));
            break;
        }
    }

    /**
     * Load the product list.
     */
    private void loadProductList() {
        ProductViewerDataManager dataManager = ProductViewerDataManager
                .getInstance();

        /*
         * load the list of products based on the user settings. load from the
         * appropriate table using any filters defined and in the sort order
         * specified.
         */

        String prodFilter = prodIdFilterTF.getText();

        SortType sortType = SortType.PROD_TIME;
        String selectedSort = sortByCbo.getItem(sortByCbo.getSelectionIndex());
        for (SortType sort : SortType.values()) {
            if (sort.getStringValue().equals(selectedSort)) {
                sortType = sort;
                break;
            }
        }

        /*
         * if loading products for a given location, load from the ProductLink
         * table
         */
        List<ProductInfo> productInfoList;
        if (listCbo.getItem(listCbo.getSelectionIndex()).equals(
                ProdListType.LOCATION.getStringValue())) {

            productInfoList = dataManager.getProductsByLocation(
                    ProdListType.LOCATION, sortType, prodFilter,
                    selectedLocTF.getText());

        } else if (listCbo.getItem(listCbo.getSelectionIndex()).equals(
                ProdListType.LATEST.getStringValue())) {
            /*
             * if loading the latest of the products, load from the PurgeProduct
             * table.
             */
            productInfoList = dataManager.getLatestProducts(
                    ProdListType.LATEST, sortType, prodFilter,
                    selectedLocTF.getText());
        } else {
            /*
             * if loading all products, then load from the TextProduct table.
             * because the product text in blobs can potentially be large, only
             * load in the pertinent info using the unique utility function. for
             * this "unique" type query, use the field number for the sort.
             * because of this, the "unique" field specified must correspond to
             * the sort order. therefore use a special sort string for the
             * retrieved concatenated field, and parse and rearrange the string
             * later
             */

            productInfoList = dataManager.getAllProducts(ProdListType.ALL,
                    sortType, prodFilter, selectedLocTF.getText());
        }

        prodInfoTable.removeAll();
        for (ProductInfo info : productInfoList) {
            TableItem item = new TableItem(prodInfoTable, SWT.NONE);
            item.setFont(JFaceResources.getTextFont());
            item.setText(0, info.getProductId());
            item.setText(1, info.getProductTimeString());
            item.setText(2, info.getPostingTimeString());
            item.setData(info);
        }

        for (int i = 0; i < prodInfoTable.getColumnCount(); i++) {
            prodInfoTable.getColumn(i).pack();
        }
    }
}
