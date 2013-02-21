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

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.hydro.productviewer.ProductViewerConstants.ProdListType;
import com.raytheon.viz.hydro.productviewer.ProductViewerConstants.SortType;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

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
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class ProductViewerDlg extends CaveSWTDialog {
    /**
     * Dialog's location and size.
     */
    private static Rectangle bounds;

    /**
     * Font used for the list and text controls.
     */
    private Font font;

    /**
     * Sash form used to allow resizing parts of the dialog.
     */
    private SashForm sashForm;

    /**
     * List displaying product information.
     */
    private List prodInfoListWidget;

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
     * ProductInfo data structure list.
     */
    private java.util.List<ProductInfo> productInfoList = null;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public ProductViewerDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Product Viewer");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        font.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();
        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent e) {
                bounds = shell.getBounds();
            }
        });

        if (bounds != null) {
            shell.setBounds(bounds);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        font = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        sashForm = new SashForm(shell, SWT.VERTICAL);
        sashForm.setLayout(new FillLayout());
        sashForm.SASH_WIDTH = 10;

        createProductInformationGroup();

        createTextViewerControl();

        createCloseButton();

        sashForm.setWeights(new int[] { 1, 2 });

        String lid = HydroDisplayManager.getInstance().getCurrentLid();
        if ((lid != null) && (lid.length() > 0)) {
            setReturnValue(lid);
            selectedLocTF.setText(lid);
        }

        loadProductList();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#shouldOpen()
     */
    @Override
    protected boolean shouldOpen() {
        return HydroDisplayManager.getInstance().isCurrentLidSelected(shell);
    }

    /**
     * Create the Product Information group container.
     */
    private void createProductInformationGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group productInfoGroup = new Group(sashForm, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        productInfoGroup.setLayout(gl);
        productInfoGroup.setLayoutData(gd);
        productInfoGroup.setText("Product Information");

        // -----------------------------------------------
        // Create the LEFT side (list box and labels)
        // -----------------------------------------------
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite leftComp = new Composite(productInfoGroup, SWT.NONE);
        gl = new GridLayout(3, false);
        leftComp.setLayout(gl);
        leftComp.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        Label prodIdLbl = new Label(leftComp, SWT.NONE);
        prodIdLbl.setText("Product Id");
        prodIdLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        Label prodTimeLbl = new Label(leftComp, SWT.NONE);
        prodTimeLbl.setText("Product Time");
        prodTimeLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Label postTimeLbl = new Label(leftComp, SWT.NONE);
        postTimeLbl.setText("Posting Time");
        postTimeLbl.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        gd.heightHint = 150;
        gd.widthHint = 450;
        gd.horizontalSpan = 3;
        prodInfoListWidget = new List(leftComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        prodInfoListWidget.setLayoutData(gd);
        prodInfoListWidget.setFont(font);
        prodInfoListWidget.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displaySelectedItem();
            }
        });

        // -----------------------------------------------
        // Create the RIGHT side (list box and labels)
        // -----------------------------------------------
        gd = new GridData(SWT.FILL, SWT.TOP, true, true);
        Composite rightComp = new Composite(productInfoGroup, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.verticalSpacing = 10;
        rightComp.setLayout(gl);
        rightComp.setLayoutData(gd);

        int labelWidth = 130;

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label listLbl = new Label(rightComp, SWT.RIGHT);
        listLbl.setText("List:");
        listLbl.setLayoutData(gd);

        gd = new GridData(275, SWT.DEFAULT);
        listCbo = new Combo(rightComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        listCbo.add(ProdListType.LOCATION.getStringValue());
        listCbo.add(ProdListType.LATEST.getStringValue());
        listCbo.add(ProdListType.ALL.getStringValue());
        listCbo.select(0);
        listCbo.setLayoutData(gd);
        listCbo.addSelectionListener(new SelectionAdapter() {
            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                loadProductList();
            }

        });

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label selectedLocLbl = new Label(rightComp, SWT.RIGHT);
        selectedLocLbl.setText("Selected Location:");
        selectedLocLbl.setLayoutData(gd);

        gd = new GridData(110, SWT.DEFAULT);
        selectedLocTF = new Text(rightComp, SWT.BORDER);
        selectedLocTF.setLayoutData(gd);
        selectedLocTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
            }
        });

        // Add a separator line
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 4;
        Label sepLbl = new Label(rightComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label prodIdFilterLbl = new Label(rightComp, SWT.RIGHT);
        prodIdFilterLbl.setText("Product Id Filter:");
        prodIdFilterLbl.setLayoutData(gd);

        gd = new GridData(110, SWT.DEFAULT);
        prodIdFilterTF = new Text(rightComp, SWT.BORDER);
        prodIdFilterTF.setLayoutData(gd);
        prodIdFilterTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
            }
        });

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label sortByLbl = new Label(rightComp, SWT.RIGHT);
        sortByLbl.setText("Sort By:");
        sortByLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        sortByCbo = new Combo(rightComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        sortByCbo.add(SortType.PROD_ID.getStringValue());
        sortByCbo.add(SortType.PROD_TIME.getStringValue());
        sortByCbo.add(SortType.POST_TIME.getStringValue());
        sortByCbo.select(0);
        sortByCbo.setLayoutData(gd);
        sortByCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                loadProductList();
            }
        });
    }

    /**
     * Create the text viewer control.
     */
    private void createTextViewerControl() {
        GridData gd = new GridData(GridData.FILL_BOTH);
        Composite textViewerComp = new Composite(sashForm, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, false);
        textViewerComp.setLayout(gridLayout);
        textViewerComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 400;
        textViewer = new StyledText(textViewerComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        textViewer.setWordWrap(true);
        textViewer.setFont(font);
        textViewer.setEditable(false);
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

    /**
     * Create the Close button.
     */
    private void createCloseButton() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        centeredComp.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                bounds = shell.getBounds();
                close();
            }
        });
    }

    /**
     * Load the product list.
     */
    private void loadProductList() {
        ProductViewerDataManager dataManager = ProductViewerDataManager
                .getInstance();
        SortType sortType = SortType.PROD_TIME;

        /*
         * load the list of products based on the user settings. load from the
         * appropriate table using any filters defined and in the sort order
         * specified.
         */

        String prodFilter = prodIdFilterTF.getText();

        if (sortByCbo.getItem(sortByCbo.getSelectionIndex()).equals(
                SortType.PROD_TIME.getStringValue())) {
            sortType = SortType.PROD_TIME;
        } else if (sortByCbo.getItem(sortByCbo.getSelectionIndex()).equals(
                SortType.POST_TIME.getStringValue())) {
            sortType = SortType.POST_TIME;
        } else if (sortByCbo.getItem(sortByCbo.getSelectionIndex()).equals(
                SortType.PROD_ID.getStringValue())) {
            sortType = SortType.PROD_ID;
        }

        /*
         * if loading products for a given location, load from the ProductLink
         * table
         */
        if (listCbo.getItem(listCbo.getSelectionIndex()).equals(
                ProdListType.LOCATION.getStringValue())) {

            productInfoList = (ArrayList<ProductInfo>) dataManager
                    .getProductsByLocation(ProdListType.LOCATION, sortType,
                            prodFilter, selectedLocTF.getText());

        } else if (listCbo.getItem(listCbo.getSelectionIndex()).equals(
                ProdListType.LATEST.getStringValue())) {
            /*
             * if loading the latest of the products, load from the PurgeProduct
             * table.
             */
            productInfoList = (ArrayList<ProductInfo>) dataManager
                    .getLatestProducts(ProdListType.LATEST, sortType,
                            prodFilter, selectedLocTF.getText());
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

            productInfoList = (ArrayList<ProductInfo>) dataManager
                    .getAllProducts(ProdListType.ALL, sortType, prodFilter,
                            selectedLocTF.getText());
        }

        // Populate the list
        loadProductListInfo(productInfoList);
    }

    /**
     * Load the product information into the widget.
     * 
     * @param productInfoList
     *            List of ProductInfo objects to load into the list widget
     */
    private void loadProductListInfo(java.util.List<ProductInfo> productInfoList) {
        String[] listItems = new String[productInfoList.size()];
        for (int i = 0; i < productInfoList.size(); i++) {
            listItems[i] = productInfoList.get(i).toString();
        }

        prodInfoListWidget.setItems(listItems);
    }

    /**
     * Display the selected item.
     */
    private void displaySelectedItem() {
        // Get the data from the list.
        ProductInfo prodInfo = productInfoList.get(prodInfoListWidget
                .getSelectionIndex());

        // Get the text product
        ProductViewerDataManager dataManager = ProductViewerDataManager
                .getInstance();
        String product = dataManager.getTextProduct(prodInfo);

        textViewer.setText(product);
    }
}
