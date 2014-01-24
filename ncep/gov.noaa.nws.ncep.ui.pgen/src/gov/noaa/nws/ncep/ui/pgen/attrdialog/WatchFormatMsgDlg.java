/*
 * WatchFormatMsgDlg
 * 
 * Date created: 22 February 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.common.dataplugin.pgen.DerivedProduct;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.productmanage.ProductConfigureDialog;
import gov.noaa.nws.ncep.ui.pgen.producttypes.ProductType;
import gov.noaa.nws.ncep.ui.pgen.store.PgenStorageException;
import gov.noaa.nws.ncep.ui.pgen.store.StorageUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.Marshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.w3c.dom.Document;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Implementation of a dialog to display information of an issued watch.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/10		#159		B. Yin   	Initial Creation.
 * 03/12		#703		B. Yin		Create SEL, SAW, WOU, etc.
 * 05/12		#772		B. Yin		Close dialog after done format.
 * 04/13        #977        S. Gilbert  PGEN Database support
 * 12/13		TTR 800		B. Yin		Added original county list
 * </pre>
 * 
 * @author B. Yin
 */

public class WatchFormatMsgDlg extends CaveJFACEDialog {

    public static final String PROD_TYPE = "TEXT";

    // top level container for all widgets
    private Composite top;

    // text message to display
    private String watchMsg;

    // instance of watch format dialog
    private WatchFormatDlg wfd;

    // dialog size
    private final int NUM_LINES = 25;

    private final int NUM_COLUMNS = 68;

    /*
     * constructor
     */
    protected WatchFormatMsgDlg(Shell parentShell, WatchFormatDlg wfd) {
        super(parentShell);
        this.wfd = wfd;
    }

    /**
     * Creates the dialog area
     */
    @Override
    public Control createDialogArea(Composite parent) {

        // Set title
        getShell().setText("Severe Weather Watch");

        top = (Composite) super.createDialogArea(parent);

        /*
         * Create the main layout for the dialog area.
         */
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout(mainLayout);

        /*
         * Create a text box for the message
         */
        Text messageBox = new Text(top, SWT.MULTI | SWT.BORDER | SWT.READ_ONLY
                | SWT.V_SCROLL);
        messageBox.setFont(new Font(messageBox.getDisplay(), "Courier", 12,
                SWT.NORMAL));
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);

        // Calculate approximate size of text box to display 25 lines at 80
        // characters each
        gd.heightHint = NUM_LINES * messageBox.getLineHeight();
        GC gc = new GC(messageBox);
        FontMetrics fm = gc.getFontMetrics();
        gd.widthHint = NUM_COLUMNS * fm.getAverageCharWidth();

        messageBox.setLayoutData(gd);
        setMessage(generateProducts(putWatcInProduct(wfd.getWatchBox()),
                "WatchText.xlt"));
        messageBox.setText(watchMsg);

        // Make sure to dispose of font
        messageBox.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                Text w = (Text) e.widget;
                w.getFont().dispose();
            }

        });

        return top;
    }

    /*
     * 
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void okPressed() {

        /*
         * Save button pressed. Save watch Message to a file
         */
        String watchNumber = String.format("%1$04d", wfd.getWatchNumber());

        String pdName = wfd.getWbDlg().drawingLayer.getActiveProduct()
                .getType();
        ProductType pt = ProductConfigureDialog.getProductTypes().get(pdName);
        if (pt != null)
            pdName = pt.getType();

        String pd1 = pdName.replaceAll(" ", "_");

        String dirPath = PgenUtil.getPgenOprDirectory() + File.separator + pd1
                + File.separator + "prod" + File.separator + "text"
                + File.separator;

        String fname = "WW" + watchNumber + ".xml";

        // if ( PgenUtil.checkFileStatus(fname) ){

        //set issue flag
        wfd.getWatchBox().setIssueFlag(1);
        
        //Make a copy of original county list
    	wfd.getWatchBox().makeOriginalCountyList( wfd.getWatchBox().getCountyList() );
    	
        // re-draw watch
        wfd.getWbDlg().drawingLayer.resetElement(wfd.getWatchBox());
        wfd.getWbDlg().mapEditor.refresh();

        // wfd.getWatchBox().saveToFile(fname);
        wfd.getWatchBox().storeProduct(fname);
        String dataURI = wfd.getWatchBox().getDataURI();

        // saveWatchText("ww"+watchNumber+".txt");

        Products pd = this.putWatcInProduct(wfd.getWatchBox());
        // saveProducts(watchMsg, dirPath + "ww" + watchNumber + ".txt");
        // saveProducts(generateProducts(pd, "SAW.xlt"), dirPath + "WW"
        // + watchNumber + ".SAW");
        // saveProducts(generateProducts(pd, "SEL.xlt"), dirPath + "WW"
        // + watchNumber + ".SEL");
        // saveProducts(generateProducts(pd, "SEV.xlt"), dirPath + "WW"
        // + watchNumber + ".SEV");
        // saveProducts(generateProducts(pd, "WOU.xlt"), dirPath + "WW"
        // + watchNumber + ".WOU");

        /*
         * Save Derived products to EDEX
         */
        List<DerivedProduct> prodList = new ArrayList<DerivedProduct>();
        prodList.add(new DerivedProduct("ww" + watchNumber + ".txt", PROD_TYPE,
                watchMsg));
        prodList.add(new DerivedProduct("WW" + watchNumber + ".SAW", PROD_TYPE,
                generateProducts(pd, "SAW.xlt")));
        prodList.add(new DerivedProduct("WW" + watchNumber + ".SEL", PROD_TYPE,
                generateProducts(pd, "SEL.xlt")));
        prodList.add(new DerivedProduct("WW" + watchNumber + ".SEV", PROD_TYPE,
                generateProducts(pd, "SEV.xlt")));
        prodList.add(new DerivedProduct("WW" + watchNumber + ".WOU", PROD_TYPE,
                generateProducts(pd, "WOU.xlt")));

        try {
            StorageUtils.storeDerivedProducts(dataURI, prodList);
        } catch (PgenStorageException e) {
            StorageUtils.showError(e);
        }

        wfd.close();
        WatchBoxAttrDlg.getInstance(null).close();
        PgenUtil.setSelectingMode();
        super.okPressed();
        // }
    }

    /*
     * 
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void cancelPressed() {

        // close the format dialog
        wfd.close();
        WatchBoxAttrDlg.getInstance(null).close();
        PgenUtil.setSelectingMode();
        super.cancelPressed();

    }

    private void saveProducts(String outStr, String outFile) {

        if (outStr != null && !outStr.isEmpty()) {

            FileTools.writeFile(outFile, outStr);

        }
    }

    private String generateProducts(Products pd, String xslt) {

        Document sw = null;

        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            DocumentBuilder db = dbf.newDocumentBuilder();
            sw = db.newDocument();
            Marshaller mar = SerializationUtil.getJaxbContext()
                    .createMarshaller();
            mar.marshal(pd, sw);
        } catch (Exception e) {
            e.printStackTrace();
        }

        DOMSource ds = new DOMSource(sw);

        // get style sheet file path
        String xsltPath = PgenStaticDataProvider.getProvider()
                .getPgenLocalizationRoot()
                + "xslt"
                + File.separator
                + "watchbox" + File.separator + xslt;

        LocalizationFile lFile = PgenStaticDataProvider.getProvider()
                .getStaticLocalizationFile(xsltPath);

        String outStr = "";
        if (lFile != null) {
            outStr = PgenUtil.applyStyleSheet(ds, lFile.getFile()
                    .getAbsolutePath());
        }

        return outStr;

    }

    /**
     * Set watch text
     * 
     * @param str
     *            The issued watch text
     */
    public void setMessage(String str) {
        this.watchMsg = str;
    }

    @Override
    /**
     * Set the location of the dialog
     * Set the OK button to Save
     */
    public int open() {

        if (this.getShell() == null) {
            this.create();
        }

        // this.getShell().setLocation(this.getShell().getParent().getLocation());
        this.getButton(IDialogConstants.OK_ID).setText("Save");
        this.getButtonBar().pack();

        return super.open();

    }

    /**
     * Create buttons on the button bar
     */
    @Override
    public void createButtonsForButtonBar(Composite parent) {

        GridLayout barGl = new GridLayout(3, false);
        parent.setLayout(barGl);

        Button editBtn = new Button(parent, SWT.PUSH);

        super.createButtonsForButtonBar(parent);

        // add re-edit button
        editBtn.setText("Re-edit");
        editBtn.setLayoutData(getButton(IDialogConstants.CANCEL_ID)
                .getLayoutData());
        editBtn.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // TODO Auto-generated method stub
            }

            @Override
            public void widgetSelected(SelectionEvent e) {

                WatchFormatMsgDlg.this.close();
            }

        });

        getButton(IDialogConstants.OK_ID).setText("Save");
    }

    private Products putWatcInProduct(WatchBox wb) {
        Layer defaultLayer = new Layer();
        // add watch collection(box and status line)
        defaultLayer.addElement(wb.getParent());

        Product defaultProduct = new Product();
        defaultProduct.addLayer(defaultLayer);
        defaultProduct.setName( "WatchBox" );
        defaultProduct.setType("WatchBox");
        
        ArrayList<Product> prds = new ArrayList<Product>();
        prds.add(defaultProduct);
        Products fileProduct = ProductConverter.convert(prds);

        return fileProduct;
    }
}
