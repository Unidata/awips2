/*
 * WatchStatusMsgDlg
 * 
 * Date created: 11 March 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.store.PgenStorageException;
import gov.noaa.nws.ncep.ui.pgen.store.StorageUtils;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Implementation of a dialog to display text message for watch status.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/10		#159		B. Yin   	Initial Creation.
 * 04/13        #977        S. Gilbert  PGEN Database support
 * </pre>
 * 
 * @author B. Yin
 */

public class WatchStatusMsgDlg extends CaveJFACEDialog {

    // top level container for all widgets
    private Composite top;

    private Text productName;

    // text message to display
    private String statusMsg;

    // instance of watch status dialog
    private WatchStatusDlg wsd;

    // dialog size
    private final int NUM_LINES = 25;

    private final int NUM_COLUMNS = 68;

    /*
     * constructor
     */
    protected WatchStatusMsgDlg(Shell parentShell, WatchStatusDlg wsd) {
        super(parentShell);
        this.wsd = wsd;
    }

    /**
     * Creates the dialog area
     */
    @Override
    public Control createDialogArea(Composite parent) {

        // Set title
        getShell().setText("Watch Status Save");

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
        messageBox.setText(statusMsg);

        productName = new Text(top, SWT.SINGLE | SWT.BORDER);
        GridData gd2 = new GridData(GridData.FILL_HORIZONTAL);
        productName.setLayoutData(gd2);
        productName.setText(generateName());

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
         * construct watch file name and save the watch to the file.
         */
        String watchNumber = String.format("%1$04d", wsd.getWatchNumber());
        String fname = "WW" + watchNumber + ".xml";

        // if ( PgenUtil.checkFileStatus(fname) ){

        // wsd.getWatchBox().saveToFile(fname);
        String dataURI = wsd.getWatchBox().storeProduct(fname);
        if (dataURI != null) {
            try {
                StorageUtils.storeDerivedProduct(dataURI,
                        productName.getText(), "TEXT", this.statusMsg);
            } catch (PgenStorageException e) {
                StorageUtils.showError(e);
            }
        }
        wsd.close();
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

        // clear status info
        wsd.getWatchBox().rmLastStatus();

        // close the format dialog?
        super.cancelPressed();

    }

    /**
     * Set watch status text
     * 
     * @param str
     *            - watch status text
     */
    public void setMessage(String str) {
        this.statusMsg = str;
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

    private String generateName() {
        SimpleDateFormat sdf = new SimpleDateFormat("yyMMdd");
        Calendar date = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        String sdate = sdf.format(date.getTime());
        String watchNumber = String.format("%1$04d", wsd.getWatchNumber());

        StringBuilder dpname = new StringBuilder("WSMenh_");
        dpname.append(watchNumber);
        dpname.append('_');
        dpname.append(sdate);
        dpname.append(".txt");
        return dpname.toString();
    }

}
