/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.vaaDialog.CcfpMsgDlg
 * 
 * 20 September 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.sigmet.CcfpInfo;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.Writer;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Singleton text product dialog for CCFP.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ---------	--------	----------	--------------------------
 * 09/10		322			G. Zhang 	Initial Creation.
 * </pre>
 * 
 * @author gzhang
 */

public class CcfpMsgDlg extends AttrDlg {

    // Constant for Button Update
    private static final int CCFP_CONSTANT_UPDATE = 20101007;

    public static final String PGEN_CCFP_XSLT = "xslt" + File.separator
            + "ccfp" + File.separator + "ccfpXml2Txt.xslt";

    // singleton instance for this class
    private static CcfpMsgDlg INSTANCE = null;

    // the instance of CcfpTimeDlg class
    private CcfpTimeDlg timeDlg = null;

    // issue and valid times
    private String issueTime;

    private String validTime;

    // Text field for displaying the text product
    private Text txtInfo;

    // Text field for the name of the file to be saved.
    private Text txtSave;

    // variables holding directory and file content.
    private String dirLocal = ".", txtFileContent = "", txtFileName = "";

    // width
    private static final int LAYOUT_WIDTH = 2;

    /**
     * constructor for this class
     * 
     * @param Shell
     *            : parent Shell of this class
     * @throws VizException
     */
    public CcfpMsgDlg(Shell parShell) throws VizException {
        super(parShell);
        // TODO Auto-generated constructor stub
    }

    /**
     * singleton creation method for this class
     * 
     * @param Shell
     *            : parent Shell of this class
     * @return
     */
    public static CcfpMsgDlg getInstance(Shell parShell) {

        if (INSTANCE == null) {
            try {
                INSTANCE = new CcfpMsgDlg(parShell);
            } catch (VizException e) {
                e.printStackTrace();
            }
        }
        return INSTANCE;
    }

    @Override
    public void setAttrForDlg(IAttribute ia) {
    }

    /**
     * method overridden from the super class for Save/Cancel buttons of this
     * class
     */
    @Override
    public void createButtonsForButtonBar(Composite parent) {
        createButton(parent, CCFP_CONSTANT_UPDATE, "Update", true);
        createButton(parent, IDialogConstants.OK_ID, "Save", true);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);

        this.getButton(CCFP_CONSTANT_UPDATE).addListener(SWT.Selection,
                new Listener() {
                    public void handleEvent(Event e) {

                        String xmlFileName = CcfpInfo.saveCcfpXmlFile(
                                getIssueTime(), getValidTime());
                        txtFileContent = CcfpInfo
                                .convertXml2Txt(
                                        xmlFileName,
                                        PgenStaticDataProvider
                                                .getProvider()
                                                .getFileAbsolutePath(
                                                        PgenStaticDataProvider
                                                                .getProvider()
                                                                .getPgenLocalizationRoot()
                                                                + PGEN_CCFP_XSLT));
                        txtInfo.setText(txtFileContent.trim());

                    }
                });
    }

    /**
     * method overridden from the super class for Save/Cancel buttons of this
     * class
     */
    @Override
    public void enableButtons() {
        this.getButton(CCFP_CONSTANT_UPDATE).setEnabled(true);
        this.getButton(IDialogConstants.CANCEL_ID).setEnabled(true);
        this.getButton(IDialogConstants.OK_ID).setEnabled(true);
    }

    /**
     * method listener overridden from the super class for Cancel button of this
     * class
     */
    @Override
    public void cancelPressed() {
        setReturnCode(CANCEL);
        close();
    }

    /**
     * method listener overridden from the super class for Save button of this
     * class: it saves the displayed text with the displayed name as a text file
     * in local directory.
     */
    @Override
    public void okPressed() {

		FileTools.writeFile(PgenUtil.getPgenActivityTextProdPath()
				+ File.separator + txtSave.getText(), 
				txtInfo.getText());

            setReturnCode(OK);
            close();

    }

    /**
     * method overridden from the super class to create the dialog area for this
     * class
     */
    @Override
    public Control createDialogArea(Composite parent) {

        Composite top = (Composite) super.createDialogArea(parent);

        GridLayout mainLayout = new GridLayout(3, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout(mainLayout);

        this.getShell().setText("Collective Convection Forecast Message");

        // this.volcano = this.volAttrDlgInstance.getVolcano();//TODO: already
        // set in attrDlg before this is open 20100309

        txtInfo = new Text(top, SWT.MULTI | SWT.BORDER | SWT.H_SCROLL
                | SWT.V_SCROLL);
        GridData gData = new GridData(800, 300);
        gData.horizontalSpan = 3;
        txtInfo.setEditable(false);
        txtInfo.setBackground(new Color(this.getShell().getDisplay(), 235, 235,
                235));
        txtInfo.setLayoutData(gData);
        txtInfo.setFont(new Font(this.getShell().getDisplay(), "Monospace", 11,
                SWT.NORMAL));
        txtInfo.setText(getFileContent());// getFileContent());

        Group top_3 = new Group(top, SWT.LEFT);
        top_3.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true,
                LAYOUT_WIDTH, 1));
        top_3.setLayout(new GridLayout(LAYOUT_WIDTH, false));

        Label lblFileName = new Label(top_3, SWT.LEFT);
        lblFileName.setText("File Name: ");

        txtSave = new Text(top_3, SWT.BORDER);// | SWT.READ_ONLY);
        txtSave.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false,
                1, 1));
        txtSave.setText(getFileName());

        return top;

    }

    /**
     * set the content for the text field; called by CcfpTimeDlg.java
     * 
     * @param txt
     */
    public void setFileContent(String txt) {
        txtFileContent = txt;
        if (txtInfo != null && (!txtInfo.isDisposed()))
            txtInfo.setText(txtFileContent);
    }

    /**
     * set the name of the text file; called by CcfpTimeDlg.java
     * 
     * @param name
     */
    public void setFileName(String name) {
        txtFileName = name;
    }

    public void setTxtSaveTxt(String fname) {
        // this.txtSave.setText(fname);
    }

    /*
     * return the text file content
     */
    private String getFileContent() {
        return this.txtFileContent;
    }

    /*
     * return the text file content
     */
    private String getFileName() {
        return txtFileName;
    }

    /**
     * return the CcfpTimeDlg instance
     * 
     * @return CcfpTimeDlg instance
     */
    public CcfpTimeDlg getTimeDlg() {
        return timeDlg;
    }

    /**
     * set the CcfpTimeDlg instance
     * 
     * @param timeDlg
     *            : CcfpTimeDlg instance
     */
    public void setTimeDlg(CcfpTimeDlg timeDlg) {
        this.timeDlg = timeDlg;
    }

    /**
     * getter for Issue time
     * 
     * @return issue time String
     */
    public String getIssueTime() {
        return issueTime;
    }

    /**
     * setter for issue time
     * 
     * @param issueTime
     */
    public void setIssueTime(String issueTime) {
        this.issueTime = issueTime;
    }

    /**
     * getter for valid time
     * 
     * @return valid time String
     */
    public String getValidTime() {
        return validTime;
    }

    /**
     * setter for valid time
     * 
     * @param validTime
     */
    public void setValidTime(String validTime) {
        this.validTime = validTime;
    }

}
