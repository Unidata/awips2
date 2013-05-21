/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.vaaDialog.CcfpTimeDlg
 * 
 * 20 September 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.sigmet.CcfpInfo;
import gov.noaa.nws.ncep.ui.pgen.store.PgenStorageException;
import gov.noaa.nws.ncep.ui.pgen.store.StorageUtils;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Singleton dialog for CCFP issue and valid times.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ---------	--------	----------	--------------------------
 * 09/10		322			G. Zhang 	Initial Creation.  
 * 07/11        #450        G. Hull     NcPathManager
 * 04/13        #977        S. Gilbert  PGEN Database support
 * </pre>
 * 
 * @author gzhang
 */

public class CcfpTimeDlg extends AttrDlg {

    // singleton instance
    private static CcfpTimeDlg INSTANCE = null;

    // width of the layout
    private static final int LAYOUT_WIDTH = 14;

    /**
     * constant for pgentype of CCFP
     */
    public static final String PGEN_TYPE_CCFP = "CCFP_SIGMET";

    /**
     * this String MUST be identical to ccfpTimes.xml's Time element issue
     * attribute
     */
    public static final String CCFP_ISSUE_TIME = "issue";

    /**
     * map each issue time to some valid times
     */
    public Map<String, String[]> issueValidTimeMap = new HashMap<String, String[]>();

    /**
     * map each issue time to certain Calendar day
     */
    public Map<String, String> issueTimeDayMap = new HashMap<String, String>();

    // parent for widgets
    protected Composite top = null;

    // Combo widgets for issue and valid times
    private Combo cmbIssTime;

    private Combo cmbVaTime;

    // editableAttrStartTime;
    private String ccfpIssueTime = "";// this.getTimes(false)[0];//"";

    // editableAttrEndTime;
    private String ccfpValidTime = "";// this.getTimes(true)[0];//"";

    public CcfpTimeDlg(Shell parShell) throws VizException {
        super(parShell);
        // TODO Auto-generated constructor stub
    }

    public static CcfpTimeDlg getInstance(Shell parShell) {
        if (INSTANCE == null) {
            try {
                INSTANCE = new CcfpTimeDlg(parShell);
            } catch (VizException e) {
                e.printStackTrace();
            }
        }
        return INSTANCE;
    }

    @Override
    public Control createDialogArea(Composite parent) {

        this.top = (Composite) super.createDialogArea(parent);

        GridLayout mainLayout = new GridLayout(LAYOUT_WIDTH, false);
        mainLayout.marginHeight = LAYOUT_WIDTH;
        mainLayout.marginWidth = LAYOUT_WIDTH;
        top.setLayout(mainLayout);
        getShell().setText("Collaborative Convective");

        this.createTimesArea(top);

        return top;
    }

    @Override
    public void createButtonsForButtonBar(Composite parent) {
        // super.createButtonsForButtonBar(parent);

        ((GridLayout) parent.getLayout()).verticalSpacing = 0;
        ((GridLayout) parent.getLayout()).marginHeight = 3;

        createButton(parent, IDialogConstants.OK_ID, "Continue", true);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);

        this.getButton(IDialogConstants.CANCEL_ID).setEnabled(true);
        this.getButton(IDialogConstants.OK_ID).setEnabled(true);

        this.getButton(IDialogConstants.CANCEL_ID).setLayoutData(
                new GridData(ctrlBtnWidth, ctrlBtnHeight));
        this.getButton(IDialogConstants.OK_ID).setLayoutData(
                new GridData(ctrlBtnWidth, ctrlBtnHeight));

    }

    @Override
    public void cancelPressed() {
        setReturnCode(CANCEL);
        close();
    }

    private void createTimesArea(Composite top) {

        // layout

        Group top_3 = new Group(top, SWT.LEFT);
        top_3.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true,
                LAYOUT_WIDTH, 1));
        top_3.setLayout(new GridLayout(LAYOUT_WIDTH, false));

        // ---Issue Time part

        Label lblIssTime = new Label(top_3, SWT.LEFT);
        lblIssTime.setText("Issue Time: ");

        fillSpaces(top_3, SWT.LEFT, 9, true);

        cmbIssTime = new Combo(top_3, SWT.LEFT | SWT.READ_ONLY);
        cmbIssTime.setItems(parseCcfpTimesFile());
        cmbIssTime.select(0);

        fillSpaces(top_3, SWT.LEFT, 3, true);

        // ---Valid Time part, Valid times depend on Issue times

        Label lblVaTime = new Label(top_3, SWT.LEFT);
        lblVaTime.setText("Valid Time: ");

        fillSpaces(top_3, SWT.LEFT, 9, true);

        String[] vtimes = issueValidTimeMap.get(cmbIssTime.getText().trim());

        cmbVaTime = new Combo(top_3, SWT.LEFT | SWT.READ_ONLY);
        cmbVaTime.setItems(vtimes);
        cmbVaTime.select(0);

        fillSpaces(top_3, SWT.LEFT, 3, true);

        // field initialization

        ccfpIssueTime = cmbIssTime.getText().trim();
        ccfpValidTime = cmbVaTime.getText().trim();

        // handle listeners

        addCmbListeners();

    }

    public String getCcfpIssueTime() {
        return ccfpIssueTime;
    }

    public void setCcfpIssueTime(String ccfpIssueTime) {
        this.ccfpIssueTime = ccfpIssueTime;
    }

    public String getCcfpValidTime() {
        return ccfpValidTime;
    }

    public void setCcfpValidTime(String ccfpValidTime) {
        this.ccfpValidTime = ccfpValidTime;
    }

    /*
     * take the empty spaces
     */
    private void fillSpaces(Composite gp, int dir, int num, boolean empty) {
        for (int i = 0; i < num; i++) {
            Label lbl = new Label(gp, dir);
            lbl.setText(empty ? "" : " ");
        }
    }

    @Override
    public void setAttrForDlg(IAttribute ia) {
        // TODO Auto-generated method stub

    }

    /**
     * save the CCFP Products into an xml file
     */
    @Override
    public void okPressed() {

        Product product = CcfpInfo.getCcfpPrds(ccfpIssueTime, ccfpValidTime);

        // save CCFP xml file then apply style sheet to get text product
        // String fileName = CcfpInfo
        // .saveCcfpXmlFile(ccfpIssueTime, ccfpValidTime);
        String activityXML;
        try {
            activityXML = StorageUtils.serializeProduct(product);
        } catch (PgenStorageException e1) {
            StorageUtils.showError(e1);
            return;
        }
        String txtPrd = CcfpInfo.convertXml2Txt(
                activityXML,
                PgenStaticDataProvider.getProvider().getFileAbsolutePath(
                        PgenStaticDataProvider.getProvider()
                                .getPgenLocalizationRoot()
                                + CcfpMsgDlg.PGEN_CCFP_XSLT));

        // close this dialog
        cancelPressed();

        // create the text product dialog then open it
        CcfpMsgDlg cmDlg = CcfpMsgDlg.getInstance(getParentShell());
        cmDlg.setIssueTime(ccfpIssueTime);
        cmDlg.setValidTime(ccfpValidTime);
        cmDlg.setTimeDlg(this);
        cmDlg.setFileContent(txtPrd.trim());
        cmDlg.setProduct(product);
        cmDlg.setFileName(getCcfpIssueTime().trim() + ".ccfp");

        cmDlg.open();

    }

    // ------------------------------------------------------------------added
    // 201001

    /*
     * read the ccfpTimes.xml file and store info in data structures
     */
    private String[] parseCcfpTimesFile() {

        ArrayList<String> issueTimes = new ArrayList<String>();
        ArrayList<String[]> validTimes = new ArrayList<String[]>();

        Document doc = null;
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        // String fileName = NmapCommon.getCcfpTimesXmlFile();

        try {

            File file = PgenStaticDataProvider.getProvider().getFile(
                    PgenStaticDataProvider.getProvider()
                            .getPgenLocalizationRoot() + "ccfpTimes.xml");

            DocumentBuilder builder = factory.newDocumentBuilder();
            doc = builder.parse(file.getAbsoluteFile());
        } catch (Exception e) {
            System.out.println("-----------" + e.getMessage());
        }

        NodeList nlist = doc.getElementsByTagNameNS("*", "*");

        for (int i = 0; i < nlist.getLength(); i++) {

            Node nElem = nlist.item(i);// each element, like: Time, etc

            // String elemName = nElem.getNodeName().trim();
            NamedNodeMap nnMap = nElem.getAttributes();

            ArrayList<String> vtimes = new ArrayList<String>();
            String itime = "";

            for (int j = 0; j < nnMap.getLength(); j++) {

                Node nAttr = nnMap.item(j);// each attribute of an element,
                                           // like: issue, valid1, or valid2

                if (CCFP_ISSUE_TIME.equalsIgnoreCase(nAttr.getNodeName())) {

                    itime = nAttr.getNodeValue();
                    handleTime(issueTimes, itime, true, itime);

                } else {
                    handleTime(vtimes, nAttr.getNodeValue(), false, itime);
                }

            }

            validTimes.add(vtimes.toArray(new String[] {}));

        }

        for (int k = 0; k < issueTimes.size(); k++)
            issueValidTimeMap.put(issueTimes.get(k), validTimes.get(k + 1));

        return issueTimes.toArray(new String[] {});

    }

    /*
     * process hour info
     */
    public void handleTime(ArrayList<String> list, String hour,
            boolean isIssueTime, String itime) {

        Boolean hourOnly = null, today = true, tomorrow = false;
        ;

        int fh = 0, ch = 0, ih = 0;
        try {
            ch = Integer.parseInt(getDayOrHour(hourOnly));
            fh = Integer.parseInt(hour);
            ih = Integer.parseInt(itime);
        } catch (Exception e) {
        }

        if (isIssueTime) {
            String day = (fh + 100) < ch ? getDayOrHour(tomorrow)
                    : getDayOrHour(today);// fh+100: from legacy code
            list.add(day + "_" + hour);
            issueTimeDayMap.put(itime, day);
        } else {
            list.add((fh < ih ? getDayOrHour(tomorrow) : issueTimeDayMap
                    .get(itime)) + "_" + hour);
        }

    }

    /*
     * get today, tomorrow, or current Hour
     */
    public String getDayOrHour(Boolean isToday) {

        Calendar today = Calendar.getInstance();

        Calendar tomorrow = Calendar.getInstance();
        tomorrow.add(Calendar.DAY_OF_MONTH, 1);

        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        SimpleDateFormat sdft = new SimpleDateFormat("HH00");
        sdft.setTimeZone(TimeZone.getTimeZone("GMT"));

        return isToday == null ? sdft.format(today.getTime())// today's hour
                : isToday ? sdf.format(today.getTime()) : sdf.format(tomorrow
                        .getTime());
    }

    /*
     * add listeners for the two Combo
     */
    private void addCmbListeners() {

        cmbIssTime.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event e) {

                String it = cmbIssTime.getText().trim();
                String[] items = issueValidTimeMap.get(it);
                cmbVaTime.setItems(items);
                cmbVaTime.select(0);

                ccfpIssueTime = it;

            }
        });

        cmbVaTime.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event e) {

                ccfpValidTime = cmbVaTime.getText().trim();

            }
        });
    }

}