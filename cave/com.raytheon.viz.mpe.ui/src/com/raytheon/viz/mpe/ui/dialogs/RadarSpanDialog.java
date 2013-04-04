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
package com.raytheon.viz.mpe.ui.dialogs;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.radartable.ReadBiasTableParam;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class RadarSpanDialog extends Dialog {

    private Shell shell;

    private Font font;

    private Button closeBtn;

    Composite bcLblComp;

    private int retval = 0;

    String span = "0.001";

    private static String[] biasLabelStrings = { "Memory Span (hrs)",
            "NPairs ", "Mean Gage(mm/hrs)", "Mean Radar(mm/hrs)", "Bias" };

    private Label[] colHdr = new Label[biasLabelStrings.length];

    private String npairs = "0.00";

    private String mgage = "1.32";

    private String mrad = "0.50";

    private String bias = "2.65";

    private int npairsT = RadarBiasTableDialog.default_bias;

    private String rid = "";

    String fxa_local_site = "";

    AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private static final SimpleDateFormat sdf = new SimpleDateFormat(
            "yyyyMMddHH");

    private static final SimpleDateFormat pgsdf = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");

    private static final SimpleDateFormat st3sdf;
    static {
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        pgsdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        String date_form = "";
        // String date_form = appsDefaults.getToken("st3_date_form");
        if ((date_form == null) || date_form.isEmpty()
                || date_form.equals("mdY")) {
            st3sdf = new SimpleDateFormat("MMM dd yyyy HH");
            st3sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        } else {
            st3sdf = sdf;
        }
    }

    protected RadarSpanDialog(Shell parentShell) {
        super(parentShell);
    }

    public RadarSpanDialog(Shell parentShell, String radarid) {
        super(parentShell);
        rid = radarid;
    }

    /**
     * Open method used to display the Group Edit Stations dialog.
     * 
     * @return Null.
     */
    public int open() {
        Shell parent = this.getParent();
        Display display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        shell.setText(rid);

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout(mainLayout);

        font = new Font(shell.getDisplay(), "Courier", 10, SWT.NORMAL);

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();

        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        font.dispose();

        return retval;
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        fxa_local_site = appsDefaults.getToken("FXA_LOCAL_SITE");
        ReadBiasTableParam rp = new ReadBiasTableParam();
        rp.read_bias_table_param(rid);

        createDateComp();
        createBiasColLblComp();

        for (int j = 0; j < 10; j++) {
            span = String.format("%-13.3f",
                    ReadBiasTableParam.biasData.mem_span[j]);
            npairs = String.format("%-10.2f",
                    ReadBiasTableParam.biasData.num_pairs[j]);
            mgage = String.format("%-10.2f",
                    ReadBiasTableParam.biasData.sumgag[j]);
            mrad = String.format("%-10.2f",
                    ReadBiasTableParam.biasData.sumrad[j]);
            bias = String
                    .format("%-10.2f", ReadBiasTableParam.biasData.bias[j]);
            createBiasListComp();
        }
        createButtonComp();
    }

    private void createButtonComp() {

        // Create a container to hold the buttons.
        GridData gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        Composite closeBtnComp = new Composite(shell, SWT.NONE);
        GridLayout closeBtnCompLayout = new GridLayout(2, true);
        closeBtnComp.setLayout(closeBtnCompLayout);
        closeBtnComp.setLayoutData(gd);

        GridData bd = new GridData(110, SWT.DEFAULT);
        closeBtn = new Button(closeBtnComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(bd);
        closeBtn.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                retval = 0;
                shell.dispose();
            }

        });
    }

    private void createDateComp() {

        GridData bd = new GridData(SWT.DEFAULT, SWT.CENTER, true, true);
        Composite dtLblComp = new Composite(shell, SWT.NONE);
        GridLayout dtLblCompLayout = new GridLayout(2, true);
        dtLblComp.setLayout(dtLblCompLayout);
        dtLblComp.setLayoutData(bd);

        bd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        Label tmslotLbl = new Label(dtLblComp, SWT.CENTER);
        tmslotLbl.setLayoutData(bd);
        Date dt = MPEDisplayManager.getCurrent().getCurrentEditDate();
        String dt3 = st3sdf.format(dt);
        tmslotLbl.setText(dt3 + "z");

        bd = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
        Label spc = new Label(dtLblComp, SWT.CENTER);
        spc.setText("");
        spc.setLayoutData(bd);

        bd = new GridData(SWT.DEFAULT, SWT.CENTER, true, true);
        Composite npLblComp = new Composite(dtLblComp, SWT.NONE);
        GridLayout npLblCompLayout = new GridLayout(2, true);
        npLblCompLayout.marginWidth = 1;
        npLblComp.setLayout(npLblCompLayout);
        npLblComp.setLayoutData(bd);

        bd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        Label npairsLbl = new Label(npLblComp, SWT.CENTER);
        npairsLbl.setLayoutData(bd);
        npairsLbl.setText("NPairs Threshold = ");

        bd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        Label npairsTxt = new Label(npLblComp, SWT.CENTER | SWT.BORDER);
        npairsTxt.setLayoutData(bd);
        npairsTxt.setText(String.valueOf(npairsT));

    }

    /**
     * Create the composite to hold the data column labels
     */
    private void createBiasColLblComp() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        bcLblComp = new Composite(shell, SWT.NONE);
        GridLayout bcLblCompLayout = new GridLayout(5, true);
        bcLblCompLayout.marginWidth = 1;
        bcLblComp.setLayout(bcLblCompLayout);
        bcLblComp.setLayoutData(gd);
        for (int i = 0; i < biasLabelStrings.length; i++) {
            colHdr[i] = new Label(bcLblComp, SWT.FILL | SWT.LEFT);
            colHdr[i].setText(biasLabelStrings[i]);
            colHdr[i].setSize(45, SWT.DEFAULT);
            gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
            colHdr[i].setLayoutData(gd);
        }

    }

    /**
     * Create the data options group and controls.
     */
    private void createBiasListComp() {

        // Create a container to hold the label and the combo box.
        // GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        // Composite biasListComp = new Composite(bcLblComp, SWT.V_SCROLL);
        // GridLayout biasListCompLayout = new GridLayout(5, false);
        // biasListComp.setLayout(biasListCompLayout);
        // gd.horizontalSpan = 5;
        // biasListComp.setLayoutData(gd);
        // biasListComp
        // .setSize(biasListComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        Label spanLbl = new Label(bcLblComp, SWT.LEFT | SWT.BORDER);
        spanLbl.setText(span);
        spanLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label npairsLbl = new Label(bcLblComp, SWT.LEFT | SWT.BORDER);
        npairsLbl.setText(npairs);
        npairsLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label mgageTxt = new Label(bcLblComp, SWT.CENTER | SWT.BORDER);
        mgageTxt.setText(mgage);
        mgageTxt.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label mradarTxt = new Label(bcLblComp, SWT.CENTER | SWT.BORDER);
        mradarTxt.setText(mrad);
        mradarTxt.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label biasTxt = new Label(bcLblComp, SWT.LEFT | SWT.BORDER);
        biasTxt.setText(bias);
        biasTxt.setLayoutData(gd);

    }
}
