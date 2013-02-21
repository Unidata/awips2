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
package com.raytheon.viz.hydro.appsdefaults;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog displaying the current Apps_defaults settings for the SHEF Decoder.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 26, 2011            mpduff     Initial creation
 * Dec 07, 2012 1353       rferrel     Make non-blocking dialog.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SHEFAppsDefaultsDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SHEFAppsDefaultsDlg.class);

    private final String CONFIG_FILE_NAME = "hydro" + File.separatorChar
            + "shefGadTokens.xml";

    /**
     * Font used in the dialog.
     */
    private Font font;

    /**
     * The text area.
     */
    private StyledText textArea;

    private List<String> tokenList = new ArrayList<String>();

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public SHEFAppsDefaultsDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("SHEF Apps_defaults Settings");
        populateTokenList();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(final Shell shell) {
        setReturnValue(false);

        font = new Font(shell.getDisplay(), "Monospace", 11, SWT.NORMAL);

        // Initialize the labels
        Composite labelComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        labelComp.setLayout(gl);

        GridData gd = new GridData(245, SWT.DEFAULT);
        Label idLbl = new Label(labelComp, SWT.NONE);
        idLbl.setLayoutData(gd);
        idLbl.setText("Token");

        Label nameLbl = new Label(labelComp, SWT.NONE);
        nameLbl.setText("Value");
        nameLbl.setLayoutData(gd);

        // Initialize text area
        GridData gd2 = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd2.widthHint = 650;
        gd2.heightHint = 500;
        textArea = new StyledText(shell, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL
                | SWT.H_SCROLL);
        textArea.setFont(font);
        textArea.setEditable(false);
        textArea.setLayoutData(gd2);

        // Add a close button
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl2 = new GridLayout(1, false);
        centeredComp.setLayout(gl2);
        GridData gd3 = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd3);

        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.setLayoutData(gd3);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        populateDlg();
    }

    private void populateDlg() {
        AppsDefaults ad = AppsDefaults.getInstance();
        String format = "%-26s %s";
        StringBuilder sb = new StringBuilder();
        for (String s : tokenList) {
            sb.append(String.format(format, s, ad.getToken(s) + "\n"));
        }

        this.textArea.setText(sb.toString());
    }

    private void populateTokenList() {
        // Read in the xml
        IPathManager pm = PathManagerFactory.getPathManager();
        System.out.println("Searching for " + CONFIG_FILE_NAME);
        File file = pm.getStaticFile(this.CONFIG_FILE_NAME);
        String configPath = null;
        if (file != null) {
            configPath = file.getAbsolutePath();
            try {
                SHEFAppsDefaultsXML xml = SerializationUtil
                        .jaxbUnmarshalFromXmlFile(SHEFAppsDefaultsXML.class,
                                configPath);
                for (String token : xml.getTokenList()) {
                    tokenList.add(token);
                }
            } catch (SerializationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
            }
        } else {
            MessageBox messageBox = new MessageBox(this.getParent(), SWT.ERROR);
            messageBox.setText("File Not Found");
            messageBox.setMessage("shefGadTokens.xml file not found.");
            messageBox.open();
        }
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
}
