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
package com.raytheon.viz.texteditor.scripting.dialogs;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Implements the Text WS Script Help selection dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 30, 2009            mfegan     Initial creation
 * Sep 20, 2012 1196       rferrel    Added DO_NOT_BLOCK to constructor
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class HelpRequestDlg extends CaveSWTDialog implements SelectionListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(HelpRequestDlg.class);

    private static final String DLG_TITLE = "Command Help";

    /** the selection list */
    private List selections = null;

    private final EnumHelpTypes type;

    public enum EnumHelpTypes {
        BASIC("On Basic Commands", "textScriptCommandsHelp.txt"), ADVANCED(
                "On Advanced Commands", "textScriptAdvancedHelp.txt");
        public final static String BASE_PATH = "textws/help";

        private final String name;

        private final String fName;

        private EnumHelpTypes(String name, String fName) {
            this.name = name;
            this.fName = fName;
        }

        public String getName() {
            return name;
        }

        public String getFileName() {
            return fName;
        }
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            shell controlling this dialog
     * @param type
     *            type of help request
     * @param token
     *            Text Editor ID token
     */
    public HelpRequestDlg(Shell parent, EnumHelpTypes type, String token) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.FILL,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        setText(DLG_TITLE);

        this.type = type;
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout layout = new GridLayout(1, false);
        layout.marginHeight = 2;
        layout.marginLeft = 0;
        layout.marginRight = 0;
        layout.verticalSpacing = 1;
        return layout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Point size = new Point(250, 500);
        shell.setSize(size);

        createClientArea();
        // TODO Move to preOpened and limit the height of the dialog.
        loadCommandList();
    }

    /**
     * Loads the script command list for the appropriate type.
     */
    private void loadCommandList() {
        try {
            String path = EnumHelpTypes.BASE_PATH + File.separator
                    + this.type.getFileName();
            File bundle = PathManagerFactory.getPathManager().getStaticFile(
                    path);
            System.out.println(bundle);
            String contents = loadFileToString(bundle);
            String[] commands = contents.split(System
                    .getProperty("line.separator"));
            for (String command : commands) {
                selections.add(command);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to load commands file " + this.type.getFileName(),
                    e);
        }
    }

    /*
     * This will be replaced with code using CAVE's internal file access
     * mechanism
     */
    private String loadFileToString(File file) throws IOException {
        Reader is = null;
        try {
            is = new FileReader(file);
            StringBuffer sb = new StringBuffer();
            char[] b = new char[8192];
            int n;
            while ((n = is.read(b)) > 0) {
                sb.append(b, 0, n);
            }
            return sb.toString();
        } finally {
            if (is != null) {
                is.close();
            }
        }
    }

    private void createClientArea() {
        selections = new List(shell, SWT.BORDER | SWT.V_SCROLL);
        selections.setLayoutData(new GridData(GridData.FILL_BOTH));
        Canvas cv = new Canvas(shell, SWT.NONE);
        GridLayout layout = new GridLayout(2, true);
        cv.setLayout(layout);
        cv.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_CENTER));

        GridData gd = new GridData();
        gd.horizontalAlignment = SWT.CENTER;
        gd.widthHint = 50;
        Button btn = new Button(cv, SWT.PUSH);
        btn.setText("Help");
        btn.setLayoutData(gd);
        btn.addSelectionListener(this);

        btn = new Button(cv, SWT.PUSH);
        btn.setText("Close");
        btn.setLayoutData(gd);
        btn.addSelectionListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse
     * .swt.events.SelectionEvent)
     */
    @Override
    public void widgetDefaultSelected(SelectionEvent e) {
        // intentionally empty
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt
     * .events.SelectionEvent)
     */
    @Override
    public void widgetSelected(SelectionEvent event) {
        Object obj = event.getSource();
        if (!(obj instanceof Button)) {
            return;
        }
        String text = ((Button) obj).getText().toLowerCase();
        if ("help".equals(text)) {
            onHelp();
        } else if ("close".equals(text)) {
            onClose();
        }
    }

    private void onHelp() {
        int indx = selections.getSelectionIndex();
        if (indx > -1) {
            String cmd = selections.getItem(indx);
            System.out.println("requesting help on " + cmd);
        }
    }

    private void onClose() {
        shell.dispose();
    }

}
