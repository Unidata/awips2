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

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import javax.xml.bind.JAXB;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Dialog displaying the current Apps_defaults settings for the SHEF Decoder.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 26, 2011            mpduff      Initial creation
 * Dec 07, 2012 1353       rferrel     Make non-blocking dialog.
 * Aug 09, 2013 2033       mschenke    Switched File.separator to IPathManager.SEPARATOR
 * Nov 04, 2013 2361       njensen     Use JAXB instead of SerializationUtil
 * Apr 08, 2016 5483       dgilling    Refactor based on CaveJFACEDialog, fix 
 *                                     hi-dpi issues.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public final class SHEFAppsDefaultsDlg extends CaveJFACEDialog {

    private static final String CONFIG_FILE_NAME = "hydro"
            + IPathManager.SEPARATOR + "shefGadTokens.xml";

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public SHEFAppsDefaultsDlg(Shell parent) {
        super(parent);
        setBlockOnOpen(false);
        setShellStyle(SWT.DIALOG_TRIM);
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);

        final Table table = new Table(composite, SWT.BORDER | SWT.MULTI
                | SWT.FULL_SELECTION | SWT.V_SCROLL);
        table.setLinesVisible(false);
        table.setHeaderVisible(true);
        table.addKeyListener(new KeyAdapter() {

            @Override
            public void keyPressed(KeyEvent e) {
                if ((e.keyCode == 'c') && (e.stateMask == SWT.CTRL)) {
                    StringBuilder sb = new StringBuilder();
                    for (TableItem item : table.getSelection()) {
                        sb.append(String.format("%-26s %s", item.getText(0),
                                item.getText(1)));
                        sb.append('\n');
                    }
                    sb.deleteCharAt(sb.length() - 1);

                    Clipboard clipboard = new Clipboard(e.display);
                    clipboard.setContents(new Object[] { sb.toString() },
                            new Transfer[] { TextTransfer.getInstance() });
                }
            }
        });

        String[] titles = { "Token", "Value" };
        for (String title : titles) {
            TableColumn column = new TableColumn(table, SWT.LEFT);
            column.setText(title);
        }

        int maxTokenLen = -Integer.MAX_VALUE;
        int maxValueLen = -Integer.MAX_VALUE;
        for (Pair<String, String> entry : getTableEntries()) {
            TableItem item = new TableItem(table, SWT.NONE);
            item.setFont(JFaceResources.getTextFont());
            String token = String.valueOf(entry.getFirst());
            maxTokenLen = Math.max(maxTokenLen, token.length());
            item.setText(0, token);
            String value = String.valueOf(entry.getSecond());
            maxValueLen = Math.max(maxValueLen, value.length());
            item.setText(1, value);
        }

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = table.getHeaderHeight()
                + (table.getItemHeight() * table.getItemCount());
        GC gc = new GC(table);
        gc.setFont(JFaceResources.getTextFont());
        gd.widthHint = (maxTokenLen + maxValueLen + 4)
                * gc.getFontMetrics().getAverageCharWidth();
        gc.dispose();
        table.setLayoutData(gd);

        for (int i = 0; i < table.getColumnCount(); i++) {
            table.getColumn(i).pack();
        }

        return composite;
    }

    @Override
    protected void buttonPressed(int buttonId) {
        switch (buttonId) {
        case IDialogConstants.CLOSE_ID:
            close();
            break;
        default:
            statusHandler.warn(String.format(
                    "Unrecognized button [%d] pressed.", buttonId));
            break;
        }
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.CLOSE_ID,
                IDialogConstants.CLOSE_LABEL, true);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("SHEF Apps_defaults Settings");
    }

    private Collection<Pair<String, String>> getTableEntries() {
        Collection<String> tokenList = getTokenList();

        Collection<Pair<String, String>> entries = new ArrayList<>(
                tokenList.size());
        AppsDefaults ad = AppsDefaults.getInstance();
        for (String s : tokenList) {
            entries.add(new Pair<String, String>(s, ad.getToken(s)));
        }

        return entries;
    }

    private Collection<String> getTokenList() {
        Collection<String> tokenList = Collections.emptyList();

        // Read in the xml
        statusHandler.debug("Searching for " + CONFIG_FILE_NAME);
        IPathManager pm = PathManagerFactory.getPathManager();
        ILocalizationFile file = pm.getStaticLocalizationFile(CONFIG_FILE_NAME);
        if (file != null) {
            try (InputStream inStream = file.openInputStream()) {
                SHEFAppsDefaultsXML xml = JAXB.unmarshal(inStream,
                        SHEFAppsDefaultsXML.class);

                tokenList = new ArrayList<>();
                for (String token : xml.getTokenList()) {
                    tokenList.add(token);
                }
            } catch (Exception e) {
                statusHandler.error(String.format("Error reading file [%s]",
                        CONFIG_FILE_NAME), e);
            }
        } else {
            MessageDialog.openError(getShell(), "File Not Found",
                    "shefGadTokens.xml file not found.");
        }

        return tokenList;
    }
}
