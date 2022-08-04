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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.text.WordUtils;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Display the CallToActions dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 12, 2010  3463     RT        Initial creation
 * Nov 07, 2012  1298     rferrel   Changes for non-blocking dialog.
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class CallToActionsDlg extends CaveJFACEDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CallToActionsDlg.class);

    private static final String psplitRE = "\n\\s*?(?:\n\\s*?)+";

    /**
     * Composite containing the Product Editor controls.
     */
    private ProductEditorComp productEditorComp;

    /**
     * Flag indicating if the CallToAction Type .
     */
    private int callToActionType = 0;

    private String[] txt = null;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent Shell.
     * @param callToActionType
     * @param CtaText
     * @param productEditorComp
     */
    public CallToActionsDlg(Shell parent, int callToActionType,
            String[] CtaText, ProductEditorComp productEditorComp) {
        super(parent);

        this.setShellStyle(
                SWT.DIALOG_TRIM | SWT.RESIZE | SWT.APPLICATION_MODAL);
        this.callToActionType = callToActionType;
        this.txt = CtaText;
        this.productEditorComp = productEditorComp;

    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Call To Actions");
    }

    @Override
    protected boolean isResizable() {
        return true;
    }

    @Override
    protected Point getInitialSize() {
        int width = GFEPreference.getInt("ProductOutputDialog_CTAWidth", 575);
        int height = GFEPreference.getInt("ProductOutputDialog_CTAHeight", 300);
        return new Point(width, height);
    }

    @Override
    protected Control createContents(Composite parent) {
        Composite comp = (Composite) super.createContents(parent);
        comp.layout();
        return comp;
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        ScrolledComposite scrolled = new ScrolledComposite(parent,
                SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
        GridLayout layout = new GridLayout(1, false);
        scrolled.setLayout(layout);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        scrolled.setLayoutData(gridData);

        Composite comp = new Composite(scrolled, SWT.NONE);
        scrolled.setContent(comp);

        layout = new GridLayout(1, false);
        comp.setLayout(layout);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        comp.setLayoutData(gridData);

        List<Object> ctaList;
        switch (callToActionType) {
        case 1: // Hazard
            ctaList = new ArrayList<>();
            for (String phenSig : this.txt) {
                ctaList.addAll(TextFmtCtaUtil.hazText(phenSig));
            }
            break;
        case 2: // Product
            ctaList = TextFmtCtaUtil.prodsText(this.txt[0]);
            break;
        case 3: // Generic
            ctaList = TextFmtCtaUtil.ctaText();
            break;
        default:
            throw new IllegalArgumentException(
                    "Unknown callToActionType: " + callToActionType);
        }
        createButtons(comp, ctaList);

        scrolled.setExpandHorizontal(true);
        scrolled.setExpandVertical(true);

        comp.pack();
        comp.layout();
        scrolled.setMinSize(comp.getSize());

        return scrolled;
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    private String formatText(String text) {
        String[] split = text.split(psplitRE);

        StringBuilder result = new StringBuilder();
        for (String str : split) {
            result.append(WordUtils.wrap(str, 66)).append("\n\n");
        }

        if (result.length() > 2) {
            result.delete(result.length() - 2, result.length());
        }
        return result.toString();
    }

    private void createButtons(Composite comp, List<?> ctaList) {

        for (Object obj : ctaList) {
            String label, text;
            if (obj instanceof String) {
                label = text = (String) obj;
            } else if (obj instanceof List) {
                @SuppressWarnings("unchecked")
                List<String> list = (List<String>) obj;
                label = list.get(0);
                text = list.get(1);
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Unrecognized entry in CallToActions.py genericCTAs: "
                                + obj);
                continue;
            }

            text = formatText(text);
            label = formatText(label);

            Button button = new Button(comp, SWT.PUSH | SWT.LEFT);
            GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true,
                    false);
            button.setLayoutData(layoutData);
            button.setText(label);
            button.setData(text);
            button.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    String text = (String) ((Button) event.widget).getData();
                    productEditorComp.insertText(text);
                }
            });
        }
    }

}
