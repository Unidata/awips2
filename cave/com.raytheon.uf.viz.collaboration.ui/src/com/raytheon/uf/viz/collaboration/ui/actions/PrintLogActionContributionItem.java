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
package com.raytheon.uf.viz.collaboration.ui.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.StyledTextPrintOptions;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;

import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.session.IPrintableView;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * An <code>ActionContributionItem</code> to print a chat conversation log to
 * printer. Utilizes <code>StyledText</code>'s native print functionality.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2012            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class PrintLogActionContributionItem extends ActionContributionItem {

    private static class PrintStyledTextAction extends Action {

        private IPrintableView view;

        private ModifyListener modListener;

        public PrintStyledTextAction(IPrintableView view) {
            super("Print Log", IconUtil.getImageDescriptor(Activator
                    .getDefault().getBundle(), "print.gif"));
            this.view = view;
            this.modListener = new ModifyListener() {

                @Override
                public void modifyText(ModifyEvent e) {
                    setEnabled(shouldEnable());
                }
            };
            this.view.getStyledText().addModifyListener(modListener);
            setEnabled(shouldEnable());
        }

        private boolean shouldEnable() {
            StyledText textControl = view.getStyledText();
            return ((textControl != null) && (!textControl.getText().isEmpty()));
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            PrinterData data = new PrintDialog(view.getSite()
                    .getWorkbenchWindow().getShell()).open();
            if (data != null) {
                Printer printer = new Printer(data);
                StyledTextPrintOptions options = new StyledTextPrintOptions();
                options.header = view.getHeaderText();
                options.footer = "\t" + StyledTextPrintOptions.PAGE_TAG;
                options.printTextFontStyle = true;
                options.printTextForeground = true;
                Runnable printJob = view.getStyledText()
                        .print(printer, options);
                printJob.run();
                printer.dispose();
            }
        }

        public void dispose() {
            StyledText textControl = view.getStyledText();
            if ((textControl != null) && (!textControl.isDisposed())) {
                textControl.removeModifyListener(modListener);
            }
        }

    }

    /**
     * Constructor.
     * 
     * @param view
     *            <code>IPrintableView</code> that will be used for printing.
     *            Ensure that <code>view.getStyleText</code> is not null or this
     *            will throw exceptions.
     */
    public PrintLogActionContributionItem(IPrintableView view) {
        super(new PrintStyledTextAction(view));
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.ActionContributionItem#dispose()
     */
    @Override
    public void dispose() {
        ((PrintStyledTextAction) getAction()).dispose();
        super.dispose();
    }

}
