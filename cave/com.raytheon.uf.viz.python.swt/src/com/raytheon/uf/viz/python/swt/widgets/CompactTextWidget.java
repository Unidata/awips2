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
package com.raytheon.uf.viz.python.swt.widgets;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;

/**
 * Yet another type of entry widget to support Serp tool
 *
 * This is basically a text entry widget but with label on the group instead if
 * next to the text entry field.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jan 15, 2018           randerso  Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public class CompactTextWidget extends InputWidget {

    /**
     * Constructor
     *
     * @param label
     */
    public CompactTextWidget(String label) {
        super();
        setLabel(label);
    }

    @Override
    public Composite buildComposite(Composite parent) {

        Group group = new Group(parent, SWT.NONE);

        GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = 2;
        gridLayout.marginHeight = 0;
        group.setLayout(gridLayout);
        group.setLayoutData(
                new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false));

        group.setText(makeGuiLabel(getLabel()));

        String s = "";
        if (getValue() != null) {
            s = getValue().toString();
        }

        int width = 10;
        if (s.length() > 10) {
            width = s.length() + 5;
        }
        GC gc = new GC(parent);
        width = Dialog.convertWidthInCharsToPixels(gc.getFontMetrics(), width);
        gc.dispose();

        text = new Text(group, SWT.BORDER);
        GridData layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, true,
                false);
        layoutData.widthHint = width;
        text.setLayoutData(layoutData);
        text.addVerifyListener(this);
        text.addModifyListener(this);
        text.addFocusListener(this);

        text.setText(s);

        group.getShell().addControlListener(new HelpRepositioner());
        group.addControlListener(new HelpRepositioner());

        return group;

    }

}
