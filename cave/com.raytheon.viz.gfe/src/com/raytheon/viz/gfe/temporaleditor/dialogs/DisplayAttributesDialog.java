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

package com.raytheon.viz.gfe.temporaleditor.dialogs;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.temporaleditor.AbstractTemporalEditorBar;
import com.raytheon.viz.gfe.temporaleditor.TEParmDisplayAttributes;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Dialog to select temporal editor parm display attributes.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/19/09     #2159      rjpeter     Initial creation.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class DisplayAttributesDialog extends CaveJFACEDialog {
    private Composite top;

    AbstractTemporalEditorBar bar;

    Parm parm;

    TEParmDisplayAttributes origParmDispAtt;

    TEParmDisplayAttributes tempParmDispAtt;

    public DisplayAttributesDialog(Shell parent, AbstractTemporalEditorBar bar,
            Parm parm) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL | SWT.CLOSE);
        this.bar = bar;
        this.parm = parm;
        origParmDispAtt = bar.getParmDisplayAttributes(parm);
        this.tempParmDispAtt = new TEParmDisplayAttributes();
        this.tempParmDispAtt.set(origParmDispAtt);
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        top.setLayout(layout);

        initializeComponents();
        top.layout();

        return top;
    }

    private void initializeComponents() {
        GridData data = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        data.horizontalSpan = 2;

        Label lab = new Label(top, SWT.NONE);
        if (tempParmDispAtt.isDisplayedAsGraphic()) {
            lab.setText("Temporal Editor, Graphic Mode");
        } else {
            lab.setText("Temporal Editor, Image Mode");
        }
        lab.setLayoutData(data);

        // create image composite first
        if (!tempParmDispAtt.isDisplayedAsGraphic()) {
            Composite imageComp = new Composite(top, SWT.BORDER);
            GridLayout gridLayout = new GridLayout();
            gridLayout.numColumns = 1;
            imageComp.setLayout(gridLayout);
            data = new GridData(SWT.BEGINNING, SWT.DEFAULT, true, false);
            imageComp.setLayoutData(data);

            Label imageLabel = new Label(imageComp, SWT.NONE);
            data = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
            imageLabel.setLayoutData(data);
            imageLabel.setText("Image Visuals");

            final Button cb = new Button(imageComp, SWT.RADIO);
            final Button crb = new Button(imageComp, SWT.RADIO);
            cb.setText("TEColorBar");
            crb.setText("TEColorRangeBar");
            cb.setSelection(tempParmDispAtt.hasColorBar());
            crb.setSelection(tempParmDispAtt.hasColorRangeBar());
            cb.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent arg0) {
                }

                public void widgetSelected(SelectionEvent arg0) {
                    tempParmDispAtt.setColorBar(cb.getSelection());
                    crb.setSelection(!cb.getSelection());
                    tempParmDispAtt.setColorRangeBar(crb.getSelection());
                }
            });
            crb.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent arg0) {
                }

                public void widgetSelected(SelectionEvent arg0) {
                    tempParmDispAtt.setColorRangeBar(crb.getSelection());
                    cb.setSelection(!crb.getSelection());
                    tempParmDispAtt.setColorBar(cb.getSelection());
                }
            });
        }

        // always create graphic composite
        Composite graphicComp = new Composite(top, SWT.BORDER);
        GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = 1;
        graphicComp.setLayout(gridLayout);

        // if graphic composite need to set span to 2
        if (tempParmDispAtt.isDisplayedAsGraphic()) {
            data = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
            data.horizontalSpan = 2;
        } else {
            data = new GridData(SWT.TOP, SWT.DEFAULT, true, false);
        }

        graphicComp.setLayoutData(data);

        Label graphicLabel = new Label(graphicComp, SWT.NONE);
        data = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        graphicLabel.setLayoutData(data);
        graphicLabel.setText("Graphic Visuals");

        final Button tb = new Button(graphicComp, SWT.CHECK);
        final Button rb = new Button(graphicComp, SWT.CHECK);
        tb.setText("TimeBar");
        rb.setText("RangeBar");
        tb.setSelection(tempParmDispAtt.hasTimeBar());
        rb.setSelection(tempParmDispAtt.hasRangeBar());
        tb.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent arg0) {
            }

            public void widgetSelected(SelectionEvent arg0) {
                tempParmDispAtt.setTimeBar(tb.getSelection());
            }
        });
        rb.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent arg0) {
            }

            public void widgetSelected(SelectionEvent arg0) {
                tempParmDispAtt.setRangeBar(rb.getSelection());
            }
        });

        if (GridType.VECTOR.equals(parm.getGridInfo().getGridType())) {
            final Button wb = new Button(graphicComp, SWT.CHECK);
            final Button wa = new Button(graphicComp, SWT.CHECK);
            wb.setText("WindBarb");
            wa.setText("WindArrow");
            wb.setSelection(tempParmDispAtt.hasWindBarb());
            wa.setSelection(tempParmDispAtt.hasWindArrow());
            wb.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent arg0) {
                }

                public void widgetSelected(SelectionEvent arg0) {
                    tempParmDispAtt.setWindBarb(wb.getSelection());
                }
            });
            wa.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent arg0) {
                }

                public void widgetSelected(SelectionEvent arg0) {
                    tempParmDispAtt.setWindArrow(wa.getSelection());
                }
            });
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Display Attributes Dialog");
    }

    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == IDialogConstants.PROCEED_ID
                || buttonId == IDialogConstants.OK_ID) {
            boolean valid = true;

            if (tempParmDispAtt.isDisplayedAsGraphic()) {
                if (!tempParmDispAtt.hasTimeBar()
                        && !tempParmDispAtt.hasRangeBar()) {
                    GridType gridType = parm.getGridInfo().getGridType();
                    if (!GridType.VECTOR.equals(gridType)
                            || (GridType.VECTOR.equals(gridType)
                                    && !tempParmDispAtt.hasWindBarb() && !tempParmDispAtt
                                    .hasWindArrow())) {
                        valid = false;
                        MessageBox mb = new MessageBox(getShell(),
                                SWT.ICON_WARNING | SWT.OK);
                        mb.setText("Graphic Attributes Error");
                        mb.setMessage("You must select at least one graphic type");
                        mb.open();
                    }
                }
            }

            if (valid) {
                bar.setParmDisplayAttributes(parm, tempParmDispAtt);
                bar.redraw();
            }
        }
        super.buttonPressed(buttonId);
    }

    @Override
    protected void cancelPressed() {
        // TODO Auto-generated method stub
        super.cancelPressed();
        bar.setParmDisplayAttributes(parm, origParmDispAtt);
        bar.redraw();
        super.cancelPressed();
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, "OK", true);
        createButton(parent, IDialogConstants.PROCEED_ID, "Apply", true);
        createButton(parent, IDialogConstants.CANCEL_ID, "Cancel", false);
    }
}
