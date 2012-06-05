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
package com.raytheon.uf.viz.collaboration.ui.feed;

import java.util.List;

import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;
import com.raytheon.uf.viz.collaboration.ui.data.AlertWord;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollaborationPreferenceContentProvider;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollaborationPreferencesLabelProvider;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationSiteFilterPreferencePage extends
        FieldEditorPreferencePage implements IWorkbenchPreferencePage {

    private TableViewer viewer;

    /**
     * 
     */
    public CollaborationSiteFilterPreferencePage() {
    }

    /**
     * @param style
     */
    public CollaborationSiteFilterPreferencePage(int style) {
        super(style);
    }

    /**
     * @param title
     * @param style
     */
    public CollaborationSiteFilterPreferencePage(String title, int style) {
        super(title, style);
        // TODO Auto-generated constructor stub
    }

    /**
     * @param title
     * @param image
     * @param style
     */
    public CollaborationSiteFilterPreferencePage(String title,
            ImageDescriptor image, int style) {
        super(title, image, style);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors
     * ()
     */
    @Override
    protected void createFieldEditors() {
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.horizontalSpan = 3;

        viewer = new TableViewer(getFieldEditorParent());
        viewer.setContentProvider(new CollaborationPreferenceContentProvider());
        viewer.setLabelProvider(new CollaborationPreferencesLabelProvider());
        viewer.getTable().setLayoutData(data);

        final StringFieldEditor stringEditor = new StringFieldEditor(
                "sitename", "Site Name", getFieldEditorParent()) {
            @Override
            protected void doLoad() {
                super.doLoad();
                this.setStringValue("");
            }
        };
        this.addField(stringEditor);

        final ColorFieldEditor colorEditor = new ColorFieldEditor(
                "coloreditor", "Color", getFieldEditorParent());
        this.addField(colorEditor);
        colorEditor.loadDefault();

        Composite buttonComp = new Composite(getFieldEditorParent(), SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.horizontalSpan = 3;
        buttonComp.setLayoutData(data);

        Button saveButton = new Button(buttonComp, SWT.PUSH);
        saveButton.setText("Save Current");
        saveButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (!stringEditor.getStringValue().isEmpty()) {
                    AlertWord word = new AlertWord(stringEditor
                            .getStringValue(), colorEditor.getColorSelector()
                            .getColorValue());
                    int index = viewer.getTable().getSelectionIndex();
                    if (index != -1) {
                        ((List<AlertWord>) viewer.getInput()).set(index, word);
                    } else {
                        ((List<AlertWord>) viewer.getInput()).add(word);
                    }
                    viewer.refresh();
                }
            }
        });
        data = new GridData(SWT.NONE, SWT.NONE, false, true);
        saveButton.setLayoutData(data);

        Button addButton = new Button(buttonComp, SWT.PUSH);
        addButton.setText("Add New");
        addButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (!stringEditor.getStringValue().isEmpty()) {
                    AlertWord word = new AlertWord(stringEditor
                            .getStringValue(), colorEditor.getColorSelector()
                            .getColorValue());
                    ((List<AlertWord>) viewer.getInput()).add(word);
                    viewer.refresh();
                }
            }
        });
        data = new GridData(SWT.NONE, SWT.NONE, false, true);
        addButton.setLayoutData(data);

        Button removeButton = new Button(buttonComp, SWT.PUSH);
        removeButton.setText("Remove Current");
        removeButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                IStructuredSelection selection = (IStructuredSelection) viewer
                        .getSelection();
                if (selection != null) {
                    AlertWord word = (AlertWord) selection.getFirstElement();
                    ((List<AlertWord>) viewer.getInput()).remove(word);
                    viewer.refresh();
                }
            }
        });
        data = new GridData(SWT.NONE, SWT.NONE, false, true);
        removeButton.setLayoutData(data);

        viewer.getTable().addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                AlertWord word = (AlertWord) e.item.getData();
                colorEditor.getColorSelector()
                        .setColorValue(
                                new RGB(word.getRed(), word.getGreen(), word
                                        .getBlue()));
                stringEditor.setStringValue(word.getText());
            }
        });
        viewer.setInput(CollaborationUtils.getAlertWords());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
    public void init(IWorkbench workbench) {
        // TODO Auto-generated method stub

    }

}
