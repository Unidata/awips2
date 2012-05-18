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
package com.raytheon.uf.viz.collaboration.ui.prefs;

import java.util.List;

import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.FontFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
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

import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.data.AlertWord;
import com.raytheon.uf.viz.collaboration.data.AlertWordWrapper;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 17, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationAlertWordsPreferencePage extends
        FieldEditorPreferencePage implements IWorkbenchPreferencePage {

    private TableViewer viewer = null;

    /**
     * 
     */
    public CollaborationAlertWordsPreferencePage() {
        super(GRID);
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
        viewer.setInput(CollaborationUtils.getAlertWords());
        viewer.getTable().setLayoutData(data);

        final StringFieldEditor stringEditor = new StringFieldEditor(
                "significantword", "Word", getFieldEditorParent()) {
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

        final FontFieldChangeEditor fontEditor = new FontFieldChangeEditor(
                "fonts", "Font", getFieldEditorParent());
        this.addField(fontEditor);

        final FileFieldEditor fileEditor = new FileFieldEditor("fileeditor",
                "Sound File", getFieldEditorParent());
        this.addField(fileEditor);

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
                    word.setSoundPath(fileEditor.getStringValue());
                    word.setFont(fontEditor
                            .getFontValue(getFieldEditorParent()));
                    ((List<AlertWord>) viewer.getInput()).set(index, word);
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
                    word.setSoundPath(fileEditor.getStringValue());
                    word.setFont(fontEditor
                            .getFontValue(getFieldEditorParent()));
                    word.setSoundPath(fileEditor.getStringValue());
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
                fontEditor.setFontValue(word.getFont(), getFieldEditorParent());
                fileEditor.setStringValue(word.getSoundPath());
            }
        });
    }

    private class FontFieldChangeEditor extends FontFieldEditor {
        public FontFieldChangeEditor(String name, String text, Composite parent) {
            super(name, text, parent);
        }

        public String getFontValue(Composite parent) {
            return getValueControl(parent).getText();
        }

        public void setFontValue(String string, Composite parent) {
            getValueControl(parent).setText(string);
            doLoad();
        }
    }

    public boolean performOk() {
        List<AlertWord> words = (List<AlertWord>) viewer.getInput();
        CollaborationUtils.saveAlertWords(words);
        AlertWordWrapper wrapper = new AlertWordWrapper();
        wrapper.setAlertWords(words.toArray(new AlertWord[0]));
        CollaborationConnection connection = CollaborationDataManager
                .getInstance().getCollaborationConnection(false);
        if (connection != null && connection.isConnected()) {
            connection.getEventPublisher().post(wrapper);
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
    public void init(IWorkbench workbench) {
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
        setDescription("Significant Words");
    }
}
