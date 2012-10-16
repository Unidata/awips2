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

import java.io.File;
import java.util.List;

import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FontDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;
import com.raytheon.uf.viz.collaboration.ui.data.AlertWord;
import com.raytheon.uf.viz.collaboration.ui.data.AlertWordWrapper;

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

        Composite fontComp = new Composite(getFieldEditorParent(), SWT.NONE);
        GridLayout layout = new GridLayout(3, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        fontComp.setLayout(layout);
        data = new GridData(SWT.FILL, SWT.NONE, true, false);
        data.horizontalSpan = 3;

        fontComp.setLayoutData(data);

        Label fontName = new Label(fontComp, SWT.NONE);
        fontName.setText("Font");
        data = new GridData(SWT.FILL, SWT.NONE, true, true);
        fontName.setLayoutData(data);

        final Label fontLabel = new Label(fontComp, SWT.NONE);
        fontLabel.setText(StringConverter.asString(Display.getCurrent()
                .getSystemFont().getFontData()[0]));
        data = new GridData(SWT.FILL, SWT.NONE, true, true);
        fontLabel.setLayoutData(data);

        Button fontButton = new Button(fontComp, SWT.PUSH);
        fontButton.setText("Change...");
        data = new GridData(SWT.FILL, SWT.NONE, true, true);

        final FileFieldEditor fileEditor = new FileFieldEditor("fileeditor",
                "Sound File", getFieldEditorParent());

        PathManager manager = (PathManager) PathManagerFactory.getPathManager();
        LocalizationContext context = manager.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile file = manager.getLocalizationFile(context,
                "collaboration" + File.separator + "sounds" + File.separator);
        if (!file.exists()) {
            file.getFile().mkdirs();
        }
        fileEditor.setFilterPath(file.getFile());
        this.addField(fileEditor);

        fontButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                FontDialog dialog = new FontDialog(Display.getCurrent()
                        .getActiveShell());
                dialog.setFontList(StringConverter.asFontDataArray(fontLabel
                        .getText()));
                FontData data = dialog.open();
                if (data != null) {
                    fontLabel.setText(StringConverter.asString(data));
                }
            }
        });
        data = new GridData(SWT.NONE, SWT.NONE, false, true);
        fontButton.setLayoutData(data);

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
                    word.setFont(fontLabel.getText());
                    word.setSoundPath(fileEditor.getStringValue());
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
                    word.setFont(fontLabel.getText());
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
                fontLabel.setText(word.getFont());
                fileEditor.setStringValue(word.getSoundPath());
            }
        });
        viewer.setInput(CollaborationUtils.getAlertWords());
    }

    public boolean performOk() {
        List<AlertWord> words = (List<AlertWord>) viewer.getInput();
        CollaborationUtils.saveAlertWords(words);
        AlertWordWrapper wrapper = new AlertWordWrapper();
        wrapper.setAlertWords(words.toArray(new AlertWord[0]));
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection != null && connection.isConnected()) {
            // refresh any open chats or sessions
            connection.postEvent(wrapper);
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
