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

package com.raytheon.viz.core.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.raytheon.uf.viz.core.Activator;

/**
 * Specifies video card preferences
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 *    
 *     Date       	Ticket#		Engineer	Description
 *     ------------	----------	-----------	--------------------------
 *     7/1/06                   chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
public class PerformancePreferences extends FieldEditorPreferencePage implements
        IWorkbenchPreferencePage {

    public PerformancePreferences() {
        super(GRID);
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
        setDescription("Specify video hardware settings (Requires restart of Viz)");
    }

    @Override
    public void createFieldEditors() {

        String[][] values = new String[][] { { "128MB", "128" },
                { "256MB", "256" }, { "384MB", "384" }, { "512MB", "512" },
                { "768MB", "768" }, { "1GB", "1024" } };

        ComboFieldEditor gpuEditor = new ComboFieldEditor(
                com.raytheon.uf.viz.core.preferences.PreferenceConstants.P_TEXTURES_CARD,
                "&Video Card Texture Cache Size:", values,
                getFieldEditorParent());

        addField(gpuEditor);
        addField(new IntegerFieldEditor(
                com.raytheon.uf.viz.core.preferences.PreferenceConstants.P_FPS,
                "&Frames Per Second:", getFieldEditorParent()));

        addField(new BooleanFieldEditor(
                com.raytheon.uf.viz.core.preferences.PreferenceConstants.P_DRAW_TILE_BOUNDARIES,
                "&Draw tile boundaries while loading", getFieldEditorParent()));

        addField(new BooleanFieldEditor(
                com.raytheon.uf.viz.core.preferences.PreferenceConstants.P_LOG_PERF,
                "&Log CAVE performance", getFieldEditorParent()));

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    public void init(IWorkbench workbench) {

    }

}
