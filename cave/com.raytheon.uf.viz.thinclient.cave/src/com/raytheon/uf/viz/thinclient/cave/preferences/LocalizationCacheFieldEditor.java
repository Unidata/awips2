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
package com.raytheon.uf.viz.thinclient.cave.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.swt.widgets.Composite;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class LocalizationCacheFieldEditor extends BooleanFieldEditor {

    private SyncLocalizationEditor syncEditor;

    public LocalizationCacheFieldEditor(String name, String label,
            Composite parent) {
        super(name, label, parent);
    }

    @Override
    protected void doLoad() {
        super.doLoad();
        if (syncEditor != null) {
            syncEditor.setEnabled(getBooleanValue());
        }
    }

    @Override
    protected void doLoadDefault() {
        super.doLoadDefault();
        if (syncEditor != null) {
            syncEditor.setEnabled(getBooleanValue());
        }
    }

    @Override
    protected void valueChanged(boolean oldValue, boolean newValue) {
        super.valueChanged(oldValue, newValue);
        if (syncEditor != null) {
            syncEditor.setEnabled(getBooleanValue());
        }
    }

    public void setSyncEditor(SyncLocalizationEditor syncEditor) {
        this.syncEditor = syncEditor;

    }

}
