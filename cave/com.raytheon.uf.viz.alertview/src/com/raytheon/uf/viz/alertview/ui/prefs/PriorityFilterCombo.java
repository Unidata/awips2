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
package com.raytheon.uf.viz.alertview.ui.prefs;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.viz.alertview.Alert.Priority;
import com.raytheon.uf.viz.alertview.filter.FilterManager;

/**
 * Class for using an {@link Combo} to select a priority level.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 25, 2015  4474     bsteffen  Initial creation
 * Aug 06, 2015  4693     bsteffen  Add setToolTipText.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PriorityFilterCombo {

    /**
     * Mapping for pretty user text to filter names for populating priority
     * combos.
     */
    private static final Map<String, String> MAPPING = getPriorityFilterMapping();

    private final Combo combo;

    public PriorityFilterCombo(Composite parent) {
        combo = new Combo(parent, SWT.READ_ONLY);
        for (Entry<String, String> entry : MAPPING.entrySet()) {
            combo.add(entry.getKey());
        }
    }

    public void setToolTipText(String string) {
        combo.setToolTipText(string);
    }

    public void setSelection(String filter) {
        for (Entry<String, String> entry : MAPPING.entrySet()) {
            if (filter.equals(entry.getValue())) {
                int index = combo.indexOf(entry.getKey());
                if (index >= 0) {
                    combo.select(index);
                } else {
                    combo.deselectAll();
                }
            }
        }
    }

    public String getSelection() {
        String filter = combo.getText();
        if (filter != null && !filter.isEmpty()) {
            return MAPPING.get(filter);
        }
        return null;
    }

    private static Map<String, String> getPriorityFilterMapping() {
        Map<String, String> mapping = new LinkedHashMap<>();
        mapping.put("None", FilterManager.NONE);
        mapping.put("Error", Priority.ERROR.name().toLowerCase());
        mapping.put("Error+Warning", FilterManager.WARN_PLUS);
        mapping.put("Error+Warning+Info", FilterManager.INFO_PLUS);
        mapping.put("All", FilterManager.ALL);
        return mapping;
    }
}
