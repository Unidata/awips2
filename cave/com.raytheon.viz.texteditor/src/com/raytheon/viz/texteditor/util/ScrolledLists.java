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

package com.raytheon.viz.texteditor.util;

import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.ScrollBar;

/**
 * Links a set of lists together.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------  --------------------------
 * 8/11/2009    2191        rjpeter     Initial creation.
 * </pre>
 * 
 * @author rjpeter
 */
public class ScrolledLists {
    private List[] lists;

    private SelectionAdapter[][] adapters;

    public ScrolledLists(List... lists) {
        this.lists = lists;
    }

    public void joinLists() {
        breakLists();
        if (lists.length >= 2) {
            adapters = new SelectionAdapter[lists.length][lists.length];
            for (int i = 0; i < lists.length; i++) {
                List sourceList = lists[i];
                for (int j = 0; j < lists.length; j++) {
                    if (i != j) {
                        List destList = lists[j];
                        final ScrollBar vBar1 = sourceList.getVerticalBar();
                        final ScrollBar vBar2 = destList.getVerticalBar();

                        adapters[i][j] = new SelectionAdapter() {
                            @Override
                            public void widgetSelected(SelectionEvent e) {
                                int y = vBar1.getSelection()
                                        * (vBar2.getMaximum() - vBar2
                                                .getThumb())
                                        / Math.max(1, vBar1.getMaximum()
                                                - vBar1.getThumb());
                                vBar2.setSelection(y);
                            }
                        };
                        vBar1.addSelectionListener(adapters[i][j]);
                    }
                }
            }

        }
    }

    public void breakLists() {
        if (adapters != null) {
            for (int i = 0; i < lists.length; i++) {
                ScrollBar source = lists[i].getVerticalBar();
                for (int j = 0; j < adapters[i].length; j++) {
                    if (adapters[i][j] != null) {
                        source.removeSelectionListener(adapters[i][j]);
                    }
                }
            }
        }

        adapters = null;
    }
}
