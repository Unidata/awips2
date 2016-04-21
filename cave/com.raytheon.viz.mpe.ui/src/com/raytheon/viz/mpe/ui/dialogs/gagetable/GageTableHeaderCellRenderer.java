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
package com.raytheon.viz.mpe.ui.dialogs.gagetable;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.UIManager;
import javax.swing.table.TableCellRenderer;

/**
 * Custom cell renderer for header cells of the Gage Table Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 2, 2009  2476       mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GageTableHeaderCellRenderer extends JTextArea implements
        TableCellRenderer {
    private static final long serialVersionUID = 8036794115176421761L;

    private final String upArrowString = "\u25BC"; // upArrow unicode for Times
                                                   // New Roman font

    private final String downArrowString = "\u25B2";// downArrow unicode for
                                                    // Times New Roman font

    public GageTableHeaderCellRenderer() {
        setLineWrap(true);
        setWrapStyleWord(true);
        setOpaque(false);
    }

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value,
            boolean isSelected, boolean hasFocus, int row, int column) {
        GageTableDataManager dataManager = GageTableDataManager.getInstance();
        GageTableSortSettings columnSettings = dataManager.getColumnSettings();
        String arrowString = upArrowString;
        setBorder(UIManager.getBorder("TableHeader.cellBorder"));
        setFont(new Font("Dialog", Font.BOLD, 12));
        
        if (columnSettings == null) {
            columnSettings = new GageTableSortSettings();
        }
        
        if (columnSettings.getSortCol1Index() == column) {
            if (columnSettings.getAscending1() == 1) {
                arrowString = downArrowString;
            } else {
                arrowString = upArrowString;
            }
            setText(value.toString() + "\n" + arrowString + " 1");
        } else if (columnSettings.getSortCol2Index() == column) {
            setText(value.toString() + " \n2");
        } else if (columnSettings.getSortCol3Index() == column) {
            setText(value.toString() + "\n3");
        } else if (columnSettings.getSortCol4Index() == column) {
            setText(value.toString() + "\n4");
        } else {
            setText(value.toString() + "\n\n");
        }
        
        setSize(new Dimension(columnSettings.getColumnWidth(), 100));

        return this;
    }
}
