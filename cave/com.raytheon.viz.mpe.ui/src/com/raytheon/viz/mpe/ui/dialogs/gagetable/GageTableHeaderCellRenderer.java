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
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.UIManager;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

/**
 * Custom cell renderer for header cells of the Gage Table Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 02, 2009  2476      mpduff      Initial creation
 * Mar 01, 2017 6158       mpduff      Changed how sorting works.
 * Jul 14, 2017 6358       mpduff      Changed method name.
 * 
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class GageTableHeaderCellRenderer extends JTextArea
        implements TableCellRenderer {
    private static final long serialVersionUID = 8036794115176421761L;

    /* upArrow unicode for Times New Roman font */
    private final String upArrowString = "\u25BC";

    /* downArrow unicode for Times New Roman font */
    private final String downArrowString = "\u25B2";

    public GageTableHeaderCellRenderer() {
        setLineWrap(true);
        setWrapStyleWord(true);
        setOpaque(false);
    }

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value,
            boolean isSelected, boolean hasFocus, int row, int column) {
        GageTableDataManager dataManager = GageTableDataManager.getInstance();
        GageTableSortSettings settings = dataManager.getSortSettings();

        List<String> sortCols = settings.getSortColumns();
        List<String> cols = new ArrayList<>(sortCols.size());

        // Remove blank column names
        for (String columnName : sortCols) {
            if (!columnName.isEmpty()) {
                cols.add(columnName);
            }
        }

        String arrowString = upArrowString;
        setBorder(UIManager.getBorder("TableHeader.cellBorder"));
        setFont(new Font("Dialog", Font.BOLD, 12));

        Enumeration<TableColumn> columns = table.getColumnModel().getColumns();
        int width = 80;
        while (columns.hasMoreElements()) {
            TableColumn tableColumn = columns.nextElement();
            String headerValue = (String) tableColumn.getHeaderValue();
            if (headerValue.equals(value)) {
                int idx = cols.indexOf(value);
                if (idx >= 0) {
                    arrowString = settings.getSortDirections().get(value)
                            ? downArrowString : upArrowString;
                    setText(value.toString() + "\n" + arrowString + " "
                            + (cols.indexOf(value) + 1));
                } else {
                    setText(value.toString() + "\n\n");
                }

                width = tableColumn.getWidth();
                break;
            }
        }

        setSize(new Dimension(width, 100));

        return this;
    }
}
