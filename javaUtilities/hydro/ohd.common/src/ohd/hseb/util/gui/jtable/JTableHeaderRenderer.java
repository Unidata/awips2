package ohd.hseb.util.gui.jtable;

import java.awt.Component;
import java.awt.Dimension;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.UIManager;

public class JTableHeaderRenderer extends JTableCellRenderer 
{
    private JTableManager _tableManager = null;
     private Map _sortedColumnsAndTheirOrdersMap = null;

    public JTableHeaderRenderer(JTableManager tableManager, Dimension columnHeaderPreferredSize)
    {
        _tableManager = tableManager;
        determineColumnSortDetails();
        setHorizontalAlignment(SwingConstants.CENTER);
        setVerticalAlignment(SwingConstants.TOP);
           
        if (columnHeaderPreferredSize != null)
        {
            setPreferredSize(columnHeaderPreferredSize);
        }
        
        setOpaque(true);

        // This call is needed because DefaultTableCellRenderer calls setBorder()
        // in its constructor, which is executed after updateUI()
        setBorder(UIManager.getBorder("TableHeader.cellBorder"));
    }
    
    public void setTableManager(JTableManager manager)
    {
        _tableManager = manager;
        determineColumnSortDetails();
    }

    public void determineColumnSortDetails()
    {
        _sortedColumnsAndTheirOrdersMap = new HashMap();
        String[] columnNamesForSortToBeDone = _tableManager.getColumnNamesForSortToBeDone();
        int[] sortOrderForSortedColumns = _tableManager.getSortOrderForSortedColumnNames();
        for(int i=0; i < columnNamesForSortToBeDone.length; i++)
        {
            _sortedColumnsAndTheirOrdersMap.put(columnNamesForSortToBeDone[i], sortOrderForSortedColumns[i]);
        }
    }

    public void updateUI()
    {
        super.updateUI();
    }

    public Component getTableCellRendererComponent(JTable table, Object value,
            boolean selected, boolean focused, int row, int column)
    {
        
       // String header = "JTableHeaderRenderer.getTableCellRendererComponent()";
       // System.out.println(header + "called.");
        
        final String upArrowString = "\u25BC";  //upArrow unicode for Times New Roman font
        final String downArrowString = "\u25B2";//downArrow unicode for Times New Roman font
        String arrowString = downArrowString;
      
        String str = value.toString();
        String columnHeaderString = value.toString();
        Integer columnSortOrder = (Integer) _sortedColumnsAndTheirOrdersMap.get(columnHeaderString);
        if(columnSortOrder != null)
        {
         //   System.out.println(columnHeaderString + "|"+ columnSortOrder);
            if ( _tableManager.isColumnAsc(columnHeaderString) )
            {
                arrowString = upArrowString;
            }
              
            str = "<html><FONT FACE=\"Times New Roman\">" + value + " " + arrowString + "<sup>" + 
                  (columnSortOrder.intValue() + 1) +"</sup></FONT></html>";
            
        }
        else
        {
            str = "<html><FONT FACE=\"Times New Roman\">" + value  +"</FONT></html>";
        }
        setValue(str);
        return this;
    }

}
