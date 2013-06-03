package ohd.hseb.util.gui.jtable;

import java.awt.Color;

public interface TableCellInterface
{
    
    Object getValue();
    String getDisplayString();

    
    String getColumnName();
    
    int compare(TableCellInterface cell);
    
    Color getCellBackgroundColor();
    void setCellBackgroundColor(Color color);
    
    
    Color getCellForegroundColor();
    void setCellForegroundColor(Color color);
    
 
}
