package ohd.hseb.util.gui.jtable;

import java.awt.Color;
import java.awt.Component;

import javax.swing.BorderFactory;
import javax.swing.JTable;
import javax.swing.border.Border;
import javax.swing.table.DefaultTableCellRenderer;

public class JTableCellRenderer extends DefaultTableCellRenderer
{

    private JTableManager _manager = null;
    private Color _origForeGroundColor = null;
    private Color _highLightForeGroundColor = null;
    private Color _highLightBackGroundColor = null;

    public JTableCellRenderer() 
    {
        _origForeGroundColor = getForeground();
        _highLightBackGroundColor = Color.BLUE;
        _highLightForeGroundColor = Color.YELLOW;
        setOpaque(true); //MUST do this for background to show up.
    }

    public void setTableManager(JTableManager manager)
    {
        _manager = manager;
    }

    public Component getTableCellRendererComponent(JTable table, 
            Object color, boolean isSelected, 
            boolean hasFocus, int row, int column) 
    {

        Color newBackgroundColor = _manager.getCellBackgroundColor(row, column);
        Color newForegroundColor = _manager.getCellForegroundColor(row, column);
        String align = _manager.getAlignment(table.getColumnName(column));
        this.setValue(table.getValueAt(row, column));
        
        if(align.equalsIgnoreCase("right"))
        {
            this.setHorizontalAlignment(RIGHT);
        }
        else  if(align.equalsIgnoreCase("left"))
        {
            this.setHorizontalAlignment(LEFT);
        }
        else
        {
            this.setHorizontalAlignment(CENTER);
        }

        //    this.setText("" +table.getValueAt(row,column));

        this.setBackground(newBackgroundColor);
        this.setForeground(newForegroundColor);

        Border border = BorderFactory.createLineBorder(Color.BLACK);
        if(isSelected)
        {
            this.setBackground(_highLightBackGroundColor);
            this.setForeground(_highLightForeGroundColor);
            this.setBorder(border);
        }
        else
        {
            if(newForegroundColor != null)
                this.setForeground(newForegroundColor);
            else
                this.setForeground(_origForeGroundColor);
            this.setBorder(null);
        }
        return this;
    }

}
