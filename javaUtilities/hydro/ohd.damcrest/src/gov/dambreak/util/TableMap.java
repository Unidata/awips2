package gov.dambreak.util;

/** 
 * In a chain of data manipulators some behaviour is common. TableMap
 * provides most of this behavour and can be subclassed by filters
 * that only need to override a handful of specific methods. TableMap 
 * implements TableModel by routing all requests to its model, and
 * TableModelListener by routing all events to its listeners. Inserting 
 * a TableMap which has not been subclassed into a chain of table filters 
 * should have no effect.
 *
 * @version 1.4 12/17/97
 * @author Philip Milne */

import javax.swing.table.*; 
import javax.swing.event.TableModelListener; 
import javax.swing.event.TableModelEvent; 

public class TableMap extends DefaultTableModel 		// *** was extends AbstractTableModel
                      implements TableModelListener {
    protected TableModel model; 

    public Class getColumnClass(int aColumn) {
        return model.getColumnClass(aColumn); 
    }
    public int getColumnCount() {
        return (model == null) ? 0 : model.getColumnCount(); 
    }
    public String getColumnName(int aColumn) {
        return model.getColumnName(aColumn); 
    }
    public TableModel getModel() {
        return model;
    }
    public int getRowCount() {
        return (model == null) ? 0 : model.getRowCount(); 
    }
    // By default, implement TableModel by forwarding all messages 
    // to the model. 

    public Object getValueAt(int aRow, int aColumn) {
        return model.getValueAt(aRow, aColumn); 
    }
    public boolean isCellEditable(int row, int column) { 
         return false; //model.isCellEditable(row, column); 
    }
    public void setModel(TableModel model) {
        this.model = model; 
        model.addTableModelListener(this); 
    }
    public void setValueAt(Object aValue, int aRow, int aColumn) {
        model.setValueAt(aValue, aRow, aColumn); 
    }
//
// Implementation of the TableModelListener interface, 
//
    // By default forward all events to all the listeners. 
    public void tableChanged(TableModelEvent e) {
	    
        fireTableChanged(e);
        
    }
}
