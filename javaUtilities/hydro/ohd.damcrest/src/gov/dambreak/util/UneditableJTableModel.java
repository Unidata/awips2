package gov.dambreak.util;

import java.util.*;

/**
 * This cutom table model is used in the output viewer to disallow editing of
 * the output table.
 */
public class UneditableJTableModel extends javax.swing.table.DefaultTableModel {
	
	private int uneditableAfterRow;
	private int fromToPairs;
	private Vector uneditableColumns;
	
	// constuctor
	public UneditableJTableModel(java.lang.Object[][] arg1, java.lang.Object[] arg2) {
		super(arg1, arg2);
	} 
	// constuctor
	public UneditableJTableModel(String[] arg1, int arg2) {
		super(arg1, arg2);
	}
// constructor
public UneditableJTableModel(int rows, int cols) {
	super(rows,cols);
	uneditableAfterRow = -1;
	fromToPairs = 0;
}
// constructor
public UneditableJTableModel(int rows, int cols, int _uneditableFromCol, int _uneditableToCol) {
	super(rows,cols);
	int uneditableFromCol, uneditableToCol;
	uneditableColumns = new Vector (1);
	uneditableFromCol = Math.min(_uneditableFromCol,_uneditableToCol);
	uneditableToCol = Math.max(_uneditableFromCol,_uneditableToCol);
	uneditableColumns.addElement(new UneditablePairs (_uneditableFromCol, _uneditableToCol));
	fromToPairs = uneditableColumns.size();
	uneditableAfterRow = -1;
	// uneditableAfterRow = rows-1;
}

public UneditableJTableModel(int rows, int cols, int [] _uneditableFromCol, int [] _uneditableToCol) {
	super(rows,cols);
	int uneditableSize = _uneditableFromCol.length;
	uneditableColumns = new Vector (uneditableSize);

	for (int i = 0; i < uneditableSize; i++)
	{
		uneditableColumns.addElement(new UneditablePairs (_uneditableFromCol[i], _uneditableToCol[i]));
	}
	fromToPairs = uneditableColumns.size();
	uneditableAfterRow = -1;
}

public boolean isCellEditable(int row, int col) {
		
		if (fromToPairs == 0)
			return false;
		else if (uneditableAfterRow != -1 && row > uneditableAfterRow)
		{
			return false;
		}
		else if (fromToPairs > 0)
		{
			for (int i = 0; i < fromToPairs; i++)
			{
				UneditablePairs pairs = (UneditablePairs) uneditableColumns.get(i);
				if(((col >= pairs.getFromCol()) && (col <= pairs.getToCol())))
				{
					return false;
				}
			}
			return true;
		}
		
		return false;
}

	private class UneditablePairs {
		private int fromCol;
		private int toCol;
		
		public UneditablePairs (int _fromCol, int _toCol){
			fromCol = Math.min(_fromCol,_toCol);
			toCol = Math.max(_fromCol,_toCol);
		}
		
		public int getFromCol() 
		{
			return fromCol;
		}
		
		public int getToCol() 
		{
			return toCol;
		}
	}
}
