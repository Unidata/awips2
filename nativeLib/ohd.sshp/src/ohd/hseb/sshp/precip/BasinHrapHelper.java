/*
 * Created on May 21, 2004
 *
 *
 */
package ohd.hseb.sshp.precip;

/**
 * @author GobsC
 *
 * Encapsulates the LineSegment definition for a basin
 * 
 */


public class BasinHrapHelper
{
    
//  ---------------------------------------------------------------------------------------

    private String _basinId = null;
    //private String name = null;
    
    private double _areaSize = 0;

    private int _rowCount = 0;
    
    private int _basinHrapRowArray[];
    private int _basinHrapBegColumnArray[];
    private int _basinHrapEndColumnArray[];

    private int _binCount = 0;

     
    private boolean _isValid = false;
    
   
// ---------------------------------------------------------------------------------------

    public BasinHrapHelper(String basinId, int rowCount)
    {
        System.out.println("BasinHrapSet() for basinId = " + basinId);
        _basinId = basinId;   

        _rowCount = rowCount;


        initializeArraysByRowCount(_rowCount);

    }

// ---------------------------------------------------------------------------------------
    
    private void initializeArraysByRowCount(int rowCount)
    {
        String header = "BasinHrapSet.initializeArraysByRowCount()";
        System.out.println(header + " rowCount = " + rowCount);
 
        _basinHrapRowArray = new int[_rowCount];
        _basinHrapBegColumnArray = new int[_rowCount];
        _basinHrapEndColumnArray = new int[_rowCount]; 
        
        return;   
    }
    
//  ---------------------------------------------------------------------------------------
    public boolean isInBounds(int hrapRow, int hrapCol)
    {
        boolean inBounds = false;
        boolean done = false;
        
        for (int r = 0; r < _basinHrapRowArray.length && (!done) ; r++)
        {
            int currentRow = _basinHrapRowArray[r];

            if (currentRow == hrapRow) //note, there can be multiple partial rows for a single hrapRow
            {                   
                if ((hrapCol >= _basinHrapBegColumnArray[r]) &&
                    (hrapCol <= _basinHrapEndColumnArray[r]))  
                {
                    //found the column we are looking for
                    inBounds = true; 
                    done = true;
                }          
          
            }
            else if (currentRow > hrapRow)  // gone beyond hrapRow, so we are done
            {
                done = true;
            }
        }    
       
        return inBounds;
    }    
//  ---------------------------------------------------------------------------------------
/*
    private boolean hasColumn(int hrapRow, int hrapColumn)
        {
            boolean foundRow = false;
        
        
            for (int i = 0; i < _basinHrapRowArray.length && (! foundRow) ; i++)
            {
                if ( _basinHrapRowArray[i] == hrapRow)
                {
                    foundRow = true;
              
                }  
            
            }    
            return foundRow;
        
        }    
*/    
//  ---------------------------------------------------------------------------------------

	public void setBasinId(String basinId)
	{
		_basinId = basinId;
	}
//  ---------------------------------------------------------------------------------------

	public String getBasinId()
	{
		return _basinId;
	}
//  ---------------------------------------------------------------------------------------
    public int getBasinHrapRow(int index)
    {
        return _basinHrapRowArray[index];   
        
    }  
//  ---------------------------------------------------------------------------------------
    public void setBasinHrapRow(int index, int rowValue)
    {
         _basinHrapRowArray[index] = rowValue;   
        
    }  
//   ---------------------------------------------------------------------------------------
    public int getBasinHrapBegColumn(int index)
    {
         return _basinHrapBegColumnArray[index];   
        
    } 
//  ---------------------------------------------------------------------------------------
   
    public void setBasinHrapBegColumn(int index, int colValue)
    {
         _basinHrapBegColumnArray[index] = colValue;   
        
    }   
 
///  ---------------------------------------------------------------------------------------
   
     
    public int getBasinHrapEndColumn(int index)
    {
         return _basinHrapEndColumnArray[index];   
    
    }
//  ---------------------------------------------------------------------------------------
   
    public void setBasinHrapEndColumn(int index, int colValue)
    {
         _basinHrapEndColumnArray[index] = colValue;   
        
    }  
//  ---------------------------------------------------------------------------------------
   
    public int getBasinNorthernRow()
    {
        int lastIndex = _basinHrapRowArray.length - 1;
        return _basinHrapRowArray[lastIndex];
    }
//  ---------------------------------------------------------------------------------------

    public int getBasinSouthernRow()
    {
        
        return _basinHrapRowArray[0];
    	
    }  
//   ---------------------------------------------------------------------------------------
    public double getAreaSize()
    {
        return _areaSize;    
        
    }
    
//  ---------------------------------------------------------------------------------------
    public void setAreaSize(double areaSize)
    {
        _areaSize = areaSize;    
        
        return;
    }

//  ---------------------------------------------------------------------------------------

    public void setIsValid(boolean isValid)
    {
    	_isValid = isValid;
    }

//  ---------------------------------------------------------------------------------------
    
    public boolean isValid()
    {
    	return _isValid;
    }
//  ---------------------------------------------------------------------------------------

    public void setRowCount(int rowCount)
    {
    	_rowCount = rowCount;
    }
 //  ---------------------------------------------------------------------------------------
   
    public int getRowCount()
    {
    	return _rowCount;
    }
//  ---------------------------------------------------------------------------------------
 
    public void setBinCount(int binCount)
    {
    	_binCount = binCount;
    }
 //  ---------------------------------------------------------------------------------------
    
    public int getBinCount()
    {
    	return _binCount;
    }
//  ---------------------------------------------------------------------------------------
    public String toString()
    {
        
        StringBuffer buffer = new StringBuffer();
        
        buffer.append("BasinId = " + _basinId + "\n");
        
        for (int i = 0; i < _rowCount; i++)
        {
            
            buffer.append(" row = " + _basinHrapRowArray[i] + " cols =  " +
                          _basinHrapBegColumnArray[i] +
                           " to " +_basinHrapEndColumnArray[i] + "\n");
             

            
        }    
        
        return buffer.toString();
    }
   
    
//  ---------------------------------------------------------------------------------------

}
