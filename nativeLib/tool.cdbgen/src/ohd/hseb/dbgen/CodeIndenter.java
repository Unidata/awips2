/*
 * Created on Oct 6, 2004
 *
 * 
 */
package ohd.hseb.dbgen;

/**
 * @author Chip Gobs
 *
 * This class encapsulates the indentation of code but providing access
 * indentation levels and an indentation string based on those levels.
 */
public class CodeIndenter
{
        
	private int _indentLevel = 0;
    private String _indentString = null;
    private String _totalIndentString = null;

    //   ---------------------------------------------------------------

    public CodeIndenter(String indentString)
    {
        _indentString = indentString;
        _indentLevel = 0;
    }

    // ---------------------------------------------------------------
   
    public void incLevel()
    {
        incLevel(1);
    }

     
    // ---------------------------------------------------------------
    
     public void incLevel(int amount)
     {
         _indentLevel += amount;
         _totalIndentString = null;
     }

    // ----------------------------------------------------------------
    
     public void decLevel(int amount)
     {
         _indentLevel -= amount;
         
         if (_indentLevel < 0)
         {
             _indentLevel = 0;
         }
         
         _totalIndentString = null;
     }
     
     // ----------------------------------------------------------------

    public void decLevel()
    {
        decLevel(1);
    }

    //   ---------------------------------------------------------------

    public void setLevel(int level)
    {
        _indentLevel = level;
        _totalIndentString = null;
    }

    //   ---------------------------------------------------------------
   
    public String getTotalIndentString()
    {

        if (_totalIndentString == null)
        {
            StringBuffer indentBuffer = new StringBuffer();
            for (int i = 0; i < _indentLevel; i++)
            {
                indentBuffer.append(_indentString);
            }
            _totalIndentString = indentBuffer.toString();
        }
        return _totalIndentString;

    }
 	
    //   ---------------------------------------------------------------
    

}
