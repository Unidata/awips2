/*
 * Created on Jun 16, 2004
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : Jun 16, 2004
 *  
 */
package ohd.hseb.util.gui;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.filechooser.FileFilter;

/**
 * @author SoodG
*/
public class ExtensionFileFilter extends FileFilter 
{
    private List _extensionList = new ArrayList();
    private String _extensionDescription = null;
    
    public ExtensionFileFilter( List extList, String extensionDescription )
    {
    	_extensionList = extList;
    	_extensionDescription = extensionDescription;
    }
    
	public boolean accept( File f ) 
	{
		boolean returnValue = false;
		
		if ( f.isDirectory() ) 
		{
			returnValue = true;
		}
		else
		{
			String extension = getExtension( f );
			
			if ( extension != null ) 
			{
				if ( extensionValid( extension ) )
				{
					returnValue = true;
				} 
				else 
				{
					returnValue = false;
				}
			}
		}
		return returnValue;
	}
	
	private boolean extensionValid( String extension )
	{
		boolean isEquals = false;
		
		for ( int i = 0; i < _extensionList.size(); i++ )
		{
			String ext = (String) _extensionList.get( i );
			if( ext.equalsIgnoreCase( extension ) )
			{
				isEquals = true;
			}
		}
		return isEquals;
	}
	
	public static String getExtension( File f ) 
	{
		String ext = null;
		if ( f != null )
		{
			String s = f.getName();
			int i = s.lastIndexOf('.');	

			if (i > 0 &&  i < s.length() - 1) 
			{
				ext = s.substring(i+1).toLowerCase();
			}
		}
		return ext;
	}

    
	public String getDescription()
    {
		return _extensionDescription;
	}
}
