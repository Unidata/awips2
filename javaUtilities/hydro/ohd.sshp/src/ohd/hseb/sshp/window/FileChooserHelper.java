package ohd.hseb.sshp.window;

import java.awt.Window;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.ExtensionFileFilter;

public class FileChooserHelper
{
    
    private String _lastUsedDirectory = null;
    private JFileChooser _fileChooser = null;
    private FileFilter _fileFilter = null;
    private String[] _acceptableExtensionArray = null;
    
    private File _selectedFile;
    
    // ------------------------------------------------------------------------------
    
    public FileChooserHelper(String defaultDirectory, 
                             String[] acceptableExtensionArray, 
                             String fileTypeDescriptionString)
    {
        _fileChooser =  new JFileChooser(defaultDirectory);
        
        _acceptableExtensionArray = acceptableExtensionArray;
        
        //add a FileFilter
        List filterList = new ArrayList();
        
        for (int i = 0 ; i < _acceptableExtensionArray.length; i++)
        {
            filterList.add( _acceptableExtensionArray[i]);
        }
        
        _fileFilter = new ExtensionFileFilter( filterList, fileTypeDescriptionString);
        _fileChooser.setFileFilter( _fileFilter );
        
        return;
    }    
     
    // ------------------------------------------------------------------------------
    private boolean endsWithProperExtension(String filePathString)
    {
        boolean hasProperExtension = false;
        
        for (int i = 0; i < _acceptableExtensionArray.length; i++)
        {
            String extension = "." + _acceptableExtensionArray[i];
            if (filePathString.endsWith(extension))
            {
                hasProperExtension = true;
                break;
            }
        }
        
        return hasProperExtension;
    }
    
    // ------------------------------------------------------------------------------
    public String displayForOpen(Window window)
    {
        return display(window, true);
    }
    
    // ------------------------------------------------------------------------------
    public String displayForSave(Window window)
    {
        return display(window, false);
    }
    
    // ------------------------------------------------------------------------------

    
    private String display(Window window, boolean forOpening)
    {
        String header = "FileChooserHelper.display(): ";
        
        String filePath = null;
        File file = null;
        int returnValue = 0;
        
        if (forOpening) 
        {
            returnValue = _fileChooser.showOpenDialog( window );
        }
        else //for Saving
        {
            returnValue = _fileChooser.showSaveDialog( window );
        }
        
        if (returnValue == JFileChooser.APPROVE_OPTION)
        {
            file =  _fileChooser.getSelectedFile();
            setSelectedFile(file);
            
            try
            {
                filePath = file.getCanonicalPath();
            
                //ensure ends with a  proper extension       
                if (! endsWithProperExtension(filePath) )
                {
                    String extension = "";
                    if (_acceptableExtensionArray.length > 0)
                    {
                        extension = "." + _acceptableExtensionArray[0];
                        filePath = filePath + extension;          
                    }
                    
                }
                
            }
            catch (IOException e)
            {
                DialogHelper.displayMessageDialog(window, "Error: File not selected.");
            }
        } 
        else
        {
            DialogHelper.displayMessageDialog(window, "File not selected.");
        }
        
        return filePath;
        
    }

    /**
     * @param selectedFile The selectedFile to set.
     */
    public void setSelectedFile(File selectedFile)
    {
        _selectedFile = selectedFile;
    }

    /**
     * @return Returns the selectedFile.
     */
    public File getSelectedFile()
    {
        return _selectedFile;
    }

    // ------------------------------------------------------------------------------
    
}
