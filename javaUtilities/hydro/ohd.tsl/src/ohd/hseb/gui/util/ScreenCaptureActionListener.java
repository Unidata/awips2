package ohd.hseb.gui.util;

import java.awt.AWTException;
import java.awt.Component;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.imageio.ImageIO;
import javax.imageio.stream.FileImageOutputStream;
import javax.imageio.stream.ImageOutputStream;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.ExtensionFileFilter;

public class ScreenCaptureActionListener implements ActionListener
{
    
    private JFileChooser _imageFileChooser = null;
    private Window _baseWindow = null;
    private Component _componentToCapture = null;
    
    // -------------------------------------------------------------------------------------
    
    public ScreenCaptureActionListener(Window componentToCapture) 
    {
        _componentToCapture = componentToCapture;
        _baseWindow = componentToCapture;
        
        return;
    }
    
    // -------------------------------------------------------------------------------------
    
    public ScreenCaptureActionListener(Window baseWindow, Component componentToCapture) 
    {
        _baseWindow = baseWindow;
        _componentToCapture = componentToCapture;
        
        return;
    }
    
    
    // -------------------------------------------------------------------------------------
    
    
    public void actionPerformed(ActionEvent event)
    {
        this.displayCaptureScreenFileChooser();

    } //end actionPerformed
    
    // -------------------------------------------------------------------------------------
    
    
    private void displayCaptureScreenFileChooser()
    {
        BufferedImage image = captureScreenToImage();
        
        if (_imageFileChooser == null)
        {
            AppsDefaults ad = new AppsDefaults();
            
            String defaultDirectory = ad.getToken("whfs_image_dir");
            _imageFileChooser = new JFileChooser(defaultDirectory);
        }
        
        
        //add a FileFilter for JPG files
        List filterList = new ArrayList();
        
        filterList.add( "JPEG" );
        filterList.add("JPG");  
        FileFilter fileFilter = new ExtensionFileFilter( filterList, "JPEG Image Files (*.jpg, *.JPG) ");
          
            
        //set the file filter    
        _imageFileChooser.setFileFilter( fileFilter );
       // _imageFileChooser.set
            
            
        //open the dialog    
        int returnVal = _imageFileChooser.showSaveDialog(_baseWindow);
                    
        if (returnVal == JFileChooser.APPROVE_OPTION)
        {
            File file =  _imageFileChooser.getSelectedFile();
         
            //ensure ends in ".jpg"
            if   ( (! file.getName().endsWith(".jpg")) &&
                   (! file.getName().endsWith(".JPG"))
                 )
            {
                String newFilePath = file.getPath() + ".jpg";  
                file = new File(newFilePath); 
            }
         
            writeImageToFile(image, file);
        } 
            
        else
        {
              DialogHelper.displayMessageDialog(_baseWindow, "Image file not saved.");
        }
                 
        return;
    }
   
    //  -----------------------------------------------------------------
    
    private BufferedImage captureScreenToImage()
    {
        
        String header = "ScreenCaptureActionListener.captureScreenToImage(): ";
        
        BufferedImage bufferedImage = null;
        try
        {
          
            Robot robot = new Robot();    
            Rectangle boundsRectangle = null;
            
            if (_componentToCapture != _baseWindow)
            {
                
                Point origin = getOriginRelativeToScreen(_componentToCapture);
               // _componentToCapture.get
                Rectangle componentBounds = _componentToCapture.getBounds();
                boundsRectangle = new Rectangle(origin.x,
                                                origin.y,
                                                componentBounds.width,
                                                componentBounds.height);
                
            }
            else
            {
                boundsRectangle = _baseWindow.getBounds();
            }
        
            bufferedImage = robot.createScreenCapture(boundsRectangle);
        
        } //end try
         
        catch (AWTException e)
        {
            e.printStackTrace();
        }
  

        return bufferedImage;
    }
    
    //  -----------------------------------------------------------------
    private Point getOriginRelativeToScreen(Component component)
    {
    
        int x = 0;
        int y = 0;
        
        Component parent = component;
        
        while (parent != null)
        {
            x += parent.getX();
            y += parent.getY();
            
            parent = parent.getParent();
        }
        
        return new Point(x, y);
        
    }
    //  -----------------------------------------------------------------
        
    private void writeImageToFile(BufferedImage image, File file)
    { 
        try
        {
            ImageOutputStream imageOutputStream = new FileImageOutputStream(file);
            ImageIO.write(image, "JPG", imageOutputStream);
            imageOutputStream.close();
        }
      
        catch(java.io.IOException e )
        {
            e.printStackTrace();  
        }        
                 
        return;
    }
    //  -----------------------------------------------------------------

}
