/*
 * Created on Dec 15, 2003
 *
 */
package ohd.hseb.util.gui;

import java.awt.*;
import java.awt.image.*;
import javax.swing.*;

import java.util.*;


/**
 * @author GobsC
 *
 * This class makes some standard image icons available to the application.
 */
public class StandardImageIcons
{
 
    public static final String UP_ARROW = "UpArrow";
    public static final String DOWN_ARROW = "DownArrow";
    
    public static final String RIGHT_ARROW = "RightArrow";
    public static final String LEFT_ARROW = "LeftArrow";
    
    public static final String DOUBLE_RIGHT_ARROW = "DoubleRightArrow";
    public static final String DOUBLE_LEFT_ARROW = "DoubleLeftArrow";
    
    public static final String CIRCLE = "Circle";
 
    private static Map _imageMap = new HashMap();
  
    
    private static int _verticalSpacing = 3;
    private static int _horizontalSpacing = 3;
 

    //----------------------------------------------------------   
   
    //----------------------------------------------------------
 
    
    public static ImageIcon getImageIcon(String imageName, Color foreground, Color background,
                                         Dimension dimension)
    {

        String key = imageName + "|" +
                     foreground.toString() + "|" +
                     background.toString() + "|" +
                     dimension.getWidth() + "|" + dimension.getHeight();
 
        BufferedImage image = (BufferedImage)_imageMap.get(key);
        Image scaledImage = null;
        
        if (image == null)
        {
            image = initBufferedImage(imageName, foreground, background, dimension);
            
            scaledImage = image.getScaledInstance(dimension.width, dimension.height, Image.SCALE_FAST);
            
            _imageMap.put(imageName, image);
        }
      
        ImageIcon imageIcon = new ImageIcon(scaledImage);
          
        return imageIcon;
    }
    
  //----------------------------------------------------------
 

    private static BufferedImage initBufferedImage(String imageName, 
                                                   Color foregroundColor,
                                                   Color backgroundColor,
                                                   Dimension dimension)
    {
        BufferedImage image = null;
         String header = "BufferedCanvas.initBufferedImage()";
        
         //TYPE_INT_ARGB allows for transparent background 
        image = new BufferedImage(dimension.width,
                                  dimension.height, 
                                 BufferedImage.TYPE_INT_ARGB);
                                 
        Graphics2D graphics = (Graphics2D) image.createGraphics();
           
        //make the icon background transparent
        for (int col = 0; col < dimension.width; col++) {
            for (int row = 0; row < dimension.height; row++) {
               image.setRGB(col, row, 0x0);
            }
         }

        //make the icons smoother looking
        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                 RenderingHints.VALUE_ANTIALIAS_ON);
     
       //old way required a backgroundColor, the new transparent background
       // way does not.
       // fillBackground(graphics, backgroundColor, dimension);
     
        if (imageName.equals(UP_ARROW))
        {
            drawUpArrow(graphics, foregroundColor, dimension);   
                  
        } 
        else if (imageName.equals(DOWN_ARROW))
        {
            drawDownArrow(graphics, foregroundColor, dimension);         
        }

        else if (imageName.equals(LEFT_ARROW))
        {
            drawLeftArrow(graphics, foregroundColor, dimension);         
        }  

        else if (imageName.equals(RIGHT_ARROW))
        {
            drawRightArrow(graphics, foregroundColor, dimension);         
        }   
        
        else if (imageName.equals(DOUBLE_LEFT_ARROW))
        {
            drawDoubleLeftArrow(graphics, foregroundColor, dimension);         
        }   
               
        else if (imageName.equals(DOUBLE_RIGHT_ARROW))
        {
            drawDoubleRightArrow(graphics, foregroundColor, dimension);         
        }            
        
        else if (imageName.equals(CIRCLE))
        {
            drawCircle(graphics, foregroundColor, dimension);   
        }
                                 
                          
        return image;
    }

  //----------------------------------------------------------
 
    private static void fillBackground(Graphics g, Color backgroundColor,
                                       Dimension dimension)
    {
        g.setColor(backgroundColor);
        g.fillRect(0, 0, dimension.width, dimension.height);
    }
 
  //----------------------------------------------------------
    private static void drawCircle(Graphics g, Color foregroundColor, Dimension dimension)
    {
        
        int middleX =  dimension.width/2;
        int middleY =  dimension.height/2;
        int x = 0;
        int y = 0;
                
        g.setColor(foregroundColor);
        
        g.fillOval(x, y, dimension.width, dimension.height);
    }
  
 //----------------------------------------------------------
        
    private static void drawUpArrow(Graphics g, Color foregroundColor, Dimension dimension)
    {
        int middleX =  dimension.width/2;
        int middleY =  dimension.height/2;

        int right = dimension.width - _horizontalSpacing;
        int left = _horizontalSpacing;
       
        int top = _verticalSpacing;
        int bottom = dimension.height - _verticalSpacing;
       
        
        int[] xPoints = { middleX, left, right};
        int[] yPoints = { top, bottom, bottom};
  
        g.setColor(foregroundColor);
        g.fillPolygon(xPoints, yPoints, 3);
        
        return;
    }

  //----------------------------------------------------------
 
    
    private static void drawDownArrow(Graphics g, Color foregroundColor, Dimension dimension)
    {
       int middleX =  dimension.width/2;
       int middleY =  dimension.height/2;

       int right = dimension.width - _horizontalSpacing;
       int left = _horizontalSpacing;
       
       int top = _verticalSpacing;
       int bottom = dimension.height - _verticalSpacing;
       
     
       int[] xPoints = { left, middleX, right};
       int[] yPoints = {top, bottom, top};
    
       g.setColor(foregroundColor);
       g.fillPolygon(xPoints, yPoints, 3);
       
       return;
    }
 
//----------------------------------------------------------
 
    private static void drawLeftArrow(Graphics g, Color foregroundColor, Dimension dimension)
    {
        int middleX =  dimension.width/2;
        int middleY =  dimension.height/2;

        int right = dimension.width - _horizontalSpacing;
        int left = _horizontalSpacing;
       
        int top = _verticalSpacing;
        int bottom = dimension.height - _verticalSpacing;
       
       
        int[] xPoints = { left, right, right};
        int[] yPoints = { middleY, top, bottom};
  
        g.setColor(foregroundColor);
        g.fillPolygon(xPoints, yPoints, 3);
        
        return;
    }

 //----------------------------------------------------------
 

    private static void drawRightArrow(Graphics g,
                                       Color foregroundColor,
                                       Dimension dimension)
    {
        int middleX =  dimension.width/2;
        int middleY =  dimension.height/2;

        int right = dimension.width - _horizontalSpacing;
        int left = _horizontalSpacing;
       
        int top = _verticalSpacing;
        int bottom = dimension.height - _verticalSpacing;
       
        int[] xPoints = { right, left, left};
        int[] yPoints = { middleY, top, bottom};
          
        g.setColor(foregroundColor);
        g.fillPolygon(xPoints, yPoints, 3);
        
        return;
    }

  //----------------------------------------------------------
  private static void drawDoubleLeftArrow(Graphics g, Color foregroundColor, Dimension dimension)
  {
      int middleX =  dimension.width/2;
      int middleY =  dimension.height/2;

      int right = dimension.width/2 - _horizontalSpacing;
      int left = _horizontalSpacing;
       
      int top = _verticalSpacing;
      int bottom = dimension.height - _verticalSpacing;
       
      int[] xPoints = { left, right, right};
      int[] yPoints = { middleY, top, bottom};
      
      int shiftAmount = right;
      int[] shiftedXPoints = shiftPointsHorizontally(xPoints, shiftAmount);
    
      g.setColor(foregroundColor);
      g.fillPolygon(xPoints, yPoints, 3);
      
      g.fillPolygon(shiftedXPoints, yPoints, 3);
        
          return;
      }
       
    //----------------------------------------------------------  
    
    private static void drawDoubleRightArrow(Graphics g,
                                         Color foregroundColor,
                                         Dimension dimension)
    {
        int middleX =  dimension.width/2;
        int middleY =  dimension.height/2;
    
        int right = dimension.width/2 - _horizontalSpacing;
        int left = _horizontalSpacing;
       
        int top = _verticalSpacing;
        int bottom = dimension.height - _verticalSpacing;
       
        int[] xPoints = { right, left, left};
        int[] yPoints = { middleY, top, bottom};
          
        int shiftAmount = right;
        int[] shiftedXPoints = shiftPointsHorizontally(xPoints, shiftAmount);
        
          
        g.setColor(foregroundColor);
        g.fillPolygon(xPoints, yPoints, 3);
        g.fillPolygon(shiftedXPoints, yPoints, 3);
  
    
        return;
    }
    //----------------------------------------------------------  
    private static int[] shiftPointsHorizontally(int[] origPointsArray, int shiftAmount)
    {
        int[] shiftedXPoints = new int[origPointsArray.length]; 
        for (int i = 0; i < origPointsArray.length; i++)
        {
            shiftedXPoints[i] = origPointsArray[i] + shiftAmount;   
        }
        
        return shiftedXPoints;
        
    }

    //---------------------------------------------------------- 

}
