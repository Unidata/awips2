package ohd.hseb.geomap.layer;

import java.awt.Color;
import java.util.Map;

import ohd.hseb.util.io.RGBTextFileReader;

public class BaseColorDeterminer implements ColorDeterminer
{
    // ------------------------------------------------------------------------

    private static Color[] _colorArray = null;
    private static String[] _colorNameArray = null;
    private static double[] _levelArray = null;
    // ------------------------------------------------------------------------

    public BaseColorDeterminer(String[] colorNameArray, double[] levelArray)
    {
        _colorArray = initColorArray(colorNameArray);
        setLevelArray(levelArray);
    }

      // ------------------------------------------------------------------------
    public Color getColorByValue(double value)
    {
        
         String header = "BaseColorDeterminer.getColorByValue(): ";
        
         int index = getColorIndexFromValue(value);
       
         
         Color color = _colorArray[index];
          
         return color;
    }
    
    //  -------------------------------------------------------------------------------------------------------
    private int getColorIndexFromValue(double value)
    {
         int index = 0;
         
         int maxValue = 10;
         int intervalCount = 20;
         
         double increment = (double) maxValue / (double) intervalCount;
         
         
         for (index = 0; index < _levelArray.length; index++)
         {
             if (value <= _levelArray[index])
             {
                 break;
             }
         }
         
         
         return index;
    }
    //  -------------------------------------------------------------------------------------------------------
    public double[] getLevelArray()
    {
        return _levelArray;
    }
    // ------------------------------------------------------------------------
    private void setLevelArray(double[] levelArray)
    {
        _levelArray = levelArray;
    }
    // ------------------------------------------------------------------------
    

    private Color[] initColorArray(String[] colorNameArray)
    {
        
        RGBTextFileReader reader = new RGBTextFileReader();
        String filePath =  "/usr/lib/X11/rgb.txt";
        
        Map<String, Color> map = reader.read(filePath);
       
        int size = colorNameArray.length;
        
        Color[] colorArray = new Color[size];
        
        for (int i = 0; i < colorNameArray.length; i++)
        {
            String colorName = colorNameArray[i].toUpperCase();
            
            Color color = map.get(colorName);
            colorArray[i] = color;
         //   System.out.println("color = " +  colorName + " " + color);
        }
        
        return colorArray;
    }
    
}
