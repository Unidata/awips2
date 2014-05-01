package ohd.hseb.color_chooser;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class NamedColorMatcher
{
    List _namedColorList = null;
    
    public NamedColorMatcher(Map nameToNamedColorMap)
    {
        _namedColorList = new ArrayList( nameToNamedColorMap.values());
    }
    
    public NamedColor getClosestNamedColor(Color color)
    {
        NamedColor bestNamedColor = null;
        Set specialWhiteNameSet = new HashSet();
        Set specialBlackNameSet = new HashSet();
        specialWhiteNameSet.add("GRAY100");
        specialBlackNameSet.add("GRAY0");

        List<NamedColor> colorList = _namedColorList;
        
        for (NamedColor namedColor : colorList)
       // for (int i = 0; i < _namedColorList.size(); i++)
        {
          //  NamedColor namedColor = (NamedColor) _namedColorList.get(i);
          
            if (isCloserColor(color, namedColor, bestNamedColor))
            {
                bestNamedColor = namedColor;
            }
         }
         
//        System.out.println("Desired Color was " + color);
//        System.out.println("Found Named Color was " + bestNamedColor);
        if ( bestNamedColor != null )
        {
            String bestNamedColorString = bestNamedColor.getName().toUpperCase();
            if ( specialWhiteNameSet.contains( bestNamedColorString ) )
            {
                bestNamedColor = new NamedColor( "WHITE", bestNamedColor );
            }
            else if ( specialBlackNameSet.contains( bestNamedColorString ) )
            {
                bestNamedColor = new NamedColor( "BLACK", bestNamedColor );
            }
        }
        return bestNamedColor;       
    }
    
    // --------------------------------------------------------------------------------------------------------
    private boolean isCloserColor(Color color, NamedColor testNamedColor, NamedColor bestNamedColor)
    {
        boolean isCloser = false;

        if ( (testNamedColor != null) && (bestNamedColor != null) )
        {         
            double distanceFromTestNamedColor = getDistance(color, testNamedColor);
            double distanceFromBestNamedColor = getDistance(color, bestNamedColor);      

            if (distanceFromTestNamedColor < distanceFromBestNamedColor)
            {
                isCloser = true;
            }
        }
        else if  ((bestNamedColor == null) && (testNamedColor != null) )
        {
            isCloser = true;  
        }     
        return isCloser;
    }
    // --------------------------------------------------------------------------------------------------------
    private double getDistance(Color color, NamedColor namedColor)
    {
        double distance = 0.0;
        
        int redDiff = color.getRed() - namedColor.getRed();
        int greenDiff = color.getGreen() - namedColor.getGreen();
        int blueDiff = color.getBlue() - namedColor.getBlue();
        
        //intentionally not taking the square root - Chip
        distance = (redDiff * redDiff)  + (greenDiff * greenDiff) + (blueDiff * blueDiff);
           
        return distance;
    }
    

}
