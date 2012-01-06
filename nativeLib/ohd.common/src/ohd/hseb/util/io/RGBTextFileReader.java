package ohd.hseb.util.io;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import ohd.hseb.util.StringParser;

public class RGBTextFileReader
{
    
    
    
    public RGBTextFileReader()
    {
        
    }
    
//  ---------------------------------------------------------------------------------------------------
    
    public Map<String, Color > read(String rgbColorFilePath )
    {
        String header = "RGBTextFileReader(). read() ";
        
        File rgbColorFile = null;
        BufferedReader reader = null;
        String line = null;
        String[] tokenizedString = null;
        String colorName = null;
        rgbColorFile = new File( rgbColorFilePath );
        Map<String, Color>  colorMap = new HashMap<String, Color>();

        try
        {
            reader = new BufferedReader( new FileReader( rgbColorFile ) );
            line = reader.readLine(); //discard the first line
            line = reader.readLine();
            while ( line != null )
            {
                tokenizedString = StringParser.tokenize( line );
                colorName = getRGBColorName( tokenizedString );

//              if the read in RGB color name exists in the ColorName table, then add to the _rgbColorMap
              
                Color color = new Color(Integer.parseInt( tokenizedString[ 0 ] ), 
                                        Integer.parseInt( tokenizedString[ 1 ] ), 
                                         Integer.parseInt( tokenizedString[ 2 ] ) );
                
                
                
                colorMap.put( colorName.toUpperCase(), color );
              
 //               System.out.println(header + "colorName =  " + colorName + " color = " + color);
                
                line = reader.readLine();
            }
        }
        catch( IOException e )
        {
            e.printStackTrace();
        }
        
        return colorMap;
    }
    // ---------------------------------------------------------------------------------------------------
    
    private String getRGBColorName( String[] tokenizedName )
    {
        StringBuffer rgbColorName = new StringBuffer();
        
        for ( int i = 3; i < tokenizedName.length;i++ )
        {
            rgbColorName.append( tokenizedName[ i ] );

            if ( i != ( tokenizedName.length - 1 ) )
            {
                rgbColorName.append( " " );
            }
        }
        
        return rgbColorName.toString();
    }
    
}
