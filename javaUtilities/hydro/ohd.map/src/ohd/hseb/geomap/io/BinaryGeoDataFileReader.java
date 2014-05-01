package ohd.hseb.geomap.io;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import ohd.hseb.util.EndianConverter;
import ohd.hseb.geomap.model.LatLonBounds;
import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.model.LatLonPolyline;

public class BinaryGeoDataFileReader
{
    private DataInputStream _reader = null;
    private LatLonBounds _latLonBounds = null;
    
    // -------------------------------------------------------------------------------
    
    public BinaryGeoDataFileReader(String filePath)
    {
        try
        {
            _reader = new DataInputStream(new BufferedInputStream(new FileInputStream(filePath)));
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
    }
    
    // -------------------------------------------------------------------------------
      
    public List<LatLonPolyline> readLineList()
    {
        String header = "BinaryGeoDataFileReader.readLineList(): ";
        
        List<LatLonPolyline> lineList = new ArrayList<LatLonPolyline> ();
       
        int lineCount = 0;
        System.out.println(header);

        try
        {

            boolean done = false;
            while (! done)
            {
                LatLonPolyline latLonPolyline = readLine();
                if (latLonPolyline != null)
                {
                    lineList.add(latLonPolyline);
                    lineCount++;
                }
                else // line == null
                {
                    done = true;
                }
            }
        
        System.out.println(header + "lineCount =  " + lineCount);

       
            _reader.close();
        }
        catch(EOFException e)
        {
            //do nothing
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        
        return lineList;
    }
    
    // -------------------------------------------------------------------------------
       
    private LatLonPolyline readLine() throws IOException
    {
        String header = "BinaryGeoDataFileReader.readLine(): ";
        
        String lid = readNCharsIntoString(9);
        String name = readNCharsIntoString(21);
        
        int order = readInt();
        
        int pointCount = readInt();
       // System.out.println(header + "lid =  " + lid);
       // System.out.println(header + "name =  " + name);

        //System.out.println(header + "pointCount =  " + pointCount);

        
        List<LatLonPoint> pointList = new ArrayList<LatLonPoint>();
        
        for (int i = 0; i < pointCount; i++)
        {
            
            float lat = readFloat();
            float lon = readFloat();
            lon *= -1;
            
            LatLonPoint latLonPoint = new LatLonPoint(lat, lon);
            
       //     System.out.println(header + "latLonPoint = " + latLonPoint);
            
            pointList.add(latLonPoint);
        }
        
        LatLonPolyline latLonPolyline = new LatLonPolyline(pointList);
        
    
        return latLonPolyline;
    }
    // -------------------------------------------------------------------------------
    private int readInt() throws IOException
    {
         
        
        int number = _reader.readInt();
         
        return EndianConverter.convert(number);
        
    }
    // -------------------------------------------------------------------------------

    private float readFloat() throws IOException
    {
        float number = _reader.readFloat();
        
        return EndianConverter.convert(number);
        
    }
    
    // -------------------------------------------------------------------------------
    private String readNCharsIntoString(int charCount) throws IOException
    {
        StringBuffer buffer = new StringBuffer();
        
        for (int i = 0; i < charCount; i++)
        {
            char c = (char) _reader.readByte();
            if (i < charCount - 1)
            {
                buffer.append(c);
            }
        }
        
        return buffer.toString();
        
    }
    // -------------------------------------------------------------------------------
}
