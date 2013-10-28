package ohd.hseb.geomap.io;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Scanner;

import ohd.hseb.geomap.model.ElevationMap;
import ohd.hseb.geomap.model.LatLonBounds;

public class ElevationFileReaderSlow
{
    private BufferedReader _reader = null;
    
    // -------------------------------------------------------------------------------
    
    public ElevationFileReaderSlow(String filePath)
    {
        try
        {
            int size = 10000000;
            _reader = new BufferedReader(new FileReader(filePath), size);
           // _reader = new BufferedReader(new FileReader(filePath));
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
    }
    
    // -------------------------------------------------------------------------------
    
    public ElevationMap read()
    {    
        ElevationMap map = null;
        
        Scanner scanner = new Scanner(_reader);
        
        try
        {
            map = readHeader(scanner);
            readBody(scanner, map);
            _reader.close();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        
    
        return map;
    }
    // -------------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------------
    public ElevationMap readHeader(Scanner scanner) throws IOException
    {
        
        String header = "ElevationFileReaderSlow.readHeader(): ";
             
        
        double lat1 =  scanner.nextDouble();
        double lon1 = scanner.nextDouble();
        
        // negate the longitude
        lon1 *= -1;
        
        double latRange = scanner.nextDouble();
        double lonRange = scanner.nextDouble();
        
        System.out.println(header + "lat1 = " + lat1 + " lon1 = " + lon1 + 
                                     "latRange = " + latRange + "longRange = " + lonRange);
        
        double lat2 = lat1 - latRange;
        double lon2 = lon1 + lonRange;
        
        double latResInArcMinutes = scanner.nextDouble();
        double lonResInArcMinutes = scanner.nextDouble();
        
        LatLonBounds latLonBounds = new LatLonBounds(lat1, lat2, lon1, lon2);
        
        System.out.println(header + "lat lon bounds = " + latLonBounds);
        
        
        double latResInDegrees = latResInArcMinutes/60.0;
        double lonResInDegrees = lonResInArcMinutes/60.0;
        
        
        ElevationMap map = new ElevationMap(latLonBounds, latResInDegrees, lonResInDegrees);
           
        return map;
        
    }
    // -------------------------------------------------------------------------------
    public void readBody(Scanner scanner, ElevationMap map) throws IOException
    {
        boolean done = false;
        
        double northLat = map.getLatLonBounds().getNorthLat();
        double southLat = map.getLatLonBounds().getSouthLat();
        double westLon = map.getLatLonBounds().getWestLon();
        double eastLon = map.getLatLonBounds().getEastLon();
        
        double currentLat = northLat;
        double currentLon = westLon;
        
        short elevation = 0;
           
        int rowCount = map.getRowCount();
        int colCount = map.getColCount();
      
        for (int row = 0; row < rowCount; row++)
        {
            for (int col = 0; col < colCount; col++)
            {
                elevation = scanner.nextShort();
                
                map.setValue(row, col, elevation);
            }
         }
    }
    
}
