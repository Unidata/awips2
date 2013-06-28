package ohd.hseb.geomap.io;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import ohd.hseb.geomap.model.LatLonBounds;
import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.model.LatLonPolyline;
import ohd.hseb.geomap.model.Polygon;

public class BCDFileReader
{
    private DataInputStream _reader = null;
    private LatLonBounds _latLonBounds = null;
    
    // -------------------------------------------------------------------------------
    
    public BCDFileReader(String filePath)
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
      
    public List<Polygon> readPolygonList()
    {
        String header = "BCDFileReader.readPolygonList(): ";
        
        List<Polygon> polygonList = new ArrayList<Polygon> ();
       
        boolean done = false;
        while (! done)
        {
            Polygon polygon = readPolygon();
            if (polygon != null)
            {
                polygonList.add(polygon);
            }
            else // polygon == null
            {
                done = true;
            }
        }
        
        System.out.println(header + "Lat lon bounds are " + _latLonBounds);

        try
        {
        _reader.close();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        return polygonList;
    }
    
    // -------------------------------------------------------------------------------
       
    public Polygon readPolygon()
    {
        String header = "BCDFileReader.readPolygon(): ";
        Polygon polygon = null;
        List<LatLonPoint> pointList = new ArrayList<LatLonPoint>();
        
        try
        {
            //read header
            
            int numPoints = _reader.readInt();
            float boundsLat1 = _reader.readFloat();
            float boundsLat2 = _reader.readFloat();
            float boundsLon1 = _reader.readFloat();
            float boundsLon2 = _reader.readFloat();
            
            if (_latLonBounds == null)
            {
                _latLonBounds = new LatLonBounds(boundsLat1, boundsLat2,  boundsLon1, boundsLon2);
            }
            else
            {
                _latLonBounds.updateLatLonBounds(boundsLat1, boundsLat2,  boundsLon1, boundsLon2);
                //System.out.println("Lat lon bounds are " + _latLonBounds);
            }
            
            //reader points
            for (int i = 0 ; i < numPoints; i++)
            {
                float lat = _reader.readFloat();
                float lon = _reader.readFloat();
                LatLonPoint point = new LatLonPoint(lat, lon);
                pointList.add(point);
            }
        }
        
        catch (EOFException e)
        {
            //not an error
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        
        if (pointList.size() > 0)
        {
            polygon = new Polygon(pointList);
        }
        
        return polygon;
        
    }
    
    // -------------------------------------------------------------------------------
    
    public List<LatLonPolyline> readLineList()
    {
        List<LatLonPolyline> lineList = new ArrayList<LatLonPolyline> ();
        
        return lineList;
    }
    
    // -------------------------------------------------------------------------------
     
    public List<LatLonPoint> readPointList()
    {
        List<LatLonPoint> pointList = new ArrayList<LatLonPoint> ();
        
        return pointList;
    }
    
    // -------------------------------------------------------------------------------
    
    public void setLatLonBounds(LatLonBounds latLonBounds)
    {
        _latLonBounds = latLonBounds;
    }
    
    //  -------------------------------------------------------------------------------
    
    public LatLonBounds getLatLonBounds()
    {
        return _latLonBounds;
    }
    
    //  -------------------------------------------------------------------------------
   
    //  -------------------------------------------------------------------------------
}
