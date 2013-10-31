package ohd.hseb.timeserieslite;

import ohd.hseb.model.RatingCurve;

public class Location
{
    private String _lid = null;
    private double _lat = 0;
    private double _lon = 0;
    private double _elevation = 0;
    private String _locationName = null;
    private boolean _isRiverStation = false;
    private boolean _isFcstPoint = false;
    private String _hsa = null;
    private double _floodStage = 0;
    private double _floodFlow = 0;
    private RatingCurve _ratingCurve = null;
    private boolean _isDCP = false;
    private boolean _isObserver = false;
    private String _telemType = null;
    
    public Location()
    {
    }
    
    public Location( Location location )
    {
        _lid = location.getLid();
        _hsa = location.getHsa();
        _lat = location.getLat();
        _lon = location.getLon();
        _elevation = location.getElevation();
        _locationName = location.getLocationName();
        _isRiverStation = location.isRiverStation();
        _floodStage = location.getFloodStage();
        _floodFlow = location.getFloodFlow();
        _ratingCurve = location.getRatingCurve();
        _isDCP = location.isDCP();
        _isObserver = location.isObserver();
        _telemType = location.getTelemType();
    }
    
    public String toString()
    {
        String string = null;
        
        string = "LID = " + _lid +
                 " HSA = " + _hsa +
                 " Lat = " + _lat +
                 " Lon = " + _lon +
                 " Elev = " + _elevation +
                 " Location Name = " + _locationName +
                 " RiverStation = " + _isRiverStation +
                 " Flood Stage = " + _floodStage +
                 " Flood Flow = " + _floodFlow + 
                 " DCP = " + _isDCP +
                 " Observer = " + _isObserver +
                 " Telem Type = " + _telemType;
        
        return string;
    }
    public void setLid( String lid )
    {
        _lid = lid;
    }
    public String getLid()
    {
        return _lid;
    }
    public void setLat( double lat )
    {
        _lat = lat;
    }
    public double getLat()
    {
        return _lat;
    }
    public void setLon( double lon )
    {
        _lon = lon;
    }
    public double getLon()
    {
        return _lon;
    }
    public void setElevation( double elevation )
    {
        _elevation = elevation;
    }
    public double getElevation()
    {
        return _elevation;
    }
    public void setLocationName( String locationName )
    {
        _locationName = locationName;
    }
    public String getLocationName()
    {
        return _locationName;
    }

    public void setRiverStation( boolean isRiverStation )
    {
        _isRiverStation = isRiverStation;
    }

    public boolean isRiverStation()
    {
        return _isRiverStation;
    }

    public void setFloodStage( double floodStage )
    {
        _floodStage = floodStage;
    }

    public double getFloodStage()
    {
        return _floodStage;
    }

    public void setHsa( String hsa )
    {
        _hsa = hsa;
    }

    public String getHsa()
    {
        return _hsa;
    }

    public void setFcstPoint( boolean isFcstPoint )
    {
        _isFcstPoint = isFcstPoint;
    }

    public String isFcstPoint()
    {
        if ( _isFcstPoint )
        {
            return "T";
        }
        else
        {
            return "F";
        }
    }

    public void setFloodFlow( double floodFlow )
    {
        _floodFlow = floodFlow;
    }

    public double getFloodFlow()
    {
        return _floodFlow;
    }

    public void setRatingCurve( RatingCurve ratingCurve )
    {
        _ratingCurve = ratingCurve;
    }

    public RatingCurve getRatingCurve()
    {
        return _ratingCurve;
    }

    public void setDCP( boolean isDCP )
    {
        _isDCP = isDCP;
    }

    public boolean isDCP()
    {
        return _isDCP;
    }

    public void setObserver( boolean isObserver )
    {
        _isObserver = isObserver;
    }

    public boolean isObserver()
    {
        return _isObserver;
    }

    public void setTelemType( String telemType )
    {
        _telemType = telemType;
    }

    public String getTelemType()
    {
        return _telemType;
    }
}
