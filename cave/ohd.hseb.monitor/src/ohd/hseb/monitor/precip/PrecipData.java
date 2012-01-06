package ohd.hseb.monitor.precip;

import ohd.hseb.db.DbTable;
import ohd.hseb.model.ParamCode;

public class PrecipData
{
    private double _ffg1Hr;
    private double _ffg3Hr;
    private double _ffg6Hr;
    
    private double _latestPrecip30Min;
    private double _latestPrecip1Hr;
    private double _latestPrecip3Hr;
    private double _latestPrecip6Hr;
    private double _latestPrecip12Hr;
    private double _latestPrecip18Hr;
    private double _latestPrecip24Hr;
    
    private double _tohPrecip1Hr;
    //at the moment, with the synoptic time scales for the PDC preprocessor,
    //these columns do not make sense.
  //  private double _tohPrecip3Hr;
  //  private double _tohPrecip6Hr;
  //  private double _tohPrecip24Hr;
    
    private double _diff1Hr;
    private double _diff3Hr;
    private double _diff6Hr;
 
    private int _ratio1Hr;
    private int _ratio3Hr;
    private int _ratio6Hr;
    
    private ParamCode _latestPrecipParamCode;
    
    private ParamCode _tohPrecip1HrParamCode;
    private ParamCode _tohPrecip3HrParamCode;
    private ParamCode _tohPrecip6HrParamCode;
    private ParamCode _tohPrecip24HrParamCode;
    
    public PrecipData()
    {
        _ffg1Hr = DbTable.getNullDouble();
        _ffg3Hr = DbTable.getNullDouble();
        _ffg6Hr = DbTable.getNullDouble();

        _latestPrecip30Min = DbTable.getNullDouble();
        _latestPrecip1Hr = DbTable.getNullDouble();
        _latestPrecip3Hr = DbTable.getNullDouble();
        _latestPrecip6Hr = DbTable.getNullDouble();
        _latestPrecip12Hr = DbTable.getNullDouble();
        _latestPrecip18Hr = DbTable.getNullDouble();
        _latestPrecip24Hr = DbTable.getNullDouble();

        _tohPrecip1Hr = DbTable.getNullDouble();
      //  _tohPrecip3Hr = DbTable.getNullDouble();
      //  _tohPrecip6Hr = DbTable.getNullDouble();
      //  _tohPrecip24Hr = DbTable.getNullDouble();

        _diff1Hr = DbTable.getNullDouble();
        _diff3Hr = DbTable.getNullDouble();
        _diff6Hr = DbTable.getNullDouble();

        _ratio1Hr = DbTable.getNullInt();
        _ratio3Hr = DbTable.getNullInt();
        _ratio6Hr = DbTable.getNullInt();
    }
    
    public double getFFG1Hr()
    {
        return _ffg1Hr;
    }
    
    public void setFFG1Hr(double ffg1Hr)
    {
        _ffg1Hr = ffg1Hr;
    }
    
    public double getFFG3Hr()
    {
        return _ffg3Hr;
    }
    
    public void setFFG3Hr(double ffg3Hr)
    {
        _ffg3Hr = ffg3Hr;
    }

    public double getFFG6Hr()
    {
        return _ffg6Hr;
    }
    
    public void setFFG6Hr(double ffg6Hr)
    {
        _ffg6Hr = ffg6Hr;
    }

    public double getTohPrecip1Hr()
    {
        return _tohPrecip1Hr;
    }
    
    public void setTohPrecip1Hr(double tohPrecip1Hr)
    {
        _tohPrecip1Hr = tohPrecip1Hr;
    }
    /*
    
    public double getTohPrecip3Hr()
    {
        return _tohPrecip3Hr;
    }
    
    public void setTohPrecip3Hr(double tohPrecip3Hr)
    {
        _tohPrecip3Hr = tohPrecip3Hr;
    }
    
    public double getTohPrecip6Hr()
    {
        return _tohPrecip6Hr;
    }
    
    public void setTohPrecip6Hr(double tohPrecip6Hr)
    {
        _tohPrecip6Hr = tohPrecip6Hr;
    }
    
    public double getTohPrecip24Hr()
    {
        return _tohPrecip24Hr;
    }
    
    public void setTohPrecip24Hr(double tohPrecip24Hr)
    {
        _tohPrecip24Hr = tohPrecip24Hr;
    }
    
    */
    
    
    public double getLatestPrecip30Min()
    {
        return _latestPrecip30Min;
    }
    
    public void setLatestPrecip30Min(double latestPrecip30Min)
    {
        _latestPrecip30Min = latestPrecip30Min;
    }
    
    
    public double getLatestPrecip1Hr()
    {
        return _latestPrecip1Hr;
    }
    
    public void setLatestPrecip1Hr(double latestPrecip1Hr)
    {
        _latestPrecip1Hr = latestPrecip1Hr;
    }
    
    public double getLatestPrecip3Hr()
    {
        return _latestPrecip3Hr;
    }
    
    public void setLatestPrecip3Hr(double latestPrecip3Hr)
    {
        _latestPrecip3Hr = latestPrecip3Hr;
    }
    
    public double getLatestPrecip6Hr()
    {
        return _latestPrecip6Hr;
    }
    
    public void setLatestPrecip6Hr(double latestPrecip6Hr)
    {
        _latestPrecip6Hr = latestPrecip6Hr;
    }
    
    public double getLatestPrecip12Hr()
    {
        return _latestPrecip12Hr;
    }
    
    public void setLatestPrecip12Hr(double latestPrecip12Hr)
    {
        _latestPrecip12Hr = latestPrecip12Hr;
    }
    
    public double getLatestPrecip18Hr()
    {
        return _latestPrecip18Hr;
    }
    
    public void setLatestPrecip18Hr(double latestPrecip18Hr)
    {
        _latestPrecip18Hr = latestPrecip18Hr;
    }
    
    public double getLatestPrecip24Hr()
    {
        return _latestPrecip24Hr;
    }
    
    public void setLatestPrecip24Hr(double latestPrecip24Hr)
    {
        _latestPrecip24Hr = latestPrecip24Hr;
    }
    
    public int getRatio1Hr()
    {
        return _ratio1Hr;
    }
    
    public void setRatio1Hr(int ratio1Hr)
    {
        _ratio1Hr = ratio1Hr;
    }
    public int getRatio3Hr()
    {
        return _ratio3Hr;
    }
    
    public void setRatio3Hr(int ratio3Hr)
    {
        _ratio3Hr = ratio3Hr;
    }
    public int getRatio6Hr()
    {
        return _ratio6Hr;
    }
    
    public void setRatio6Hr(int ratio6Hr)
    {
        _ratio6Hr = ratio6Hr;
    }
    public double getDiff1Hr()
    {
        return _diff1Hr;
    }
    
    public void setDiff1Hr(double diff1Hr)
    {
        _diff1Hr = diff1Hr;
    }
    public double getDiff3Hr()
    {
        return _diff3Hr;
    }
    
    public void setDiff3Hr(double diff3Hr)
    {
        _diff3Hr = diff3Hr;
    }
    
    public double getDiff6Hr()
    {
        return _diff6Hr;
    }
    
    public void setDiff6Hr(double diff6Hr)
    {
        _diff6Hr = diff6Hr;
    }

    public ParamCode getLatestPrecipParamCode()
    {
        return _latestPrecipParamCode;
    }
    
    public String getLatestPrecipParamCodeString()
    {
        if(_latestPrecipParamCode != null)
            return _latestPrecipParamCode.toString();
        else
            return null;
    }
    
    public void setLatestPrecipParamCode(ParamCode latestPrecipParamCode)
    {
        _latestPrecipParamCode = latestPrecipParamCode;
    }

    public ParamCode getTohPrecip1HrParamCode()
    {
        return _tohPrecip1HrParamCode;
    }
    
    public String getTohPrecip1HrParamCodeString()
    {
        if(_tohPrecip1HrParamCode != null)
            return _tohPrecip1HrParamCode.toString();
        else
            return null;
    }

    public void setTohPrecip1HrParamCode(ParamCode tohPrecip1HrParamCode)
    {
        _tohPrecip1HrParamCode = tohPrecip1HrParamCode;
    }

    public ParamCode getTohPrecip3HrParamCode()
    {
        return _tohPrecip3HrParamCode;
    }

    public String getTohPrecip3HrParamCodeString()
    {
        if(_tohPrecip3HrParamCode != null)
            return _tohPrecip3HrParamCode.toString();
        else
            return null;
    }

    public void setTohPrecip3HrParamCode(ParamCode tohPrecip3HrParamCode)
    {
        _tohPrecip3HrParamCode = tohPrecip3HrParamCode;
    }

    public ParamCode getTohPrecip6HrParamCode()
    {
        return _tohPrecip6HrParamCode;
    }

    public String getTohPrecip6HrParamCodeString()
    {
        if(_tohPrecip6HrParamCode != null)
            return _tohPrecip6HrParamCode.toString();
        else
            return null;
    }

    public void setTohPrecip6HrParamCode(ParamCode tohPrecip6HrParamCode)
    {
        _tohPrecip6HrParamCode = tohPrecip6HrParamCode;
    }

    public ParamCode getTohPrecip24HrParamCode()
    {
        return _tohPrecip24HrParamCode;
    }

    public String getTohPrecip24HrParamCodeString()
    {
        if(_tohPrecip24HrParamCode != null)
            return _tohPrecip24HrParamCode.toString();
        else
            return null;
    }

    public void setTohPrecip24HrParamCode(ParamCode tohPrecip24HrParamCode)
    {
        _tohPrecip24HrParamCode = tohPrecip24HrParamCode;
    }
    
    public boolean isDataAvailable()
    {
        boolean result = true;
        if(
                DbTable.isNull(getLatestPrecip30Min()) &&
                DbTable.isNull(getLatestPrecip1Hr()) &&
                DbTable.isNull(getLatestPrecip3Hr()) &&
                DbTable.isNull(getLatestPrecip6Hr()) &&
                DbTable.isNull(getLatestPrecip12Hr()) &&
                DbTable.isNull(getLatestPrecip18Hr()) &&
                DbTable.isNull(getLatestPrecip24Hr()) &&
                DbTable.isNull(getTohPrecip1Hr()) 

                /*
           &&  getTohPrecip3Hr() == DbTable.getNullDouble() &&
            getTohPrecip6Hr() == DbTable.getNullDouble() &&
            getTohPrecip24Hr() == DbTable.getNullDouble() 
                 */
        )
        {
            result = false;
        }
            
        return result;    
    }
    
    public void setTohParamCodeByHour(int hour, ParamCode paramCode)
    {
        switch(hour)
        {
            case 1:
                setTohPrecip1HrParamCode(paramCode);
                break;
            case 3:
                setTohPrecip3HrParamCode(paramCode);
                break;
            case 6:
                setTohPrecip6HrParamCode(paramCode);
                break;
            case 24:
                setTohPrecip24HrParamCode(paramCode);
                break;
        }
    }
    
    public void setTohPrecipByHour(int hour, double precipTotal)
    {
        switch(hour)
        {
            case 1:
                setTohPrecip1Hr(precipTotal);
                break;
                
                /*
            case 3:
                setTohPrecip3Hr(precipTotal);
                break;
            case 6:
                setTohPrecip6Hr(precipTotal);
                break;
            case 24:
                setTohPrecip24Hr(precipTotal);
                break;
                */
        }
    }
    
    public double getTohPrecipByHour(int hour)
    {
        double returnValue = DbTable.getNullDouble();
        switch (hour)
        {
            case 1:
                returnValue = getTohPrecip1Hr();
                break;
            /*    
            case 3:
                returnValue = getTohPrecip3Hr();
                break;
            case 6:
                returnValue = getTohPrecip6Hr();
                break;
            case 24:
                returnValue = getTohPrecip24Hr();
                break;
             */
        }
        return returnValue;
    }
    
    public void setLatestPrecipByHour(float hour, double precipTotal)
    {

        if (hour == 0.5)
        {
            setLatestPrecip30Min(precipTotal);
        }
        else if (hour == 1)
        {
            setLatestPrecip1Hr(precipTotal);
        }
        else if (hour == 3)
        {
            setLatestPrecip3Hr(precipTotal);
        }
        else if (hour == 6)
        {
            setLatestPrecip6Hr(precipTotal);
        }
        else if (hour == 12)
        {
            setLatestPrecip12Hr(precipTotal);
        }
        else if (hour == 18)
        {
            setLatestPrecip18Hr(precipTotal);
        }
        else if (hour == 24)
        {
            setLatestPrecip24Hr(precipTotal);
        }

        return;
    }

    public double getLatestPrecipByHour(float hour)
    {
        double returnValue = DbTable.getNullDouble();
     
        if (hour == 0.5)
        {
            returnValue = getLatestPrecip30Min();
        }
        if (hour == 1)
        {
            returnValue = getLatestPrecip1Hr();
        }
        else if (hour == 3)
        {
            returnValue = getLatestPrecip3Hr();
        }
        else if (hour == 6)
        {
            returnValue = getLatestPrecip6Hr();
        }
        else if (hour == 12)
        {
            returnValue = getLatestPrecip12Hr();
        }
        else if (hour == 18)
        {
            returnValue = getLatestPrecip18Hr();
        }
        else if (hour == 24)
        {
            returnValue = getLatestPrecip24Hr();
        }
         
  
        return returnValue;
    }
}

