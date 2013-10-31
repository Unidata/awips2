package ohd.hseb.raxbase.model;

import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxbase.util.TSManager;
import ohd.hseb.raxdb.generated.AvgRecord;

public class Average
{
    private String _lid = null;
    private String _pe = null;
    private String _dur = null;
    private short _idur;
    private String _ts = null;
    private String _extremum = null;
    private String _p = null;
    private double _jan;
    private double _feb;
    private double _mar;
    private double _apr;
    private double _may;
    private double _jun;
    private double _jul;
    private double _aug;
    private double _sep;
    private double _oct;
    private double _nov;
    private double _dec;
    private long _calcdate;
    
    public Average(){}

    public Average( Average average )
    {
        _lid = average.getLid();
        _pe = average.getPe();
        _dur = average.getDur();
        _idur = average.getIdur();
        _ts = average.getTs();
        _extremum = average.getExtremum();
        _p = average.getP();
        _jan = average.getJan();
        _feb = average.getFeb();
        _mar = average.getMar();
        _apr = average.getApr();
        _may = average.getMay();
        _jun = average.getJun();
        _jul = average.getJul();
        _aug = average.getAug();
        _sep = average.getSep();
        _oct = average.getOct();
        _nov = average.getNov();
        _dec = average.getDec();
        _calcdate = average.getCalcdate();
    }
    
    public Average( AvgRecord averageRecord )
    {
        _lid = averageRecord.getLid();
        _pe = PEManager.getPEFromPe1Pe2( averageRecord.getPe1(), averageRecord.getPe2() );
        _dur = averageRecord.getDur();
        _idur = averageRecord.getIdur();
        _ts = TSManager.getTSFromTandS( averageRecord.getT(), averageRecord.getS() );
        _extremum = averageRecord.getE();
        _p = averageRecord.getP();
        _jan = averageRecord.getJan();
        _feb = averageRecord.getFeb();
        _mar = averageRecord.getMar();
        _apr = averageRecord.getApr();
        _may = averageRecord.getMay();
        _jun = averageRecord.getJun();
        _jul = averageRecord.getJul();
        _aug = averageRecord.getAug();
        _sep = averageRecord.getSep();
        _oct = averageRecord.getOct();
        _nov = averageRecord.getNov();
        _dec = averageRecord.getDec();
        _calcdate = averageRecord.getCalcdate();
    }
    
    public static Average getAverage( AvgRecord record )
    {
        return ( new Average( record ) );
    }
    
    public static AvgRecord getAverageRecord( Average average )
    {
        AvgRecord averageRecord = new AvgRecord();
        
        averageRecord.setLid( average.getLid() );
        averageRecord.setPe1( PEManager.getPe1FromPE( average.getPe() ) );
        averageRecord.setPe2( PEManager.getPe2FromPE( average.getPe() ) );
        averageRecord.setDur( average.getDur() );
        averageRecord.setIdur( average.getIdur() );
        averageRecord.setT( TSManager.getTFromTS( average.getTs() ) );
        averageRecord.setS( TSManager.getSFromTS( average.getTs() ) );
        averageRecord.setE( average.getExtremum() );
        averageRecord.setP( average.getP() );
        averageRecord.setJan( average.getJan() );
        averageRecord.setFeb( average.getFeb() );
        averageRecord.setMar( average.getMar() );
        averageRecord.setApr( average.getApr() );
        averageRecord.setMay( average.getMay() );
        averageRecord.setJun( average.getJun() );
        averageRecord.setJul( average.getJul() );
        averageRecord.setAug( average.getAug() );
        averageRecord.setSep( average.getSep() );
        averageRecord.setOct( average.getOct() );
        averageRecord.setNov( average.getNov() );
        averageRecord.setDec( average.getDec() );
        averageRecord.setCalcdate( average.getCalcdate() );
        
        return averageRecord;
    }
    
    public String toString()
    {
        String returnString = "LID = " + _lid + " | PE = " + _pe + " | Dur = " + _dur + " | IDur = " + _idur + 
                              " | TS = " + _ts + " | Extremum = " + _extremum + " | P = " + _p + 
                              " | Jan = " + _jan + " | Feb = " + _feb + " | Mar = " + _mar +
                              " | Apr = " + _apr + " | May = " + _may + " | Jun = " + _jun +
                              " | Jul = " + _jul + " | Aug = " + _aug + " | Sep = " + _sep +
                              " | Oct = " + _oct + " | Nov = " + _nov + " | Dec = " + _dec +
                              " | CalcDate = " + _calcdate;
        
        return returnString;
    }
    
    public String keyString()
    {
        String keyString = "LID = " + getKeyValue( _lid ) + " | PE = " + getKeyValue( _pe ) + " | Dur = " + getKeyValue( _dur ) + 
                           " | IDur = " + getKeyValue( _idur ) + " | TS = " + getKeyValue( _ts ) + " | Extremum = " + getKeyValue( _extremum ) + 
                           " | P = " + getKeyValue( _p );
        
        return keyString;
    }
    
    private String getKeyValue( String str )
    {
        String returnValue = "NULL";
        
        if ( str != null )
        {
            returnValue = str;
        }
        
        return returnValue;
    }
    
    private String getKeyValue( short number )
    {
        return ( getKeyValue( number + "" ) );  
    }
    
    public void setLid( String lid )
    {
        _lid = lid;
    }

    public String getLid()
    {
        return _lid;
    }

    public void setPe( String pe )
    {
        _pe = pe;
    }

    public String getPe()
    {
        return _pe;
    }

    public void setDur( String dur )
    {
        _dur = dur;
    }

    public String getDur()
    {
        return _dur;
    }

    public void setIdur( short idur )
    {
        _idur = idur;
    }

    public short getIdur()
    {
        return _idur;
    }

    public void setTs( String ts )
    {
        _ts = ts;
    }

    public String getTs()
    {
        return _ts;
    }

    public void setExtremum( String extremum )
    {
        _extremum = extremum;
    }

    public String getExtremum()
    {
        return _extremum;
    }

    public void setP( String p )
    {
        _p = p;
    }

    public String getP()
    {
        return _p;
    }

    public void setJan( double jan )
    {
        _jan = jan;
    }

    public double getJan()
    {
        return _jan;
    }

    public void setFeb( double feb )
    {
        _feb = feb;
    }

    public double getFeb()
    {
        return _feb;
    }

    public void setMar( double mar )
    {
        _mar = mar;
    }

    public double getMar()
    {
        return _mar;
    }

    public void setApr( double apr )
    {
        _apr = apr;
    }

    public double getApr()
    {
        return _apr;
    }

    public void setMay( double may )
    {
        _may = may;
    }

    public double getMay()
    {
        return _may;
    }

    public void setJun( double jun )
    {
        _jun = jun;
    }

    public double getJun()
    {
        return _jun;
    }

    public void setJul( double jul )
    {
        _jul = jul;
    }

    public double getJul()
    {
        return _jul;
    }

    public void setAug( double aug )
    {
        _aug = aug;
    }

    public double getAug()
    {
        return _aug;
    }

    public void setSep( double sep )
    {
        _sep = sep;
    }

    public double getSep()
    {
        return _sep;
    }

    public void setOct( double oct )
    {
        _oct = oct;
    }

    public double getOct()
    {
        return _oct;
    }

    public void setNov( double nov )
    {
        _nov = nov;
    }

    public double getNov()
    {
        return _nov;
    }

    public void setDec( double dec )
    {
        _dec = dec;
    }

    public double getDec()
    {
        return _dec;
    }

    public void setCalcdate( long calcdate )
    {
        _calcdate = calcdate;
    }

    public long getCalcdate()
    {
        return _calcdate;
    }
}
