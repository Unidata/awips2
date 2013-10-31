package ohd.hseb.monitor;

import javax.swing.JOptionPane;

import ohd.hseb.model.ParamCode;
import ohd.hseb.monitor.precip.PrecipMonitorJTableRowData;
import ohd.hseb.monitor.river.RiverMonitorJTableRowData;
import ohd.hseb.timeserieslite.TimeSeriesLite;

public class MonitorTimeSeriesLiteManager
{
    private String _connectionString;
    
    public MonitorTimeSeriesLiteManager(String connectionString)
    {
        _connectionString = connectionString;
    }
    
    public boolean displayTimeSeriesLite(MonitorFrame mainFrame, RiverMonitorJTableRowData rowData)
    {
        String obsTypeSource = rowData.getObsTypeSource();
        String fcstTypeSource = rowData.getFcstTypeSource();
        String primaryPe = rowData.getPrimaryRiverPe();
        String locationId = rowData.getLid();
        
        String obsParamCode = null;
        String fcstParamCode = null;
        boolean result = false;

        if(obsTypeSource == null && fcstTypeSource == null)
        {
            result = false;
        }
        else
        {
            if(obsTypeSource != null)
                obsParamCode = primaryPe+"I"+obsTypeSource+"Z";
            if(fcstTypeSource != null)
                fcstParamCode = primaryPe +"I"+fcstTypeSource+"Z";

            System.out.println("Obs:"+obsParamCode);
            System.out.println("Fcst:"+fcstParamCode);

            String tslArgs[]= {"ohd.hseb.timeserieslite.rivermon.RiverMonDrawingMgr",
                    _connectionString, locationId, obsParamCode, fcstParamCode};

            TimeSeriesLite tsl = new TimeSeriesLite();
            tsl.display(tslArgs, false);
            result = true;
        }
     
        if(!result)
        {
            String textMsg = "<HTML><BODY>Latestobs and/or MaxFcst is unavailable.<BR>" +
            "Unable to display TimeSeriesLite.</BODY></HTML>";
            JOptionPane.showMessageDialog(mainFrame, textMsg,"TimeSeriesLite", JOptionPane.PLAIN_MESSAGE);
        }
        return result;
    }
    
    public boolean displayTimeSeriesLite(MonitorFrame mainFrame, PrecipMonitorJTableRowData rowData, int hour)
    {
        ParamCode paramCode = null;
        switch(hour)
        {
            case 1:
                paramCode = rowData.getPrecipData().getTohPrecip1HrParamCode();
                break;
            case 3:
                paramCode = rowData.getPrecipData().getTohPrecip3HrParamCode();
                break;
            case 6:
                paramCode = rowData.getPrecipData().getTohPrecip6HrParamCode();
                break;
            case 24:
                paramCode = rowData.getPrecipData().getTohPrecip24HrParamCode();
                break;
        }
        
        String obsTypeSource = null;
        
        String pe = null;
        String dur = null;
        String extremum = null;
        
        if(paramCode != null)
        {
          obsTypeSource = paramCode.getTypeSource();
          pe = "PP";
          dur = "H";
          extremum = paramCode.getExtremum();
        }
            
        String locationId = rowData.getLid();
        
        String obsParamCode = null;
        String fcstParamCode = null;
        boolean result = false;
       
        if(paramCode == null)
        {
            result = false;
        }
        else
        {
            if(obsTypeSource != null)
            {
                obsParamCode = pe+dur+obsTypeSource+extremum;
                fcstParamCode = pe + dur + "FF"+ extremum;
            }
           
            System.out.println("Obs:"+obsParamCode);
            System.out.println("Fcst:"+fcstParamCode);

            String tslArgs[]= {"ohd.hseb.timeserieslite.pdc.PDCDrawingMgr",
                    _connectionString, locationId, obsParamCode, fcstParamCode};

            TimeSeriesLite tsl = new TimeSeriesLite();
            tsl.display(tslArgs, false);
            result = true;
        }
     
        if(!result)
        {
            String textMsg = "<HTML><BODY>Top-of-hour 1 hr Precip data is unavailable.<BR>" +
            "Unable to display TimeSeriesLite.</BODY></HTML>";
            JOptionPane.showMessageDialog(mainFrame, textMsg,"TimeSeriesLite", JOptionPane.PLAIN_MESSAGE);
        }
        return result;
    }
  
}
