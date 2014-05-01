package ohd.hseb.fp_vtec_info;

import java.util.ArrayList;

public class FpCurPrevVtec 
{
	private String  _locId;
	private String  _locName;
	private String  _locStream;
	private String  _locPe;
	private double  _floodStg;
	private double  _floodFlow;
	private long    _obsBeginTime;
	private long    _fcstEndTime;
	private double  _adjustendhrs;
	private String  _curAction;	
	private String  _curPhenom;
	private String  _curSignif;
	private String  _curSeverity;
	private String  _curCause;
	private String  _curRecord;
	private int     _curEtn; 
	private double  _curCrestValue;
	private long    _curBeginTime;
	private long    _curEndTime;	
	private long    _curRiseTime;
	private long    _curCrestTime;
	private long    _curFallTime;
	private String  _prevAction;	
	private String  _prevPhenom;
	private String  _prevSignif;
	private String  _prevSeverity;
	private String  _prevCause;
	private String  _prevRecord;
	private int     _prevEtn;  
	private double  _prevCrestValue;
	private String  _prevRiseType;
	private String  _prevCrestType;
	private String  _prevFallType;
	private long    _prevBeginTime;
	private long    _prevEndTime;	
	private long    _prevRiseTime;
	private long    _prevCrestTime;
	private long    _prevFallTime;
	private long    _prevProductTime;
	private ArrayList _obsValuetTimeList;
	private ArrayList _fcstValueTimeList;
	
	public FpCurPrevVtec()
	{
		
	}
        
	public String getLocId() 
	{
		return _locId;
	}

	public void setLocId(String locId) 
	{
		_locId = locId;
	}
	public String getLocName() 
	{
		return _locName;
	}

	public void setLocName(String locName) 
	{
		_locName = locName;
	}
	
	public String getLocStream() 
	{
		return _locStream;
	}

	public void setLocStream(String locStream) 
	{
		_locStream = locStream;
	}
	
	public String getLocPe() 
	{
		return _locPe;
	}

	public void setLocPe(String locPe) 
	{
		_locPe = locPe;
	}
	
	public double getFloodStg() 
	{
		return _floodStg;
	}

	public void setFloodStg(double floodStg) 
	{
		_floodStg = floodStg;
	}
	
	public double getFloodFlow() 
	{
		return _floodFlow;
	}

	public void setFloodFlow(double floodFlow) 
	{
		_floodFlow = floodFlow;
	}
	
	public long getObsBeginTime() 
	{
		return _obsBeginTime;
	}

	public void setObsBeginTime(long obsBeginTime) 
	{
		_obsBeginTime = obsBeginTime;
	}
	
	public long getFcstEndTime() 
	{
		return _fcstEndTime;
	}

	public void setFcstEndTime(long fcstEndTime) 
	{
		_fcstEndTime = fcstEndTime;
	}
	
	public double getAdjustEndhrs() 
	{
		return _adjustendhrs;
	}

	public void setAdjustEndhrs(double adjustendhrs) 
	{
		_adjustendhrs = adjustendhrs;
	}
	
	public String getcurAction() 
	{
		return _curAction;
	}

	public void setcurAction(String curAction) 
	{
		_curAction = curAction;
	}

        public String getcurPhenom() 
	{
		return _curPhenom;
	}

	public void setcurPhenom(String curPhenom) 
	{
		_curPhenom = curPhenom;
	}

    public String getcurSignif() 
	{
		return _curSignif;
	}

	public void setcurSignif(String curSignif) 
	{
		_curSignif = curSignif;
	}
	
	public String getcurSeverity() 
	{
		return _curSeverity;
	}

	public void setcurSeverity(String curSeverity) 
	{
		_curSeverity = curSeverity;
	}
	
	public String getcurCause() 
	{
		return _curCause;
	}

	public void setcurCause(String curCause) 
	{
		_curCause = curCause;
	}
	
	public String getcurRecord() 
	{
		return _curRecord;
	}

	public void setcurRecord(String curRecord) 
	{
		_curRecord = curRecord;
	}
	
	public int getcurEtn() 
	{
		return _curEtn;
	}

	public void setcurEtn(int curEtn) 
	{
		_curEtn = curEtn;
	}
	
	public double getcurCrestValue()
	{
		return _curCrestValue;
	}
	
	public void setcurCrestValue(double curCrestValue)
	{
		_curCrestValue = curCrestValue;		
	}
	
	public long getcurBeginTime() 
	{
		return _curBeginTime;
	}

	public void setcurBeginTime(long curBeginTime) 
	{
		_curBeginTime = curBeginTime;
	}

        public long getcurEndTime() 
	{
		return _curEndTime;
	}

	public void setcurEndTime(long curEndTime) 
	{
		_curEndTime = curEndTime;
	}
        
	public long getcurRiseTime() 
	{
		return _curRiseTime;
	}

	public void setcurRiseTime(long curRiseTime) 
	{
		_curRiseTime = curRiseTime;
	}
	
	public long getcurCrestTime() 
	{
		return _curCrestTime;
	}

	public void setcurCrestTime(long curCrestTime) 
	{
		_curCrestTime = curCrestTime;
	}
	
	public long getcurFallTime() 
	{
		return _curFallTime;
	}

	public void setcurFallTime(long curFallTime) 
	{
		_curFallTime = curFallTime;
	}
	
	
	public String getprevAction() 
	{
		return _prevAction;
	}

	public void setprevAction(String prevAction) 
	{
		_prevAction = prevAction;
	}

    public String getprevPhenom() 
	{
		return _prevPhenom;
	}

	public void setprevPhenom(String prevPhenom) 
	{
		_prevPhenom = prevPhenom;
	}

        public String getprevSignif() 
	{
		return _prevSignif;
	}

	public void setprevSignif(String prevSignif) 
	{
		_prevSignif = prevSignif;
	}
	
	public String getprevSeverity() 
	{
		return _prevSeverity;
	}

	public void setprevSeverity(String prevSeverity) 
	{
		_prevSeverity = prevSeverity;
	}
	
	public String getprevCause() 
	{
		return _prevCause;
	}

	public void setprevCause(String prevCause) 
	{
		_prevCause = prevCause;
	}
	
	public String getprevRecord() 
	{
		return _prevRecord;
	}

	public void setprevRecord(String prevRecord) 
	{
		_prevRecord = prevRecord;
	}
	
	public int getprevEtn() 
	{
		return _prevEtn;
	}

	public void setprevEtn(int prevEtn) 
	{
		_prevEtn = prevEtn;
	}
	
	public double getprevCrestValue()
	{
		return _prevCrestValue;
	}
	
	public void setprevCrestValue(double prevCrestValue)
	{
		_prevCrestValue = prevCrestValue;		
	}
	
	public String getprevRiseType() 
	{
		return _prevRiseType;
	}

	public void setprevRiseType(String prevRiseType) 
	{
		_prevRiseType = prevRiseType;
	}
	
	public String getprevCrestType() 
	{
		return _prevCrestType;
	}

	public void setprevCrestType(String prevCrestType) 
	{
		_prevCrestType = prevCrestType;
	}
	
	public String getprevFallType() 
	{
		return _prevFallType;
	}

	public void setprevFallType(String prevFallType) 
	{
		_prevFallType = prevFallType;
	}
	
	public long getprevBeginTime() 
	{
		return _prevBeginTime;
	}

	public void setprevBeginTime(long prevBeginTime) 
	{
		_prevBeginTime = prevBeginTime;
	}

    public long getprevEndTime() 
	{
		return _prevEndTime;
	}

	public void setprevEndTime(long prevEndTime) 
	{
		_prevEndTime = prevEndTime;
	}
        
	public long getprevRiseTime() 
	{
		return _prevRiseTime;
	}

	public void setprevRiseTime(long prevRiseTime) 
	{
		_prevRiseTime = prevRiseTime;
	}
	
	public long getprevCrestTime() 
	{
		return _prevCrestTime;
	}

	public void setprevCrestTime(long prevCrestTime) 
	{
		_prevCrestTime = prevCrestTime;
	}
	
	public long getprevFallTime() 
	{
		return _prevFallTime;
	}

	public void setprevFallTime(long prevFallTime) 
	{
		_prevFallTime = prevFallTime;
	}
	
	public long getprevProductTime() 
	{
		return _prevProductTime;
	}

	public void setprevProductTime(long prevProductTime) 
	{
		_prevProductTime = prevProductTime;
	}
	
	public String toString()
	{
	  String str = null;
	  return str;
	}

	public ArrayList getObsValuetTimeList() {
		return _obsValuetTimeList;
	}

	public void setObsValuetTimeList(ArrayList obsValuetTimeList) {
		_obsValuetTimeList = obsValuetTimeList;
	}

	public ArrayList getFcstValueTimeList() {
		return _fcstValueTimeList;
	}

	public void setFcstValueTimeList(ArrayList fcstValueTimeList) {
		_fcstValueTimeList = fcstValueTimeList;
	}
	
	
}
