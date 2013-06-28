
/**
 * Write a description of class SacSmaState here.
 * 
 * @author Chip Gobs
 * @version 6/20/03
 */
package ohd.hseb.model.sacsma;
import java.text.*;

import ohd.hseb.db.DbTimeHelper;

public class SacSmaState
{
    private String _basinId = null;
    private String _source = null;
    private long _validTime = 0;
    private long _basisTime = 0;
    private long _postingTime = 0;
    
    
	private double _uztwc = 0.0;
	private double _uzfwc = 0.0;
	private double _lztwc = 0.0;
	private double _lzfsc = 0.0;
	private double _lzfpc = 0.0;
	private double _adimc = 0.0;

	//private double[] _fgco = new double[6];
	//private double[] _rsum = new double[7];

	//private double _ppe = 0.0;
	//private double _psc = 0.0;
	//private double _pta = 0.0;
	//private double _pwe = 0.0;
	
	/**
	 * Constructor for objects of class SacSmaState
	 */
	public SacSmaState()
	{
		
	}
    
    public SacSmaState(SacSmaState state)
    {
           
        setBasinId(state.getBasinId());
        setSource(state.getSource());
        setValidTime(state.getValidTime());
        setBasisTime(state.getBasisTime());
        setPostingTime(state.getPostingTime());
        
       
        setUztwc(state.getUztwc());
        setUzfwc(state.getUzfwc());
        setLztwc(state.getLztwc());
        setLzfsc(state.getLzfsc());
        setLzfpc(state.getLzfpc());
        setAdimc(state.getAdimc());
        
        return;
        
    }
	
	public SacSmaState(double[] stateArray)
	{
		int i = 0;
		
	    setUztwc(stateArray[i++]);
		setUzfwc(stateArray[i++]);
		setLztwc(stateArray[i++]);
		setLzfsc(stateArray[i++]);
		setLzfpc(stateArray[i++]);
		setAdimc(stateArray[i++]);
		
		//consider setting
		//fpcoArray
		//rsumArray
		
		//setPpe(stateArray[i++]);
		//setPsc(stateArray[i++]);
		//setPta(stateArray[i++]);
		//setPwe(stateArray[i++]);
		
	    	
	
	}

    public String toString()
    {
    	String formatString = "#####.#####";
    	NumberFormat format = new DecimalFormat(formatString);
    	
        
        
        
        String string = "Basin Id = " + _basinId +
                        " data source = " + _source + 
                        " valid time = " + DbTimeHelper.getDateTimeStringFromLongTime(_validTime) +
                        " basis time = " + DbTimeHelper.getDateTimeStringFromLongTime(_basisTime) +
                        " posting time = " + DbTimeHelper.getDateTimeStringFromLongTime(_postingTime) + 
                        "\n" +
                        " uztwc = " + format.format(_uztwc)  + 
						" uzfwc = " + format.format(_uzfwc)  +
		                " lztwc = " + format.format(_lztwc)  +
                		" lzfsc = " + format.format(_lzfsc)  +
                		" lzfpc = " + format.format(_lzfpc)  +
                		" adimc = " + format.format(_adimc);
		//" rsum =  " + getRsumString()  +  "\n";
		
		//"ppe = " + _ppe  + "\n" +
		//"pse = " + _psc  +
		//"pta = " + _pta  +
		//"pwe = " + _pwe  + "\n";

        return string;
    }

    public void setBasinId(String basinId)
    {
        _basinId = basinId;
    }

    public String getBasinId()
    {
        return _basinId;
    }

    public void setSource(String source)
    {
        _source = source;
    }

    public String getSource()
    {
        return _source;
    }

    public void setValidTime(long validTime)
    {
        _validTime = validTime;
    }

    public long getValidTime()
    {
        return _validTime;
    }

    public void setBasisTime(long basisTime)
    {
        _basisTime = basisTime;
    }

    public long getBasisTime()
    {
        return _basisTime;
    }

    public void setPostingTime(long postingTime)
    {
        _postingTime = postingTime;
    }

    public long getPostingTime()
    {
        return _postingTime;
    }
    
    public boolean equals(SacSmaState state)
    {
        boolean result = false;
       
        
         
        if (
          (getBasinId().equalsIgnoreCase(state.getBasinId())) &&
          (getSource().equalsIgnoreCase(state.getSource()))  &&
          (getValidTime() == state.getValidTime()) &&
          (getBasisTime() == state.getBasisTime()) &&
          (getPostingTime() == state.getPostingTime()) &&
          (getUztwc() == state.getUztwc()) &&
          (getUzfwc() == state.getUzfwc()) &&
          (getLztwc() == state.getLztwc()) &&
          (getLzfsc() == state.getLzfsc()) &&
          (getLzfpc() == state.getLzfpc()) &&
          (getAdimc() == state.getAdimc()))
         {
            result = true;
         }
       
       return result;    
    }
    
    
    
    public boolean equalsValues(SacSmaState state)
    {
        boolean result = false;
   
    
     
        if (
          //(getBasinId().equalsIgnoreCase(state.getBasinId())) &&
          //(getSource().equalsIgnoreCase(state.getSource()))  &&
         // (getValidTime() == state.getValidTime()) &&
         // (getBasisTime() == state.getBasisTime()) &&
         //  (getPostingTime() == state.getPostingTime()) &&
          (getUztwc() == state.getUztwc()) &&
          (getUzfwc() == state.getUzfwc()) &&
          (getLztwc() == state.getLztwc()) &&
          (getLzfsc() == state.getLzfsc()) &&
          (getLzfpc() == state.getLzfpc()) &&
          (getAdimc() == state.getAdimc()))
         {
            result = true;
         }
   
       return result;    
    }
    
    /*
    public String getRsumString()
    {
		String formatString = "#####.###";
		NumberFormat format = new DecimalFormat(formatString);
    	
        StringBuffer rsumStringBuffer = new StringBuffer();
        
        for (int i = 0 ; i < _rsum.length; i++)
        {
            rsumStringBuffer.append(format.format(_rsum[i]) + " ");
        }
        
        return rsumStringBuffer.toString();
    }
    */
    

	public void setUztwc(double uztwc) {
		this._uztwc = uztwc;
	}

	public double getUztwc()
	{
		return _uztwc;
	}

	public void setUzfwc(double uzfwc) {
		this._uzfwc = uzfwc;
	}

	public double getUzfwc() {
		return _uzfwc;
	}

	public  void setLztwc(double lztwc) {
		this._lztwc = lztwc;
	}

	public  double getLztwc() {
		return _lztwc;
	}

	public void setLzfsc(double lzfsc) {
		this._lzfsc = lzfsc;
	}

	public double getLzfsc() {
		return _lzfsc;
	}

	public void setLzfpc(double lzfpc) {
		this._lzfpc = lzfpc;
	}

	public double getLzfpc() {
		return _lzfpc;
	}

	public void setAdimc(double adimc) {
		this._adimc = adimc;
	}

	public double getAdimc() {
		return _adimc;
	}

/*
	public void setRsum(double[] rsum)
	{
		this._rsum = rsum;
	}
*/
/*
	public double[] getRsum()
	{
		return _rsum;
	}
*/	
	/*
	public void setPpe(double ppe)
	{
		this._ppe = ppe;
	}

    
	public double getPpe()
	{
			return _ppe;
	}
	
	
	public void setPwe(double pwe)
	{
		this._pwe = pwe;
	}

	public double getPwe()
	{
		return _pwe;
	}

	public void setPsc(double psc)
	{
		this._psc = psc;
	}


	public double getPsc()
	{
		return _psc;
	}

	public void setPta(double _pta) {
		this._pta = _pta;
	}

	public double getPta() {
		return _pta;
	}
*/
	

	
}
