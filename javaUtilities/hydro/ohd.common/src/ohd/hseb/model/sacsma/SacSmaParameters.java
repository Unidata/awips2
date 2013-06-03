/*
 * Created on Jun 20, 2003
 *
 * 
 */
package ohd.hseb.model.sacsma;
import java.text.*;

/**
 * @author gobsc
 *
 *
 */
public class SacSmaParameters
{

       private String _basinId;
       private String _source;
       private long _validTime;
       private long _postingTime;


	// these were common block parameter variables in the FORTRAN code
	   private double _uztwm = 0.0;
	   private double _uzfwm = 0.0;
	   private double _uzk = 0.0;
	   private double _pctim = 0.0;
	   private double _adimp = 0.0;
	   private double _riva = 0.0;
	   private double _zperc = 0.0;
	   private double _rexp = 0.0;
	
	   private double _lztwm = 0.0;
	   private double _lzfsm = 0.0;
	   private double _lzfpm = 0.0;
	   private double _lzsk = 0.0;
	   private double _lzpk = 0.0;
		
	   private double _pfree = 0.0;
	  
	  //these 2 are calculated
	  //SAVED = RSERV * (LZFPM + LZFSM)
	 // private double _saved = 0.0; //aka rserv?
	  //PAREA = 1.0 - PCTIM - ADIMP
	 // private double _parea = 0.0;

	  private double _rserv = 0.0;
	  
	  private double _side = 0.0;
	
      //other sac params - not soil related
      
      private double _peadj = 0.0;
      private double _pxadj = 0.0;
      private double _efc = 0.0;
    

  
    

   public SacSmaParameters()
   {
   }
   
   public SacSmaParameters(SacSmaParameters params)
   {
       //copy constructor
         
       setBasinId(params.getBasinId());
       setSource(params.getSource());
       setValidTime(params.getValidTime());
       setPostingTime(params.getPostingTime());
       
     
       setUztwm(params.getUztwm());
       setUzfwm(params.getUzfwm());
       setUzk(params.getUzk());
       setPctim(params.getPctim());
       setAdimp(params.getAdimp());
       setRiva(params.getRiva());
       setZperc(params.getZperc());
       setRexp(params.getRexp());
       
       setLztwm(params.getLztwm());
       setLzfsm(params.getLzfsm());
       setLzfpm(params.getLzfpm());  
       setLzsk(params.getLzsk());
       setLzpk(params.getLzpk());
   
   
       setPfree(params.getPfree());   
       setRserv(params.getRserv());
       setSide(params.getSide());
       
       return;
   }

  /**
   * 
   * @param paramArray
   * The parameters are doubles in this order:
   * UZTWM, UZFWM, UZK, PCTIM, ADIMP, RIVA, ZPERC, REXP,LZTWM, LZFSM, LZFPM, LZSK, LZPK, PFREE, RSERV, SIDE
   */
   public SacSmaParameters(double[] paramArray)
   {
       int i = 0;
       setUztwm(paramArray[i++]);
	   setUzfwm(paramArray[i++]);
	   setUzk(paramArray[i++]);
	   setPctim(paramArray[i++]);
	   setAdimp(paramArray[i++]);
	   setRiva(paramArray[i++]);
	   setZperc(paramArray[i++]);
	   setRexp(paramArray[i++]);
	   setLztwm(paramArray[i++]);
	   setLzfsm(paramArray[i++]);

 	   setLzfpm(paramArray[i++]);
	   setLzsk(paramArray[i++]);
	   setLzpk(paramArray[i++]);
	   setPfree(paramArray[i++]);
	
	   setRserv(paramArray[i++]);
	   setSide(paramArray[i++]);

   }
   
   public boolean equals(SacSmaParameters params)
   {
         boolean result = false;  
         
         if (
            (getBasinId().equalsIgnoreCase(params.getBasinId())) &&
            (getSource().equalsIgnoreCase(params.getSource()))  &&
            (getValidTime() == params.getValidTime()) &&     
            (getPostingTime() == params.getPostingTime()) &&
            (getUztwm() == params.getUztwm()) &&
            (getUzfwm() == params.getUzfwm()) &&
            (getUzk() == params.getUzk()) &&
            (getPctim() == params.getPctim()) &&
            (getAdimp() == params.getAdimp()) &&
            (getRiva() == params.getRiva()) &&
            (getZperc() == params.getZperc()) &&
            (getRexp() == params.getRexp()) &&
            (getLztwm() == params.getLztwm()) &&
            (getLzfsm() == params.getLzfsm()) &&
            (getLzfpm() == params.getLzfpm()) &&
            (getLzsk() == params.getLzsk()) &&
            (getLzpk() == params.getLzpk()) &&
            (getPfree() == params.getPfree()) &&
            (getRserv() == params.getRserv()) &&
            (getSide() == params.getSide()) &&
            (getPeadj() == params.getPeadj()) &&
            (getPxadj() == params.getPxadj()) &&
            (getEfc() == params.getEfc())
            )
             
            {
                result = true;
            }
       
         return result;    
    }
    
    
    public boolean equalsValues(SacSmaParameters params)
    {
            boolean result = false;  
         
            if (
              // (getBasinId().equalsIgnoreCase(params.getBasinId())) &&
              //(getSource().equalsIgnoreCase(params.getSource()))  &&
              // (getValidTime() == params.getValidTime()) &&     
             //  (getPostingTime() == params.getPostingTime()) &&
               (getUztwm() == params.getUztwm()) &&
               (getUzfwm() == params.getUzfwm()) &&
               (getUzk() == params.getUzk()) &&
               (getPctim() == params.getPctim()) &&
               (getAdimp() == params.getAdimp()) &&
               (getRiva() == params.getRiva()) &&
               (getZperc() == params.getZperc()) &&
               (getRexp() == params.getRexp()) &&
               (getLztwm() == params.getLztwm()) &&
               (getLzfsm() == params.getLzfsm()) &&
               (getLzfpm() == params.getLzfpm()) &&
               (getLzsk() == params.getLzsk()) &&
               (getLzpk() == params.getLzpk()) &&
               (getPfree() == params.getPfree()) &&
               (getRserv() == params.getRserv()) &&
               (getSide() == params.getSide()) &&
               (getPeadj() == params.getPeadj()) &&
               (getPxadj() == params.getPxadj()) &&
               (getEfc() == params.getEfc())
               )
             
               {
                   result = true;
               }
       
            return result;    
    }


    public String toString()
    {
	    String formatString = "#####.#####";
	    NumberFormat f = new DecimalFormat(formatString);
    
   	    String outString = "Params = " +
   	               " uztwm = " + f.format(_uztwm) +
	               " uzfwm = " + f.format(_uzfwm) +
	               " uzk = " + f.format(_uzk) +
	               " pctim = " + f.format(_pctim) + "\n" +
                   " adimp = " + f.format(_adimp) +
	               " riva = " + f.format(_riva) +
	               " zperc = " + f.format(_zperc) +
	
               	   " rexp = " + f.format(_rexp) + "\n" +
          	       " lztwm = " + f.format(_lztwm) +
	               " lzfsm = " + f.format(_lzfsm) +
	
	               " lzfpm = " + f.format(_lzfpm) +
	
	               " lzsk = " + f.format(_lzsk) + "\n" +
	               " lzpk = " + f.format(_lzpk) +
	               " pfree = " + f.format(_pfree) +
	               " rserv = " + f.format(_rserv) + 
	               " side = " + f.format(_side) + "\n" +
	               " parea = " + f.format(getParea())  + "\n";
                   
    	return outString;
   	
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
    
    public void setPostingTime(long postingTime)
    {
        _postingTime = postingTime;
    }
    
    public long getPostingTime()
    {
        return _postingTime;
    }

// accessors

	public void setUztwm(double uztwm) {
		this._uztwm = uztwm;
	}

	public double getUztwm() {
		return _uztwm;
	}

	public void setUzfwm(double uzfwm) {
		this._uzfwm = uzfwm;
	}

	public double getUzfwm() {
		return _uzfwm;
	}

	public void setUzk(double uzk) {
		this._uzk = uzk;
	}

	public double getUzk() {
		return _uzk;
	}

	public void setPctim(double pctim) {
		this._pctim = pctim;
	}

	public double getPctim() {
		return _pctim;
	}

	public void setAdimp(double adimp) {
		this._adimp = adimp;
	}

	public double getAdimp() {
		return _adimp;
	}

	public void setRiva(double riva) {
		this._riva = riva;
	}

	public double getRiva() {
		return _riva;
	}

	public void setZperc(double zperc) {
		this._zperc = zperc;
	}

	public double getZperc() {
		return _zperc;
	}

	public void setRexp(double rexp) {
		this._rexp = rexp;
	}

	public double getRexp() {
		return _rexp;
	}

	public void setLztwm(double lztwm) {
		this._lztwm = lztwm;
	}

	public double getLztwm() {
		return _lztwm;
	}

	public void setLzfsm(double lzfsm) {
		this._lzfsm = lzfsm;
	}

	public double getLzfsm() {
		return _lzfsm;
	}

	public void setLzfpm(double lzfpm) {
		this._lzfpm = lzfpm;
	}

	public double getLzfpm() {
		return _lzfpm;
	}

	public void setLzsk(double lzsk) {
		this._lzsk = lzsk;
	}

	public double getLzsk() {
		return _lzsk;
	}

	public void setLzpk(double lzpk) {
		this._lzpk = lzpk;
	}

	public double getLzpk() {
		return _lzpk;
	}

	public void setPfree(double pfree) {
		this._pfree = pfree;
	}

	public double getPfree() {
		return _pfree;
	}

	public void setSide(double side) {
		this._side = side;
	}

	public double getSide() {
		return _side;
	}


	public double getSaved()
	{   //calculated parameter
		//
		return (_rserv * (_lzfpm + _lzfsm));
		//return _saved;
	}

	//public void setParea(double parea) {
	//	this._parea = parea;
	//}

	public double getParea()
	{   //calculated parameter
		return (1.0 - _pctim - _adimp);
		//return _parea;
	}

	public void setRserv(double rserv)
	{
		this._rserv = rserv;
	}

	public double getRserv()
	{
		return _rserv;
	}
    
    
    public double getPeadj()
    {
        return _peadj;
    }

   
    public void setPeadj(double peadj)
    {
        _peadj = peadj;
    }

  
    public double getPxadj()
    {
        return _pxadj;
    }

    public void setPxadj(double pxadj)
    {
        _pxadj = pxadj;
    }

    public void setEfc(double efc)
    {
        _efc = efc;
    }

    public double getEfc()
    {
        return _efc;
    }
	



} //end SacSmaParameters
