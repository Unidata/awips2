package ohd.hseb.model.sacsma;


import java.text.*;
//import java.io.*;

import ohd.hseb.measurement.*;
import ohd.hseb.model.*;


/**
 * 
 * 
 * @author Chip Gobs
 * @version 0.5
 */
public class SacSmaRainfallRunoffModel implements RainfallRunoffModel
{
    private static final int _millisPerHour = 60 * 60 * 1000; 
    private SacSmaParameters _params;
    private SacSmaState _state;
    
    private boolean _useStaticMape = false;
    private MonthlyValues _monthlyMapeAdjustmentValues = null;
    
    private RegularTimeSeriesHolder _potentialEvapTimeSeriesHolder = null;
   
    private MonthlyValues _monthlyMapeValues = null;
  
    private double _peAdjust = 1.0;
    private double _pxAdjust = 1.0;
  
   
   // calculated, during the initial setting of params from SacSmaParameters to
   // this object's variables
   //private double _saved = 0.0;
  // private double _parea = 0.0;
	

	//private double[] _fgco = new double[6]; //unused
	private double[] _rsum = new double[7];

	// private double _ppe = 0.0;
	//private double _psc = 0.0;
//	private double _pta = 0.0;
//	private double _pwe = 0.0;
	
	// common block sums
	private double _srot = 0.0;
	private double _simpvt = 0.0;
	private double _srodt = 0.0;
	private double _srost = 0.0;
	private double _sintft = 0.0;
	private double _sgwfp = 0.0;
	private double _sgwfs = 0.0;
	private double _srecht = 0.0;
	private double _sett = 0.0;
	
	private double _se1 = 0.0;
//	private double _se2 = 0.0;
	private double _se3 = 0.0;
	private double _se4 = 0.0;
	private double _se5 = 0.0;
	
	
	
    //variables that are initialized in the main section of code
    //that I need to pass to the doIncrements section
    
    private double _dlzp = 0.0;
    private double _dlzs = 0.0;
	private double _duz = 0.0;
	
	private double _sbf = 0.0;
	private double _ssur = 0.0;
	private double _sif = 0.0;
	private double _sperc = 0.0;
	private double _sdro = 0.0;
	private double _spbf = 0.0;
	
	private double _pinc = 0.0;
	
	
	//variables that could have been local, but belong to the
	// object so that results of the operation can be verified
	private double _bfp = 0.0;
	private double _bfs = 0.0;
	private double _roimp = 0.0;
	private double _tci = 0.0;
    
    //private PrintWriter _runoffWriter = null;
    //private PrintWriter _internalsWriter = null;
    
    private MeasuringUnit _precipUnit = MeasuringUnit.mm;
    private MeasuringUnit _runoffUnit = MeasuringUnit.mm;
      

//--------------------------------------------------------------------------	
    /**
     * Constructor for objects of class SacSmaModel
     */
    public SacSmaRainfallRunoffModel(SacSmaState state,
                       SacSmaParameters params,
                       double peAdjust,
                       double pxAdjust,
                       boolean useStaticMape,
                       MonthlyValues monthlyValues,
                       RegularTimeSeriesHolder potentialEvapTimeSeriesHolder)
    {
        
     //   _basinId = params.getBasinId();
     //   _paramsSource = params.getSource();
     //   _stateSource = state.getSource();
        
        // says whether using MAPE time series or static monthly MAPE values
        _useStaticMape = useStaticMape;
        
        if (_useStaticMape)
        {
            _monthlyMapeValues = monthlyValues;        
        }
        else
        {
            _monthlyMapeAdjustmentValues = monthlyValues;    
        }
        
        // can be null
        _potentialEvapTimeSeriesHolder = potentialEvapTimeSeriesHolder;
        
    	setParameters(params);
    	
    	setState(state);
        
        _peAdjust = peAdjust;
        _pxAdjust = pxAdjust;
     
    }
//  --------------------------------------------------------------------------  
  
    private static String getStateString(SacSmaState state)
    {
            String formatString = "#####.#####";
            NumberFormat format = new DecimalFormat(formatString);
        
            String string = " uztwc = " + format.format(state.getUztwc())  + 
                           " uzfwc = " + format.format(state.getUzfwc())  +
            " lztwc = " + format.format(state.getLztwc())  + "\n" +
            " lzfsc = " + format.format(state.getLzfsc())  + 
            " lzfpc = " + format.format(state.getLzfpc())  +
            " adimc = " + format.format(state.getAdimc())  + "\n";
            
            
            //" rsum =  " + state.getRsumString()  +  "\n";


            return string;
        } //end getStateString
       
 
//  --------------------------------------------------------------------------  
    
    public RegularTimeSeries calculateRunoff( long startTime,
                                              long endTime,      
                                              RegularTimeSeries precipTimeSeries)
                                              
                                           
    {
        
        String header = "SacSmaModel.calculateRunoff(): ";
        
         
        final double hoursPerDay = 24;
        int hoursPerInterval = 1;
        
        // the time series start time is one hour after the startTime,
        // which represents the last saved model state time
        long tsStartTime = startTime + (hoursPerInterval * _millisPerHour);
        
     
        double intervalPortionOfDay = hoursPerInterval/hoursPerDay;
        
        RegularTimeSeries runoffTimeSeries = 
                new RegularTimeSeries(tsStartTime, endTime, 1, _runoffUnit);
   
         
        double evaporationAmount = 0.0;
        double precipAmount = 0.0;

 
        String formatString = "#####.#####";
        NumberFormat formatter = new DecimalFormat(formatString);

           // System.out.println("Java version of SacSma:");


        //setupTestLogFiles();
        
        String runoffMessage = null;
        String internalsMessage = null;

        internalsMessage = "at startup: " + 
                         getStateString(getState()) +
                         getParameters().toString();
                     
        //System.out.println(internalsMessage);
       
       // _internalsWriter.println(internalsMessage);

    
        long millisPerHour = 60 * 60 * 1000;

        RegularTimeSeries potentialEvapTimeSeries = _potentialEvapTimeSeriesHolder.getTimeSeries();
  
 
        int hoursCount = Math.min(precipTimeSeries.getMeasurementCount(),
                                       potentialEvapTimeSeries.getMeasurementCount());

        
        //for each piece of input data
         
        //fill in the time series out to the length of the model run
        precipTimeSeries.stretchTimeSeries(tsStartTime, endTime, 0.0);
        
        
        if (! _useStaticMape) //using time series
        {
            potentialEvapTimeSeries.stretchTimeSeries(tsStartTime, endTime, 0.0);
        }
        
       
        // at the start of the calculation, reset the basis time for the state
        _state.setBasisTime(System.currentTimeMillis());
        
     
        for (long time = tsStartTime;  time <= endTime; time += millisPerHour)
        {           
            Measurement precipMeasurement = precipTimeSeries.getMeasurementByTime(time);    
        
            if ((precipMeasurement != null) && (! precipMeasurement.isMissing()))
            {
                precipMeasurement = precipMeasurement.getCopy(_precipUnit);
        
                precipAmount = precipMeasurement.getValue();
                
                if (precipAmount < 0.0)
                {
                    precipAmount = 0.0;    
                }
                else
                {
                    precipAmount *= _pxAdjust;    
                }
            }
            else
            {
                precipAmount = 0.0;
            }
            
            
            if (_useStaticMape)
            {
                double dailyEvaporationAmount = _monthlyMapeValues.getValue(time);
                evaporationAmount = dailyEvaporationAmount * intervalPortionOfDay;
                
                evaporationAmount *= _peAdjust;
                
            }
            else  //using time series
            {
                Measurement evapMeasurement = potentialEvapTimeSeries.getMeasurementByTime(time);
                                  
                if (evapMeasurement != null)
                {
                    evapMeasurement = evapMeasurement.getCopy(_precipUnit);
                    evaporationAmount = evapMeasurement.getValue();
            
                    double adjustmentFactor = _monthlyMapeAdjustmentValues.getValue(time);                    
                    evaporationAmount *= adjustmentFactor;
                    
                    evaporationAmount *= _peAdjust;              
                }
                else //evapMeasurement == null
                {
                    evaporationAmount = 0.0;   
                }
            }
            
           
            double totalChannelInflow = 
                                  calculate(intervalPortionOfDay,
                                            precipAmount,
                                            evaporationAmount);
            
            // reset the valid time                                
            _state.setValidTime(time);
                                       
                                                
                                            
            /*
            System.out.println(header + "runoff = " + totalChannelInflow + 
                                        " precip = " + precipAmount + 
                                        " poten. evap = " + evaporationAmount);                                    
             */                               
                                            
            Measurement totalChannelInflowMeasurement =
                        new Measurement(totalChannelInflow, _runoffUnit);                                
                                            
            runoffTimeSeries.setMeasurementByTime(totalChannelInflowMeasurement, time);
        
        }
        return runoffTimeSeries;
    }
//  --------------------------------------------------------------------------  
    
    private void setParameters(SacSmaParameters params)
    {
        
        _params = new SacSmaParameters(params);
        
    	//_uztwm = params.getUztwm();
    	//_uzfwm = params.getUzfwm();
    	//_uzk = params.getUzk();
    	
    	//_pctim = params.getPctim();
    	//_adimp = params.getAdimp();
    	//_riva  = params.getRiva();
    	//_zperc = params.getZperc();
    	//_rexp = params.getRexp();
        //_lztwm = params.getLztwm();
    	//_lzfsm = params.getLzfsm();
    	//_lzfpm = params.getLzfpm();
    	//_lzsk = params.getLzsk();
    	//_lzpk = params.getLzpk();
    	
    	//_pfree = params.getPfree();	
    	//_rserv = params.getRserv();
		//_side = params.getSide();
    
    	
    } //end setParameters
 
//  --------------------------------------------------------------------------  

    
	public SacSmaParameters getParameters()
	{
	    SacSmaParameters params = new SacSmaParameters();
	   	
        
        /*
        
        params.setBasinId(_basinId);
        params.setSource(_stateSource);

        
		params.setUztwm( _uztwm );
	    params.setUzfwm( _uzfwm );
	    params.setUzk( _uzk );
    	
	    params.setPctim(_pctim );
	    params.setAdimp(  _adimp );
	    params.setRiva(_riva );
	    params.setZperc(_zperc );
	    params.setRexp(_rexp );
	    params.setLztwm(_lztwm );
	    params.setLzfsm(_lzfsm );
	    params.setLzfpm(_lzfpm );
	    params.setLzsk(_lzsk );
	    params.setLzpk(_lzpk );
    	
	    params.setPfree( _pfree );
	    params.setSide(_side );
	    
	    params.setRserv(_rserv);
	    */
        
        params = new SacSmaParameters(_params);
	 	 
        return params;

	} //end getParameters

//  --------------------------------------------------------------------------  
    
    public void setState(SacSmaState state)
    {
        
        _state = new SacSmaState(state);
        
		//_uztwc = state.getUztwc();
	    //_uzfwc = state.getUzfwc();
	    
		//_lztwc = state.getLztwc();
		
		//_lzfsc = state.getLzfsc();
	    //_lzfpc = state.getLzfpc();
	    
		//_adimc = state.getAdimc();

		//private double[] _fgco = new double[6];
	//	_rsum = new double[ state.getRsum().length ];
	//	for (int i = 0; i < state.getRsum().length; i++)
	//	{
	//	    _rsum[i] = state.getRsum()[i];	
	//	}
		

	//	_ppe = state.getPpe();
//		_psc = state.getPsc();
//		_pta = state.getPta();
//		_pwe = state.getPwe();
		
    }
    
//--------------------------------------------
	public SacSmaState getState()
	{
  	    SacSmaState state = new SacSmaState(_state);


		//private double[] _fgco = new double[6];
		//double[] localRsum = new double[_rsum.length];
		//for (int i = 0; i < _rsum.length; i++)
		//{
		//	localRsum[i] = _rsum[i];	
		//}
	    //state.setRsum(localRsum);


		//	_ppe = state.getPpe();
		//state.setPsc(_psc );
		//state.setPta(_pta );
		//state.setPwe(_pwe );
	
        return state;	
	}
	
//  --------------------------------------------------------------------------  
	
	public double getSaved()
	{   //calculated parameter
		//
		return (_params.getRserv() * (_params.getLzfpm() + _params.getLzfsm()));
		//return _saved;
	}

	//public void setParea(double parea) {
	//	this._parea = parea;
	//}

//  --------------------------------------------------------------------------  


	public double getParea()
	{   //calculated parameter
		return (1.0 - _params.getPctim() - _params.getAdimp());
		//return _parea;
	}
//--------------------------------------------
    /**
     * calculate()
     * 
     * @param  timeInterval   a sample parameter for a method
     * @return     void
     */
    
    /*
     *  Translation Notes:
     *  dt = timeInterval in fraction of a day (for example, 1 hour = 1/24.0)
     *  pxv = ranfallPlusMelt
     *  ep = evaporation
     *  tci = totalChannelInflowMinusRunoffDepth
     */
    
    public double calculate(double dt /*dt */,
                          double rainfallPlusMelt /*pxv */,
                          double evaporation /* ep*/)
    {
        double evapTransDemand = evaporation;
        
        double e1;
        double e2;
        double e3;
        
        double residualEvapTransDemand = 0.0;
        
        //later put this in the object itself
        boolean useFrozenGroundCalcs = false; 
        
        
        // old code does this:
        //evapTransDemand = evaporation * evaporationDistribution(Kint)
        
        // compute et from upper zone
        
        e1 = evapTransDemand * (_state.getUztwc() /  _params.getUztwm() );
        
        residualEvapTransDemand = evapTransDemand - e1;
        
        //subtract off e1
        _state.setUztwc(_state.getUztwc() - e1);
        
        // introduce e2
        e2 = 0.0;
        
        boolean perform220Check = false;
        
        if (_state.getUztwc() < 0.0)
        {
            e1 += _state.getUztwc();
            _state.setUztwc(0.0);
            residualEvapTransDemand = evapTransDemand - e1;
            
            if (_state.getUzfwc() < residualEvapTransDemand) 
            {
                //e2 is evap from UZFWC
                e2 = _state.getUzfwc();
                //trace("setting _uzfwc to 0.0");
                _state.setUzfwc(0.0);
                
                 residualEvapTransDemand -= e2;
                 perform220Check = false;  //simulates goto 225
            }
            else //_state.getUzfwc() >= residualEvapTransDemand //simulates 221
            {
            	 //trace("221");
                 e2 =  residualEvapTransDemand;
                 
                 _state.setUzfwc(_state.getUzfwc() - e2);
               
                 //trace("near 221, _uzfwc = " + _uzfwc);
                 residualEvapTransDemand = 0.0;
                 
                 perform220Check = true;
            }
            
        }
        else //(state.getUztwc() >= 0.0) //simulates goto 220
        {
            perform220Check = true;
        }
        
        
        if (perform220Check)
        {
        	//trace("220");
            if (( _state.getUztwc() /  _params.getUztwm() ) < (_state.getUzfwc() / _params.getUzfwm()))
            {
                // upper zone free water ratio exceeds upper zone tension
                // water ratio, this transfer free water to tension
                
                double uzrat = ( (_state.getUztwc() +  _state.getUzfwc()) / ( _params.getUztwm() + _params.getUzfwm()) );
                               
                _state.setUztwc(( _params.getUztwm() * uzrat));
                
                _state.setUzfwc((_params.getUzfwm() * uzrat));
				//trace("near 220, _uzfwc = " + _uzfwc);
            }
        }
        
        // line 225 equiv
        //trace("225");
        if (_state.getUztwc() < 0.00001)
        {
            _state.setUztwc(0.0);
        }
        
        if ( _state.getUzfwc() < 0.00001)
        {		
            
            _state.setUzfwc(0.0);
			//trace("near 225, _uzfwc set to 0.0");
        }
        
        // compute et from the lower zone
        // compute et from LZTWC (E3)
        
        e3 = residualEvapTransDemand * (_state.getLztwc() / 
                                        ( _params.getUztwm() +
                                         _params.getLztwm()) );
        
        _state.setLztwc(_state.getLztwc() - e3);
         
        if (_state.getLztwc() < 0.0 ) //simulates the goto 226 stuff
        {
           // e3 cannot exceed lztwc
           e3 += _state.getLztwc();
           
           _state.setLztwc(0.0);
        }
       
		//trace("226");
		
		//simulates line 226
        double ratlzt = _state.getLztwc() / _params.getLztwm();
        double ratlz = (_state.getLztwc() + _state.getLzfpc() + _state.getLzfsc() - getSaved()) / 
                       (_params.getLztwm() + _params.getLzfpm() + _params.getLzfsm() - getSaved());
        
        if (ratlzt < ratlz) 
        {
             // resupply lower zone tension water from lower
             // zone free water if more water available there
             
             double del = (ratlz -ratlzt) * _params.getLztwm();
             
             // transfer from lzfwc to lztwc
             
             
            _state.setLztwc(_state.getLztwc() + del); 
            _state.setLzfsc(_state.getLzfsc() - del);
            
             
             if (_state.getLzfsc() < 0.0) 
             {
                 //if transfer exceeds lzfsc then remainder
                 // comes from lzfpc
                 _state.setLzfpc(_state.getLzfpc() +  _state.getLzfsc());
                 _state.setLzfsc(0.0);
             }
        }
      
       
       //line 230 stuff
	   //trace("230");
       if (_state.getLztwc() < 0.00001)
       {
           _state.setLztwc(0.0);
       }
       
       // compute et from _adimp area - e5
       double e5 = e1 + (residualEvapTransDemand + e2) *
                 ((_state.getAdimc() -e1 - _state.getUztwc())/(_params.getUztwm() + _params.getLztwm()));
                 
       //adjust adminc, additional impervious area storage, for evaporation
       
       
       _state.setAdimc(_state.getAdimc() - e5);
       
       if (_state.getAdimc() < 0.0) 
       {
           // e5 cannot exceed adminc
           e5 += _state.getAdimc();
           
           _state.setAdimc(0.0);
       }
       
       //simulates line 231
	   //trace("231");
       e5 *= _params.getAdimp();
       // e5 is et from the area _adimp
       
       //--------------------------
       // compute percolation and runoff amounts
       
       double twx = rainfallPlusMelt + _state.getUztwc() - _params.getUztwm();
       // twx is the time interval  available moisture in excess of 
       // uztw requirements
       
       if (twx < 0.0)
       {
          //all moisture held in uztw - no excess
          _state.setUztwc(_state.getUztwc() + rainfallPlusMelt);
          twx = 0.0;
          
       }
       else //(twc >= 0.0) //simulates line 232
       {
           _state.setUztwc(_params.getUztwm()); 
       }
       
       //line 233
	   //trace("233");
       
       _state.setAdimc(_state.getAdimc() + ( rainfallPlusMelt - twx) );
     
       //compute impervious area runoff
       _roimp =  rainfallPlusMelt * _params.getPctim();
       // roimp is runoff from the minimum impervious area
       
       _simpvt += _roimp;
       
       //initialize time interval sums
       
       _sbf = 0.0;
       _ssur = 0.0;
       _sif = 0.0;
       _sperc = 0.0;
       _sdro = 0.0;
       _spbf = 0.0;
       
       //determine computational time increments for the basic time interval
    
       //make sure that the rounding here is done the same way that fortran does 
       // it
       //incrementCount = ninc
       long ninc = (long) Math.floor(1.0 + (0.2 * ( _state.getUzfwc() + twx)));
    
       //ninc = number of time increments that the time interval is
       // is divided into for further soil-moisture accounting. no one increment
       // will exceed 5.0 mm of uzfwc + pav
       
       //dinc = incrementLength
       double dinc  = (1.0 / ninc) * dt;
       
       //trace("dinc RESULT = " + dinc + "with inputs:");
       //trace("ninc = " + ninc);
	   //trace("timeInterval = " + dt);
	   //trace("END dinc block");
       
       
       // dinc = length of each increment in days
       

       //_pinc = _moisturePerIncrement
       _pinc = twx / ninc;
       //  moisturePerIncrement = amount of available moisture per increment
       // compute free water depletion fractions for the time increment being use
       // basic depletions are for one day
       
       _duz = 1.0 -  Math.pow((1.0 - _params.getUzk()),  dinc);
       _dlzp = 1.0 -  Math.pow((1.0 - _params.getLzpk()),  dinc);
       _dlzs = 1.0 -  Math.pow((1.0 - _params.getLzsk()),  dinc);

       //trace("_dlzp result = " + _dlzp + " with inputs"); 
	   //trace("_lzpk = " + _lzpk);
       //trace("dinc = " + dinc);

  	   //trace("_dlzs result = " + _dlzs + " with inputs"); 
  	   //trace("_lzsk = " + _lzsk);
	   //trace("dinc = " + dinc);

    
       // start incremental do loop for the time interval
       
       for (int i = 0; i < ninc; i++)
       {
		    //trace("doIncrements loop, iteration # " + i);
           doIncrements();
       }
       
       
       //compute sums and adjust runoff amounts by the area over
       // which they are generated
       
       double eUsed = e1 + e2 + e3;
       //eUsed is the et from pArea which is 1.0-_adimp-pctim
       _sif *= getParea();
       
       //separate channel component of baseflow
       //from the non-channel component
       
       double tbf = _sbf * getParea();
       //tbf is toal baseflow
       
       double bfcc = tbf * (1.0/ (1.0 + _params.getSide()) );
       //bfcc is baseflow, channel component
       
        _bfp= _spbf* getParea()/(1.0 + _params.getSide());
       
       // error #2, had _bfs = bfcc = bfp;
       _bfs = bfcc - _bfp;
       
       if (_bfs < 0.0)
       {
          _bfs = 0.0;
       }
       
       double bfncc = tbf - bfcc;
       // bfncc is baseflow, non-channel component
       
       //add to monthly sums
       _sintft += _sif;
       _sgwfp += _bfp;
       _sgwfs += _bfs;
       _srecht += bfncc;
       _srost += _ssur;
       _srodt += _sdro;
       
       //compute total channel inflow for the time interval
       _tci = _roimp + _sdro + _ssur +_sif + bfcc;
       
       //compute e4-et from riparian vegetarian 
       double e4 = (evapTransDemand - eUsed) * _params.getRiva();
       
       //subtract e4 from channel inflow
       _tci = _tci - e4;
       
       if (_tci < 0.0)
       {
          e4 += _tci;
          _tci = 0.0;
       }
       
       //trace("250");
       _srot += _tci;
       
       //compute total evapotranspiration - tet   
       eUsed *= getParea();
       double tet = eUsed + e5 + e4;
       
       _sett += tet;
       
       _se1 += e1* getParea();
	   _se3 += e3 * getParea();
       
	   _se4 += e4;
       _se5 += e5;
       
       //check that adimc is >= uztwc
       if (  _state.getAdimc() < _state.getUztwc())
       {
           _state.setAdimc(  _state.getUztwc());
       }
       
    // add to sums of runoff components
    _rsum[0] += _tci;
    _rsum[1] += _roimp;
    _rsum[2] += _sdro;
    _rsum[3] += _ssur;
    _rsum[4] += _sif;
    _rsum[5] += _bfs;
    _rsum[6] += _bfp;
 
     return _tci;
    } //end calculate
    
    //---------------------------------------------------------------------
    
    private void doIncrements()
    {
      //put in lines from pages 2b to 3b
      double adsur = 0.0;
      
      //compute direct runoff (from _adimp area)
      double ratio = (  _state.getAdimc() - _state.getUztwc()) / _params.getLztwm();
      
      if (ratio <  0.0)
      {
         ratio = 0.0;
      }
      
      double addro = _pinc * Math.pow(ratio, 2);
      //addro is the amount of direct runoff from the area _adimp
      
      //compute baseflow and keep track of time interval sum
      
      double bf =  _state.getLzfpc() * _dlzp;
      
      _state.setLzfpc( _state.getLzfpc() - bf);
      
      
      if ( _state.getLzfpc() <= 0.0001) 
      {
         bf +=  _state.getLzfpc();
         _state.setLzfpc(0.0);
      }
      
      //simulates line 234
	  //trace("234");
      _sbf += bf;
      _spbf += bf;
      bf = _state.getLzfsc() * _dlzs;
      
      
      _state.setLzfsc(_state.getLzfsc()- bf);
      if (_state.getLzfsc() <= 0.0001) 
      {
         bf += _state.getLzfsc();
         _state.setLzfsc(0.0);
      }
      
      //line 235
	  //trace("235");
      _sbf += bf;
      
      //computer percolation - if no water available then skip
      
      if ((_pinc +  _state.getUzfwc()) <= 0.01)
      {
          
          _state.setUzfwc( _state.getUzfwc() + _pinc);
  		  //trace("near 235, _uzfwc = " + _uzfwc);
          
          //skip most of the rest//need to goto 249
      }
      else //water available line  // 251 equiv
      {
		//trace("251");
          double percm = (_params.getLzfpm() * _dlzp) + (_params.getLzfsm()* _dlzs);
          double perc = percm * ( _state.getUzfwc() / _params.getUzfwm());

          //trace("result percm = " + percm + " with inputs :");
          //trace("_lzfpm = " + _lzfpm);
 		  //trace("dlzp = " + _dlzp);
 		  //trace("_lzfsm = " + _lzfsm);
 		  //trace("_dlzs = " + _dlzs);

          //trace("result perc = " + perc + " with inputs:" );
          //trace("percm = " + percm);
          //trace("_uzfwc = " + _uzfwc);
		  //trace("_uzfwm = " + _uzfwm);

 

          double defr = 1.0 - ( (_state.getLztwc() +  _state.getLzfpc() + _state.getLzfsc()) / (_params.getLztwm() + _params.getLzfpm() + _params.getLzfsm()) );
          //trace ("near 251 defr = " + defr);
         
 		  //trace ("_lztwc = " + _lztwc);
 		  //trace ("_lzfpc = " + _lzfpc);
 		  //trace ("_lzfsc = " + _lzfsc);
 		  //trace ("_lztwm = " + _lztwm);
 		  //trace ("_lzfpm = " + _lzfpm);
 		  //trace ("_lzfsm = " + _lzfsm);
 		  //trace ("end of 251 defr block");


		  //defr is the lower zone moisture deficiency ratio
          
          double fr = 1.0;
          //fr is the change in percolation withdrawal due to frozen ground
          
          double fi = 1.0;
          // fi is the change in interflow withdrawal due to frozen ground
          
          //if (useFrozenGroundCalc) 
          //{
          //    _uzdefr = 1.0 - ((_uztwc+_uzfwc) / (_uztwm + _uzfwm ));
          //    frozenGround(); 
          //}
          
          //error # 3
          //trace("input perc = " + perc );
          perc = perc * (1.0 + (_params.getZperc() * Math.pow(defr, _params.getRexp()))) * fr;
		  //trace("before 241,  result perc = " + perc);          
		  //trace("_zperc = " + _zperc);
  		  //trace("defr = " + defr);
  		  //trace("_rexp = " + _rexp);
  		  //trace("fr = " + fr);
          //trace("end perc print block");


          //Note: percolation occurs from uzfwc before pav is added
          if (perc >=  _state.getUzfwc())
          {
              //percolation rate exceeds uzfwc
          
              perc =  _state.getUzfwc();
          }
          
          //trace("241");
          //error #4 - found 6/30/03


          _state.setUzfwc(_state.getUzfwc() - perc);
		  //trace("near 241, _uzfwc = " + _uzfwc);
          
          // check to see if percolation exceeds lower zone deficiency
          
          double check = _state.getLztwc() +  _state.getLzfpc() + _state.getLzfsc() + perc - _params.getLztwm() - _params.getLzfpm() -_params.getLzfsm();
          if (check > 0.0) 
          {
              perc -= check;
              
              _state.setUzfwc(_state.getUzfwc() + check);
			  //trace("near 241, part 2, _uzfwc = " + _uzfwc);
              
          }
          
          //trace("242");
          _sperc += perc;
          //sperc is the time interval summation of perc
          
          //compute interflow and keep track of time interval sum
          // note: moisturePerIncrement has not yet been added
          
          double del =  _state.getUzfwc() * _duz * fi;
          _sif += del;

		  //trace("near 242, _uzfwc = " + _uzfwc);
          _state.setUzfwc(_state.getUzfwc() - del);
          
          //distribute percolated water into the lower zones
          //tension water must be filled first except for the pfree area
          //perct is percolation to tension water and perfc is percolation 
          //going to free water
          double percf = 0.0;
          
          double perct = perc * (1.0 - _params.getPfree());
          if ((perct + _state.getLztwc()) <=  _params.getLztwm())
          {
              _state.setLztwc(_state.getLztwc() + perct);
              percf = 0.0;
          }
          else // (perct + _lztwc) >  _lztwm) // simulates line 234
          {
			  //trace("234");
              percf = perct + _state.getLztwc() - _params.getLztwm();
              
              _state.setLztwc(_params.getLztwm());
          }
          
          //distribute percolation in excess tension requirements
          //among the free water storages
          
          
          //line 244 equiv
		  //trace("244");
          percf += (perc * _params.getPfree());
          
          if (percf != 0.0)  // may have problem with exact equivalence
          {
              double hpl = _params.getLzfpm() / (_params.getLzfpm() + _params.getLzfsm());
              //hpl is the relative size of the primary storage
              //as compared with toal lower zone free water storage
              
              double ratlp =  _state.getLzfpc() / _params.getLzfpm();
              double ratls = _state.getLzfsc() / _params.getLzfsm();
              //ratlp and ratls are content to capacity rations, or
              //in other words, the realtive fullness of each storage
              
              double fracp = (hpl*2.0 * (1.0 - ratlp)) /
                      (( 1.0 - ratlp) + (1.0 - ratls));
                 
              //fracp is the fraction going to primary
              if (fracp > 1.0) 
              {
                 fracp = 1.0;
              }
              
              double percp = percf * fracp;
              double percs = percf - percp;
              
              //percp and percs are the amount of the excess
              //percolation going to primary and supplemental
              //storages, respectively
              
              _state.setLzfsc(_state.getLzfsc() + percs);
              
              if (_state.getLzfsc() > _params.getLzfsm()) 
              {
                  percs = percs - _state.getLzfsc() + _params.getLzfsm();
                  _state.setLzfsc(_params.getLzfsm());
              }
            
            
              //line 246 equiv
			  //trace("246");
              
              _state.setLzfpc( _state.getLzfpc() + (percf-percs));
            
              //check to make sure _lzfpc does not exceed _lzfpm
              
              if (_state.getLzfpc() > _params.getLzfpm()) 
              {
                 double excess = _state.getLzfpc() - _params.getLzfpm();
                 
                 _state.setLztwc(_state.getLztwc() + excess);
                 
                 _state.setLzfpc(_params.getLzfpm());
                 
              }
          }
         
          //line 245 equiv
		  //trace("245");
          //distribute moisturePerIncrement between _uzfwc and surface runoff
          if (_pinc != 0.0)
          {
              //check if moisturePerIncrement exceeds _uzfwm
              
              if ((_pinc +  _state.getUzfwc()) <= _params.getUzfwm()) 
              {
                   //no surface runoff
                  _state.setUzfwc(_state.getUzfwc() + _pinc);
					//trace("near 245, _uzfwc = " + _uzfwc);
                   //simulate goto 249 - it will flow through correctly as is
              }  
              else
              {
                 //simulate line 248
				 //trace("248");
                 //compute surface runoff (sur) and keep track of time
                 // interval sum
                 double sur = _pinc +  _state.getUzfwc() - _params.getUzfwm();
                 

                 _state.setUzfwc(_params.getUzfwm());
                 //trace("near 248, _uzfwc = " + _uzfwc);

                 _ssur += (sur* getParea());
                 adsur = sur*(1.0 - (addro/_pinc) );
                 //adsur is the amount of surface runoff which comes
                 //from that portion of _adimp which is not
                 //currently generating direct runoff. addro/moisturePerIncrement
                 // is the fraction of _adimp currently generating
                 // direct runoff
                 
                 _ssur += (adsur*_params.getAdimp());
                 
                
              }
          }
      }
      
      //line 249 equiv
	  //trace("249");
      
      _state.setAdimc(_state.getAdimc() + ((_pinc) - addro) - adsur );
      if (_state.getAdimc() > _params.getUztwm() + _params.getLztwm())
      {
      	  addro += (_state.getAdimc()) - (_params.getUztwm() + _params.getLztwm());
          
          _state.setAdimc(_params.getUztwm() + _params.getLztwm());
      	
      }	
     
      //trace("247");
      _sdro += (addro*_params.getAdimp());
      if (_state.getAdimc() < 0.00001)
      {
         _state.setAdimc(0.0);
      }
   
      
    } //end doIncrements
    
	// --------------------------------------------------------------------------
   
    
    public String toString()
    {
    	String stateString = getState().toString();
    	String paramString = getParameters().toString();
        String modelString = stateString + " " + paramString;
        
        return modelString;	
    }
    
    // --------------------------------------------------------------------------


/*
    public void setupTestLogFiles()
    {
        try
        {
            _runoffWriter =
               new PrintWriter(new FileOutputStream("D:/Data/code/SiteSpecific/output/java_runoff.out"));

            _internalsWriter = 
                new PrintWriter(new FileOutputStream("D:/Data/code/SiteSpecific/output/java_internals.out"));
        }
        catch(java.io.IOException e)
        {
            System.out.println("Unable to open the output files.");
            System.exit(1);
        }
    }
*/
    // --------------------------------------------------------------------------
   
   
    public RainfallRunoffModelType getModelType()
    {
       return RainfallRunoffModelType.SAC_SMA;
    } 
   
    // --------------------------------------------------------------------------
   
} //end class SacSmaModel
