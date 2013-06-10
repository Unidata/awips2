/*
 * Created on Jun 9, 2004
 *
 * Filename : RatingShiftEntry.java 
 * Author   : Gautam Sood
 * Last Revision Date : Jun 9, 2004
 *  
 */
/**
 * @author Gautam Sood
*/

package ohd.hseb.raxbase.model;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxdb.generated.RaxRatingShiftRecord;


public class RaxRatingShift
{
	private String _lid = null;
    private String _pe = null;
    private double _ratingTableNumber = 0.00;
	private long _beginDate = 0;
	private String _source = null;
    private double _valA = 0;
    private double _valB = 0;
    private double _valC = 0;
    private double _valD = 0;
    private double _shiftA = 0;
    private double _shiftB = 0;
    private double _shiftC = 0;
    private double _shiftD = 0;
    private double _datumAdjustment = 0;
    
	public RaxRatingShift()
	{
	}
	
	public RaxRatingShift( RaxRatingShift raxRatingShift )
	{
		setLid( raxRatingShift.getLid() );
        setPe( raxRatingShift.getPe() );
        setRatingTableNumber( raxRatingShift.getRatingTableNumber() );
        setBeginDate( raxRatingShift.getBeginDate() );
        setSource( raxRatingShift.getSource() );
        setValA( raxRatingShift.getValA() );
        setValB( raxRatingShift.getValB() );
        setValC( raxRatingShift.getValC() );
        setValD( raxRatingShift.getValD() );
        setShiftA( raxRatingShift.getShiftA() );
        setShiftB( raxRatingShift.getShiftB() );
        setShiftC( raxRatingShift.getShiftC() );
        setShiftD( raxRatingShift.getShiftD() );
        setDatumAdjustment( raxRatingShift.getDatumAdjustment() );
	}
    
    public String toString()
    {
        return "Lid = " + _lid + " | Pe = " + _pe + " | RatingTableNumber = " + _ratingTableNumber +
               " | BeginDate = " + DbTimeHelper.getDateTimeStringFromLongTime( _beginDate ) + 
               " | Source = " + _source + " | ValA-D = " + _valA + "|" + _valB + "|" + 
               "|" + _valC + "|" + _valD + " | ShiftA-D = " + _shiftA + "|" + _shiftB + 
               "|" + _shiftC + "|" + _shiftC + " | DatumAdjustment = " + _datumAdjustment;
    }
    
    public RaxRatingShift( RaxRatingShiftRecord raxRatingShiftRecord )
    {
        setLid( raxRatingShiftRecord.getLid() );
        setPe( PEManager.getPEFromPe1Pe2( raxRatingShiftRecord.getPe1(), raxRatingShiftRecord.getPe2() ) );
        setRatingTableNumber( raxRatingShiftRecord.getTbl_ver() );
        setBeginDate( raxRatingShiftRecord.getBegin_date() );
        setSource( raxRatingShiftRecord.getSrc() );
        setValA( raxRatingShiftRecord.getVal_a() );
        setValB( raxRatingShiftRecord.getVal_b() );
        setValC( raxRatingShiftRecord.getVal_c() );
        setValD( raxRatingShiftRecord.getVal_d() );
        setShiftA( raxRatingShiftRecord.getSh_a() );
        setShiftB( raxRatingShiftRecord.getSh_b() );
        setShiftC( raxRatingShiftRecord.getSh_c() );
        setShiftD( raxRatingShiftRecord.getSh_d() );
        setDatumAdjustment( raxRatingShiftRecord.getDatum_adj() );
    }
    
    public static RaxRatingShift getRaxRatingShift( RaxRatingShiftRecord raxRatingShiftRecord )
    {
        return ( new RaxRatingShift( raxRatingShiftRecord ) );
    }

    public static RaxRatingShiftRecord getRaxRatingShiftRecord( RaxRatingShift raxRatingShift )
    {
        RaxRatingShiftRecord raxRatingShiftRecord = new RaxRatingShiftRecord();
        
        raxRatingShiftRecord.setLid( raxRatingShift.getLid() );
        raxRatingShiftRecord.setPe1( PEManager.getPe1FromPE( raxRatingShift.getPe() ) );
        raxRatingShiftRecord.setPe2( PEManager.getPe2FromPE( raxRatingShift.getPe() ) );
        raxRatingShiftRecord.setTbl_ver( raxRatingShift.getRatingTableNumber() );
        raxRatingShiftRecord.setBegin_date( raxRatingShift.getBeginDate() );
        raxRatingShiftRecord.setSrc( raxRatingShift.getSource() );
        raxRatingShiftRecord.setVal_a( raxRatingShift.getValA() );
        raxRatingShiftRecord.setVal_b( raxRatingShift.getValB() );
        raxRatingShiftRecord.setVal_c( raxRatingShift.getValC() );
        raxRatingShiftRecord.setVal_d( raxRatingShift.getValD() );
        raxRatingShiftRecord.setSh_a( raxRatingShift.getShiftA() );
        raxRatingShiftRecord.setSh_b( raxRatingShift.getShiftB() );
        raxRatingShiftRecord.setSh_c( raxRatingShift.getShiftC() );
        raxRatingShiftRecord.setSh_d( raxRatingShift.getShiftD() );
        raxRatingShiftRecord.setDatum_adj( raxRatingShift.getDatumAdjustment() );
        
        return raxRatingShiftRecord;
    }

	public void setLid(String lid)
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

    public void setRatingTableNumber( double ratingTableNumber )
    {
        _ratingTableNumber = ratingTableNumber;
    }

    public double getRatingTableNumber()
    {
        return _ratingTableNumber;
    }

    public void setBeginDate( long beginDate )
    {
        _beginDate = beginDate;
    }

    public long getBeginDate()
    {
        return _beginDate;
    }

    public void setSource( String source )
    {
        _source = source;
    }

    public String getSource()
    {
        return _source;
    }

    public void setValA( double valA )
    {
        _valA = valA;
    }

    public double getValA()
    {
        return _valA;
    }

    public void setValB( double valB )
    {
        _valB = valB;
    }

    public double getValB()
    {
        return _valB;
    }

    public void setValC( double valC )
    {
        _valC = valC;
    }

    public double getValC()
    {
        return _valC;
    }

    public void setValD( double valD )
    {
        _valD = valD;
    }

    public double getValD()
    {
        return _valD;
    }

    public void setShiftA( double shiftA )
    {
        _shiftA = shiftA;
    }

    public double getShiftA()
    {
        return _shiftA;
    }

    public void setShiftB( double shiftB )
    {
        _shiftB = shiftB;
    }

    public double getShiftB()
    {
        return _shiftB;
    }

    public void setShiftC( double shiftC )
    {
        _shiftC = shiftC;
    }

    public double getShiftC()
    {
        return _shiftC;
    }

    public void setShiftD( double shiftD )
    {
        _shiftD = shiftD;
    }

    public double getShiftD()
    {
        return _shiftD;
    }

    public void setDatumAdjustment( double datumAdjustment )
    {
        _datumAdjustment = datumAdjustment;
    }

    public double getDatumAdjustment()
    {
        return _datumAdjustment;
    }

}
