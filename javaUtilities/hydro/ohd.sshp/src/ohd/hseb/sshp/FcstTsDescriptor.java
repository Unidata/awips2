/*
 * Created on Dec 9, 2003
 *
 * Filename : FcstTsDescriptor.java 
 * Author   : Gautam Sood
 * Last Revision Date : Dec 9, 2003
 *  
 */
package ohd.hseb.sshp;

import java.text.*;


public class FcstTsDescriptor implements IngestFilterTsDescriptor
{
    private String _tableName =         null;
	private String _lid = 				null;
	private String _pe = 				null;
	private String _ts = 				null;
	private String _extremum = 			null;
	private String _shef_qual_code = 	null;
	private String _product_id = 		null;
	private double _value = 			0.0;
	private float _probability = 		0;
	private int _quality_code = 		0;
	private short _revision = 			0;
	private short _dur = 				0;
	private long _producttime = 		0;
	private long _postingtime = 		0;
	private long _validtime = 			0;
	private long _basistime = 			0;

	//*****************************************************************************
	// Empty constructor
	//*****************************************************************************
	public FcstTsDescriptor(String tableName)
	{
        setTableName(tableName);
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

	public void setDur( short dur )
	{
		_dur = dur;
	}

	public short getDur()
	{
		return _dur;
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

	public void setValue ( double value )
	{
		_value = value;
	}

	public double getValue()
	{
		return _value;
	}

	public void setShef_qual_code( String shef_qual_code )
	{
		_shef_qual_code = shef_qual_code;
	}

	public String getShef_qual_code()
	{
		return _shef_qual_code;
	}

	public void setQuality_code( int quality_code )
	{
		_quality_code = quality_code;
	}

	public int getQuality_code()
	{
		return _quality_code;
	}

	public void setRevision( short revision )
	{
		_revision = revision;
	}

	public short getRevision()
	{
		return _revision;
	}

	public void setProduct_id( String product_id )
	{
		_product_id = product_id;
	}

	public String getProduct_id()
	{
		return _product_id;
	}

	public void setProducttime( long producttime )
	{
		_producttime = producttime;
	}

	public long getProducttime()
	{
		return _producttime;
	}

	public void setPostingtime( long postingtime )
	{
		_postingtime = postingtime;
	}

	public long getPostingtime()
	{
		return _postingtime;
	}

	public void setProbability( float probability )
	{
		_probability = probability;
	}

	public float getProbability()
	{
		return _probability;
	}

	public void setValidtime( long validtime )
	{
		_validtime = validtime;
	}

	public long getValidtime()
	{
		return _validtime;
	}

	public void setBasistime( long basistime )
	{
		_basistime = basistime;
	}

	public long getBasistime()
	{
		return _basistime;
	}

	public String toString()
	{
		String formatString = "#####.#####";
		NumberFormat f = new DecimalFormat(formatString);

		String outString = "FcstHeight: " +
					" Lid = " + _lid +
					" Table NAme = " + _tableName + 
					" pe = " + _pe + 
					" ts = " + _ts + "\n" + 
					" Extremum = " + _extremum +
					" Shef_qual_code = " + _shef_qual_code +
					" Product ID = " + _product_id + "\n" +  
					" Value = " + f.format( _value ) +
					" Probability = " + f.format( _probability ) + 
					" Quality Code = " + _quality_code +  "\n" + 
					" Revision = " + _revision +
					" Dur = " + _dur + 
					" Product Time = " + _producttime +  "\n" + 
					" Posting Time = " + _postingtime + 
					" Valid Time = " + _validtime +
					" Basis Time = " + _basistime;
		return outString;
   }

    public void setTableName(String tableName)
    {
        _tableName = tableName;
    }

    public String getTableName()
    {
        return _tableName;
    }
}

