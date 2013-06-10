/*
 * Created on Dec 9, 2003
 *
 * Filename : Height.java 
 * Author   : Gautam Sood
 * Last Revision Date : Dec 9, 2003
 *  
 */
package ohd.hseb.sshp;

import java.text.*;


public class ProcTsDescriptor implements IngestFilterTsDescriptor
{
        
    private String _tableName =         null;
	private String _lid = 				null;
	private String _pe = 				null;
	private String _ts = 				null;
    private short     _dur=                0;
	private String _extremum = 			null;
	

	//*****************************************************************************
	// Empty constructor
	//*****************************************************************************
	public ProcTsDescriptor(String tableName) 
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
					" dur = " + _dur;
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

