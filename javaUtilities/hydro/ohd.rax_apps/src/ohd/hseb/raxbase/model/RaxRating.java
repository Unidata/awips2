package ohd.hseb.raxbase.model;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxdb.generated.RaxRatingRecord;
import ohd.hseb.util.StringDataConverter;


public class RaxRating
{
    public static final short MISSING = -9999;

    private String _lid = null;
    private String _pe = null;
    private double _table = MISSING;
    private long _validDate = MISSING;
    private String _src = null;
    private String _othagid = null;
    private boolean _rfsInput = false;
    private List _raxRatingPointList = new ArrayList();
    private String _units = null;
    private String _interpolate = null;
    private List _raxRatingOffsetList = new ArrayList();
    private double _allowStage = MISSING;
    
    public RaxRating(){}
    
    public RaxRating( String lid, List raxRatingPointList )
    {
        setLid( lid );
        setRaxRatingPointList( raxRatingPointList );
    }
    
    public RaxRating( RaxRating raxRating )
    {
        setLid( raxRating.getLid() );
        setPe( raxRating.getPe() );
        setTable( raxRating.getTable() );
        setValidDate( raxRating.getValidDate() );
        setSrc( raxRating.getSrc() );
        setOthagid( raxRating.getOthagid() );
        setRfsInput( raxRating.isRfsInput() );
        setRaxRatingPointList( raxRating.getRaxRatingPointList() );
        setUnits( raxRating.getUnits() );
        setInterpolate( raxRating.getInterpolate() );
        setOffsets( raxRating.getOffsets() );
        setAllowStage( raxRating.getAllowStage() );
    }
    
    public String toString()
    {
        return "Lid = " + _lid + " | Pe = " + _pe + " | RatingTableNumber = " + _table +
        " | ValidDate = " + DbTimeHelper.getDateTimeStringFromLongTime( _validDate ) + 
        " | Source = " + _src + " | OthAgID = " + _othagid + " | RfsInput = " + _rfsInput + 
        " | RaxRatingPointList = " + _raxRatingPointList + " | Units = " + _units + 
        " | Interpolate = " + _interpolate + " | Offsets = " + _raxRatingOffsetList +
        " | AllowStage = " + _allowStage;
    }

    public RaxRating( RaxRatingRecord raxRatingRecord )
    {
        String rfsInput = raxRatingRecord.getRfs_input();
        List raxRatingPointRecordsList = new ArrayList();
        
        setLid( raxRatingRecord.getLid() );
        setPe( PEManager.getPEFromPe1Pe2( raxRatingRecord.getPe1(), raxRatingRecord.getPe2() ) );
        setTable( raxRatingRecord.getTbl() );
        setValidDate( raxRatingRecord.getValid_date() );
        setSrc( raxRatingRecord.getSrc() );
        setOthagid( raxRatingRecord.getOthagid() );
        if ( ( rfsInput != null ) &&
             ( rfsInput.equalsIgnoreCase( "Y" ) ||
             ( rfsInput.equalsIgnoreCase( "1" ) ) ) )
        {
            setRfsInput( true );
        }
        else
        {
            setRfsInput( false );
        }
        
        setRaxRatingPointList( getRaxRatingPointListFromRecord( raxRatingRecord.getStgflow() ) );
        
        setUnits( raxRatingRecord.getUnits() );
        setInterpolate( raxRatingRecord.getInterpolate() );
        setOffsets( getRaxRatingOffsetListFromRecord( raxRatingRecord.getOffsets() ) );
        setAllowStage( raxRatingRecord.getAllowstg() );
    }
    
    public static RaxRating getRaxRating( RaxRatingRecord record )
    {
        return ( new RaxRating( record ) );
    }
    
    public String keyString()
    {
        return "Lid = " + _lid + " | PE = " + _pe + " Tbl = " + _table + " ValidDate = " + DbTimeHelper.getDateStringFromLongTime( _validDate ) + " Src = " + _src;
    }

    public static RaxRatingRecord getRaxRatingRecord( RaxRating raxRating )
    {
        RaxRatingRecord record = new RaxRatingRecord();
        boolean rfsInput = raxRating.isRfsInput();

        record.setLid( raxRating.getLid() );
        record.setPe1( PEManager.getPe1FromPE( raxRating.getPe() ) );
        record.setPe2( PEManager.getPe2FromPE( raxRating.getPe() ) );
        record.setTbl( raxRating.getTable() );
        record.setValid_date( raxRating.getValidDate() );
        record.setSrc( raxRating.getSrc() );
        record.setOthagid( raxRating.getOthagid() );
        
        if ( rfsInput )
        {
            record.setRfs_input( "Y" );
        }
        else
        {
            record.setRfs_input( "N" );
        }

        record.setStgflow( getStgFlowFromRaxRatingPointList( raxRating.getRaxRatingPointList() ) );
        record.setUnits( raxRating.getUnits() );
        record.setInterpolate( raxRating.getInterpolate() );
        record.setOffsets( getOffSetsFromRaxOffsetsList( raxRating.getOffsets() ) );
        record.setAllowstg( raxRating.getAllowStage() );
        
        return record;
    }
    
    private static String getStgFlowFromRaxRatingPointList( List raxRatingPointList )
    {
        StringBuffer stgFlowStringBuffer = null;
        RaxRatingPoint raxRatingPoint = new RaxRatingPoint();
        String returnString = null;
        
        if ( ( raxRatingPointList != null ) && ( ! raxRatingPointList.isEmpty() ) )
        {
            stgFlowStringBuffer = new StringBuffer();
            stgFlowStringBuffer.append( "{{" );

            for ( int i = 0; i < raxRatingPointList.size(); i++ )
            {
                raxRatingPoint = (RaxRatingPoint) raxRatingPointList.get( i );

                stgFlowStringBuffer.append( raxRatingPoint.getStage() + "," + raxRatingPoint.getDischarge() + "}" );

                if ( i == ( raxRatingPointList.size() - 1 ) )
                {
                    stgFlowStringBuffer.append( "}" );
                }
                else
                {
                    stgFlowStringBuffer.append( ",{"  );
                }
            }
        }
        
        if ( stgFlowStringBuffer != null )
        {
            returnString = stgFlowStringBuffer.toString();
        }
        return returnString;
    }
    
    private static String getOffSetsFromRaxOffsetsList( List raxOffsetsList )
    {
        StringBuffer offsetsStringBuffer = null;
        RaxRatingOffset raxRatingOffset = new RaxRatingOffset();
        String returnString = null;
        
        if ( ( raxOffsetsList != null ) && ( ! raxOffsetsList.isEmpty() ) )
        {
            offsetsStringBuffer = new StringBuffer();
            
            offsetsStringBuffer.append( "{{" );

            for ( int i = 0; i < raxOffsetsList.size(); i++ )
            {
                raxRatingOffset = (RaxRatingOffset) raxOffsetsList.get( i );

                offsetsStringBuffer.append( raxRatingOffset.getStage() + "," + raxRatingOffset.getOffset() + "}" );

                if ( i == ( raxOffsetsList.size() - 1 ) )
                {
                    offsetsStringBuffer.append( "}" );
                }
                else
                {
                    offsetsStringBuffer.append( ",{" );
                }
            }
        }
        if ( offsetsStringBuffer != null )
        {
            returnString = offsetsStringBuffer.toString();
        }
        return returnString;
    }
    
    private List getRaxRatingPointListFromRecord( String stageFlowString )
    {
        List raxRatingPointList = new ArrayList();
        RaxRatingPoint ratingPoint = new RaxRatingPoint();
        StringDataConverter converter = new StringDataConverter();
        
        if ( stageFlowString != null )
        {
            StringTokenizer tokens = new StringTokenizer( stageFlowString, "},{" );

            // {{-5.36,0},{16.5,58529},{18,64562},{20,72081},{21,79601}}

            while ( tokens.hasMoreTokens() )
            {
                ratingPoint = new RaxRatingPoint();

                ratingPoint.setStage( converter.getDoubleValue( tokens.nextToken() ) );
                ratingPoint.setDischarge( converter.getDoubleValue( tokens.nextToken() ) );

                raxRatingPointList.add( ratingPoint );
            }
        }
        return raxRatingPointList;
    }
    
    private List getRaxRatingOffsetListFromRecord( String offSetsString )
    {
        List raxRatingOffsetsList = new ArrayList();

        if ( offSetsString != null )
        {
            RaxRatingOffset offset = new RaxRatingOffset();
            StringDataConverter converter = new StringDataConverter();

            StringTokenizer tokens = new StringTokenizer( offSetsString, "},{" );

            while ( tokens.hasMoreTokens() )
            {
                offset = new RaxRatingOffset();

                offset.setStage( converter.getDoubleValue( tokens.nextToken() ) );
                offset.setOffset( converter.getDoubleValue( tokens.nextToken() ) );

                raxRatingOffsetsList.add( offset );
            }
        }
        return raxRatingOffsetsList; 
    }

/**
 * 
 * @param stage - Stage value
 * @return discharge - Extracts the discharge value for the given Stage from the RaxRatingPointList.  If the stage is not found, a value of MISSING (-9999) is returned
 */
    public double getDischarge( double stage )
    {
        double discharge = MISSING;
        
        for ( int i = 0; i < _raxRatingPointList.size(); i++ )
        {
            RaxRatingPoint raxRatingPoint = (RaxRatingPoint) _raxRatingPointList.get( i );
            
            if ( raxRatingPoint.getStage() == stage )
            {
                discharge = raxRatingPoint.getDischarge();
                break;
            }
        }
        
        return discharge;
    }
    
    public void addOffset( RaxRatingOffset raxRatingOffset )
    {
        int index = getStageIndexFromOffsetsList( raxRatingOffset.getStage() );

        if ( index != MISSING )
        {
            if ( doesStageExistInOffsetList( raxRatingOffset.getStage() ) )
            {
                _raxRatingOffsetList.set( index, raxRatingOffset );
            }
            else
            {
                _raxRatingOffsetList.add( index, raxRatingOffset );
            }
        }
        else
        {
            _raxRatingOffsetList.add( raxRatingOffset );
        }
    }
    
    private boolean doesStageExistInOffsetList( double stage )
    {
        boolean exists = false;
        
        for ( int i = 0; i < _raxRatingOffsetList.size(); i++ )
        {
            RaxRatingOffset raxRatingOffset = (RaxRatingOffset) _raxRatingOffsetList.get( i );
    
            if ( raxRatingOffset.getStage() == stage )
            {
                exists = true;
                break;
            }
        }
        return exists;
    }
    
    private int getStageIndexFromOffsetsList( double stage )
    {
        int index = MISSING;
        
        for ( int i = 0; i < _raxRatingOffsetList.size(); i++ )
        {
            RaxRatingOffset raxRatingOffset = (RaxRatingOffset) _raxRatingOffsetList.get( i );
            
            if ( raxRatingOffset.getStage() >= stage )
            {
                index = i;
                break;
            }
        }
        return index;
    }
    
    public void addRatingPoint( RaxRatingPoint raxRatingPoint )
    {
        int index = getStageIndexFromRatingPointList( raxRatingPoint.getStage() );

        if ( index != MISSING )
        {
            if ( doesStageExistInRatingPointList( raxRatingPoint.getStage() ) )
            {
                _raxRatingPointList.set( index, raxRatingPoint );
            }
            else
            {
                _raxRatingPointList.add( index, raxRatingPoint );
            }
        }
        else
        {
            _raxRatingPointList.add( raxRatingPoint );
        }
    }
    
    private int getStageIndexFromRatingPointList( double stage )
    {
        int index = MISSING;
        
        for ( int i = 0; i < _raxRatingPointList.size(); i++ )
        {
            RaxRatingPoint raxRatingPoint = (RaxRatingPoint) _raxRatingPointList.get( i );
            
            if ( raxRatingPoint.getStage() >= stage )
            {
                index = i;
                break;
            }
        }
        return index;
    }
    
    private boolean doesStageExistInRatingPointList( double stage )
    {
        boolean exists = false;
        
        for ( int i = 0; i < _raxRatingPointList.size(); i++ )
        {
            RaxRatingPoint raxRatingPoint = (RaxRatingPoint) _raxRatingPointList.get( i );
    
            if ( raxRatingPoint.getStage() == stage )
            {
                exists = true;
                break;
            }
        }
        return exists;
    }


    public void removeRatingPoint( RaxRatingPoint raxRatingPoint )
    {
        int index = getStageIndexFromRatingPointList( raxRatingPoint.getStage() );
        
        _raxRatingPointList.remove( index );
    }
    
    public void removeOffset( RaxRatingOffset raxRatingOffset )
    {
        int index = getStageIndexFromOffsetsList( raxRatingOffset.getStage() );
        
        _raxRatingOffsetList.remove( index );
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

    public void setTable( double table )
    {
        _table = table;
    }

    public double getTable()
    {
        return _table;
    }

    public void setValidDate( long validDate )
    {
        _validDate = validDate;
    }

    public long getValidDate()
    {
        return _validDate;
    }

    public void setSrc( String src )
    {
        _src = src;
    }

    public String getSrc()
    {
        return _src;
    }

    public void setOthagid( String othagid )
    {
        _othagid = othagid;
    }

    public String getOthagid()
    {
        return _othagid;
    }

    public void setRfsInput( boolean rfsInput )
    {
        _rfsInput = rfsInput;
    }

    public boolean isRfsInput()
    {
        return _rfsInput;
    }

    public void setRaxRatingPointList( List raxRatingPointList )
    {
        _raxRatingPointList = raxRatingPointList;
    }

    public List getRaxRatingPointList()
    {
        return _raxRatingPointList;
    }

    public void setUnits( String units )
    {
        _units = units;
    }

    public String getUnits()
    {
        return _units;
    }

    public void setInterpolate( String interpolate )
    {
        _interpolate = interpolate;
    }

    public String getInterpolate()
    {
        return _interpolate;
    }

    public void setOffsets( List raxRatingOffsets )
    {
        _raxRatingOffsetList = raxRatingOffsets;
    }

    public List getOffsets()
    {
        return _raxRatingOffsetList;
    }

    public void setAllowStage( double allowStage )
    {
        _allowStage = allowStage;
    }

    public double getAllowStage()
    {
        return _allowStage;
    }

}
