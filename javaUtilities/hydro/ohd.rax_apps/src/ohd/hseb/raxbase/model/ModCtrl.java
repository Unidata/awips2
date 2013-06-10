package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.ModctrlRecord;

public class ModCtrl
{
    private String _modName = null;
    private boolean _load = false;
    private boolean _fetchOper = false;
    private boolean _fetchSpin = false;
    
    public ModCtrl(){}
    
    public ModCtrl( ModCtrl modCtrl )
    {
        setModName( modCtrl.getModName() );
        setLoad( modCtrl.isLoad() );
        setFetchOper( modCtrl.isFetchOper() );
        setFetchSpin( modCtrl.isFetchSpin() );
    }

    public ModCtrl( ModctrlRecord modCtrlRecord )
    {
        setModName( modCtrlRecord.getMod_name() );
        setLoad( getBooleanFromInt( modCtrlRecord.getLoad() ) );
        setFetchOper( getBooleanFromInt( modCtrlRecord.getFetch_oper() ) );
        setFetchSpin( getBooleanFromInt( modCtrlRecord.getFetch_spin() ) );
    }

    public static ModCtrl getModCtrl( ModctrlRecord modCtrlRecord )
    {
        return ( new ModCtrl( modCtrlRecord ) );
    }
    
    public static ModctrlRecord getModCtrlRecord( ModCtrl modCtrl )
    {
        ModctrlRecord record = new ModctrlRecord();
        
        record.setMod_name( modCtrl.getModName() );
        record.setLoad( getIntFromBoolean( modCtrl.isLoad() ) );
        record.setFetch_oper( getIntFromBoolean( modCtrl.isFetchOper() ) );
        record.setFetch_spin( getIntFromBoolean( modCtrl.isFetchSpin() ) );
        
        return record;
    }
    
    public String toString()
    {
        return "ModName = " + _modName + 
               " | Load = " + _load + 
               " | FetchOper = " + _fetchOper + 
               " | FetchSpin = " + _fetchSpin;
    }
    
    public String keyString()
    {
        return "ModName = " + _modName;
    }
    
    private boolean getBooleanFromInt( int intValue )
    {
        boolean booleanValue = false;
        
        if ( intValue == 1 )
        {
            booleanValue = true;
        }
        return booleanValue;
    }
    
    private static int getIntFromBoolean( boolean booleanValue )
    {
        int intValue = 0;
        
        if ( booleanValue == true )
        {
            intValue = 1;
        }
        
        return intValue;
    }
    
    public void setModName( String modName )
    {
        _modName = modName;
    }

    public String getModName()
    {
        return _modName;
    }

    public void setLoad( boolean load )
    {
        _load = load;
    }

    public boolean isLoad()
    {
        return _load;
    }

    public void setFetchOper( boolean fetchOper )
    {
        _fetchOper = fetchOper;
    }

    public boolean isFetchOper()
    {
        return _fetchOper;
    }

    public void setFetchSpin( boolean fetchSpin )
    {
        _fetchSpin = fetchSpin;
    }

    public boolean isFetchSpin()
    {
        return _fetchSpin;
    }
}
