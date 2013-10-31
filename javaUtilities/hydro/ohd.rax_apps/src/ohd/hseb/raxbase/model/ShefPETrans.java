package ohd.hseb.raxbase.model;

import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxdb.generated.ShefpetransRecord;

public class ShefPETrans
{
    private static final short MISSING = -9999;
    
    private String _pe = null;
    private String _codePosition = null;
    private int _codedValue = MISSING;
    private String _valueTrans = null;
    
    public ShefPETrans(){}
    
    public ShefPETrans( ShefPETrans shefPeTrans )
    {
        setPe( shefPeTrans.getPe() );
        setCodePosition( shefPeTrans.getCodePosition() );
        setCodedValue( shefPeTrans.getCodedValue() );
        setValueTrans( shefPeTrans.getValueTrans() );
    }
    
    public ShefPETrans( ShefpetransRecord shefPeTransRecord )
    {
        setPe( PEManager.getPEFromPe1Pe2( shefPeTransRecord.getPe1(), shefPeTransRecord.getPe2() ) );
        setCodePosition( shefPeTransRecord.getCode_position() );
        setCodedValue( shefPeTransRecord.getCoded_value() );
        setValueTrans( shefPeTransRecord.getValue_trans() );
    }
    
    public static ShefPETrans getShefPeTrans( ShefpetransRecord shefPeTransRecord )
    {
        return new ShefPETrans ( shefPeTransRecord );
    }
    
    public static ShefpetransRecord getShefPeTransRecord( ShefPETrans shefPeTrans )
    {
        ShefpetransRecord record = new ShefpetransRecord();
        
        record.setPe1( PEManager.getPe1FromPE( shefPeTrans.getPe() ) );
        record.setPe2( PEManager.getPe2FromPE( shefPeTrans.getPe() ) );
        record.setCode_position( shefPeTrans.getCodePosition() );
        record.setCoded_value( shefPeTrans.getCodedValue() );
        record.setValue_trans( shefPeTrans.getValueTrans() );
        
        return record;
    }
    
    public String toString()
    {
        return "PE = " + _pe + 
               " | CodePosition = " + _codePosition +
               " | CodedValue = " + _codedValue +
               " | ValueTrans = " + _valueTrans;
    }
    
    public String keyString()
    {
        return "PE = " + _pe +
               " | CodedValue = " + _codedValue +
               " | CodePosition = " + _codePosition;
    }
    
    public void setPe( String pe )
    {
        _pe = pe;
    }

    public String getPe()
    {
        return _pe;
    }

    public void setCodePosition( String codePosition )
    {
        _codePosition = codePosition;
    }

    public String getCodePosition()
    {
        return _codePosition;
    }

    public void setCodedValue( int codedValue )
    {
        _codedValue = codedValue;
    }

    public int getCodedValue()
    {
        return _codedValue;
    }

    public void setValueTrans( String valueTrans )
    {
        _valueTrans = valueTrans;
    }

    public String getValueTrans()
    {
        return _valueTrans;
    }

}
