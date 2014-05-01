package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.ProdRecord;

public class Prod
{
    private static final short MISSING = -9999;
    
    private String _id = null;
    private int _pmax = MISSING;
    private int _pap = MISSING;
    private int _err = MISSING;
    private String _gra = null;
    private String _pr1 = null;
    private String _net = null;

    public Prod(){}
    
    public Prod( Prod prod )
    {
        setId( prod.getId() );
        setPmax( prod.getPmax() );
        setPap( prod.getPap() );
        setErr( prod.getErr() );
        setGra( prod.getGra() );
        setPr1( prod.getPr1() );
        setNet( prod.getNet() );
    }
    
    public Prod( ProdRecord prodRecord )
    {
        setId( prodRecord.getId() );
        setPmax( prodRecord.getPmax() );
        setPap( prodRecord.getPap() );
        setErr( prodRecord.getErr() );
        setGra( prodRecord.getGra() );
        setPr1( prodRecord.getPr1() );
        setNet( prodRecord.getNet() );
    }
    
    public static Prod getProd( ProdRecord prodRecord )
    {
        return new Prod( prodRecord );
    }
    
    public static ProdRecord getProdRecord( Prod prod )
    {
        ProdRecord record = new ProdRecord();
        
        record.setId( prod.getId() );
        record.setPmax( prod.getPmax() );
        record.setPap( prod.getPap() );
        record.setErr( prod.getErr() );
        record.setGra( prod.getGra() );
        record.setPr1( prod.getPr1() );
        record.setNet( prod.getNet() );

        return record;
    }

    public String toString()
    {
        return "Id = " + _id +
               " | Pmax = " + _pmax +
               " | Pap = " + _pap +
               " | Err = " + _err + 
               " | Gra = " + _gra +
               " | Pr1 = " + _pr1 +
               " | Net = " + _net;
    }
    
    public String keyString()
    {
        return "Id = " + _id;
    }
    
    public String getId()
    {
        return _id;
    }

    public void setId(String id)
    {
        _id = id ;
    }

    public int getPmax()
    {
        return _pmax;
    }

    public void setPmax(int pmax)
    {
        _pmax = pmax ;
    }

    public int getPap()
    {
        return _pap;
    }

    public void setPap(int pap)
    {
        _pap = pap ;
    }

    public int getErr()
    {
        return _err;
    }

    public void setErr(int err)
    {
        _err = err ;
    }

    public String getGra()
    {
        return _gra;
    }

    public void setGra(String gra)
    {
        _gra = gra ;
    }

    public String getPr1()
    {
        return _pr1;
    }

    public void setPr1(String pr1)
    {
        _pr1 = pr1 ;
    }

    public String getNet()
    {
        return _net;
    }

    public void setNet(String net)
    {
        _net = net ;
    }
}