package ohd.hseb.raxbase.util;

public class PEManager
{
    public static String getPEFromPe1Pe2( String p, String e )
    {
        return p + e;
    }
    
    public static String getPe1FromPE( String pe )
    {
        String pe1 = null;
        
        if ( pe != null )
        {
            pe1 = "" + pe.charAt( 0 );
        }
        return ( pe1 );
    }
    
    public static String getPe2FromPE( String pe )
    {
        String pe2 = null;
        
        if ( pe != null )
        {
            pe2 = "" + pe.charAt( 1 );
        }
        
        return ( pe2 );
    }
}
