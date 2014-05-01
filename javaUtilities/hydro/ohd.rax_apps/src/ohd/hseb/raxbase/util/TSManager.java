package ohd.hseb.raxbase.util;

public class TSManager
{
    public static String getTSFromTandS( String t, String s )
    {
        return t + s;
    }
    
    public static String getTFromTS( String ts )
    {
        return ( "" + ts.charAt( 0 ) );
    }
    
    public static String getSFromTS( String ts )
    {
        return ( "" + ts.charAt( 1 ) );
    }
}
