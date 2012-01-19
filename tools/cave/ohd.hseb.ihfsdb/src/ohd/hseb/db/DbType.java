/*
 * Created on Sep 17, 2004
 *
 *
 */
package ohd.hseb.db;

/**
 * @author GobsC
 *
 *
 */
public class DbType
{
    public static final DbType Informix = new DbType("Informix");
    public static final DbType PostgreSQL = new DbType("PostgreSQL");
    
    private static final DbType[] DBTypeArray = { Informix, PostgreSQL };
    
    private String _name = null;
    // -----------------------------------------------------------------------
    private DbType(String name)
    {
        _name = name;
        
        return;
        
    }
    
//  -----------------------------------------------------------------------
    public boolean equals(Object object)
    {
        boolean result = false;
        
        DbType type = (DbType) object;
        
        if (this._name.equals(type._name))
        {
            result = true;
        }
        
        return result;
    }
 
//  -----------------------------------------------------------------------

    public String toString()
    {
        return _name;   
    }
//  -----------------------------------------------------------------------
}
